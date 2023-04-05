{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A GtkFlowBox positions child widgets in sequence according to its
-- orientation.
-- 
-- For instance, with the horizontal orientation, the widgets will be
-- arranged from left to right, starting a new row under the previous
-- row when necessary. Reducing the width in this case will require more
-- rows, so a larger height will be requested.
-- 
-- Likewise, with the vertical orientation, the widgets will be arranged
-- from top to bottom, starting a new column to the right when necessary.
-- Reducing the height will require more columns, so a larger width will
-- be requested.
-- 
-- The size request of a GtkFlowBox alone may not be what you expect; if you
-- need to be able to shrink it along both axes and dynamically reflow its
-- children, you may have to wrap it in a t'GI.Gtk.Objects.ScrolledWindow.ScrolledWindow' to enable that.
-- 
-- The children of a GtkFlowBox can be dynamically sorted and filtered.
-- 
-- Although a GtkFlowBox must have only t'GI.Gtk.Objects.FlowBoxChild.FlowBoxChild' children,
-- you can add any kind of widget to it via 'GI.Gtk.Objects.Container.containerAdd', and
-- a GtkFlowBoxChild widget will automatically be inserted between
-- the box and the widget.
-- 
-- Also see t'GI.Gtk.Objects.ListBox.ListBox'.
-- 
-- GtkFlowBox was added in GTK+ 3.12.
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >flowbox
-- >├── flowboxchild
-- >│   ╰── <child>
-- >├── flowboxchild
-- >│   ╰── <child>
-- >┊
-- >╰── [rubberband]
-- 
-- 
-- GtkFlowBox uses a single CSS node with name flowbox. GtkFlowBoxChild
-- uses a single CSS node with name flowboxchild.
-- For rubberband selection, a subnode with name rubberband is used.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.FlowBox
    ( 

-- * Exported types
    FlowBox(..)                             ,
    IsFlowBox                               ,
    toFlowBox                               ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindModel]("GI.Gtk.Objects.FlowBox#g:method:bindModel"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insert]("GI.Gtk.Objects.FlowBox#g:method:insert"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [invalidateFilter]("GI.Gtk.Objects.FlowBox#g:method:invalidateFilter"), [invalidateSort]("GI.Gtk.Objects.FlowBox#g:method:invalidateSort"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [selectAll]("GI.Gtk.Objects.FlowBox#g:method:selectAll"), [selectChild]("GI.Gtk.Objects.FlowBox#g:method:selectChild"), [selectedForeach]("GI.Gtk.Objects.FlowBox#g:method:selectedForeach"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unselectAll]("GI.Gtk.Objects.FlowBox#g:method:unselectAll"), [unselectChild]("GI.Gtk.Objects.FlowBox#g:method:unselectChild"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActivateOnSingleClick]("GI.Gtk.Objects.FlowBox#g:method:getActivateOnSingleClick"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildAtIndex]("GI.Gtk.Objects.FlowBox#g:method:getChildAtIndex"), [getChildAtPos]("GI.Gtk.Objects.FlowBox#g:method:getChildAtPos"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getColumnSpacing]("GI.Gtk.Objects.FlowBox#g:method:getColumnSpacing"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getHomogeneous]("GI.Gtk.Objects.FlowBox#g:method:getHomogeneous"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMaxChildrenPerLine]("GI.Gtk.Objects.FlowBox#g:method:getMaxChildrenPerLine"), [getMinChildrenPerLine]("GI.Gtk.Objects.FlowBox#g:method:getMinChildrenPerLine"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getOrientation]("GI.Gtk.Interfaces.Orientable#g:method:getOrientation"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getRowSpacing]("GI.Gtk.Objects.FlowBox#g:method:getRowSpacing"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSelectedChildren]("GI.Gtk.Objects.FlowBox#g:method:getSelectedChildren"), [getSelectionMode]("GI.Gtk.Objects.FlowBox#g:method:getSelectionMode"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActivateOnSingleClick]("GI.Gtk.Objects.FlowBox#g:method:setActivateOnSingleClick"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setColumnSpacing]("GI.Gtk.Objects.FlowBox#g:method:setColumnSpacing"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFilterFunc]("GI.Gtk.Objects.FlowBox#g:method:setFilterFunc"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHadjustment]("GI.Gtk.Objects.FlowBox#g:method:setHadjustment"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setHomogeneous]("GI.Gtk.Objects.FlowBox#g:method:setHomogeneous"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMaxChildrenPerLine]("GI.Gtk.Objects.FlowBox#g:method:setMaxChildrenPerLine"), [setMinChildrenPerLine]("GI.Gtk.Objects.FlowBox#g:method:setMinChildrenPerLine"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setOrientation]("GI.Gtk.Interfaces.Orientable#g:method:setOrientation"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setRowSpacing]("GI.Gtk.Objects.FlowBox#g:method:setRowSpacing"), [setSelectionMode]("GI.Gtk.Objects.FlowBox#g:method:setSelectionMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setSortFunc]("GI.Gtk.Objects.FlowBox#g:method:setSortFunc"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setVadjustment]("GI.Gtk.Objects.FlowBox#g:method:setVadjustment"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveFlowBoxMethod                    ,
#endif

-- ** bindModel #method:bindModel#

#if defined(ENABLE_OVERLOADING)
    FlowBoxBindModelMethodInfo              ,
#endif
    flowBoxBindModel                        ,


-- ** getActivateOnSingleClick #method:getActivateOnSingleClick#

#if defined(ENABLE_OVERLOADING)
    FlowBoxGetActivateOnSingleClickMethodInfo,
#endif
    flowBoxGetActivateOnSingleClick         ,


-- ** getChildAtIndex #method:getChildAtIndex#

#if defined(ENABLE_OVERLOADING)
    FlowBoxGetChildAtIndexMethodInfo        ,
#endif
    flowBoxGetChildAtIndex                  ,


-- ** getChildAtPos #method:getChildAtPos#

#if defined(ENABLE_OVERLOADING)
    FlowBoxGetChildAtPosMethodInfo          ,
#endif
    flowBoxGetChildAtPos                    ,


-- ** getColumnSpacing #method:getColumnSpacing#

#if defined(ENABLE_OVERLOADING)
    FlowBoxGetColumnSpacingMethodInfo       ,
#endif
    flowBoxGetColumnSpacing                 ,


-- ** getHomogeneous #method:getHomogeneous#

#if defined(ENABLE_OVERLOADING)
    FlowBoxGetHomogeneousMethodInfo         ,
#endif
    flowBoxGetHomogeneous                   ,


-- ** getMaxChildrenPerLine #method:getMaxChildrenPerLine#

#if defined(ENABLE_OVERLOADING)
    FlowBoxGetMaxChildrenPerLineMethodInfo  ,
#endif
    flowBoxGetMaxChildrenPerLine            ,


-- ** getMinChildrenPerLine #method:getMinChildrenPerLine#

#if defined(ENABLE_OVERLOADING)
    FlowBoxGetMinChildrenPerLineMethodInfo  ,
#endif
    flowBoxGetMinChildrenPerLine            ,


-- ** getRowSpacing #method:getRowSpacing#

#if defined(ENABLE_OVERLOADING)
    FlowBoxGetRowSpacingMethodInfo          ,
#endif
    flowBoxGetRowSpacing                    ,


-- ** getSelectedChildren #method:getSelectedChildren#

#if defined(ENABLE_OVERLOADING)
    FlowBoxGetSelectedChildrenMethodInfo    ,
#endif
    flowBoxGetSelectedChildren              ,


-- ** getSelectionMode #method:getSelectionMode#

#if defined(ENABLE_OVERLOADING)
    FlowBoxGetSelectionModeMethodInfo       ,
#endif
    flowBoxGetSelectionMode                 ,


-- ** insert #method:insert#

#if defined(ENABLE_OVERLOADING)
    FlowBoxInsertMethodInfo                 ,
#endif
    flowBoxInsert                           ,


-- ** invalidateFilter #method:invalidateFilter#

#if defined(ENABLE_OVERLOADING)
    FlowBoxInvalidateFilterMethodInfo       ,
#endif
    flowBoxInvalidateFilter                 ,


-- ** invalidateSort #method:invalidateSort#

#if defined(ENABLE_OVERLOADING)
    FlowBoxInvalidateSortMethodInfo         ,
#endif
    flowBoxInvalidateSort                   ,


-- ** new #method:new#

    flowBoxNew                              ,


-- ** selectAll #method:selectAll#

#if defined(ENABLE_OVERLOADING)
    FlowBoxSelectAllMethodInfo              ,
#endif
    flowBoxSelectAll                        ,


-- ** selectChild #method:selectChild#

#if defined(ENABLE_OVERLOADING)
    FlowBoxSelectChildMethodInfo            ,
#endif
    flowBoxSelectChild                      ,


-- ** selectedForeach #method:selectedForeach#

#if defined(ENABLE_OVERLOADING)
    FlowBoxSelectedForeachMethodInfo        ,
#endif
    flowBoxSelectedForeach                  ,


-- ** setActivateOnSingleClick #method:setActivateOnSingleClick#

#if defined(ENABLE_OVERLOADING)
    FlowBoxSetActivateOnSingleClickMethodInfo,
#endif
    flowBoxSetActivateOnSingleClick         ,


-- ** setColumnSpacing #method:setColumnSpacing#

#if defined(ENABLE_OVERLOADING)
    FlowBoxSetColumnSpacingMethodInfo       ,
#endif
    flowBoxSetColumnSpacing                 ,


-- ** setFilterFunc #method:setFilterFunc#

#if defined(ENABLE_OVERLOADING)
    FlowBoxSetFilterFuncMethodInfo          ,
#endif
    flowBoxSetFilterFunc                    ,


-- ** setHadjustment #method:setHadjustment#

#if defined(ENABLE_OVERLOADING)
    FlowBoxSetHadjustmentMethodInfo         ,
#endif
    flowBoxSetHadjustment                   ,


-- ** setHomogeneous #method:setHomogeneous#

#if defined(ENABLE_OVERLOADING)
    FlowBoxSetHomogeneousMethodInfo         ,
#endif
    flowBoxSetHomogeneous                   ,


-- ** setMaxChildrenPerLine #method:setMaxChildrenPerLine#

#if defined(ENABLE_OVERLOADING)
    FlowBoxSetMaxChildrenPerLineMethodInfo  ,
#endif
    flowBoxSetMaxChildrenPerLine            ,


-- ** setMinChildrenPerLine #method:setMinChildrenPerLine#

#if defined(ENABLE_OVERLOADING)
    FlowBoxSetMinChildrenPerLineMethodInfo  ,
#endif
    flowBoxSetMinChildrenPerLine            ,


-- ** setRowSpacing #method:setRowSpacing#

#if defined(ENABLE_OVERLOADING)
    FlowBoxSetRowSpacingMethodInfo          ,
#endif
    flowBoxSetRowSpacing                    ,


-- ** setSelectionMode #method:setSelectionMode#

#if defined(ENABLE_OVERLOADING)
    FlowBoxSetSelectionModeMethodInfo       ,
#endif
    flowBoxSetSelectionMode                 ,


-- ** setSortFunc #method:setSortFunc#

#if defined(ENABLE_OVERLOADING)
    FlowBoxSetSortFuncMethodInfo            ,
#endif
    flowBoxSetSortFunc                      ,


-- ** setVadjustment #method:setVadjustment#

#if defined(ENABLE_OVERLOADING)
    FlowBoxSetVadjustmentMethodInfo         ,
#endif
    flowBoxSetVadjustment                   ,


-- ** unselectAll #method:unselectAll#

#if defined(ENABLE_OVERLOADING)
    FlowBoxUnselectAllMethodInfo            ,
#endif
    flowBoxUnselectAll                      ,


-- ** unselectChild #method:unselectChild#

#if defined(ENABLE_OVERLOADING)
    FlowBoxUnselectChildMethodInfo          ,
#endif
    flowBoxUnselectChild                    ,




 -- * Properties


-- ** activateOnSingleClick #attr:activateOnSingleClick#
-- | Determines whether children can be activated with a single
-- click, or require a double-click.

#if defined(ENABLE_OVERLOADING)
    FlowBoxActivateOnSingleClickPropertyInfo,
#endif
    constructFlowBoxActivateOnSingleClick   ,
#if defined(ENABLE_OVERLOADING)
    flowBoxActivateOnSingleClick            ,
#endif
    getFlowBoxActivateOnSingleClick         ,
    setFlowBoxActivateOnSingleClick         ,


-- ** columnSpacing #attr:columnSpacing#
-- | The amount of horizontal space between two children.

#if defined(ENABLE_OVERLOADING)
    FlowBoxColumnSpacingPropertyInfo        ,
#endif
    constructFlowBoxColumnSpacing           ,
#if defined(ENABLE_OVERLOADING)
    flowBoxColumnSpacing                    ,
#endif
    getFlowBoxColumnSpacing                 ,
    setFlowBoxColumnSpacing                 ,


-- ** homogeneous #attr:homogeneous#
-- | Determines whether all children should be allocated the
-- same size.

#if defined(ENABLE_OVERLOADING)
    FlowBoxHomogeneousPropertyInfo          ,
#endif
    constructFlowBoxHomogeneous             ,
#if defined(ENABLE_OVERLOADING)
    flowBoxHomogeneous                      ,
#endif
    getFlowBoxHomogeneous                   ,
    setFlowBoxHomogeneous                   ,


-- ** maxChildrenPerLine #attr:maxChildrenPerLine#
-- | The maximum amount of children to request space for consecutively
-- in the given orientation.

#if defined(ENABLE_OVERLOADING)
    FlowBoxMaxChildrenPerLinePropertyInfo   ,
#endif
    constructFlowBoxMaxChildrenPerLine      ,
#if defined(ENABLE_OVERLOADING)
    flowBoxMaxChildrenPerLine               ,
#endif
    getFlowBoxMaxChildrenPerLine            ,
    setFlowBoxMaxChildrenPerLine            ,


-- ** minChildrenPerLine #attr:minChildrenPerLine#
-- | The minimum number of children to allocate consecutively
-- in the given orientation.
-- 
-- Setting the minimum children per line ensures
-- that a reasonably small height will be requested
-- for the overall minimum width of the box.

#if defined(ENABLE_OVERLOADING)
    FlowBoxMinChildrenPerLinePropertyInfo   ,
#endif
    constructFlowBoxMinChildrenPerLine      ,
#if defined(ENABLE_OVERLOADING)
    flowBoxMinChildrenPerLine               ,
#endif
    getFlowBoxMinChildrenPerLine            ,
    setFlowBoxMinChildrenPerLine            ,


-- ** rowSpacing #attr:rowSpacing#
-- | The amount of vertical space between two children.

#if defined(ENABLE_OVERLOADING)
    FlowBoxRowSpacingPropertyInfo           ,
#endif
    constructFlowBoxRowSpacing              ,
#if defined(ENABLE_OVERLOADING)
    flowBoxRowSpacing                       ,
#endif
    getFlowBoxRowSpacing                    ,
    setFlowBoxRowSpacing                    ,


-- ** selectionMode #attr:selectionMode#
-- | The selection mode used by the flow  box.

#if defined(ENABLE_OVERLOADING)
    FlowBoxSelectionModePropertyInfo        ,
#endif
    constructFlowBoxSelectionMode           ,
#if defined(ENABLE_OVERLOADING)
    flowBoxSelectionMode                    ,
#endif
    getFlowBoxSelectionMode                 ,
    setFlowBoxSelectionMode                 ,




 -- * Signals


-- ** activateCursorChild #signal:activateCursorChild#

    FlowBoxActivateCursorChildCallback      ,
#if defined(ENABLE_OVERLOADING)
    FlowBoxActivateCursorChildSignalInfo    ,
#endif
    afterFlowBoxActivateCursorChild         ,
    onFlowBoxActivateCursorChild            ,


-- ** childActivated #signal:childActivated#

    FlowBoxChildActivatedCallback           ,
#if defined(ENABLE_OVERLOADING)
    FlowBoxChildActivatedSignalInfo         ,
#endif
    afterFlowBoxChildActivated              ,
    onFlowBoxChildActivated                 ,


-- ** moveCursor #signal:moveCursor#

    FlowBoxMoveCursorCallback               ,
#if defined(ENABLE_OVERLOADING)
    FlowBoxMoveCursorSignalInfo             ,
#endif
    afterFlowBoxMoveCursor                  ,
    onFlowBoxMoveCursor                     ,


-- ** selectAll #signal:selectAll#

    FlowBoxSelectAllCallback                ,
#if defined(ENABLE_OVERLOADING)
    FlowBoxSelectAllSignalInfo              ,
#endif
    afterFlowBoxSelectAll                   ,
    onFlowBoxSelectAll                      ,


-- ** selectedChildrenChanged #signal:selectedChildrenChanged#

    FlowBoxSelectedChildrenChangedCallback  ,
#if defined(ENABLE_OVERLOADING)
    FlowBoxSelectedChildrenChangedSignalInfo,
#endif
    afterFlowBoxSelectedChildrenChanged     ,
    onFlowBoxSelectedChildrenChanged        ,


-- ** toggleCursorChild #signal:toggleCursorChild#

    FlowBoxToggleCursorChildCallback        ,
#if defined(ENABLE_OVERLOADING)
    FlowBoxToggleCursorChildSignalInfo      ,
#endif
    afterFlowBoxToggleCursorChild           ,
    onFlowBoxToggleCursorChild              ,


-- ** unselectAll #signal:unselectAll#

    FlowBoxUnselectAllCallback              ,
#if defined(ENABLE_OVERLOADING)
    FlowBoxUnselectAllSignalInfo            ,
#endif
    afterFlowBoxUnselectAll                 ,
    onFlowBoxUnselectAll                    ,




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
import qualified GI.Gio.Interfaces.ListModel as Gio.ListModel
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Orientable as Gtk.Orientable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Adjustment as Gtk.Adjustment
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.FlowBoxChild as Gtk.FlowBoxChild
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype FlowBox = FlowBox (SP.ManagedPtr FlowBox)
    deriving (Eq)

instance SP.ManagedPtrNewtype FlowBox where
    toManagedPtr (FlowBox p) = p

foreign import ccall "gtk_flow_box_get_type"
    c_gtk_flow_box_get_type :: IO B.Types.GType

instance B.Types.TypedObject FlowBox where
    glibType = c_gtk_flow_box_get_type

instance B.Types.GObject FlowBox

-- | Type class for types which can be safely cast to `FlowBox`, for instance with `toFlowBox`.
class (SP.GObject o, O.IsDescendantOf FlowBox o) => IsFlowBox o
instance (SP.GObject o, O.IsDescendantOf FlowBox o) => IsFlowBox o

instance O.HasParentTypes FlowBox
type instance O.ParentTypes FlowBox = '[Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable, Gtk.Orientable.Orientable]

-- | Cast to `FlowBox`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toFlowBox :: (MIO.MonadIO m, IsFlowBox o) => o -> m FlowBox
toFlowBox = MIO.liftIO . B.ManagedPtr.unsafeCastTo FlowBox

-- | Convert 'FlowBox' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe FlowBox) where
    gvalueGType_ = c_gtk_flow_box_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr FlowBox)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr FlowBox)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject FlowBox ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveFlowBoxMethod (t :: Symbol) (o :: *) :: * where
    ResolveFlowBoxMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveFlowBoxMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveFlowBoxMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveFlowBoxMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveFlowBoxMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveFlowBoxMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveFlowBoxMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveFlowBoxMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveFlowBoxMethod "bindModel" o = FlowBoxBindModelMethodInfo
    ResolveFlowBoxMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveFlowBoxMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveFlowBoxMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveFlowBoxMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveFlowBoxMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveFlowBoxMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveFlowBoxMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveFlowBoxMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveFlowBoxMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveFlowBoxMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveFlowBoxMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveFlowBoxMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveFlowBoxMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveFlowBoxMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveFlowBoxMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveFlowBoxMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveFlowBoxMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveFlowBoxMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveFlowBoxMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveFlowBoxMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveFlowBoxMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveFlowBoxMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveFlowBoxMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveFlowBoxMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveFlowBoxMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveFlowBoxMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveFlowBoxMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveFlowBoxMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveFlowBoxMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveFlowBoxMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveFlowBoxMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveFlowBoxMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveFlowBoxMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveFlowBoxMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveFlowBoxMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveFlowBoxMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveFlowBoxMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveFlowBoxMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveFlowBoxMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveFlowBoxMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveFlowBoxMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveFlowBoxMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveFlowBoxMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveFlowBoxMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveFlowBoxMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveFlowBoxMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveFlowBoxMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveFlowBoxMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveFlowBoxMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveFlowBoxMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveFlowBoxMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveFlowBoxMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveFlowBoxMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveFlowBoxMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveFlowBoxMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveFlowBoxMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveFlowBoxMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveFlowBoxMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveFlowBoxMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveFlowBoxMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveFlowBoxMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveFlowBoxMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveFlowBoxMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveFlowBoxMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveFlowBoxMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveFlowBoxMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveFlowBoxMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveFlowBoxMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveFlowBoxMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveFlowBoxMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveFlowBoxMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveFlowBoxMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveFlowBoxMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveFlowBoxMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveFlowBoxMethod "insert" o = FlowBoxInsertMethodInfo
    ResolveFlowBoxMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveFlowBoxMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveFlowBoxMethod "invalidateFilter" o = FlowBoxInvalidateFilterMethodInfo
    ResolveFlowBoxMethod "invalidateSort" o = FlowBoxInvalidateSortMethodInfo
    ResolveFlowBoxMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveFlowBoxMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveFlowBoxMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveFlowBoxMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveFlowBoxMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveFlowBoxMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveFlowBoxMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveFlowBoxMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveFlowBoxMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveFlowBoxMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveFlowBoxMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveFlowBoxMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveFlowBoxMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveFlowBoxMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveFlowBoxMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveFlowBoxMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveFlowBoxMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveFlowBoxMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveFlowBoxMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveFlowBoxMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveFlowBoxMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveFlowBoxMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveFlowBoxMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveFlowBoxMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveFlowBoxMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveFlowBoxMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveFlowBoxMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveFlowBoxMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveFlowBoxMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveFlowBoxMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveFlowBoxMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveFlowBoxMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveFlowBoxMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveFlowBoxMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveFlowBoxMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveFlowBoxMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveFlowBoxMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveFlowBoxMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveFlowBoxMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveFlowBoxMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveFlowBoxMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveFlowBoxMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveFlowBoxMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveFlowBoxMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveFlowBoxMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveFlowBoxMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveFlowBoxMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveFlowBoxMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveFlowBoxMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveFlowBoxMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveFlowBoxMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveFlowBoxMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveFlowBoxMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveFlowBoxMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveFlowBoxMethod "selectAll" o = FlowBoxSelectAllMethodInfo
    ResolveFlowBoxMethod "selectChild" o = FlowBoxSelectChildMethodInfo
    ResolveFlowBoxMethod "selectedForeach" o = FlowBoxSelectedForeachMethodInfo
    ResolveFlowBoxMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveFlowBoxMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveFlowBoxMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveFlowBoxMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveFlowBoxMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveFlowBoxMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveFlowBoxMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveFlowBoxMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveFlowBoxMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveFlowBoxMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveFlowBoxMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveFlowBoxMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveFlowBoxMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveFlowBoxMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveFlowBoxMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveFlowBoxMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveFlowBoxMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveFlowBoxMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveFlowBoxMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveFlowBoxMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveFlowBoxMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveFlowBoxMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveFlowBoxMethod "unselectAll" o = FlowBoxUnselectAllMethodInfo
    ResolveFlowBoxMethod "unselectChild" o = FlowBoxUnselectChildMethodInfo
    ResolveFlowBoxMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveFlowBoxMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveFlowBoxMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveFlowBoxMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveFlowBoxMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveFlowBoxMethod "getActivateOnSingleClick" o = FlowBoxGetActivateOnSingleClickMethodInfo
    ResolveFlowBoxMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveFlowBoxMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveFlowBoxMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveFlowBoxMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveFlowBoxMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveFlowBoxMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveFlowBoxMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveFlowBoxMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveFlowBoxMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveFlowBoxMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveFlowBoxMethod "getChildAtIndex" o = FlowBoxGetChildAtIndexMethodInfo
    ResolveFlowBoxMethod "getChildAtPos" o = FlowBoxGetChildAtPosMethodInfo
    ResolveFlowBoxMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveFlowBoxMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveFlowBoxMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveFlowBoxMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveFlowBoxMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveFlowBoxMethod "getColumnSpacing" o = FlowBoxGetColumnSpacingMethodInfo
    ResolveFlowBoxMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveFlowBoxMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveFlowBoxMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveFlowBoxMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveFlowBoxMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveFlowBoxMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveFlowBoxMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveFlowBoxMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveFlowBoxMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveFlowBoxMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveFlowBoxMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveFlowBoxMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveFlowBoxMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveFlowBoxMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveFlowBoxMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveFlowBoxMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveFlowBoxMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveFlowBoxMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveFlowBoxMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveFlowBoxMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveFlowBoxMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveFlowBoxMethod "getHomogeneous" o = FlowBoxGetHomogeneousMethodInfo
    ResolveFlowBoxMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveFlowBoxMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveFlowBoxMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveFlowBoxMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveFlowBoxMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveFlowBoxMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveFlowBoxMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveFlowBoxMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveFlowBoxMethod "getMaxChildrenPerLine" o = FlowBoxGetMaxChildrenPerLineMethodInfo
    ResolveFlowBoxMethod "getMinChildrenPerLine" o = FlowBoxGetMinChildrenPerLineMethodInfo
    ResolveFlowBoxMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveFlowBoxMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveFlowBoxMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveFlowBoxMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveFlowBoxMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveFlowBoxMethod "getOrientation" o = Gtk.Orientable.OrientableGetOrientationMethodInfo
    ResolveFlowBoxMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveFlowBoxMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveFlowBoxMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveFlowBoxMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveFlowBoxMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveFlowBoxMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveFlowBoxMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveFlowBoxMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveFlowBoxMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveFlowBoxMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveFlowBoxMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveFlowBoxMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveFlowBoxMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveFlowBoxMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveFlowBoxMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveFlowBoxMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveFlowBoxMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveFlowBoxMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveFlowBoxMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveFlowBoxMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveFlowBoxMethod "getRowSpacing" o = FlowBoxGetRowSpacingMethodInfo
    ResolveFlowBoxMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveFlowBoxMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveFlowBoxMethod "getSelectedChildren" o = FlowBoxGetSelectedChildrenMethodInfo
    ResolveFlowBoxMethod "getSelectionMode" o = FlowBoxGetSelectionModeMethodInfo
    ResolveFlowBoxMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveFlowBoxMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveFlowBoxMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveFlowBoxMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveFlowBoxMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveFlowBoxMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveFlowBoxMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveFlowBoxMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveFlowBoxMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveFlowBoxMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveFlowBoxMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveFlowBoxMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveFlowBoxMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveFlowBoxMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveFlowBoxMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveFlowBoxMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveFlowBoxMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveFlowBoxMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveFlowBoxMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveFlowBoxMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveFlowBoxMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveFlowBoxMethod "setActivateOnSingleClick" o = FlowBoxSetActivateOnSingleClickMethodInfo
    ResolveFlowBoxMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveFlowBoxMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveFlowBoxMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveFlowBoxMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveFlowBoxMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveFlowBoxMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveFlowBoxMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveFlowBoxMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveFlowBoxMethod "setColumnSpacing" o = FlowBoxSetColumnSpacingMethodInfo
    ResolveFlowBoxMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveFlowBoxMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveFlowBoxMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveFlowBoxMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveFlowBoxMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveFlowBoxMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveFlowBoxMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveFlowBoxMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveFlowBoxMethod "setFilterFunc" o = FlowBoxSetFilterFuncMethodInfo
    ResolveFlowBoxMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveFlowBoxMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveFlowBoxMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveFlowBoxMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveFlowBoxMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveFlowBoxMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveFlowBoxMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveFlowBoxMethod "setHadjustment" o = FlowBoxSetHadjustmentMethodInfo
    ResolveFlowBoxMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveFlowBoxMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveFlowBoxMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveFlowBoxMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveFlowBoxMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveFlowBoxMethod "setHomogeneous" o = FlowBoxSetHomogeneousMethodInfo
    ResolveFlowBoxMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveFlowBoxMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveFlowBoxMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveFlowBoxMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveFlowBoxMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveFlowBoxMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveFlowBoxMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveFlowBoxMethod "setMaxChildrenPerLine" o = FlowBoxSetMaxChildrenPerLineMethodInfo
    ResolveFlowBoxMethod "setMinChildrenPerLine" o = FlowBoxSetMinChildrenPerLineMethodInfo
    ResolveFlowBoxMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveFlowBoxMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveFlowBoxMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveFlowBoxMethod "setOrientation" o = Gtk.Orientable.OrientableSetOrientationMethodInfo
    ResolveFlowBoxMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveFlowBoxMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveFlowBoxMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveFlowBoxMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveFlowBoxMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveFlowBoxMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveFlowBoxMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveFlowBoxMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveFlowBoxMethod "setRowSpacing" o = FlowBoxSetRowSpacingMethodInfo
    ResolveFlowBoxMethod "setSelectionMode" o = FlowBoxSetSelectionModeMethodInfo
    ResolveFlowBoxMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveFlowBoxMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveFlowBoxMethod "setSortFunc" o = FlowBoxSetSortFuncMethodInfo
    ResolveFlowBoxMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveFlowBoxMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveFlowBoxMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveFlowBoxMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveFlowBoxMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveFlowBoxMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveFlowBoxMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveFlowBoxMethod "setVadjustment" o = FlowBoxSetVadjustmentMethodInfo
    ResolveFlowBoxMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveFlowBoxMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveFlowBoxMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveFlowBoxMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveFlowBoxMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveFlowBoxMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveFlowBoxMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveFlowBoxMethod t FlowBox, O.OverloadedMethod info FlowBox p) => OL.IsLabel t (FlowBox -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveFlowBoxMethod t FlowBox, O.OverloadedMethod info FlowBox p, R.HasField t FlowBox p) => R.HasField t FlowBox p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveFlowBoxMethod t FlowBox, O.OverloadedMethodInfo info FlowBox) => OL.IsLabel t (O.MethodProxy info FlowBox) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal FlowBox::activate-cursor-child
-- | The [activateCursorChild](#g:signal:activateCursorChild) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user activates the /@box@/.
type FlowBoxActivateCursorChildCallback =
    IO ()

type C_FlowBoxActivateCursorChildCallback =
    Ptr FlowBox ->                          -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_FlowBoxActivateCursorChildCallback`.
foreign import ccall "wrapper"
    mk_FlowBoxActivateCursorChildCallback :: C_FlowBoxActivateCursorChildCallback -> IO (FunPtr C_FlowBoxActivateCursorChildCallback)

wrap_FlowBoxActivateCursorChildCallback :: 
    GObject a => (a -> FlowBoxActivateCursorChildCallback) ->
    C_FlowBoxActivateCursorChildCallback
wrap_FlowBoxActivateCursorChildCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [activateCursorChild](#signal:activateCursorChild) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' flowBox #activateCursorChild callback
-- @
-- 
-- 
onFlowBoxActivateCursorChild :: (IsFlowBox a, MonadIO m) => a -> ((?self :: a) => FlowBoxActivateCursorChildCallback) -> m SignalHandlerId
onFlowBoxActivateCursorChild obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FlowBoxActivateCursorChildCallback wrapped
    wrapped'' <- mk_FlowBoxActivateCursorChildCallback wrapped'
    connectSignalFunPtr obj "activate-cursor-child" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [activateCursorChild](#signal:activateCursorChild) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' flowBox #activateCursorChild callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterFlowBoxActivateCursorChild :: (IsFlowBox a, MonadIO m) => a -> ((?self :: a) => FlowBoxActivateCursorChildCallback) -> m SignalHandlerId
afterFlowBoxActivateCursorChild obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FlowBoxActivateCursorChildCallback wrapped
    wrapped'' <- mk_FlowBoxActivateCursorChildCallback wrapped'
    connectSignalFunPtr obj "activate-cursor-child" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data FlowBoxActivateCursorChildSignalInfo
instance SignalInfo FlowBoxActivateCursorChildSignalInfo where
    type HaskellCallbackType FlowBoxActivateCursorChildSignalInfo = FlowBoxActivateCursorChildCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_FlowBoxActivateCursorChildCallback cb
        cb'' <- mk_FlowBoxActivateCursorChildCallback cb'
        connectSignalFunPtr obj "activate-cursor-child" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox::activate-cursor-child"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#g:signal:activateCursorChild"})

#endif

-- signal FlowBox::child-activated
-- | The [childActivated](#g:signal:childActivated) signal is emitted when a child has been
-- activated by the user.
type FlowBoxChildActivatedCallback =
    Gtk.FlowBoxChild.FlowBoxChild
    -- ^ /@child@/: the child that is activated
    -> IO ()

type C_FlowBoxChildActivatedCallback =
    Ptr FlowBox ->                          -- object
    Ptr Gtk.FlowBoxChild.FlowBoxChild ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_FlowBoxChildActivatedCallback`.
foreign import ccall "wrapper"
    mk_FlowBoxChildActivatedCallback :: C_FlowBoxChildActivatedCallback -> IO (FunPtr C_FlowBoxChildActivatedCallback)

wrap_FlowBoxChildActivatedCallback :: 
    GObject a => (a -> FlowBoxChildActivatedCallback) ->
    C_FlowBoxChildActivatedCallback
wrap_FlowBoxChildActivatedCallback gi'cb gi'selfPtr child _ = do
    child' <- (newObject Gtk.FlowBoxChild.FlowBoxChild) child
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  child'


-- | Connect a signal handler for the [childActivated](#signal:childActivated) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' flowBox #childActivated callback
-- @
-- 
-- 
onFlowBoxChildActivated :: (IsFlowBox a, MonadIO m) => a -> ((?self :: a) => FlowBoxChildActivatedCallback) -> m SignalHandlerId
onFlowBoxChildActivated obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FlowBoxChildActivatedCallback wrapped
    wrapped'' <- mk_FlowBoxChildActivatedCallback wrapped'
    connectSignalFunPtr obj "child-activated" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [childActivated](#signal:childActivated) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' flowBox #childActivated callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterFlowBoxChildActivated :: (IsFlowBox a, MonadIO m) => a -> ((?self :: a) => FlowBoxChildActivatedCallback) -> m SignalHandlerId
afterFlowBoxChildActivated obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FlowBoxChildActivatedCallback wrapped
    wrapped'' <- mk_FlowBoxChildActivatedCallback wrapped'
    connectSignalFunPtr obj "child-activated" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data FlowBoxChildActivatedSignalInfo
instance SignalInfo FlowBoxChildActivatedSignalInfo where
    type HaskellCallbackType FlowBoxChildActivatedSignalInfo = FlowBoxChildActivatedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_FlowBoxChildActivatedCallback cb
        cb'' <- mk_FlowBoxChildActivatedCallback cb'
        connectSignalFunPtr obj "child-activated" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox::child-activated"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#g:signal:childActivated"})

#endif

-- signal FlowBox::move-cursor
-- | The [moveCursor](#g:signal:moveCursor) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user initiates a cursor movement.
-- 
-- Applications should not connect to it, but may emit it with
-- @/g_signal_emit_by_name()/@ if they need to control the cursor
-- programmatically.
-- 
-- The default bindings for this signal come in two variants,
-- the variant with the Shift modifier extends the selection,
-- the variant without the Shift modifer does not.
-- There are too many key combinations to list them all here.
-- 
-- * Arrow keys move by individual children
-- * Home\/End keys move to the ends of the box
-- * PageUp\/PageDown keys move vertically by pages
type FlowBoxMoveCursorCallback =
    Gtk.Enums.MovementStep
    -- ^ /@step@/: the granularity fo the move, as a t'GI.Gtk.Enums.MovementStep'
    -> Int32
    -- ^ /@count@/: the number of /@step@/ units to move
    -> IO Bool
    -- ^ __Returns:__ 'P.True' to stop other handlers from being invoked for the event.
    -- 'P.False' to propagate the event further.

type C_FlowBoxMoveCursorCallback =
    Ptr FlowBox ->                          -- object
    CUInt ->
    Int32 ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_FlowBoxMoveCursorCallback`.
foreign import ccall "wrapper"
    mk_FlowBoxMoveCursorCallback :: C_FlowBoxMoveCursorCallback -> IO (FunPtr C_FlowBoxMoveCursorCallback)

wrap_FlowBoxMoveCursorCallback :: 
    GObject a => (a -> FlowBoxMoveCursorCallback) ->
    C_FlowBoxMoveCursorCallback
wrap_FlowBoxMoveCursorCallback gi'cb gi'selfPtr step count _ = do
    let step' = (toEnum . fromIntegral) step
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  step' count
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [moveCursor](#signal:moveCursor) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' flowBox #moveCursor callback
-- @
-- 
-- 
onFlowBoxMoveCursor :: (IsFlowBox a, MonadIO m) => a -> ((?self :: a) => FlowBoxMoveCursorCallback) -> m SignalHandlerId
onFlowBoxMoveCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FlowBoxMoveCursorCallback wrapped
    wrapped'' <- mk_FlowBoxMoveCursorCallback wrapped'
    connectSignalFunPtr obj "move-cursor" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [moveCursor](#signal:moveCursor) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' flowBox #moveCursor callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterFlowBoxMoveCursor :: (IsFlowBox a, MonadIO m) => a -> ((?self :: a) => FlowBoxMoveCursorCallback) -> m SignalHandlerId
afterFlowBoxMoveCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FlowBoxMoveCursorCallback wrapped
    wrapped'' <- mk_FlowBoxMoveCursorCallback wrapped'
    connectSignalFunPtr obj "move-cursor" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data FlowBoxMoveCursorSignalInfo
instance SignalInfo FlowBoxMoveCursorSignalInfo where
    type HaskellCallbackType FlowBoxMoveCursorSignalInfo = FlowBoxMoveCursorCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_FlowBoxMoveCursorCallback cb
        cb'' <- mk_FlowBoxMoveCursorCallback cb'
        connectSignalFunPtr obj "move-cursor" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox::move-cursor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#g:signal:moveCursor"})

#endif

-- signal FlowBox::select-all
-- | The [selectAll](#g:signal:selectAll) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to select all children of the box, if
-- the selection mode permits it.
-- 
-- The default bindings for this signal is Ctrl-a.
type FlowBoxSelectAllCallback =
    IO ()

type C_FlowBoxSelectAllCallback =
    Ptr FlowBox ->                          -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_FlowBoxSelectAllCallback`.
foreign import ccall "wrapper"
    mk_FlowBoxSelectAllCallback :: C_FlowBoxSelectAllCallback -> IO (FunPtr C_FlowBoxSelectAllCallback)

wrap_FlowBoxSelectAllCallback :: 
    GObject a => (a -> FlowBoxSelectAllCallback) ->
    C_FlowBoxSelectAllCallback
wrap_FlowBoxSelectAllCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [selectAll](#signal:selectAll) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' flowBox #selectAll callback
-- @
-- 
-- 
onFlowBoxSelectAll :: (IsFlowBox a, MonadIO m) => a -> ((?self :: a) => FlowBoxSelectAllCallback) -> m SignalHandlerId
onFlowBoxSelectAll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FlowBoxSelectAllCallback wrapped
    wrapped'' <- mk_FlowBoxSelectAllCallback wrapped'
    connectSignalFunPtr obj "select-all" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [selectAll](#signal:selectAll) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' flowBox #selectAll callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterFlowBoxSelectAll :: (IsFlowBox a, MonadIO m) => a -> ((?self :: a) => FlowBoxSelectAllCallback) -> m SignalHandlerId
afterFlowBoxSelectAll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FlowBoxSelectAllCallback wrapped
    wrapped'' <- mk_FlowBoxSelectAllCallback wrapped'
    connectSignalFunPtr obj "select-all" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data FlowBoxSelectAllSignalInfo
instance SignalInfo FlowBoxSelectAllSignalInfo where
    type HaskellCallbackType FlowBoxSelectAllSignalInfo = FlowBoxSelectAllCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_FlowBoxSelectAllCallback cb
        cb'' <- mk_FlowBoxSelectAllCallback cb'
        connectSignalFunPtr obj "select-all" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox::select-all"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#g:signal:selectAll"})

#endif

-- signal FlowBox::selected-children-changed
-- | The [selectedChildrenChanged](#g:signal:selectedChildrenChanged) signal is emitted when the
-- set of selected children changes.
-- 
-- Use 'GI.Gtk.Objects.FlowBox.flowBoxSelectedForeach' or
-- 'GI.Gtk.Objects.FlowBox.flowBoxGetSelectedChildren' to obtain the
-- selected children.
type FlowBoxSelectedChildrenChangedCallback =
    IO ()

type C_FlowBoxSelectedChildrenChangedCallback =
    Ptr FlowBox ->                          -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_FlowBoxSelectedChildrenChangedCallback`.
foreign import ccall "wrapper"
    mk_FlowBoxSelectedChildrenChangedCallback :: C_FlowBoxSelectedChildrenChangedCallback -> IO (FunPtr C_FlowBoxSelectedChildrenChangedCallback)

wrap_FlowBoxSelectedChildrenChangedCallback :: 
    GObject a => (a -> FlowBoxSelectedChildrenChangedCallback) ->
    C_FlowBoxSelectedChildrenChangedCallback
wrap_FlowBoxSelectedChildrenChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [selectedChildrenChanged](#signal:selectedChildrenChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' flowBox #selectedChildrenChanged callback
-- @
-- 
-- 
onFlowBoxSelectedChildrenChanged :: (IsFlowBox a, MonadIO m) => a -> ((?self :: a) => FlowBoxSelectedChildrenChangedCallback) -> m SignalHandlerId
onFlowBoxSelectedChildrenChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FlowBoxSelectedChildrenChangedCallback wrapped
    wrapped'' <- mk_FlowBoxSelectedChildrenChangedCallback wrapped'
    connectSignalFunPtr obj "selected-children-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [selectedChildrenChanged](#signal:selectedChildrenChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' flowBox #selectedChildrenChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterFlowBoxSelectedChildrenChanged :: (IsFlowBox a, MonadIO m) => a -> ((?self :: a) => FlowBoxSelectedChildrenChangedCallback) -> m SignalHandlerId
afterFlowBoxSelectedChildrenChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FlowBoxSelectedChildrenChangedCallback wrapped
    wrapped'' <- mk_FlowBoxSelectedChildrenChangedCallback wrapped'
    connectSignalFunPtr obj "selected-children-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data FlowBoxSelectedChildrenChangedSignalInfo
instance SignalInfo FlowBoxSelectedChildrenChangedSignalInfo where
    type HaskellCallbackType FlowBoxSelectedChildrenChangedSignalInfo = FlowBoxSelectedChildrenChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_FlowBoxSelectedChildrenChangedCallback cb
        cb'' <- mk_FlowBoxSelectedChildrenChangedCallback cb'
        connectSignalFunPtr obj "selected-children-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox::selected-children-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#g:signal:selectedChildrenChanged"})

#endif

-- signal FlowBox::toggle-cursor-child
-- | The [toggleCursorChild](#g:signal:toggleCursorChild) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which toggles the selection of the child that has the focus.
-- 
-- The default binding for this signal is Ctrl-Space.
type FlowBoxToggleCursorChildCallback =
    IO ()

type C_FlowBoxToggleCursorChildCallback =
    Ptr FlowBox ->                          -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_FlowBoxToggleCursorChildCallback`.
foreign import ccall "wrapper"
    mk_FlowBoxToggleCursorChildCallback :: C_FlowBoxToggleCursorChildCallback -> IO (FunPtr C_FlowBoxToggleCursorChildCallback)

wrap_FlowBoxToggleCursorChildCallback :: 
    GObject a => (a -> FlowBoxToggleCursorChildCallback) ->
    C_FlowBoxToggleCursorChildCallback
wrap_FlowBoxToggleCursorChildCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [toggleCursorChild](#signal:toggleCursorChild) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' flowBox #toggleCursorChild callback
-- @
-- 
-- 
onFlowBoxToggleCursorChild :: (IsFlowBox a, MonadIO m) => a -> ((?self :: a) => FlowBoxToggleCursorChildCallback) -> m SignalHandlerId
onFlowBoxToggleCursorChild obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FlowBoxToggleCursorChildCallback wrapped
    wrapped'' <- mk_FlowBoxToggleCursorChildCallback wrapped'
    connectSignalFunPtr obj "toggle-cursor-child" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [toggleCursorChild](#signal:toggleCursorChild) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' flowBox #toggleCursorChild callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterFlowBoxToggleCursorChild :: (IsFlowBox a, MonadIO m) => a -> ((?self :: a) => FlowBoxToggleCursorChildCallback) -> m SignalHandlerId
afterFlowBoxToggleCursorChild obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FlowBoxToggleCursorChildCallback wrapped
    wrapped'' <- mk_FlowBoxToggleCursorChildCallback wrapped'
    connectSignalFunPtr obj "toggle-cursor-child" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data FlowBoxToggleCursorChildSignalInfo
instance SignalInfo FlowBoxToggleCursorChildSignalInfo where
    type HaskellCallbackType FlowBoxToggleCursorChildSignalInfo = FlowBoxToggleCursorChildCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_FlowBoxToggleCursorChildCallback cb
        cb'' <- mk_FlowBoxToggleCursorChildCallback cb'
        connectSignalFunPtr obj "toggle-cursor-child" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox::toggle-cursor-child"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#g:signal:toggleCursorChild"})

#endif

-- signal FlowBox::unselect-all
-- | The [unselectAll](#g:signal:unselectAll) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to unselect all children of the box, if
-- the selection mode permits it.
-- 
-- The default bindings for this signal is Ctrl-Shift-a.
type FlowBoxUnselectAllCallback =
    IO ()

type C_FlowBoxUnselectAllCallback =
    Ptr FlowBox ->                          -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_FlowBoxUnselectAllCallback`.
foreign import ccall "wrapper"
    mk_FlowBoxUnselectAllCallback :: C_FlowBoxUnselectAllCallback -> IO (FunPtr C_FlowBoxUnselectAllCallback)

wrap_FlowBoxUnselectAllCallback :: 
    GObject a => (a -> FlowBoxUnselectAllCallback) ->
    C_FlowBoxUnselectAllCallback
wrap_FlowBoxUnselectAllCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [unselectAll](#signal:unselectAll) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' flowBox #unselectAll callback
-- @
-- 
-- 
onFlowBoxUnselectAll :: (IsFlowBox a, MonadIO m) => a -> ((?self :: a) => FlowBoxUnselectAllCallback) -> m SignalHandlerId
onFlowBoxUnselectAll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FlowBoxUnselectAllCallback wrapped
    wrapped'' <- mk_FlowBoxUnselectAllCallback wrapped'
    connectSignalFunPtr obj "unselect-all" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [unselectAll](#signal:unselectAll) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' flowBox #unselectAll callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterFlowBoxUnselectAll :: (IsFlowBox a, MonadIO m) => a -> ((?self :: a) => FlowBoxUnselectAllCallback) -> m SignalHandlerId
afterFlowBoxUnselectAll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FlowBoxUnselectAllCallback wrapped
    wrapped'' <- mk_FlowBoxUnselectAllCallback wrapped'
    connectSignalFunPtr obj "unselect-all" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data FlowBoxUnselectAllSignalInfo
instance SignalInfo FlowBoxUnselectAllSignalInfo where
    type HaskellCallbackType FlowBoxUnselectAllSignalInfo = FlowBoxUnselectAllCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_FlowBoxUnselectAllCallback cb
        cb'' <- mk_FlowBoxUnselectAllCallback cb'
        connectSignalFunPtr obj "unselect-all" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox::unselect-all"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#g:signal:unselectAll"})

#endif

-- VVV Prop "activate-on-single-click"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@activate-on-single-click@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' flowBox #activateOnSingleClick
-- @
getFlowBoxActivateOnSingleClick :: (MonadIO m, IsFlowBox o) => o -> m Bool
getFlowBoxActivateOnSingleClick obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "activate-on-single-click"

-- | Set the value of the “@activate-on-single-click@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' flowBox [ #activateOnSingleClick 'Data.GI.Base.Attributes.:=' value ]
-- @
setFlowBoxActivateOnSingleClick :: (MonadIO m, IsFlowBox o) => o -> Bool -> m ()
setFlowBoxActivateOnSingleClick obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "activate-on-single-click" val

-- | Construct a `GValueConstruct` with valid value for the “@activate-on-single-click@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFlowBoxActivateOnSingleClick :: (IsFlowBox o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructFlowBoxActivateOnSingleClick val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "activate-on-single-click" val

#if defined(ENABLE_OVERLOADING)
data FlowBoxActivateOnSingleClickPropertyInfo
instance AttrInfo FlowBoxActivateOnSingleClickPropertyInfo where
    type AttrAllowedOps FlowBoxActivateOnSingleClickPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FlowBoxActivateOnSingleClickPropertyInfo = IsFlowBox
    type AttrSetTypeConstraint FlowBoxActivateOnSingleClickPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint FlowBoxActivateOnSingleClickPropertyInfo = (~) Bool
    type AttrTransferType FlowBoxActivateOnSingleClickPropertyInfo = Bool
    type AttrGetType FlowBoxActivateOnSingleClickPropertyInfo = Bool
    type AttrLabel FlowBoxActivateOnSingleClickPropertyInfo = "activate-on-single-click"
    type AttrOrigin FlowBoxActivateOnSingleClickPropertyInfo = FlowBox
    attrGet = getFlowBoxActivateOnSingleClick
    attrSet = setFlowBoxActivateOnSingleClick
    attrTransfer _ v = do
        return v
    attrConstruct = constructFlowBoxActivateOnSingleClick
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.activateOnSingleClick"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#g:attr:activateOnSingleClick"
        })
#endif

-- VVV Prop "column-spacing"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@column-spacing@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' flowBox #columnSpacing
-- @
getFlowBoxColumnSpacing :: (MonadIO m, IsFlowBox o) => o -> m Word32
getFlowBoxColumnSpacing obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "column-spacing"

-- | Set the value of the “@column-spacing@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' flowBox [ #columnSpacing 'Data.GI.Base.Attributes.:=' value ]
-- @
setFlowBoxColumnSpacing :: (MonadIO m, IsFlowBox o) => o -> Word32 -> m ()
setFlowBoxColumnSpacing obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "column-spacing" val

-- | Construct a `GValueConstruct` with valid value for the “@column-spacing@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFlowBoxColumnSpacing :: (IsFlowBox o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructFlowBoxColumnSpacing val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "column-spacing" val

#if defined(ENABLE_OVERLOADING)
data FlowBoxColumnSpacingPropertyInfo
instance AttrInfo FlowBoxColumnSpacingPropertyInfo where
    type AttrAllowedOps FlowBoxColumnSpacingPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FlowBoxColumnSpacingPropertyInfo = IsFlowBox
    type AttrSetTypeConstraint FlowBoxColumnSpacingPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint FlowBoxColumnSpacingPropertyInfo = (~) Word32
    type AttrTransferType FlowBoxColumnSpacingPropertyInfo = Word32
    type AttrGetType FlowBoxColumnSpacingPropertyInfo = Word32
    type AttrLabel FlowBoxColumnSpacingPropertyInfo = "column-spacing"
    type AttrOrigin FlowBoxColumnSpacingPropertyInfo = FlowBox
    attrGet = getFlowBoxColumnSpacing
    attrSet = setFlowBoxColumnSpacing
    attrTransfer _ v = do
        return v
    attrConstruct = constructFlowBoxColumnSpacing
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.columnSpacing"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#g:attr:columnSpacing"
        })
#endif

-- VVV Prop "homogeneous"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@homogeneous@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' flowBox #homogeneous
-- @
getFlowBoxHomogeneous :: (MonadIO m, IsFlowBox o) => o -> m Bool
getFlowBoxHomogeneous obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "homogeneous"

-- | Set the value of the “@homogeneous@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' flowBox [ #homogeneous 'Data.GI.Base.Attributes.:=' value ]
-- @
setFlowBoxHomogeneous :: (MonadIO m, IsFlowBox o) => o -> Bool -> m ()
setFlowBoxHomogeneous obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "homogeneous" val

-- | Construct a `GValueConstruct` with valid value for the “@homogeneous@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFlowBoxHomogeneous :: (IsFlowBox o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructFlowBoxHomogeneous val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "homogeneous" val

#if defined(ENABLE_OVERLOADING)
data FlowBoxHomogeneousPropertyInfo
instance AttrInfo FlowBoxHomogeneousPropertyInfo where
    type AttrAllowedOps FlowBoxHomogeneousPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FlowBoxHomogeneousPropertyInfo = IsFlowBox
    type AttrSetTypeConstraint FlowBoxHomogeneousPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint FlowBoxHomogeneousPropertyInfo = (~) Bool
    type AttrTransferType FlowBoxHomogeneousPropertyInfo = Bool
    type AttrGetType FlowBoxHomogeneousPropertyInfo = Bool
    type AttrLabel FlowBoxHomogeneousPropertyInfo = "homogeneous"
    type AttrOrigin FlowBoxHomogeneousPropertyInfo = FlowBox
    attrGet = getFlowBoxHomogeneous
    attrSet = setFlowBoxHomogeneous
    attrTransfer _ v = do
        return v
    attrConstruct = constructFlowBoxHomogeneous
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.homogeneous"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#g:attr:homogeneous"
        })
#endif

-- VVV Prop "max-children-per-line"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@max-children-per-line@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' flowBox #maxChildrenPerLine
-- @
getFlowBoxMaxChildrenPerLine :: (MonadIO m, IsFlowBox o) => o -> m Word32
getFlowBoxMaxChildrenPerLine obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "max-children-per-line"

-- | Set the value of the “@max-children-per-line@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' flowBox [ #maxChildrenPerLine 'Data.GI.Base.Attributes.:=' value ]
-- @
setFlowBoxMaxChildrenPerLine :: (MonadIO m, IsFlowBox o) => o -> Word32 -> m ()
setFlowBoxMaxChildrenPerLine obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "max-children-per-line" val

-- | Construct a `GValueConstruct` with valid value for the “@max-children-per-line@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFlowBoxMaxChildrenPerLine :: (IsFlowBox o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructFlowBoxMaxChildrenPerLine val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "max-children-per-line" val

#if defined(ENABLE_OVERLOADING)
data FlowBoxMaxChildrenPerLinePropertyInfo
instance AttrInfo FlowBoxMaxChildrenPerLinePropertyInfo where
    type AttrAllowedOps FlowBoxMaxChildrenPerLinePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FlowBoxMaxChildrenPerLinePropertyInfo = IsFlowBox
    type AttrSetTypeConstraint FlowBoxMaxChildrenPerLinePropertyInfo = (~) Word32
    type AttrTransferTypeConstraint FlowBoxMaxChildrenPerLinePropertyInfo = (~) Word32
    type AttrTransferType FlowBoxMaxChildrenPerLinePropertyInfo = Word32
    type AttrGetType FlowBoxMaxChildrenPerLinePropertyInfo = Word32
    type AttrLabel FlowBoxMaxChildrenPerLinePropertyInfo = "max-children-per-line"
    type AttrOrigin FlowBoxMaxChildrenPerLinePropertyInfo = FlowBox
    attrGet = getFlowBoxMaxChildrenPerLine
    attrSet = setFlowBoxMaxChildrenPerLine
    attrTransfer _ v = do
        return v
    attrConstruct = constructFlowBoxMaxChildrenPerLine
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.maxChildrenPerLine"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#g:attr:maxChildrenPerLine"
        })
#endif

-- VVV Prop "min-children-per-line"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@min-children-per-line@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' flowBox #minChildrenPerLine
-- @
getFlowBoxMinChildrenPerLine :: (MonadIO m, IsFlowBox o) => o -> m Word32
getFlowBoxMinChildrenPerLine obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "min-children-per-line"

-- | Set the value of the “@min-children-per-line@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' flowBox [ #minChildrenPerLine 'Data.GI.Base.Attributes.:=' value ]
-- @
setFlowBoxMinChildrenPerLine :: (MonadIO m, IsFlowBox o) => o -> Word32 -> m ()
setFlowBoxMinChildrenPerLine obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "min-children-per-line" val

-- | Construct a `GValueConstruct` with valid value for the “@min-children-per-line@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFlowBoxMinChildrenPerLine :: (IsFlowBox o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructFlowBoxMinChildrenPerLine val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "min-children-per-line" val

#if defined(ENABLE_OVERLOADING)
data FlowBoxMinChildrenPerLinePropertyInfo
instance AttrInfo FlowBoxMinChildrenPerLinePropertyInfo where
    type AttrAllowedOps FlowBoxMinChildrenPerLinePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FlowBoxMinChildrenPerLinePropertyInfo = IsFlowBox
    type AttrSetTypeConstraint FlowBoxMinChildrenPerLinePropertyInfo = (~) Word32
    type AttrTransferTypeConstraint FlowBoxMinChildrenPerLinePropertyInfo = (~) Word32
    type AttrTransferType FlowBoxMinChildrenPerLinePropertyInfo = Word32
    type AttrGetType FlowBoxMinChildrenPerLinePropertyInfo = Word32
    type AttrLabel FlowBoxMinChildrenPerLinePropertyInfo = "min-children-per-line"
    type AttrOrigin FlowBoxMinChildrenPerLinePropertyInfo = FlowBox
    attrGet = getFlowBoxMinChildrenPerLine
    attrSet = setFlowBoxMinChildrenPerLine
    attrTransfer _ v = do
        return v
    attrConstruct = constructFlowBoxMinChildrenPerLine
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.minChildrenPerLine"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#g:attr:minChildrenPerLine"
        })
#endif

-- VVV Prop "row-spacing"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@row-spacing@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' flowBox #rowSpacing
-- @
getFlowBoxRowSpacing :: (MonadIO m, IsFlowBox o) => o -> m Word32
getFlowBoxRowSpacing obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "row-spacing"

-- | Set the value of the “@row-spacing@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' flowBox [ #rowSpacing 'Data.GI.Base.Attributes.:=' value ]
-- @
setFlowBoxRowSpacing :: (MonadIO m, IsFlowBox o) => o -> Word32 -> m ()
setFlowBoxRowSpacing obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "row-spacing" val

-- | Construct a `GValueConstruct` with valid value for the “@row-spacing@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFlowBoxRowSpacing :: (IsFlowBox o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructFlowBoxRowSpacing val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "row-spacing" val

#if defined(ENABLE_OVERLOADING)
data FlowBoxRowSpacingPropertyInfo
instance AttrInfo FlowBoxRowSpacingPropertyInfo where
    type AttrAllowedOps FlowBoxRowSpacingPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FlowBoxRowSpacingPropertyInfo = IsFlowBox
    type AttrSetTypeConstraint FlowBoxRowSpacingPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint FlowBoxRowSpacingPropertyInfo = (~) Word32
    type AttrTransferType FlowBoxRowSpacingPropertyInfo = Word32
    type AttrGetType FlowBoxRowSpacingPropertyInfo = Word32
    type AttrLabel FlowBoxRowSpacingPropertyInfo = "row-spacing"
    type AttrOrigin FlowBoxRowSpacingPropertyInfo = FlowBox
    attrGet = getFlowBoxRowSpacing
    attrSet = setFlowBoxRowSpacing
    attrTransfer _ v = do
        return v
    attrConstruct = constructFlowBoxRowSpacing
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.rowSpacing"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#g:attr:rowSpacing"
        })
#endif

-- VVV Prop "selection-mode"
   -- Type: TInterface (Name {namespace = "Gtk", name = "SelectionMode"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@selection-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' flowBox #selectionMode
-- @
getFlowBoxSelectionMode :: (MonadIO m, IsFlowBox o) => o -> m Gtk.Enums.SelectionMode
getFlowBoxSelectionMode obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "selection-mode"

-- | Set the value of the “@selection-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' flowBox [ #selectionMode 'Data.GI.Base.Attributes.:=' value ]
-- @
setFlowBoxSelectionMode :: (MonadIO m, IsFlowBox o) => o -> Gtk.Enums.SelectionMode -> m ()
setFlowBoxSelectionMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "selection-mode" val

-- | Construct a `GValueConstruct` with valid value for the “@selection-mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFlowBoxSelectionMode :: (IsFlowBox o, MIO.MonadIO m) => Gtk.Enums.SelectionMode -> m (GValueConstruct o)
constructFlowBoxSelectionMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "selection-mode" val

#if defined(ENABLE_OVERLOADING)
data FlowBoxSelectionModePropertyInfo
instance AttrInfo FlowBoxSelectionModePropertyInfo where
    type AttrAllowedOps FlowBoxSelectionModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FlowBoxSelectionModePropertyInfo = IsFlowBox
    type AttrSetTypeConstraint FlowBoxSelectionModePropertyInfo = (~) Gtk.Enums.SelectionMode
    type AttrTransferTypeConstraint FlowBoxSelectionModePropertyInfo = (~) Gtk.Enums.SelectionMode
    type AttrTransferType FlowBoxSelectionModePropertyInfo = Gtk.Enums.SelectionMode
    type AttrGetType FlowBoxSelectionModePropertyInfo = Gtk.Enums.SelectionMode
    type AttrLabel FlowBoxSelectionModePropertyInfo = "selection-mode"
    type AttrOrigin FlowBoxSelectionModePropertyInfo = FlowBox
    attrGet = getFlowBoxSelectionMode
    attrSet = setFlowBoxSelectionMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructFlowBoxSelectionMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.selectionMode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#g:attr:selectionMode"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList FlowBox
type instance O.AttributeList FlowBox = FlowBoxAttributeList
type FlowBoxAttributeList = ('[ '("activateOnSingleClick", FlowBoxActivateOnSingleClickPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("columnSpacing", FlowBoxColumnSpacingPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("homogeneous", FlowBoxHomogeneousPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("maxChildrenPerLine", FlowBoxMaxChildrenPerLinePropertyInfo), '("minChildrenPerLine", FlowBoxMinChildrenPerLinePropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("orientation", Gtk.Orientable.OrientableOrientationPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("rowSpacing", FlowBoxRowSpacingPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("selectionMode", FlowBoxSelectionModePropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
flowBoxActivateOnSingleClick :: AttrLabelProxy "activateOnSingleClick"
flowBoxActivateOnSingleClick = AttrLabelProxy

flowBoxColumnSpacing :: AttrLabelProxy "columnSpacing"
flowBoxColumnSpacing = AttrLabelProxy

flowBoxHomogeneous :: AttrLabelProxy "homogeneous"
flowBoxHomogeneous = AttrLabelProxy

flowBoxMaxChildrenPerLine :: AttrLabelProxy "maxChildrenPerLine"
flowBoxMaxChildrenPerLine = AttrLabelProxy

flowBoxMinChildrenPerLine :: AttrLabelProxy "minChildrenPerLine"
flowBoxMinChildrenPerLine = AttrLabelProxy

flowBoxRowSpacing :: AttrLabelProxy "rowSpacing"
flowBoxRowSpacing = AttrLabelProxy

flowBoxSelectionMode :: AttrLabelProxy "selectionMode"
flowBoxSelectionMode = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList FlowBox = FlowBoxSignalList
type FlowBoxSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activateCursorChild", FlowBoxActivateCursorChildSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childActivated", FlowBoxChildActivatedSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveCursor", FlowBoxMoveCursorSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectAll", FlowBoxSelectAllSignalInfo), '("selectedChildrenChanged", FlowBoxSelectedChildrenChangedSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("toggleCursorChild", FlowBoxToggleCursorChildSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("unselectAll", FlowBoxUnselectAllSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method FlowBox::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "FlowBox" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_flow_box_new" gtk_flow_box_new :: 
    IO (Ptr FlowBox)

-- | Creates a GtkFlowBox.
-- 
-- /Since: 3.12/
flowBoxNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m FlowBox
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.FlowBox.FlowBox' container
flowBoxNew  = liftIO $ do
    result <- gtk_flow_box_new
    checkUnexpectedReturnNULL "flowBoxNew" result
    result' <- (newObject FlowBox) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method FlowBox::bind_model
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "model"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "ListModel" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GListModel to be bound to @box"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "create_widget_func"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "FlowBoxCreateWidgetFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a function that creates widgets for items"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 3
--           , argDestroy = 4
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "user data passed to @create_widget_func"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data_free_func"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "function for freeing @user_data"
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

foreign import ccall "gtk_flow_box_bind_model" gtk_flow_box_bind_model :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    Ptr Gio.ListModel.ListModel ->          -- model : TInterface (Name {namespace = "Gio", name = "ListModel"})
    FunPtr Gtk.Callbacks.C_FlowBoxCreateWidgetFunc -> -- create_widget_func : TInterface (Name {namespace = "Gtk", name = "FlowBoxCreateWidgetFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- user_data_free_func : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Binds /@model@/ to /@box@/.
-- 
-- If /@box@/ was already bound to a model, that previous binding is
-- destroyed.
-- 
-- The contents of /@box@/ are cleared and then filled with widgets that
-- represent items from /@model@/. /@box@/ is updated whenever /@model@/ changes.
-- If /@model@/ is 'P.Nothing', /@box@/ is left empty.
-- 
-- It is undefined to add or remove widgets directly (for example, with
-- 'GI.Gtk.Objects.FlowBox.flowBoxInsert' or 'GI.Gtk.Objects.Container.containerAdd') while /@box@/ is bound to a
-- model.
-- 
-- Note that using a model is incompatible with the filtering and sorting
-- functionality in GtkFlowBox. When using a model, filtering and sorting
-- should be implemented by the model.
-- 
-- /Since: 3.18/
flowBoxBindModel ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a, Gio.ListModel.IsListModel b) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> Maybe (b)
    -- ^ /@model@/: the t'GI.Gio.Interfaces.ListModel.ListModel' to be bound to /@box@/
    -> Gtk.Callbacks.FlowBoxCreateWidgetFunc
    -- ^ /@createWidgetFunc@/: a function that creates widgets for items
    -> m ()
flowBoxBindModel box model createWidgetFunc = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    maybeModel <- case model of
        Nothing -> return nullPtr
        Just jModel -> do
            jModel' <- unsafeManagedPtrCastPtr jModel
            return jModel'
    createWidgetFunc' <- Gtk.Callbacks.mk_FlowBoxCreateWidgetFunc (Gtk.Callbacks.wrap_FlowBoxCreateWidgetFunc Nothing (Gtk.Callbacks.drop_closures_FlowBoxCreateWidgetFunc createWidgetFunc))
    let userData = castFunPtrToPtr createWidgetFunc'
    let userDataFreeFunc = SP.safeFreeFunPtrPtr
    gtk_flow_box_bind_model box' maybeModel createWidgetFunc' userData userDataFreeFunc
    touchManagedPtr box
    whenJust model touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxBindModelMethodInfo
instance (signature ~ (Maybe (b) -> Gtk.Callbacks.FlowBoxCreateWidgetFunc -> m ()), MonadIO m, IsFlowBox a, Gio.ListModel.IsListModel b) => O.OverloadedMethod FlowBoxBindModelMethodInfo a signature where
    overloadedMethod = flowBoxBindModel

instance O.OverloadedMethodInfo FlowBoxBindModelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxBindModel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxBindModel"
        })


#endif

-- method FlowBox::get_activate_on_single_click
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_flow_box_get_activate_on_single_click" gtk_flow_box_get_activate_on_single_click :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    IO CInt

-- | Returns whether children activate on single clicks.
-- 
-- /Since: 3.12/
flowBoxGetActivateOnSingleClick ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if children are activated on single click,
    --     'P.False' otherwise
flowBoxGetActivateOnSingleClick box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_flow_box_get_activate_on_single_click box'
    let result' = (/= 0) result
    touchManagedPtr box
    return result'

#if defined(ENABLE_OVERLOADING)
data FlowBoxGetActivateOnSingleClickMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxGetActivateOnSingleClickMethodInfo a signature where
    overloadedMethod = flowBoxGetActivateOnSingleClick

instance O.OverloadedMethodInfo FlowBoxGetActivateOnSingleClickMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxGetActivateOnSingleClick",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxGetActivateOnSingleClick"
        })


#endif

-- method FlowBox::get_child_at_index
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "idx"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the position of the child"
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
--               (TInterface Name { namespace = "Gtk" , name = "FlowBoxChild" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_flow_box_get_child_at_index" gtk_flow_box_get_child_at_index :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    Int32 ->                                -- idx : TBasicType TInt
    IO (Ptr Gtk.FlowBoxChild.FlowBoxChild)

-- | Gets the nth child in the /@box@/.
-- 
-- /Since: 3.12/
flowBoxGetChildAtIndex ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> Int32
    -- ^ /@idx@/: the position of the child
    -> m (Maybe Gtk.FlowBoxChild.FlowBoxChild)
    -- ^ __Returns:__ the child widget, which will
    --     always be a t'GI.Gtk.Objects.FlowBoxChild.FlowBoxChild' or 'P.Nothing' in case no child widget
    --     with the given index exists.
flowBoxGetChildAtIndex box idx = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_flow_box_get_child_at_index box' idx
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.FlowBoxChild.FlowBoxChild) result'
        return result''
    touchManagedPtr box
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data FlowBoxGetChildAtIndexMethodInfo
instance (signature ~ (Int32 -> m (Maybe Gtk.FlowBoxChild.FlowBoxChild)), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxGetChildAtIndexMethodInfo a signature where
    overloadedMethod = flowBoxGetChildAtIndex

instance O.OverloadedMethodInfo FlowBoxGetChildAtIndexMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxGetChildAtIndex",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxGetChildAtIndex"
        })


#endif

-- method FlowBox::get_child_at_pos
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the x coordinate of the child"
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
--                 { rawDocText = Just "the y coordinate of the child"
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
--               (TInterface Name { namespace = "Gtk" , name = "FlowBoxChild" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_flow_box_get_child_at_pos" gtk_flow_box_get_child_at_pos :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    IO (Ptr Gtk.FlowBoxChild.FlowBoxChild)

-- | Gets the child in the (/@x@/, /@y@/) position.
-- 
-- /Since: 3.22.6/
flowBoxGetChildAtPos ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> Int32
    -- ^ /@x@/: the x coordinate of the child
    -> Int32
    -- ^ /@y@/: the y coordinate of the child
    -> m (Maybe Gtk.FlowBoxChild.FlowBoxChild)
    -- ^ __Returns:__ the child widget, which will
    --     always be a t'GI.Gtk.Objects.FlowBoxChild.FlowBoxChild' or 'P.Nothing' in case no child widget
    --     exists for the given x and y coordinates.
flowBoxGetChildAtPos box x y = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_flow_box_get_child_at_pos box' x y
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.FlowBoxChild.FlowBoxChild) result'
        return result''
    touchManagedPtr box
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data FlowBoxGetChildAtPosMethodInfo
instance (signature ~ (Int32 -> Int32 -> m (Maybe Gtk.FlowBoxChild.FlowBoxChild)), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxGetChildAtPosMethodInfo a signature where
    overloadedMethod = flowBoxGetChildAtPos

instance O.OverloadedMethodInfo FlowBoxGetChildAtPosMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxGetChildAtPos",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxGetChildAtPos"
        })


#endif

-- method FlowBox::get_column_spacing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_flow_box_get_column_spacing" gtk_flow_box_get_column_spacing :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    IO Word32

-- | Gets the horizontal spacing.
-- 
-- /Since: 3.12/
flowBoxGetColumnSpacing ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> m Word32
    -- ^ __Returns:__ the horizontal spacing
flowBoxGetColumnSpacing box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_flow_box_get_column_spacing box'
    touchManagedPtr box
    return result

#if defined(ENABLE_OVERLOADING)
data FlowBoxGetColumnSpacingMethodInfo
instance (signature ~ (m Word32), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxGetColumnSpacingMethodInfo a signature where
    overloadedMethod = flowBoxGetColumnSpacing

instance O.OverloadedMethodInfo FlowBoxGetColumnSpacingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxGetColumnSpacing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxGetColumnSpacing"
        })


#endif

-- method FlowBox::get_homogeneous
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_flow_box_get_homogeneous" gtk_flow_box_get_homogeneous :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    IO CInt

-- | Returns whether the box is homogeneous (all children are the
-- same size). See 'GI.Gtk.Objects.Box.boxSetHomogeneous'.
-- 
-- /Since: 3.12/
flowBoxGetHomogeneous ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the box is homogeneous.
flowBoxGetHomogeneous box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_flow_box_get_homogeneous box'
    let result' = (/= 0) result
    touchManagedPtr box
    return result'

#if defined(ENABLE_OVERLOADING)
data FlowBoxGetHomogeneousMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxGetHomogeneousMethodInfo a signature where
    overloadedMethod = flowBoxGetHomogeneous

instance O.OverloadedMethodInfo FlowBoxGetHomogeneousMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxGetHomogeneous",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxGetHomogeneous"
        })


#endif

-- method FlowBox::get_max_children_per_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_flow_box_get_max_children_per_line" gtk_flow_box_get_max_children_per_line :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    IO Word32

-- | Gets the maximum number of children per line.
-- 
-- /Since: 3.12/
flowBoxGetMaxChildrenPerLine ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> m Word32
    -- ^ __Returns:__ the maximum number of children per line
flowBoxGetMaxChildrenPerLine box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_flow_box_get_max_children_per_line box'
    touchManagedPtr box
    return result

#if defined(ENABLE_OVERLOADING)
data FlowBoxGetMaxChildrenPerLineMethodInfo
instance (signature ~ (m Word32), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxGetMaxChildrenPerLineMethodInfo a signature where
    overloadedMethod = flowBoxGetMaxChildrenPerLine

instance O.OverloadedMethodInfo FlowBoxGetMaxChildrenPerLineMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxGetMaxChildrenPerLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxGetMaxChildrenPerLine"
        })


#endif

-- method FlowBox::get_min_children_per_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_flow_box_get_min_children_per_line" gtk_flow_box_get_min_children_per_line :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    IO Word32

-- | Gets the minimum number of children per line.
-- 
-- /Since: 3.12/
flowBoxGetMinChildrenPerLine ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> m Word32
    -- ^ __Returns:__ the minimum number of children per line
flowBoxGetMinChildrenPerLine box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_flow_box_get_min_children_per_line box'
    touchManagedPtr box
    return result

#if defined(ENABLE_OVERLOADING)
data FlowBoxGetMinChildrenPerLineMethodInfo
instance (signature ~ (m Word32), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxGetMinChildrenPerLineMethodInfo a signature where
    overloadedMethod = flowBoxGetMinChildrenPerLine

instance O.OverloadedMethodInfo FlowBoxGetMinChildrenPerLineMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxGetMinChildrenPerLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxGetMinChildrenPerLine"
        })


#endif

-- method FlowBox::get_row_spacing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_flow_box_get_row_spacing" gtk_flow_box_get_row_spacing :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    IO Word32

-- | Gets the vertical spacing.
-- 
-- /Since: 3.12/
flowBoxGetRowSpacing ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> m Word32
    -- ^ __Returns:__ the vertical spacing
flowBoxGetRowSpacing box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_flow_box_get_row_spacing box'
    touchManagedPtr box
    return result

#if defined(ENABLE_OVERLOADING)
data FlowBoxGetRowSpacingMethodInfo
instance (signature ~ (m Word32), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxGetRowSpacingMethodInfo a signature where
    overloadedMethod = flowBoxGetRowSpacing

instance O.OverloadedMethodInfo FlowBoxGetRowSpacingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxGetRowSpacing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxGetRowSpacing"
        })


#endif

-- method FlowBox::get_selected_children
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
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
--                  (TInterface Name { namespace = "Gtk" , name = "FlowBoxChild" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_flow_box_get_selected_children" gtk_flow_box_get_selected_children :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    IO (Ptr (GList (Ptr Gtk.FlowBoxChild.FlowBoxChild)))

-- | Creates a list of all selected children.
-- 
-- /Since: 3.12/
flowBoxGetSelectedChildren ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> m [Gtk.FlowBoxChild.FlowBoxChild]
    -- ^ __Returns:__ 
    --     A t'GI.GLib.Structs.List.List' containing the t'GI.Gtk.Objects.Widget.Widget' for each selected child.
    --     Free with @/g_list_free()/@ when done.
flowBoxGetSelectedChildren box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_flow_box_get_selected_children box'
    result' <- unpackGList result
    result'' <- mapM (newObject Gtk.FlowBoxChild.FlowBoxChild) result'
    g_list_free result
    touchManagedPtr box
    return result''

#if defined(ENABLE_OVERLOADING)
data FlowBoxGetSelectedChildrenMethodInfo
instance (signature ~ (m [Gtk.FlowBoxChild.FlowBoxChild]), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxGetSelectedChildrenMethodInfo a signature where
    overloadedMethod = flowBoxGetSelectedChildren

instance O.OverloadedMethodInfo FlowBoxGetSelectedChildrenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxGetSelectedChildren",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxGetSelectedChildren"
        })


#endif

-- method FlowBox::get_selection_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "SelectionMode" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_flow_box_get_selection_mode" gtk_flow_box_get_selection_mode :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    IO CUInt

-- | Gets the selection mode of /@box@/.
-- 
-- /Since: 3.12/
flowBoxGetSelectionMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> m Gtk.Enums.SelectionMode
    -- ^ __Returns:__ the t'GI.Gtk.Enums.SelectionMode'
flowBoxGetSelectionMode box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    result <- gtk_flow_box_get_selection_mode box'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr box
    return result'

#if defined(ENABLE_OVERLOADING)
data FlowBoxGetSelectionModeMethodInfo
instance (signature ~ (m Gtk.Enums.SelectionMode), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxGetSelectionModeMethodInfo a signature where
    overloadedMethod = flowBoxGetSelectionMode

instance O.OverloadedMethodInfo FlowBoxGetSelectionModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxGetSelectionMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxGetSelectionMode"
        })


#endif

-- method FlowBox::insert
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the #GtkWidget to add"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the position to insert @child in"
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

foreign import ccall "gtk_flow_box_insert" gtk_flow_box_insert :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- position : TBasicType TInt
    IO ()

-- | Inserts the /@widget@/ into /@box@/ at /@position@/.
-- 
-- If a sort function is set, the widget will actually be inserted
-- at the calculated position and this function has the same effect
-- as 'GI.Gtk.Objects.Container.containerAdd'.
-- 
-- If /@position@/ is -1, or larger than the total number of children
-- in the /@box@/, then the /@widget@/ will be appended to the end.
-- 
-- /Since: 3.12/
flowBoxInsert ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> b
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' to add
    -> Int32
    -- ^ /@position@/: the position to insert /@child@/ in
    -> m ()
flowBoxInsert box widget position = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    widget' <- unsafeManagedPtrCastPtr widget
    gtk_flow_box_insert box' widget' position
    touchManagedPtr box
    touchManagedPtr widget
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxInsertMethodInfo
instance (signature ~ (b -> Int32 -> m ()), MonadIO m, IsFlowBox a, Gtk.Widget.IsWidget b) => O.OverloadedMethod FlowBoxInsertMethodInfo a signature where
    overloadedMethod = flowBoxInsert

instance O.OverloadedMethodInfo FlowBoxInsertMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxInsert",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxInsert"
        })


#endif

-- method FlowBox::invalidate_filter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_flow_box_invalidate_filter" gtk_flow_box_invalidate_filter :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    IO ()

-- | Updates the filtering for all children.
-- 
-- Call this function when the result of the filter
-- function on the /@box@/ is changed due ot an external
-- factor. For instance, this would be used if the
-- filter function just looked for a specific search
-- term, and the entry with the string has changed.
-- 
-- /Since: 3.12/
flowBoxInvalidateFilter ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> m ()
flowBoxInvalidateFilter box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    gtk_flow_box_invalidate_filter box'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxInvalidateFilterMethodInfo
instance (signature ~ (m ()), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxInvalidateFilterMethodInfo a signature where
    overloadedMethod = flowBoxInvalidateFilter

instance O.OverloadedMethodInfo FlowBoxInvalidateFilterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxInvalidateFilter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxInvalidateFilter"
        })


#endif

-- method FlowBox::invalidate_sort
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_flow_box_invalidate_sort" gtk_flow_box_invalidate_sort :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    IO ()

-- | Updates the sorting for all children.
-- 
-- Call this when the result of the sort function on
-- /@box@/ is changed due to an external factor.
-- 
-- /Since: 3.12/
flowBoxInvalidateSort ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> m ()
flowBoxInvalidateSort box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    gtk_flow_box_invalidate_sort box'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxInvalidateSortMethodInfo
instance (signature ~ (m ()), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxInvalidateSortMethodInfo a signature where
    overloadedMethod = flowBoxInvalidateSort

instance O.OverloadedMethodInfo FlowBoxInvalidateSortMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxInvalidateSort",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxInvalidateSort"
        })


#endif

-- method FlowBox::select_all
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_flow_box_select_all" gtk_flow_box_select_all :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    IO ()

-- | Select all children of /@box@/, if the selection
-- mode allows it.
-- 
-- /Since: 3.12/
flowBoxSelectAll ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> m ()
flowBoxSelectAll box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    gtk_flow_box_select_all box'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxSelectAllMethodInfo
instance (signature ~ (m ()), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxSelectAllMethodInfo a signature where
    overloadedMethod = flowBoxSelectAll

instance O.OverloadedMethodInfo FlowBoxSelectAllMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxSelectAll",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxSelectAll"
        })


#endif

-- method FlowBox::select_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "child"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBoxChild" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a child of @box" , sinceVersion = Nothing }
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

foreign import ccall "gtk_flow_box_select_child" gtk_flow_box_select_child :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    Ptr Gtk.FlowBoxChild.FlowBoxChild ->    -- child : TInterface (Name {namespace = "Gtk", name = "FlowBoxChild"})
    IO ()

-- | Selects a single child of /@box@/, if the selection
-- mode allows it.
-- 
-- /Since: 3.12/
flowBoxSelectChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a, Gtk.FlowBoxChild.IsFlowBoxChild b) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> b
    -- ^ /@child@/: a child of /@box@/
    -> m ()
flowBoxSelectChild box child = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    child' <- unsafeManagedPtrCastPtr child
    gtk_flow_box_select_child box' child'
    touchManagedPtr box
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxSelectChildMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsFlowBox a, Gtk.FlowBoxChild.IsFlowBoxChild b) => O.OverloadedMethod FlowBoxSelectChildMethodInfo a signature where
    overloadedMethod = flowBoxSelectChild

instance O.OverloadedMethodInfo FlowBoxSelectChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxSelectChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxSelectChild"
        })


#endif

-- method FlowBox::selected_foreach
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBoxForeachFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the function to call for each selected child"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeCall
--           , argClosure = 2
--           , argDestroy = -1
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
--                 { rawDocText = Just "user data to pass to the function"
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

foreign import ccall "gtk_flow_box_selected_foreach" gtk_flow_box_selected_foreach :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    FunPtr Gtk.Callbacks.C_FlowBoxForeachFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "FlowBoxForeachFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    IO ()

-- | Calls a function for each selected child.
-- 
-- Note that the selection cannot be modified from within
-- this function.
-- 
-- /Since: 3.12/
flowBoxSelectedForeach ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> Gtk.Callbacks.FlowBoxForeachFunc
    -- ^ /@func@/: the function to call for each selected child
    -> m ()
flowBoxSelectedForeach box func = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    func' <- Gtk.Callbacks.mk_FlowBoxForeachFunc (Gtk.Callbacks.wrap_FlowBoxForeachFunc Nothing (Gtk.Callbacks.drop_closures_FlowBoxForeachFunc func))
    let data_ = nullPtr
    gtk_flow_box_selected_foreach box' func' data_
    safeFreeFunPtr $ castFunPtrToPtr func'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxSelectedForeachMethodInfo
instance (signature ~ (Gtk.Callbacks.FlowBoxForeachFunc -> m ()), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxSelectedForeachMethodInfo a signature where
    overloadedMethod = flowBoxSelectedForeach

instance O.OverloadedMethodInfo FlowBoxSelectedForeachMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxSelectedForeach",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxSelectedForeach"
        })


#endif

-- method FlowBox::set_activate_on_single_click
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "single"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE to emit child-activated on a single click"
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

foreign import ccall "gtk_flow_box_set_activate_on_single_click" gtk_flow_box_set_activate_on_single_click :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    CInt ->                                 -- single : TBasicType TBoolean
    IO ()

-- | If /@single@/ is 'P.True', children will be activated when you click
-- on them, otherwise you need to double-click.
-- 
-- /Since: 3.12/
flowBoxSetActivateOnSingleClick ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> Bool
    -- ^ /@single@/: 'P.True' to emit child-activated on a single click
    -> m ()
flowBoxSetActivateOnSingleClick box single = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    let single' = (fromIntegral . fromEnum) single
    gtk_flow_box_set_activate_on_single_click box' single'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxSetActivateOnSingleClickMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxSetActivateOnSingleClickMethodInfo a signature where
    overloadedMethod = flowBoxSetActivateOnSingleClick

instance O.OverloadedMethodInfo FlowBoxSetActivateOnSingleClickMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxSetActivateOnSingleClick",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxSetActivateOnSingleClick"
        })


#endif

-- method FlowBox::set_column_spacing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "spacing"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the spacing to use" , sinceVersion = Nothing }
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

foreign import ccall "gtk_flow_box_set_column_spacing" gtk_flow_box_set_column_spacing :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    Word32 ->                               -- spacing : TBasicType TUInt
    IO ()

-- | Sets the horizontal space to add between children.
-- See the [FlowBox:columnSpacing]("GI.Gtk.Objects.FlowBox#g:attr:columnSpacing") property.
-- 
-- /Since: 3.12/
flowBoxSetColumnSpacing ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> Word32
    -- ^ /@spacing@/: the spacing to use
    -> m ()
flowBoxSetColumnSpacing box spacing = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    gtk_flow_box_set_column_spacing box' spacing
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxSetColumnSpacingMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxSetColumnSpacingMethodInfo a signature where
    overloadedMethod = flowBoxSetColumnSpacing

instance O.OverloadedMethodInfo FlowBoxSetColumnSpacingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxSetColumnSpacing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxSetColumnSpacing"
        })


#endif

-- method FlowBox::set_filter_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filter_func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBoxFilterFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "callback that\n    lets you filter which children to show"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "user data passed to @filter_func"
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
--                 { rawDocText = Just "destroy notifier for @user_data"
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

foreign import ccall "gtk_flow_box_set_filter_func" gtk_flow_box_set_filter_func :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    FunPtr Gtk.Callbacks.C_FlowBoxFilterFunc -> -- filter_func : TInterface (Name {namespace = "Gtk", name = "FlowBoxFilterFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | By setting a filter function on the /@box@/ one can decide dynamically
-- which of the children to show. For instance, to implement a search
-- function that only shows the children matching the search terms.
-- 
-- The /@filterFunc@/ will be called for each child after the call, and
-- it will continue to be called each time a child changes (via
-- 'GI.Gtk.Objects.FlowBoxChild.flowBoxChildChanged') or when 'GI.Gtk.Objects.FlowBox.flowBoxInvalidateFilter'
-- is called.
-- 
-- Note that using a filter function is incompatible with using a model
-- (see 'GI.Gtk.Objects.FlowBox.flowBoxBindModel').
-- 
-- /Since: 3.12/
flowBoxSetFilterFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> Maybe (Gtk.Callbacks.FlowBoxFilterFunc)
    -- ^ /@filterFunc@/: callback that
    --     lets you filter which children to show
    -> m ()
flowBoxSetFilterFunc box filterFunc = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    maybeFilterFunc <- case filterFunc of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jFilterFunc -> do
            jFilterFunc' <- Gtk.Callbacks.mk_FlowBoxFilterFunc (Gtk.Callbacks.wrap_FlowBoxFilterFunc Nothing (Gtk.Callbacks.drop_closures_FlowBoxFilterFunc jFilterFunc))
            return jFilterFunc'
    let userData = castFunPtrToPtr maybeFilterFunc
    let destroy = SP.safeFreeFunPtrPtr
    gtk_flow_box_set_filter_func box' maybeFilterFunc userData destroy
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxSetFilterFuncMethodInfo
instance (signature ~ (Maybe (Gtk.Callbacks.FlowBoxFilterFunc) -> m ()), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxSetFilterFuncMethodInfo a signature where
    overloadedMethod = flowBoxSetFilterFunc

instance O.OverloadedMethodInfo FlowBoxSetFilterFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxSetFilterFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxSetFilterFunc"
        })


#endif

-- method FlowBox::set_hadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "adjustment"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Adjustment" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "an adjustment which should be adjusted\n   when the focus is moved among the descendents of @container"
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

foreign import ccall "gtk_flow_box_set_hadjustment" gtk_flow_box_set_hadjustment :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    Ptr Gtk.Adjustment.Adjustment ->        -- adjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    IO ()

-- | Hooks up an adjustment to focus handling in /@box@/.
-- The adjustment is also used for autoscrolling during
-- rubberband selection. See 'GI.Gtk.Objects.ScrolledWindow.scrolledWindowGetHadjustment'
-- for a typical way of obtaining the adjustment, and
-- 'GI.Gtk.Objects.FlowBox.flowBoxSetVadjustment'for setting the vertical
-- adjustment.
-- 
-- The adjustments have to be in pixel units and in the same
-- coordinate system as the allocation for immediate children
-- of the box.
-- 
-- /Since: 3.12/
flowBoxSetHadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a, Gtk.Adjustment.IsAdjustment b) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> b
    -- ^ /@adjustment@/: an adjustment which should be adjusted
    --    when the focus is moved among the descendents of /@container@/
    -> m ()
flowBoxSetHadjustment box adjustment = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    adjustment' <- unsafeManagedPtrCastPtr adjustment
    gtk_flow_box_set_hadjustment box' adjustment'
    touchManagedPtr box
    touchManagedPtr adjustment
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxSetHadjustmentMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsFlowBox a, Gtk.Adjustment.IsAdjustment b) => O.OverloadedMethod FlowBoxSetHadjustmentMethodInfo a signature where
    overloadedMethod = flowBoxSetHadjustment

instance O.OverloadedMethodInfo FlowBoxSetHadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxSetHadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxSetHadjustment"
        })


#endif

-- method FlowBox::set_homogeneous
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "homogeneous"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "%TRUE to create equal allotments,\n  %FALSE for variable allotments"
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

foreign import ccall "gtk_flow_box_set_homogeneous" gtk_flow_box_set_homogeneous :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    CInt ->                                 -- homogeneous : TBasicType TBoolean
    IO ()

-- | Sets the [FlowBox:homogeneous]("GI.Gtk.Objects.FlowBox#g:attr:homogeneous") property of /@box@/, controlling
-- whether or not all children of /@box@/ are given equal space
-- in the box.
-- 
-- /Since: 3.12/
flowBoxSetHomogeneous ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> Bool
    -- ^ /@homogeneous@/: 'P.True' to create equal allotments,
    --   'P.False' for variable allotments
    -> m ()
flowBoxSetHomogeneous box homogeneous = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    let homogeneous' = (fromIntegral . fromEnum) homogeneous
    gtk_flow_box_set_homogeneous box' homogeneous'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxSetHomogeneousMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxSetHomogeneousMethodInfo a signature where
    overloadedMethod = flowBoxSetHomogeneous

instance O.OverloadedMethodInfo FlowBoxSetHomogeneousMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxSetHomogeneous",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxSetHomogeneous"
        })


#endif

-- method FlowBox::set_max_children_per_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_children"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the maximum number of children per line"
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

foreign import ccall "gtk_flow_box_set_max_children_per_line" gtk_flow_box_set_max_children_per_line :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    Word32 ->                               -- n_children : TBasicType TUInt
    IO ()

-- | Sets the maximum number of children to request and
-- allocate space for in /@box@/’s orientation.
-- 
-- Setting the maximum number of children per line
-- limits the overall natural size request to be no more
-- than /@nChildren@/ children long in the given orientation.
-- 
-- /Since: 3.12/
flowBoxSetMaxChildrenPerLine ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> Word32
    -- ^ /@nChildren@/: the maximum number of children per line
    -> m ()
flowBoxSetMaxChildrenPerLine box nChildren = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    gtk_flow_box_set_max_children_per_line box' nChildren
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxSetMaxChildrenPerLineMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxSetMaxChildrenPerLineMethodInfo a signature where
    overloadedMethod = flowBoxSetMaxChildrenPerLine

instance O.OverloadedMethodInfo FlowBoxSetMaxChildrenPerLineMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxSetMaxChildrenPerLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxSetMaxChildrenPerLine"
        })


#endif

-- method FlowBox::set_min_children_per_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_children"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the minimum number of children per line"
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

foreign import ccall "gtk_flow_box_set_min_children_per_line" gtk_flow_box_set_min_children_per_line :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    Word32 ->                               -- n_children : TBasicType TUInt
    IO ()

-- | Sets the minimum number of children to line up
-- in /@box@/’s orientation before flowing.
-- 
-- /Since: 3.12/
flowBoxSetMinChildrenPerLine ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> Word32
    -- ^ /@nChildren@/: the minimum number of children per line
    -> m ()
flowBoxSetMinChildrenPerLine box nChildren = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    gtk_flow_box_set_min_children_per_line box' nChildren
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxSetMinChildrenPerLineMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxSetMinChildrenPerLineMethodInfo a signature where
    overloadedMethod = flowBoxSetMinChildrenPerLine

instance O.OverloadedMethodInfo FlowBoxSetMinChildrenPerLineMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxSetMinChildrenPerLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxSetMinChildrenPerLine"
        })


#endif

-- method FlowBox::set_row_spacing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "spacing"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the spacing to use" , sinceVersion = Nothing }
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

foreign import ccall "gtk_flow_box_set_row_spacing" gtk_flow_box_set_row_spacing :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    Word32 ->                               -- spacing : TBasicType TUInt
    IO ()

-- | Sets the vertical space to add between children.
-- See the [FlowBox:rowSpacing]("GI.Gtk.Objects.FlowBox#g:attr:rowSpacing") property.
-- 
-- /Since: 3.12/
flowBoxSetRowSpacing ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> Word32
    -- ^ /@spacing@/: the spacing to use
    -> m ()
flowBoxSetRowSpacing box spacing = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    gtk_flow_box_set_row_spacing box' spacing
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxSetRowSpacingMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxSetRowSpacingMethodInfo a signature where
    overloadedMethod = flowBoxSetRowSpacing

instance O.OverloadedMethodInfo FlowBoxSetRowSpacingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxSetRowSpacing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxSetRowSpacing"
        })


#endif

-- method FlowBox::set_selection_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "mode"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionMode" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new selection mode"
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

foreign import ccall "gtk_flow_box_set_selection_mode" gtk_flow_box_set_selection_mode :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    CUInt ->                                -- mode : TInterface (Name {namespace = "Gtk", name = "SelectionMode"})
    IO ()

-- | Sets how selection works in /@box@/.
-- See t'GI.Gtk.Enums.SelectionMode' for details.
-- 
-- /Since: 3.12/
flowBoxSetSelectionMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> Gtk.Enums.SelectionMode
    -- ^ /@mode@/: the new selection mode
    -> m ()
flowBoxSetSelectionMode box mode = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    let mode' = (fromIntegral . fromEnum) mode
    gtk_flow_box_set_selection_mode box' mode'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxSetSelectionModeMethodInfo
instance (signature ~ (Gtk.Enums.SelectionMode -> m ()), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxSetSelectionModeMethodInfo a signature where
    overloadedMethod = flowBoxSetSelectionMode

instance O.OverloadedMethodInfo FlowBoxSetSelectionModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxSetSelectionMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxSetSelectionMode"
        })


#endif

-- method FlowBox::set_sort_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sort_func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBoxSortFunc" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the sort function" , sinceVersion = Nothing }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "user data passed to @sort_func"
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
--                 { rawDocText = Just "destroy notifier for @user_data"
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

foreign import ccall "gtk_flow_box_set_sort_func" gtk_flow_box_set_sort_func :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    FunPtr Gtk.Callbacks.C_FlowBoxSortFunc -> -- sort_func : TInterface (Name {namespace = "Gtk", name = "FlowBoxSortFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | By setting a sort function on the /@box@/, one can dynamically
-- reorder the children of the box, based on the contents of
-- the children.
-- 
-- The /@sortFunc@/ will be called for each child after the call,
-- and will continue to be called each time a child changes (via
-- 'GI.Gtk.Objects.FlowBoxChild.flowBoxChildChanged') and when 'GI.Gtk.Objects.FlowBox.flowBoxInvalidateSort'
-- is called.
-- 
-- Note that using a sort function is incompatible with using a model
-- (see 'GI.Gtk.Objects.FlowBox.flowBoxBindModel').
-- 
-- /Since: 3.12/
flowBoxSetSortFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> Maybe (Gtk.Callbacks.FlowBoxSortFunc)
    -- ^ /@sortFunc@/: the sort function
    -> m ()
flowBoxSetSortFunc box sortFunc = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    maybeSortFunc <- case sortFunc of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jSortFunc -> do
            jSortFunc' <- Gtk.Callbacks.mk_FlowBoxSortFunc (Gtk.Callbacks.wrap_FlowBoxSortFunc Nothing (Gtk.Callbacks.drop_closures_FlowBoxSortFunc jSortFunc))
            return jSortFunc'
    let userData = castFunPtrToPtr maybeSortFunc
    let destroy = SP.safeFreeFunPtrPtr
    gtk_flow_box_set_sort_func box' maybeSortFunc userData destroy
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxSetSortFuncMethodInfo
instance (signature ~ (Maybe (Gtk.Callbacks.FlowBoxSortFunc) -> m ()), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxSetSortFuncMethodInfo a signature where
    overloadedMethod = flowBoxSetSortFunc

instance O.OverloadedMethodInfo FlowBoxSetSortFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxSetSortFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxSetSortFunc"
        })


#endif

-- method FlowBox::set_vadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "adjustment"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Adjustment" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "an adjustment which should be adjusted\n   when the focus is moved among the descendents of @container"
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

foreign import ccall "gtk_flow_box_set_vadjustment" gtk_flow_box_set_vadjustment :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    Ptr Gtk.Adjustment.Adjustment ->        -- adjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    IO ()

-- | Hooks up an adjustment to focus handling in /@box@/.
-- The adjustment is also used for autoscrolling during
-- rubberband selection. See 'GI.Gtk.Objects.ScrolledWindow.scrolledWindowGetVadjustment'
-- for a typical way of obtaining the adjustment, and
-- 'GI.Gtk.Objects.FlowBox.flowBoxSetHadjustment'for setting the horizontal
-- adjustment.
-- 
-- The adjustments have to be in pixel units and in the same
-- coordinate system as the allocation for immediate children
-- of the box.
-- 
-- /Since: 3.12/
flowBoxSetVadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a, Gtk.Adjustment.IsAdjustment b) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> b
    -- ^ /@adjustment@/: an adjustment which should be adjusted
    --    when the focus is moved among the descendents of /@container@/
    -> m ()
flowBoxSetVadjustment box adjustment = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    adjustment' <- unsafeManagedPtrCastPtr adjustment
    gtk_flow_box_set_vadjustment box' adjustment'
    touchManagedPtr box
    touchManagedPtr adjustment
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxSetVadjustmentMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsFlowBox a, Gtk.Adjustment.IsAdjustment b) => O.OverloadedMethod FlowBoxSetVadjustmentMethodInfo a signature where
    overloadedMethod = flowBoxSetVadjustment

instance O.OverloadedMethodInfo FlowBoxSetVadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxSetVadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxSetVadjustment"
        })


#endif

-- method FlowBox::unselect_all
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_flow_box_unselect_all" gtk_flow_box_unselect_all :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    IO ()

-- | Unselect all children of /@box@/, if the selection
-- mode allows it.
-- 
-- /Since: 3.12/
flowBoxUnselectAll ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> m ()
flowBoxUnselectAll box = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    gtk_flow_box_unselect_all box'
    touchManagedPtr box
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxUnselectAllMethodInfo
instance (signature ~ (m ()), MonadIO m, IsFlowBox a) => O.OverloadedMethod FlowBoxUnselectAllMethodInfo a signature where
    overloadedMethod = flowBoxUnselectAll

instance O.OverloadedMethodInfo FlowBoxUnselectAllMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxUnselectAll",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxUnselectAll"
        })


#endif

-- method FlowBox::unselect_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFlowBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "child"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FlowBoxChild" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a child of @box" , sinceVersion = Nothing }
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

foreign import ccall "gtk_flow_box_unselect_child" gtk_flow_box_unselect_child :: 
    Ptr FlowBox ->                          -- box : TInterface (Name {namespace = "Gtk", name = "FlowBox"})
    Ptr Gtk.FlowBoxChild.FlowBoxChild ->    -- child : TInterface (Name {namespace = "Gtk", name = "FlowBoxChild"})
    IO ()

-- | Unselects a single child of /@box@/, if the selection
-- mode allows it.
-- 
-- /Since: 3.12/
flowBoxUnselectChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsFlowBox a, Gtk.FlowBoxChild.IsFlowBoxChild b) =>
    a
    -- ^ /@box@/: a t'GI.Gtk.Objects.FlowBox.FlowBox'
    -> b
    -- ^ /@child@/: a child of /@box@/
    -> m ()
flowBoxUnselectChild box child = liftIO $ do
    box' <- unsafeManagedPtrCastPtr box
    child' <- unsafeManagedPtrCastPtr child
    gtk_flow_box_unselect_child box' child'
    touchManagedPtr box
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data FlowBoxUnselectChildMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsFlowBox a, Gtk.FlowBoxChild.IsFlowBoxChild b) => O.OverloadedMethod FlowBoxUnselectChildMethodInfo a signature where
    overloadedMethod = flowBoxUnselectChild

instance O.OverloadedMethodInfo FlowBoxUnselectChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.FlowBox.flowBoxUnselectChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-FlowBox.html#v:flowBoxUnselectChild"
        })


#endif


