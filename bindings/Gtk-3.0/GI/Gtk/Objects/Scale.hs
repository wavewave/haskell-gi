{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A GtkScale is a slider control used to select a numeric value.
-- To use it, you’ll probably want to investigate the methods on
-- its base class, t'GI.Gtk.Objects.Range.Range', in addition to the methods for GtkScale itself.
-- To set the value of a scale, you would normally use 'GI.Gtk.Objects.Range.rangeSetValue'.
-- To detect changes to the value, you would normally use the
-- [Range::valueChanged]("GI.Gtk.Objects.Range#g:signal:valueChanged") signal.
-- 
-- Note that using the same upper and lower bounds for the t'GI.Gtk.Objects.Scale.Scale' (through
-- the t'GI.Gtk.Objects.Range.Range' methods) will hide the slider itself. This is useful for
-- applications that want to show an undeterminate value on the scale, without
-- changing the layout of the application (such as movie or music players).
-- 
-- = GtkScale as GtkBuildable
-- 
-- GtkScale supports a custom @\<marks>@ element, which can contain multiple
-- @\<mark>@ elements. The “value” and “position” attributes have the same
-- meaning as 'GI.Gtk.Objects.Scale.scaleAddMark' parameters of the same name. If the
-- element is not empty, its content is taken as the markup to show at
-- the mark. It can be translated with the usual ”translatable” and
-- “context” attributes.
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >scale[.fine-tune][.marks-before][.marks-after]
-- >├── marks.top
-- >│   ├── mark
-- >│   ┊    ├── [label]
-- >│   ┊    ╰── indicator
-- >┊   ┊
-- >│   ╰── mark
-- >├── [value]
-- >├── contents
-- >│   ╰── trough
-- >│       ├── slider
-- >│       ├── [highlight]
-- >│       ╰── [fill]
-- >╰── marks.bottom
-- >    ├── mark
-- >    ┊    ├── indicator
-- >    ┊    ╰── [label]
-- >    ╰── mark
-- 
-- 
-- GtkScale has a main CSS node with name scale and a subnode for its contents,
-- with subnodes named trough and slider.
-- 
-- The main node gets the style class .fine-tune added when the scale is in
-- \'fine-tuning\' mode.
-- 
-- If the scale has an origin (see 'GI.Gtk.Objects.Scale.scaleSetHasOrigin'), there is a
-- subnode with name highlight below the trough node that is used for rendering
-- the highlighted part of the trough.
-- 
-- If the scale is showing a fill level (see 'GI.Gtk.Objects.Range.rangeSetShowFillLevel'),
-- there is a subnode with name fill below the trough node that is used for
-- rendering the filled in part of the trough.
-- 
-- If marks are present, there is a marks subnode before or after the contents
-- node, below which each mark gets a node with name mark. The marks nodes get
-- either the .top or .bottom style class.
-- 
-- The mark node has a subnode named indicator. If the mark has text, it also
-- has a subnode named label. When the mark is either above or left of the
-- scale, the label subnode is the first when present. Otherwise, the indicator
-- subnode is the first.
-- 
-- The main CSS node gets the \'marks-before\' and\/or \'marks-after\' style classes
-- added depending on what marks are present.
-- 
-- If the scale is displaying the value (see [Scale:drawValue]("GI.Gtk.Objects.Scale#g:attr:drawValue")), there is
-- subnode with name value.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Scale
    ( 

-- * Exported types
    Scale(..)                               ,
    IsScale                                 ,
    toScale                                 ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMark]("GI.Gtk.Objects.Scale#g:method:addMark"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childNotify]("GI.Gtk.Objects.Widget#g:method:childNotify"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [clearMarks]("GI.Gtk.Objects.Scale#g:method:clearMarks"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAdjustment]("GI.Gtk.Objects.Range#g:method:getAdjustment"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDigits]("GI.Gtk.Objects.Scale#g:method:getDigits"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getDrawValue]("GI.Gtk.Objects.Scale#g:method:getDrawValue"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFillLevel]("GI.Gtk.Objects.Range#g:method:getFillLevel"), [getFlippable]("GI.Gtk.Objects.Range#g:method:getFlippable"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasOrigin]("GI.Gtk.Objects.Scale#g:method:getHasOrigin"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getInverted]("GI.Gtk.Objects.Range#g:method:getInverted"), [getLayout]("GI.Gtk.Objects.Scale#g:method:getLayout"), [getLayoutOffsets]("GI.Gtk.Objects.Scale#g:method:getLayoutOffsets"), [getLowerStepperSensitivity]("GI.Gtk.Objects.Range#g:method:getLowerStepperSensitivity"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMinSliderSize]("GI.Gtk.Objects.Range#g:method:getMinSliderSize"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getOrientation]("GI.Gtk.Interfaces.Orientable#g:method:getOrientation"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRangeRect]("GI.Gtk.Objects.Range#g:method:getRangeRect"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getRestrictToFillLevel]("GI.Gtk.Objects.Range#g:method:getRestrictToFillLevel"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getRoundDigits]("GI.Gtk.Objects.Range#g:method:getRoundDigits"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getShowFillLevel]("GI.Gtk.Objects.Range#g:method:getShowFillLevel"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getSliderRange]("GI.Gtk.Objects.Range#g:method:getSliderRange"), [getSliderSizeFixed]("GI.Gtk.Objects.Range#g:method:getSliderSizeFixed"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getUpperStepperSensitivity]("GI.Gtk.Objects.Range#g:method:getUpperStepperSensitivity"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getValue]("GI.Gtk.Objects.Range#g:method:getValue"), [getValuePos]("GI.Gtk.Objects.Scale#g:method:getValuePos"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAdjustment]("GI.Gtk.Objects.Range#g:method:setAdjustment"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDigits]("GI.Gtk.Objects.Scale#g:method:setDigits"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setDrawValue]("GI.Gtk.Objects.Scale#g:method:setDrawValue"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFillLevel]("GI.Gtk.Objects.Range#g:method:setFillLevel"), [setFlippable]("GI.Gtk.Objects.Range#g:method:setFlippable"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasOrigin]("GI.Gtk.Objects.Scale#g:method:setHasOrigin"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setIncrements]("GI.Gtk.Objects.Range#g:method:setIncrements"), [setInverted]("GI.Gtk.Objects.Range#g:method:setInverted"), [setLowerStepperSensitivity]("GI.Gtk.Objects.Range#g:method:setLowerStepperSensitivity"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMinSliderSize]("GI.Gtk.Objects.Range#g:method:setMinSliderSize"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setOrientation]("GI.Gtk.Interfaces.Orientable#g:method:setOrientation"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRange]("GI.Gtk.Objects.Range#g:method:setRange"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setRestrictToFillLevel]("GI.Gtk.Objects.Range#g:method:setRestrictToFillLevel"), [setRoundDigits]("GI.Gtk.Objects.Range#g:method:setRoundDigits"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setShowFillLevel]("GI.Gtk.Objects.Range#g:method:setShowFillLevel"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setSliderSizeFixed]("GI.Gtk.Objects.Range#g:method:setSliderSizeFixed"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setUpperStepperSensitivity]("GI.Gtk.Objects.Range#g:method:setUpperStepperSensitivity"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setValue]("GI.Gtk.Objects.Range#g:method:setValue"), [setValuePos]("GI.Gtk.Objects.Scale#g:method:setValuePos"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveScaleMethod                      ,
#endif

-- ** addMark #method:addMark#

#if defined(ENABLE_OVERLOADING)
    ScaleAddMarkMethodInfo                  ,
#endif
    scaleAddMark                            ,


-- ** clearMarks #method:clearMarks#

#if defined(ENABLE_OVERLOADING)
    ScaleClearMarksMethodInfo               ,
#endif
    scaleClearMarks                         ,


-- ** getDigits #method:getDigits#

#if defined(ENABLE_OVERLOADING)
    ScaleGetDigitsMethodInfo                ,
#endif
    scaleGetDigits                          ,


-- ** getDrawValue #method:getDrawValue#

#if defined(ENABLE_OVERLOADING)
    ScaleGetDrawValueMethodInfo             ,
#endif
    scaleGetDrawValue                       ,


-- ** getHasOrigin #method:getHasOrigin#

#if defined(ENABLE_OVERLOADING)
    ScaleGetHasOriginMethodInfo             ,
#endif
    scaleGetHasOrigin                       ,


-- ** getLayout #method:getLayout#

#if defined(ENABLE_OVERLOADING)
    ScaleGetLayoutMethodInfo                ,
#endif
    scaleGetLayout                          ,


-- ** getLayoutOffsets #method:getLayoutOffsets#

#if defined(ENABLE_OVERLOADING)
    ScaleGetLayoutOffsetsMethodInfo         ,
#endif
    scaleGetLayoutOffsets                   ,


-- ** getValuePos #method:getValuePos#

#if defined(ENABLE_OVERLOADING)
    ScaleGetValuePosMethodInfo              ,
#endif
    scaleGetValuePos                        ,


-- ** new #method:new#

    scaleNew                                ,


-- ** newWithRange #method:newWithRange#

    scaleNewWithRange                       ,


-- ** setDigits #method:setDigits#

#if defined(ENABLE_OVERLOADING)
    ScaleSetDigitsMethodInfo                ,
#endif
    scaleSetDigits                          ,


-- ** setDrawValue #method:setDrawValue#

#if defined(ENABLE_OVERLOADING)
    ScaleSetDrawValueMethodInfo             ,
#endif
    scaleSetDrawValue                       ,


-- ** setHasOrigin #method:setHasOrigin#

#if defined(ENABLE_OVERLOADING)
    ScaleSetHasOriginMethodInfo             ,
#endif
    scaleSetHasOrigin                       ,


-- ** setValuePos #method:setValuePos#

#if defined(ENABLE_OVERLOADING)
    ScaleSetValuePosMethodInfo              ,
#endif
    scaleSetValuePos                        ,




 -- * Properties


-- ** digits #attr:digits#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ScaleDigitsPropertyInfo                 ,
#endif
    constructScaleDigits                    ,
    getScaleDigits                          ,
#if defined(ENABLE_OVERLOADING)
    scaleDigits                             ,
#endif
    setScaleDigits                          ,


-- ** drawValue #attr:drawValue#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ScaleDrawValuePropertyInfo              ,
#endif
    constructScaleDrawValue                 ,
    getScaleDrawValue                       ,
#if defined(ENABLE_OVERLOADING)
    scaleDrawValue                          ,
#endif
    setScaleDrawValue                       ,


-- ** hasOrigin #attr:hasOrigin#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ScaleHasOriginPropertyInfo              ,
#endif
    constructScaleHasOrigin                 ,
    getScaleHasOrigin                       ,
#if defined(ENABLE_OVERLOADING)
    scaleHasOrigin                          ,
#endif
    setScaleHasOrigin                       ,


-- ** valuePos #attr:valuePos#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ScaleValuePosPropertyInfo               ,
#endif
    constructScaleValuePos                  ,
    getScaleValuePos                        ,
#if defined(ENABLE_OVERLOADING)
    scaleValuePos                           ,
#endif
    setScaleValuePos                        ,




 -- * Signals


-- ** formatValue #signal:formatValue#

    ScaleFormatValueCallback                ,
#if defined(ENABLE_OVERLOADING)
    ScaleFormatValueSignalInfo              ,
#endif
    afterScaleFormatValue                   ,
    onScaleFormatValue                      ,




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
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Orientable as Gtk.Orientable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Adjustment as Gtk.Adjustment
import {-# SOURCE #-} qualified GI.Gtk.Objects.Range as Gtk.Range
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import qualified GI.Pango.Objects.Layout as Pango.Layout

-- | Memory-managed wrapper type.
newtype Scale = Scale (SP.ManagedPtr Scale)
    deriving (Eq)

instance SP.ManagedPtrNewtype Scale where
    toManagedPtr (Scale p) = p

foreign import ccall "gtk_scale_get_type"
    c_gtk_scale_get_type :: IO B.Types.GType

instance B.Types.TypedObject Scale where
    glibType = c_gtk_scale_get_type

instance B.Types.GObject Scale

-- | Type class for types which can be safely cast to `Scale`, for instance with `toScale`.
class (SP.GObject o, O.IsDescendantOf Scale o) => IsScale o
instance (SP.GObject o, O.IsDescendantOf Scale o) => IsScale o

instance O.HasParentTypes Scale
type instance O.ParentTypes Scale = '[Gtk.Range.Range, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable, Gtk.Orientable.Orientable]

-- | Cast to `Scale`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toScale :: (MIO.MonadIO m, IsScale o) => o -> m Scale
toScale = MIO.liftIO . B.ManagedPtr.unsafeCastTo Scale

-- | Convert 'Scale' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Scale) where
    gvalueGType_ = c_gtk_scale_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Scale)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Scale)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Scale ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveScaleMethod (t :: Symbol) (o :: *) :: * where
    ResolveScaleMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveScaleMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveScaleMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveScaleMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveScaleMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveScaleMethod "addMark" o = ScaleAddMarkMethodInfo
    ResolveScaleMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveScaleMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveScaleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveScaleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveScaleMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveScaleMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveScaleMethod "childNotify" o = Gtk.Widget.WidgetChildNotifyMethodInfo
    ResolveScaleMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveScaleMethod "clearMarks" o = ScaleClearMarksMethodInfo
    ResolveScaleMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveScaleMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveScaleMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveScaleMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveScaleMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveScaleMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveScaleMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveScaleMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveScaleMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveScaleMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveScaleMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveScaleMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveScaleMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveScaleMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveScaleMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveScaleMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveScaleMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveScaleMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveScaleMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveScaleMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveScaleMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveScaleMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveScaleMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveScaleMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveScaleMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveScaleMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveScaleMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveScaleMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveScaleMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveScaleMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveScaleMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveScaleMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveScaleMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveScaleMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveScaleMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveScaleMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveScaleMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveScaleMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveScaleMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveScaleMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveScaleMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveScaleMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveScaleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveScaleMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveScaleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveScaleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveScaleMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveScaleMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveScaleMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveScaleMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveScaleMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveScaleMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveScaleMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveScaleMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveScaleMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveScaleMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveScaleMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveScaleMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveScaleMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveScaleMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveScaleMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveScaleMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveScaleMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveScaleMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveScaleMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveScaleMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveScaleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveScaleMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveScaleMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveScaleMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveScaleMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveScaleMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveScaleMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveScaleMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveScaleMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveScaleMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveScaleMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveScaleMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveScaleMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveScaleMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveScaleMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveScaleMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveScaleMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveScaleMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveScaleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveScaleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveScaleMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveScaleMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveScaleMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveScaleMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveScaleMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveScaleMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveScaleMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveScaleMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveScaleMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveScaleMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveScaleMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveScaleMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveScaleMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveScaleMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveScaleMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveScaleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveScaleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveScaleMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveScaleMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveScaleMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveScaleMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveScaleMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveScaleMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveScaleMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveScaleMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveScaleMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveScaleMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveScaleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveScaleMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveScaleMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveScaleMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveScaleMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveScaleMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveScaleMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveScaleMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveScaleMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveScaleMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveScaleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveScaleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveScaleMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveScaleMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveScaleMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveScaleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveScaleMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveScaleMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveScaleMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveScaleMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveScaleMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveScaleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveScaleMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveScaleMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveScaleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveScaleMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveScaleMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveScaleMethod "getAdjustment" o = Gtk.Range.RangeGetAdjustmentMethodInfo
    ResolveScaleMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveScaleMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveScaleMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveScaleMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveScaleMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveScaleMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveScaleMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveScaleMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveScaleMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveScaleMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveScaleMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveScaleMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveScaleMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveScaleMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveScaleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveScaleMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveScaleMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveScaleMethod "getDigits" o = ScaleGetDigitsMethodInfo
    ResolveScaleMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveScaleMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveScaleMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveScaleMethod "getDrawValue" o = ScaleGetDrawValueMethodInfo
    ResolveScaleMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveScaleMethod "getFillLevel" o = Gtk.Range.RangeGetFillLevelMethodInfo
    ResolveScaleMethod "getFlippable" o = Gtk.Range.RangeGetFlippableMethodInfo
    ResolveScaleMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveScaleMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveScaleMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveScaleMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveScaleMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveScaleMethod "getHasOrigin" o = ScaleGetHasOriginMethodInfo
    ResolveScaleMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveScaleMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveScaleMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveScaleMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveScaleMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveScaleMethod "getInverted" o = Gtk.Range.RangeGetInvertedMethodInfo
    ResolveScaleMethod "getLayout" o = ScaleGetLayoutMethodInfo
    ResolveScaleMethod "getLayoutOffsets" o = ScaleGetLayoutOffsetsMethodInfo
    ResolveScaleMethod "getLowerStepperSensitivity" o = Gtk.Range.RangeGetLowerStepperSensitivityMethodInfo
    ResolveScaleMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveScaleMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveScaleMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveScaleMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveScaleMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveScaleMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveScaleMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveScaleMethod "getMinSliderSize" o = Gtk.Range.RangeGetMinSliderSizeMethodInfo
    ResolveScaleMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveScaleMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveScaleMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveScaleMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveScaleMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveScaleMethod "getOrientation" o = Gtk.Orientable.OrientableGetOrientationMethodInfo
    ResolveScaleMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveScaleMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveScaleMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveScaleMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveScaleMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveScaleMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveScaleMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveScaleMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveScaleMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveScaleMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveScaleMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveScaleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveScaleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveScaleMethod "getRangeRect" o = Gtk.Range.RangeGetRangeRectMethodInfo
    ResolveScaleMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveScaleMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveScaleMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveScaleMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveScaleMethod "getRestrictToFillLevel" o = Gtk.Range.RangeGetRestrictToFillLevelMethodInfo
    ResolveScaleMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveScaleMethod "getRoundDigits" o = Gtk.Range.RangeGetRoundDigitsMethodInfo
    ResolveScaleMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveScaleMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveScaleMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveScaleMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveScaleMethod "getShowFillLevel" o = Gtk.Range.RangeGetShowFillLevelMethodInfo
    ResolveScaleMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveScaleMethod "getSliderRange" o = Gtk.Range.RangeGetSliderRangeMethodInfo
    ResolveScaleMethod "getSliderSizeFixed" o = Gtk.Range.RangeGetSliderSizeFixedMethodInfo
    ResolveScaleMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveScaleMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveScaleMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveScaleMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveScaleMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveScaleMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveScaleMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveScaleMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveScaleMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveScaleMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveScaleMethod "getUpperStepperSensitivity" o = Gtk.Range.RangeGetUpperStepperSensitivityMethodInfo
    ResolveScaleMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveScaleMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveScaleMethod "getValue" o = Gtk.Range.RangeGetValueMethodInfo
    ResolveScaleMethod "getValuePos" o = ScaleGetValuePosMethodInfo
    ResolveScaleMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveScaleMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveScaleMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveScaleMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveScaleMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveScaleMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveScaleMethod "setAdjustment" o = Gtk.Range.RangeSetAdjustmentMethodInfo
    ResolveScaleMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveScaleMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveScaleMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveScaleMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveScaleMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveScaleMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveScaleMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveScaleMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveScaleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveScaleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveScaleMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveScaleMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveScaleMethod "setDigits" o = ScaleSetDigitsMethodInfo
    ResolveScaleMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveScaleMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveScaleMethod "setDrawValue" o = ScaleSetDrawValueMethodInfo
    ResolveScaleMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveScaleMethod "setFillLevel" o = Gtk.Range.RangeSetFillLevelMethodInfo
    ResolveScaleMethod "setFlippable" o = Gtk.Range.RangeSetFlippableMethodInfo
    ResolveScaleMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveScaleMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveScaleMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveScaleMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveScaleMethod "setHasOrigin" o = ScaleSetHasOriginMethodInfo
    ResolveScaleMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveScaleMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveScaleMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveScaleMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveScaleMethod "setIncrements" o = Gtk.Range.RangeSetIncrementsMethodInfo
    ResolveScaleMethod "setInverted" o = Gtk.Range.RangeSetInvertedMethodInfo
    ResolveScaleMethod "setLowerStepperSensitivity" o = Gtk.Range.RangeSetLowerStepperSensitivityMethodInfo
    ResolveScaleMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveScaleMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveScaleMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveScaleMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveScaleMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveScaleMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveScaleMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveScaleMethod "setMinSliderSize" o = Gtk.Range.RangeSetMinSliderSizeMethodInfo
    ResolveScaleMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveScaleMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveScaleMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveScaleMethod "setOrientation" o = Gtk.Orientable.OrientableSetOrientationMethodInfo
    ResolveScaleMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveScaleMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveScaleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveScaleMethod "setRange" o = Gtk.Range.RangeSetRangeMethodInfo
    ResolveScaleMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveScaleMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveScaleMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveScaleMethod "setRestrictToFillLevel" o = Gtk.Range.RangeSetRestrictToFillLevelMethodInfo
    ResolveScaleMethod "setRoundDigits" o = Gtk.Range.RangeSetRoundDigitsMethodInfo
    ResolveScaleMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveScaleMethod "setShowFillLevel" o = Gtk.Range.RangeSetShowFillLevelMethodInfo
    ResolveScaleMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveScaleMethod "setSliderSizeFixed" o = Gtk.Range.RangeSetSliderSizeFixedMethodInfo
    ResolveScaleMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveScaleMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveScaleMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveScaleMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveScaleMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveScaleMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveScaleMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveScaleMethod "setUpperStepperSensitivity" o = Gtk.Range.RangeSetUpperStepperSensitivityMethodInfo
    ResolveScaleMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveScaleMethod "setValue" o = Gtk.Range.RangeSetValueMethodInfo
    ResolveScaleMethod "setValuePos" o = ScaleSetValuePosMethodInfo
    ResolveScaleMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveScaleMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveScaleMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveScaleMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveScaleMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveScaleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveScaleMethod t Scale, O.OverloadedMethod info Scale p) => OL.IsLabel t (Scale -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveScaleMethod t Scale, O.OverloadedMethod info Scale p, R.HasField t Scale p) => R.HasField t Scale p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveScaleMethod t Scale, O.OverloadedMethodInfo info Scale) => OL.IsLabel t (O.MethodProxy info Scale) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Scale::format-value
-- | Signal which allows you to change how the scale value is displayed.
-- Connect a signal handler which returns an allocated string representing
-- /@value@/. That string will then be used to display the scale\'s value.
-- 
-- If no user-provided handlers are installed, the value will be displayed on
-- its own, rounded according to the value of the [Scale:digits]("GI.Gtk.Objects.Scale#g:attr:digits") property.
-- 
-- Here\'s an example signal handler which displays a value 1.0 as
-- with \"-->1.0\<--\".
-- 
-- === /C code/
-- >
-- >static gchar*
-- >format_value_callback (GtkScale *scale,
-- >                       gdouble   value)
-- >{
-- >  return g_strdup_printf ("-->\%0.*g<--",
-- >                          gtk_scale_get_digits (scale), value);
-- > }
type ScaleFormatValueCallback =
    Double
    -- ^ /@value@/: the value to format
    -> IO T.Text
    -- ^ __Returns:__ allocated string representing /@value@/

type C_ScaleFormatValueCallback =
    Ptr Scale ->                            -- object
    CDouble ->
    Ptr () ->                               -- user_data
    IO CString

-- | Generate a function pointer callable from C code, from a `C_ScaleFormatValueCallback`.
foreign import ccall "wrapper"
    mk_ScaleFormatValueCallback :: C_ScaleFormatValueCallback -> IO (FunPtr C_ScaleFormatValueCallback)

wrap_ScaleFormatValueCallback :: 
    GObject a => (a -> ScaleFormatValueCallback) ->
    C_ScaleFormatValueCallback
wrap_ScaleFormatValueCallback gi'cb gi'selfPtr value _ = do
    let value' = realToFrac value
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  value'
    result' <- textToCString result
    return result'


-- | Connect a signal handler for the [formatValue](#signal:formatValue) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' scale #formatValue callback
-- @
-- 
-- 
onScaleFormatValue :: (IsScale a, MonadIO m) => a -> ((?self :: a) => ScaleFormatValueCallback) -> m SignalHandlerId
onScaleFormatValue obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ScaleFormatValueCallback wrapped
    wrapped'' <- mk_ScaleFormatValueCallback wrapped'
    connectSignalFunPtr obj "format-value" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [formatValue](#signal:formatValue) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' scale #formatValue callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterScaleFormatValue :: (IsScale a, MonadIO m) => a -> ((?self :: a) => ScaleFormatValueCallback) -> m SignalHandlerId
afterScaleFormatValue obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ScaleFormatValueCallback wrapped
    wrapped'' <- mk_ScaleFormatValueCallback wrapped'
    connectSignalFunPtr obj "format-value" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ScaleFormatValueSignalInfo
instance SignalInfo ScaleFormatValueSignalInfo where
    type HaskellCallbackType ScaleFormatValueSignalInfo = ScaleFormatValueCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ScaleFormatValueCallback cb
        cb'' <- mk_ScaleFormatValueCallback cb'
        connectSignalFunPtr obj "format-value" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale::format-value"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#g:signal:formatValue"})

#endif

-- VVV Prop "digits"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@digits@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' scale #digits
-- @
getScaleDigits :: (MonadIO m, IsScale o) => o -> m Int32
getScaleDigits obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "digits"

-- | Set the value of the “@digits@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' scale [ #digits 'Data.GI.Base.Attributes.:=' value ]
-- @
setScaleDigits :: (MonadIO m, IsScale o) => o -> Int32 -> m ()
setScaleDigits obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "digits" val

-- | Construct a `GValueConstruct` with valid value for the “@digits@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructScaleDigits :: (IsScale o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructScaleDigits val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "digits" val

#if defined(ENABLE_OVERLOADING)
data ScaleDigitsPropertyInfo
instance AttrInfo ScaleDigitsPropertyInfo where
    type AttrAllowedOps ScaleDigitsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ScaleDigitsPropertyInfo = IsScale
    type AttrSetTypeConstraint ScaleDigitsPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint ScaleDigitsPropertyInfo = (~) Int32
    type AttrTransferType ScaleDigitsPropertyInfo = Int32
    type AttrGetType ScaleDigitsPropertyInfo = Int32
    type AttrLabel ScaleDigitsPropertyInfo = "digits"
    type AttrOrigin ScaleDigitsPropertyInfo = Scale
    attrGet = getScaleDigits
    attrSet = setScaleDigits
    attrTransfer _ v = do
        return v
    attrConstruct = constructScaleDigits
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale.digits"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#g:attr:digits"
        })
#endif

-- VVV Prop "draw-value"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@draw-value@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' scale #drawValue
-- @
getScaleDrawValue :: (MonadIO m, IsScale o) => o -> m Bool
getScaleDrawValue obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "draw-value"

-- | Set the value of the “@draw-value@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' scale [ #drawValue 'Data.GI.Base.Attributes.:=' value ]
-- @
setScaleDrawValue :: (MonadIO m, IsScale o) => o -> Bool -> m ()
setScaleDrawValue obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "draw-value" val

-- | Construct a `GValueConstruct` with valid value for the “@draw-value@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructScaleDrawValue :: (IsScale o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructScaleDrawValue val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "draw-value" val

#if defined(ENABLE_OVERLOADING)
data ScaleDrawValuePropertyInfo
instance AttrInfo ScaleDrawValuePropertyInfo where
    type AttrAllowedOps ScaleDrawValuePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ScaleDrawValuePropertyInfo = IsScale
    type AttrSetTypeConstraint ScaleDrawValuePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ScaleDrawValuePropertyInfo = (~) Bool
    type AttrTransferType ScaleDrawValuePropertyInfo = Bool
    type AttrGetType ScaleDrawValuePropertyInfo = Bool
    type AttrLabel ScaleDrawValuePropertyInfo = "draw-value"
    type AttrOrigin ScaleDrawValuePropertyInfo = Scale
    attrGet = getScaleDrawValue
    attrSet = setScaleDrawValue
    attrTransfer _ v = do
        return v
    attrConstruct = constructScaleDrawValue
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale.drawValue"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#g:attr:drawValue"
        })
#endif

-- VVV Prop "has-origin"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@has-origin@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' scale #hasOrigin
-- @
getScaleHasOrigin :: (MonadIO m, IsScale o) => o -> m Bool
getScaleHasOrigin obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "has-origin"

-- | Set the value of the “@has-origin@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' scale [ #hasOrigin 'Data.GI.Base.Attributes.:=' value ]
-- @
setScaleHasOrigin :: (MonadIO m, IsScale o) => o -> Bool -> m ()
setScaleHasOrigin obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "has-origin" val

-- | Construct a `GValueConstruct` with valid value for the “@has-origin@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructScaleHasOrigin :: (IsScale o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructScaleHasOrigin val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "has-origin" val

#if defined(ENABLE_OVERLOADING)
data ScaleHasOriginPropertyInfo
instance AttrInfo ScaleHasOriginPropertyInfo where
    type AttrAllowedOps ScaleHasOriginPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ScaleHasOriginPropertyInfo = IsScale
    type AttrSetTypeConstraint ScaleHasOriginPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ScaleHasOriginPropertyInfo = (~) Bool
    type AttrTransferType ScaleHasOriginPropertyInfo = Bool
    type AttrGetType ScaleHasOriginPropertyInfo = Bool
    type AttrLabel ScaleHasOriginPropertyInfo = "has-origin"
    type AttrOrigin ScaleHasOriginPropertyInfo = Scale
    attrGet = getScaleHasOrigin
    attrSet = setScaleHasOrigin
    attrTransfer _ v = do
        return v
    attrConstruct = constructScaleHasOrigin
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale.hasOrigin"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#g:attr:hasOrigin"
        })
#endif

-- VVV Prop "value-pos"
   -- Type: TInterface (Name {namespace = "Gtk", name = "PositionType"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@value-pos@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' scale #valuePos
-- @
getScaleValuePos :: (MonadIO m, IsScale o) => o -> m Gtk.Enums.PositionType
getScaleValuePos obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "value-pos"

-- | Set the value of the “@value-pos@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' scale [ #valuePos 'Data.GI.Base.Attributes.:=' value ]
-- @
setScaleValuePos :: (MonadIO m, IsScale o) => o -> Gtk.Enums.PositionType -> m ()
setScaleValuePos obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "value-pos" val

-- | Construct a `GValueConstruct` with valid value for the “@value-pos@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructScaleValuePos :: (IsScale o, MIO.MonadIO m) => Gtk.Enums.PositionType -> m (GValueConstruct o)
constructScaleValuePos val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "value-pos" val

#if defined(ENABLE_OVERLOADING)
data ScaleValuePosPropertyInfo
instance AttrInfo ScaleValuePosPropertyInfo where
    type AttrAllowedOps ScaleValuePosPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ScaleValuePosPropertyInfo = IsScale
    type AttrSetTypeConstraint ScaleValuePosPropertyInfo = (~) Gtk.Enums.PositionType
    type AttrTransferTypeConstraint ScaleValuePosPropertyInfo = (~) Gtk.Enums.PositionType
    type AttrTransferType ScaleValuePosPropertyInfo = Gtk.Enums.PositionType
    type AttrGetType ScaleValuePosPropertyInfo = Gtk.Enums.PositionType
    type AttrLabel ScaleValuePosPropertyInfo = "value-pos"
    type AttrOrigin ScaleValuePosPropertyInfo = Scale
    attrGet = getScaleValuePos
    attrSet = setScaleValuePos
    attrTransfer _ v = do
        return v
    attrConstruct = constructScaleValuePos
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale.valuePos"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#g:attr:valuePos"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Scale
type instance O.AttributeList Scale = ScaleAttributeList
type ScaleAttributeList = ('[ '("adjustment", Gtk.Range.RangeAdjustmentPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("digits", ScaleDigitsPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("drawValue", ScaleDrawValuePropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("fillLevel", Gtk.Range.RangeFillLevelPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasOrigin", ScaleHasOriginPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("inverted", Gtk.Range.RangeInvertedPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("lowerStepperSensitivity", Gtk.Range.RangeLowerStepperSensitivityPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("orientation", Gtk.Orientable.OrientableOrientationPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("restrictToFillLevel", Gtk.Range.RangeRestrictToFillLevelPropertyInfo), '("roundDigits", Gtk.Range.RangeRoundDigitsPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("showFillLevel", Gtk.Range.RangeShowFillLevelPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("upperStepperSensitivity", Gtk.Range.RangeUpperStepperSensitivityPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("valuePos", ScaleValuePosPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
scaleDigits :: AttrLabelProxy "digits"
scaleDigits = AttrLabelProxy

scaleDrawValue :: AttrLabelProxy "drawValue"
scaleDrawValue = AttrLabelProxy

scaleHasOrigin :: AttrLabelProxy "hasOrigin"
scaleHasOrigin = AttrLabelProxy

scaleValuePos :: AttrLabelProxy "valuePos"
scaleValuePos = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Scale = ScaleSignalList
type ScaleSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("adjustBounds", Gtk.Range.RangeAdjustBoundsSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("changeValue", Gtk.Range.RangeChangeValueSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("formatValue", ScaleFormatValueSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("moveSlider", Gtk.Range.RangeMoveSliderSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("valueChanged", Gtk.Range.RangeValueChangedSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Scale::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "orientation"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Orientation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the scale\8217s orientation."
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkAdjustment which sets the range\n             of the scale, or %NULL to create a new adjustment."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Scale" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_scale_new" gtk_scale_new :: 
    CUInt ->                                -- orientation : TInterface (Name {namespace = "Gtk", name = "Orientation"})
    Ptr Gtk.Adjustment.Adjustment ->        -- adjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    IO (Ptr Scale)

-- | Creates a new t'GI.Gtk.Objects.Scale.Scale'.
-- 
-- /Since: 3.0/
scaleNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Adjustment.IsAdjustment a) =>
    Gtk.Enums.Orientation
    -- ^ /@orientation@/: the scale’s orientation.
    -> Maybe (a)
    -- ^ /@adjustment@/: the t'GI.Gtk.Objects.Adjustment.Adjustment' which sets the range
    --              of the scale, or 'P.Nothing' to create a new adjustment.
    -> m Scale
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Scale.Scale'
scaleNew orientation adjustment = liftIO $ do
    let orientation' = (fromIntegral . fromEnum) orientation
    maybeAdjustment <- case adjustment of
        Nothing -> return nullPtr
        Just jAdjustment -> do
            jAdjustment' <- unsafeManagedPtrCastPtr jAdjustment
            return jAdjustment'
    result <- gtk_scale_new orientation' maybeAdjustment
    checkUnexpectedReturnNULL "scaleNew" result
    result' <- (newObject Scale) result
    whenJust adjustment touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Scale::new_with_range
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "orientation"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Orientation" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the scale\8217s orientation."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "min"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "minimum value" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "max"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "maximum value" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "step"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "step increment (tick size) used with keyboard shortcuts"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Scale" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_scale_new_with_range" gtk_scale_new_with_range :: 
    CUInt ->                                -- orientation : TInterface (Name {namespace = "Gtk", name = "Orientation"})
    CDouble ->                              -- min : TBasicType TDouble
    CDouble ->                              -- max : TBasicType TDouble
    CDouble ->                              -- step : TBasicType TDouble
    IO (Ptr Scale)

-- | Creates a new scale widget with the given orientation that lets the
-- user input a number between /@min@/ and /@max@/ (including /@min@/ and /@max@/)
-- with the increment /@step@/.  /@step@/ must be nonzero; it’s the distance
-- the slider moves when using the arrow keys to adjust the scale
-- value.
-- 
-- Note that the way in which the precision is derived works best if /@step@/
-- is a power of ten. If the resulting precision is not suitable for your
-- needs, use 'GI.Gtk.Objects.Scale.scaleSetDigits' to correct it.
-- 
-- /Since: 3.0/
scaleNewWithRange ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Gtk.Enums.Orientation
    -- ^ /@orientation@/: the scale’s orientation.
    -> Double
    -- ^ /@min@/: minimum value
    -> Double
    -- ^ /@max@/: maximum value
    -> Double
    -- ^ /@step@/: step increment (tick size) used with keyboard shortcuts
    -> m Scale
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Scale.Scale'
scaleNewWithRange orientation min max step = liftIO $ do
    let orientation' = (fromIntegral . fromEnum) orientation
    let min' = realToFrac min
    let max' = realToFrac max
    let step' = realToFrac step
    result <- gtk_scale_new_with_range orientation' min' max' step'
    checkUnexpectedReturnNULL "scaleNewWithRange" result
    result' <- (newObject Scale) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Scale::add_mark
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "scale"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Scale" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScale" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the value at which the mark is placed, must be between\n  the lower and upper limits of the scales\8217 adjustment"
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
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PositionType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "where to draw the mark. For a horizontal scale, #GTK_POS_TOP\n  and %GTK_POS_LEFT are drawn above the scale, anything else below.\n  For a vertical scale, #GTK_POS_LEFT and %GTK_POS_TOP are drawn to\n  the left of the scale, anything else to the right."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "markup"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Text to be shown at the mark, using [Pango markup][PangoMarkupFormat], or %NULL"
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

foreign import ccall "gtk_scale_add_mark" gtk_scale_add_mark :: 
    Ptr Scale ->                            -- scale : TInterface (Name {namespace = "Gtk", name = "Scale"})
    CDouble ->                              -- value : TBasicType TDouble
    CUInt ->                                -- position : TInterface (Name {namespace = "Gtk", name = "PositionType"})
    CString ->                              -- markup : TBasicType TUTF8
    IO ()

-- | Adds a mark at /@value@/.
-- 
-- A mark is indicated visually by drawing a tick mark next to the scale,
-- and GTK+ makes it easy for the user to position the scale exactly at the
-- marks value.
-- 
-- If /@markup@/ is not 'P.Nothing', text is shown next to the tick mark.
-- 
-- To remove marks from a scale, use 'GI.Gtk.Objects.Scale.scaleClearMarks'.
-- 
-- /Since: 2.16/
scaleAddMark ::
    (B.CallStack.HasCallStack, MonadIO m, IsScale a) =>
    a
    -- ^ /@scale@/: a t'GI.Gtk.Objects.Scale.Scale'
    -> Double
    -- ^ /@value@/: the value at which the mark is placed, must be between
    --   the lower and upper limits of the scales’ adjustment
    -> Gtk.Enums.PositionType
    -- ^ /@position@/: where to draw the mark. For a horizontal scale, @/GTK_POS_TOP/@
    --   and 'GI.Gtk.Enums.PositionTypeLeft' are drawn above the scale, anything else below.
    --   For a vertical scale, @/GTK_POS_LEFT/@ and 'GI.Gtk.Enums.PositionTypeTop' are drawn to
    --   the left of the scale, anything else to the right.
    -> Maybe (T.Text)
    -- ^ /@markup@/: Text to be shown at the mark, using [Pango markup][PangoMarkupFormat], or 'P.Nothing'
    -> m ()
scaleAddMark scale value position markup = liftIO $ do
    scale' <- unsafeManagedPtrCastPtr scale
    let value' = realToFrac value
    let position' = (fromIntegral . fromEnum) position
    maybeMarkup <- case markup of
        Nothing -> return nullPtr
        Just jMarkup -> do
            jMarkup' <- textToCString jMarkup
            return jMarkup'
    gtk_scale_add_mark scale' value' position' maybeMarkup
    touchManagedPtr scale
    freeMem maybeMarkup
    return ()

#if defined(ENABLE_OVERLOADING)
data ScaleAddMarkMethodInfo
instance (signature ~ (Double -> Gtk.Enums.PositionType -> Maybe (T.Text) -> m ()), MonadIO m, IsScale a) => O.OverloadedMethod ScaleAddMarkMethodInfo a signature where
    overloadedMethod = scaleAddMark

instance O.OverloadedMethodInfo ScaleAddMarkMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale.scaleAddMark",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#v:scaleAddMark"
        })


#endif

-- method Scale::clear_marks
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "scale"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Scale" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScale" , sinceVersion = Nothing }
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

foreign import ccall "gtk_scale_clear_marks" gtk_scale_clear_marks :: 
    Ptr Scale ->                            -- scale : TInterface (Name {namespace = "Gtk", name = "Scale"})
    IO ()

-- | Removes any marks that have been added with 'GI.Gtk.Objects.Scale.scaleAddMark'.
-- 
-- /Since: 2.16/
scaleClearMarks ::
    (B.CallStack.HasCallStack, MonadIO m, IsScale a) =>
    a
    -- ^ /@scale@/: a t'GI.Gtk.Objects.Scale.Scale'
    -> m ()
scaleClearMarks scale = liftIO $ do
    scale' <- unsafeManagedPtrCastPtr scale
    gtk_scale_clear_marks scale'
    touchManagedPtr scale
    return ()

#if defined(ENABLE_OVERLOADING)
data ScaleClearMarksMethodInfo
instance (signature ~ (m ()), MonadIO m, IsScale a) => O.OverloadedMethod ScaleClearMarksMethodInfo a signature where
    overloadedMethod = scaleClearMarks

instance O.OverloadedMethodInfo ScaleClearMarksMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale.scaleClearMarks",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#v:scaleClearMarks"
        })


#endif

-- method Scale::get_digits
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "scale"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Scale" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScale" , sinceVersion = Nothing }
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

foreign import ccall "gtk_scale_get_digits" gtk_scale_get_digits :: 
    Ptr Scale ->                            -- scale : TInterface (Name {namespace = "Gtk", name = "Scale"})
    IO Int32

-- | Gets the number of decimal places that are displayed in the value.
scaleGetDigits ::
    (B.CallStack.HasCallStack, MonadIO m, IsScale a) =>
    a
    -- ^ /@scale@/: a t'GI.Gtk.Objects.Scale.Scale'
    -> m Int32
    -- ^ __Returns:__ the number of decimal places that are displayed
scaleGetDigits scale = liftIO $ do
    scale' <- unsafeManagedPtrCastPtr scale
    result <- gtk_scale_get_digits scale'
    touchManagedPtr scale
    return result

#if defined(ENABLE_OVERLOADING)
data ScaleGetDigitsMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsScale a) => O.OverloadedMethod ScaleGetDigitsMethodInfo a signature where
    overloadedMethod = scaleGetDigits

instance O.OverloadedMethodInfo ScaleGetDigitsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale.scaleGetDigits",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#v:scaleGetDigits"
        })


#endif

-- method Scale::get_draw_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "scale"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Scale" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScale" , sinceVersion = Nothing }
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

foreign import ccall "gtk_scale_get_draw_value" gtk_scale_get_draw_value :: 
    Ptr Scale ->                            -- scale : TInterface (Name {namespace = "Gtk", name = "Scale"})
    IO CInt

-- | Returns whether the current value is displayed as a string
-- next to the slider.
scaleGetDrawValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsScale a) =>
    a
    -- ^ /@scale@/: a t'GI.Gtk.Objects.Scale.Scale'
    -> m Bool
    -- ^ __Returns:__ whether the current value is displayed as a string
scaleGetDrawValue scale = liftIO $ do
    scale' <- unsafeManagedPtrCastPtr scale
    result <- gtk_scale_get_draw_value scale'
    let result' = (/= 0) result
    touchManagedPtr scale
    return result'

#if defined(ENABLE_OVERLOADING)
data ScaleGetDrawValueMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsScale a) => O.OverloadedMethod ScaleGetDrawValueMethodInfo a signature where
    overloadedMethod = scaleGetDrawValue

instance O.OverloadedMethodInfo ScaleGetDrawValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale.scaleGetDrawValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#v:scaleGetDrawValue"
        })


#endif

-- method Scale::get_has_origin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "scale"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Scale" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScale" , sinceVersion = Nothing }
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

foreign import ccall "gtk_scale_get_has_origin" gtk_scale_get_has_origin :: 
    Ptr Scale ->                            -- scale : TInterface (Name {namespace = "Gtk", name = "Scale"})
    IO CInt

-- | Returns whether the scale has an origin.
-- 
-- /Since: 3.4/
scaleGetHasOrigin ::
    (B.CallStack.HasCallStack, MonadIO m, IsScale a) =>
    a
    -- ^ /@scale@/: a t'GI.Gtk.Objects.Scale.Scale'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the scale has an origin.
scaleGetHasOrigin scale = liftIO $ do
    scale' <- unsafeManagedPtrCastPtr scale
    result <- gtk_scale_get_has_origin scale'
    let result' = (/= 0) result
    touchManagedPtr scale
    return result'

#if defined(ENABLE_OVERLOADING)
data ScaleGetHasOriginMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsScale a) => O.OverloadedMethod ScaleGetHasOriginMethodInfo a signature where
    overloadedMethod = scaleGetHasOrigin

instance O.OverloadedMethodInfo ScaleGetHasOriginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale.scaleGetHasOrigin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#v:scaleGetHasOrigin"
        })


#endif

-- method Scale::get_layout
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "scale"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Scale" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkScale" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Pango" , name = "Layout" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_scale_get_layout" gtk_scale_get_layout :: 
    Ptr Scale ->                            -- scale : TInterface (Name {namespace = "Gtk", name = "Scale"})
    IO (Ptr Pango.Layout.Layout)

-- | Gets the t'GI.Pango.Objects.Layout.Layout' used to display the scale. The returned
-- object is owned by the scale so does not need to be freed by
-- the caller.
-- 
-- /Since: 2.4/
scaleGetLayout ::
    (B.CallStack.HasCallStack, MonadIO m, IsScale a) =>
    a
    -- ^ /@scale@/: A t'GI.Gtk.Objects.Scale.Scale'
    -> m (Maybe Pango.Layout.Layout)
    -- ^ __Returns:__ the t'GI.Pango.Objects.Layout.Layout' for this scale,
    --     or 'P.Nothing' if the [Scale:drawValue]("GI.Gtk.Objects.Scale#g:attr:drawValue") property is 'P.False'.
scaleGetLayout scale = liftIO $ do
    scale' <- unsafeManagedPtrCastPtr scale
    result <- gtk_scale_get_layout scale'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Pango.Layout.Layout) result'
        return result''
    touchManagedPtr scale
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ScaleGetLayoutMethodInfo
instance (signature ~ (m (Maybe Pango.Layout.Layout)), MonadIO m, IsScale a) => O.OverloadedMethod ScaleGetLayoutMethodInfo a signature where
    overloadedMethod = scaleGetLayout

instance O.OverloadedMethodInfo ScaleGetLayoutMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale.scaleGetLayout",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#v:scaleGetLayout"
        })


#endif

-- method Scale::get_layout_offsets
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "scale"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Scale" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScale" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store X offset of layout, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store Y offset of layout, or %NULL"
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

foreign import ccall "gtk_scale_get_layout_offsets" gtk_scale_get_layout_offsets :: 
    Ptr Scale ->                            -- scale : TInterface (Name {namespace = "Gtk", name = "Scale"})
    Ptr Int32 ->                            -- x : TBasicType TInt
    Ptr Int32 ->                            -- y : TBasicType TInt
    IO ()

-- | Obtains the coordinates where the scale will draw the
-- t'GI.Pango.Objects.Layout.Layout' representing the text in the scale. Remember
-- when using the t'GI.Pango.Objects.Layout.Layout' function you need to convert to
-- and from pixels using @/PANGO_PIXELS()/@ or 'GI.Pango.Constants.SCALE'.
-- 
-- If the [Scale:drawValue]("GI.Gtk.Objects.Scale#g:attr:drawValue") property is 'P.False', the return
-- values are undefined.
-- 
-- /Since: 2.4/
scaleGetLayoutOffsets ::
    (B.CallStack.HasCallStack, MonadIO m, IsScale a) =>
    a
    -- ^ /@scale@/: a t'GI.Gtk.Objects.Scale.Scale'
    -> m ((Int32, Int32))
scaleGetLayoutOffsets scale = liftIO $ do
    scale' <- unsafeManagedPtrCastPtr scale
    x <- allocMem :: IO (Ptr Int32)
    y <- allocMem :: IO (Ptr Int32)
    gtk_scale_get_layout_offsets scale' x y
    x' <- peek x
    y' <- peek y
    touchManagedPtr scale
    freeMem x
    freeMem y
    return (x', y')

#if defined(ENABLE_OVERLOADING)
data ScaleGetLayoutOffsetsMethodInfo
instance (signature ~ (m ((Int32, Int32))), MonadIO m, IsScale a) => O.OverloadedMethod ScaleGetLayoutOffsetsMethodInfo a signature where
    overloadedMethod = scaleGetLayoutOffsets

instance O.OverloadedMethodInfo ScaleGetLayoutOffsetsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale.scaleGetLayoutOffsets",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#v:scaleGetLayoutOffsets"
        })


#endif

-- method Scale::get_value_pos
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "scale"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Scale" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScale" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "PositionType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_scale_get_value_pos" gtk_scale_get_value_pos :: 
    Ptr Scale ->                            -- scale : TInterface (Name {namespace = "Gtk", name = "Scale"})
    IO CUInt

-- | Gets the position in which the current value is displayed.
scaleGetValuePos ::
    (B.CallStack.HasCallStack, MonadIO m, IsScale a) =>
    a
    -- ^ /@scale@/: a t'GI.Gtk.Objects.Scale.Scale'
    -> m Gtk.Enums.PositionType
    -- ^ __Returns:__ the position in which the current value is displayed
scaleGetValuePos scale = liftIO $ do
    scale' <- unsafeManagedPtrCastPtr scale
    result <- gtk_scale_get_value_pos scale'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr scale
    return result'

#if defined(ENABLE_OVERLOADING)
data ScaleGetValuePosMethodInfo
instance (signature ~ (m Gtk.Enums.PositionType), MonadIO m, IsScale a) => O.OverloadedMethod ScaleGetValuePosMethodInfo a signature where
    overloadedMethod = scaleGetValuePos

instance O.OverloadedMethodInfo ScaleGetValuePosMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale.scaleGetValuePos",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#v:scaleGetValuePos"
        })


#endif

-- method Scale::set_digits
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "scale"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Scale" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScale" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "digits"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the number of decimal places to display,\n    e.g. use 1 to display 1.0, 2 to display 1.00, etc"
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

foreign import ccall "gtk_scale_set_digits" gtk_scale_set_digits :: 
    Ptr Scale ->                            -- scale : TInterface (Name {namespace = "Gtk", name = "Scale"})
    Int32 ->                                -- digits : TBasicType TInt
    IO ()

-- | Sets the number of decimal places that are displayed in the value. Also
-- causes the value of the adjustment to be rounded to this number of digits,
-- so the retrieved value matches the displayed one, if [Scale:drawValue]("GI.Gtk.Objects.Scale#g:attr:drawValue") is
-- 'P.True' when the value changes. If you want to enforce rounding the value when
-- [Scale:drawValue]("GI.Gtk.Objects.Scale#g:attr:drawValue") is 'P.False', you can set [Range:roundDigits]("GI.Gtk.Objects.Range#g:attr:roundDigits") instead.
-- 
-- Note that rounding to a small number of digits can interfere with
-- the smooth autoscrolling that is built into t'GI.Gtk.Objects.Scale.Scale'. As an alternative,
-- you can use the [Scale::formatValue]("GI.Gtk.Objects.Scale#g:signal:formatValue") signal to format the displayed
-- value yourself.
scaleSetDigits ::
    (B.CallStack.HasCallStack, MonadIO m, IsScale a) =>
    a
    -- ^ /@scale@/: a t'GI.Gtk.Objects.Scale.Scale'
    -> Int32
    -- ^ /@digits@/: the number of decimal places to display,
    --     e.g. use 1 to display 1.0, 2 to display 1.00, etc
    -> m ()
scaleSetDigits scale digits = liftIO $ do
    scale' <- unsafeManagedPtrCastPtr scale
    gtk_scale_set_digits scale' digits
    touchManagedPtr scale
    return ()

#if defined(ENABLE_OVERLOADING)
data ScaleSetDigitsMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsScale a) => O.OverloadedMethod ScaleSetDigitsMethodInfo a signature where
    overloadedMethod = scaleSetDigits

instance O.OverloadedMethodInfo ScaleSetDigitsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale.scaleSetDigits",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#v:scaleSetDigits"
        })


#endif

-- method Scale::set_draw_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "scale"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Scale" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScale" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "draw_value"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to draw the value"
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

foreign import ccall "gtk_scale_set_draw_value" gtk_scale_set_draw_value :: 
    Ptr Scale ->                            -- scale : TInterface (Name {namespace = "Gtk", name = "Scale"})
    CInt ->                                 -- draw_value : TBasicType TBoolean
    IO ()

-- | Specifies whether the current value is displayed as a string next
-- to the slider.
scaleSetDrawValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsScale a) =>
    a
    -- ^ /@scale@/: a t'GI.Gtk.Objects.Scale.Scale'
    -> Bool
    -- ^ /@drawValue@/: 'P.True' to draw the value
    -> m ()
scaleSetDrawValue scale drawValue = liftIO $ do
    scale' <- unsafeManagedPtrCastPtr scale
    let drawValue' = (fromIntegral . fromEnum) drawValue
    gtk_scale_set_draw_value scale' drawValue'
    touchManagedPtr scale
    return ()

#if defined(ENABLE_OVERLOADING)
data ScaleSetDrawValueMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsScale a) => O.OverloadedMethod ScaleSetDrawValueMethodInfo a signature where
    overloadedMethod = scaleSetDrawValue

instance O.OverloadedMethodInfo ScaleSetDrawValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale.scaleSetDrawValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#v:scaleSetDrawValue"
        })


#endif

-- method Scale::set_has_origin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "scale"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Scale" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScale" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "has_origin"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if the scale has an origin"
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

foreign import ccall "gtk_scale_set_has_origin" gtk_scale_set_has_origin :: 
    Ptr Scale ->                            -- scale : TInterface (Name {namespace = "Gtk", name = "Scale"})
    CInt ->                                 -- has_origin : TBasicType TBoolean
    IO ()

-- | If [Scale:hasOrigin]("GI.Gtk.Objects.Scale#g:attr:hasOrigin") is set to 'P.True' (the default), the scale will
-- highlight the part of the trough between the origin (bottom or left side)
-- and the current value.
-- 
-- /Since: 3.4/
scaleSetHasOrigin ::
    (B.CallStack.HasCallStack, MonadIO m, IsScale a) =>
    a
    -- ^ /@scale@/: a t'GI.Gtk.Objects.Scale.Scale'
    -> Bool
    -- ^ /@hasOrigin@/: 'P.True' if the scale has an origin
    -> m ()
scaleSetHasOrigin scale hasOrigin = liftIO $ do
    scale' <- unsafeManagedPtrCastPtr scale
    let hasOrigin' = (fromIntegral . fromEnum) hasOrigin
    gtk_scale_set_has_origin scale' hasOrigin'
    touchManagedPtr scale
    return ()

#if defined(ENABLE_OVERLOADING)
data ScaleSetHasOriginMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsScale a) => O.OverloadedMethod ScaleSetHasOriginMethodInfo a signature where
    overloadedMethod = scaleSetHasOrigin

instance O.OverloadedMethodInfo ScaleSetHasOriginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale.scaleSetHasOrigin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#v:scaleSetHasOrigin"
        })


#endif

-- method Scale::set_value_pos
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "scale"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Scale" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkScale" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PositionType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the position in which the current value is displayed"
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

foreign import ccall "gtk_scale_set_value_pos" gtk_scale_set_value_pos :: 
    Ptr Scale ->                            -- scale : TInterface (Name {namespace = "Gtk", name = "Scale"})
    CUInt ->                                -- pos : TInterface (Name {namespace = "Gtk", name = "PositionType"})
    IO ()

-- | Sets the position in which the current value is displayed.
scaleSetValuePos ::
    (B.CallStack.HasCallStack, MonadIO m, IsScale a) =>
    a
    -- ^ /@scale@/: a t'GI.Gtk.Objects.Scale.Scale'
    -> Gtk.Enums.PositionType
    -- ^ /@pos@/: the position in which the current value is displayed
    -> m ()
scaleSetValuePos scale pos = liftIO $ do
    scale' <- unsafeManagedPtrCastPtr scale
    let pos' = (fromIntegral . fromEnum) pos
    gtk_scale_set_value_pos scale' pos'
    touchManagedPtr scale
    return ()

#if defined(ENABLE_OVERLOADING)
data ScaleSetValuePosMethodInfo
instance (signature ~ (Gtk.Enums.PositionType -> m ()), MonadIO m, IsScale a) => O.OverloadedMethod ScaleSetValuePosMethodInfo a signature where
    overloadedMethod = scaleSetValuePos

instance O.OverloadedMethodInfo ScaleSetValuePosMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Scale.scaleSetValuePos",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Scale.html#v:scaleSetValuePos"
        })


#endif


