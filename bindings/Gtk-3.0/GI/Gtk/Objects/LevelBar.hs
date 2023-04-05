{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.LevelBar.LevelBar' is a bar widget that can be used
-- as a level indicator. Typical use cases are displaying the strength
-- of a password, or showing the charge level of a battery.
-- 
-- Use 'GI.Gtk.Objects.LevelBar.levelBarSetValue' to set the current value, and
-- 'GI.Gtk.Objects.LevelBar.levelBarAddOffsetValue' to set the value offsets at which
-- the bar will be considered in a different state. GTK will add a few
-- offsets by default on the level bar: 'GI.Gtk.Constants.LEVEL_BAR_OFFSET_LOW',
-- 'GI.Gtk.Constants.LEVEL_BAR_OFFSET_HIGH' and 'GI.Gtk.Constants.LEVEL_BAR_OFFSET_FULL', with
-- values 0.25, 0.75 and 1.0 respectively.
-- 
-- Note that it is your responsibility to update preexisting offsets
-- when changing the minimum or maximum value. GTK+ will simply clamp
-- them to the new range.
-- 
-- == Adding a custom offset on the bar
-- 
-- 
-- === /C code/
-- >
-- >
-- >static GtkWidget *
-- >create_level_bar (void)
-- >{
-- >  GtkWidget *widget;
-- >  GtkLevelBar *bar;
-- >
-- >  widget = gtk_level_bar_new ();
-- >  bar = GTK_LEVEL_BAR (widget);
-- >
-- >  // This changes the value of the default low offset
-- >
-- >  gtk_level_bar_add_offset_value (bar,
-- >                                  GTK_LEVEL_BAR_OFFSET_LOW,
-- >                                  0.10);
-- >
-- >  // This adds a new offset to the bar; the application will
-- >  // be able to change its color CSS like this:
-- >  //
-- >  // levelbar block.my-offset {
-- >  //   background-color: magenta;
-- >  //   border-style: solid;
-- >  //   border-color: black;
-- >  //   border-style: 1px;
-- >  // }
-- >
-- >  gtk_level_bar_add_offset_value (bar, "my-offset", 0.60);
-- >
-- >  return widget;
-- >}
-- 
-- 
-- The default interval of values is between zero and one, but it’s possible to
-- modify the interval using 'GI.Gtk.Objects.LevelBar.levelBarSetMinValue' and
-- 'GI.Gtk.Objects.LevelBar.levelBarSetMaxValue'. The value will be always drawn in proportion to
-- the admissible interval, i.e. a value of 15 with a specified interval between
-- 10 and 20 is equivalent to a value of 0.5 with an interval between 0 and 1.
-- When @/GTK_LEVEL_BAR_MODE_DISCRETE/@ is used, the bar level is rendered
-- as a finite number of separated blocks instead of a single one. The number
-- of blocks that will be rendered is equal to the number of units specified by
-- the admissible interval.
-- 
-- For instance, to build a bar rendered with five blocks, it’s sufficient to
-- set the minimum value to 0 and the maximum value to 5 after changing the indicator
-- mode to discrete.
-- 
-- GtkLevelBar was introduced in GTK+ 3.6.
-- 
-- = GtkLevelBar as GtkBuildable
-- 
-- The GtkLevelBar implementation of the GtkBuildable interface supports a
-- custom @\<offsets>@ element, which can contain any number of @\<offset>@ elements,
-- each of which must have \"name\" and \"value\" attributes.
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >levelbar[.discrete]
-- >╰── trough
-- >    ├── block.filled.level-name
-- >    ┊
-- >    ├── block.empty
-- >    ┊
-- 
-- 
-- GtkLevelBar has a main CSS node with name levelbar and one of the style
-- classes .discrete or .continuous and a subnode with name trough. Below the
-- trough node are a number of nodes with name block and style class .filled
-- or .empty. In continuous mode, there is exactly one node of each, in discrete
-- mode, the number of filled and unfilled nodes corresponds to blocks that are
-- drawn. The block.filled nodes also get a style class .level-name corresponding
-- to the level for the current value.
-- 
-- In horizontal orientation, the nodes are always arranged from left to right,
-- regardless of text direction.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.LevelBar
    ( 

-- * Exported types
    LevelBar(..)                            ,
    IsLevelBar                              ,
    toLevelBar                              ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addOffsetValue]("GI.Gtk.Objects.LevelBar#g:method:addOffsetValue"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childNotify]("GI.Gtk.Objects.Widget#g:method:childNotify"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeOffsetValue]("GI.Gtk.Objects.LevelBar#g:method:removeOffsetValue"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getInverted]("GI.Gtk.Objects.LevelBar#g:method:getInverted"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMaxValue]("GI.Gtk.Objects.LevelBar#g:method:getMaxValue"), [getMinValue]("GI.Gtk.Objects.LevelBar#g:method:getMinValue"), [getMode]("GI.Gtk.Objects.LevelBar#g:method:getMode"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOffsetValue]("GI.Gtk.Objects.LevelBar#g:method:getOffsetValue"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getOrientation]("GI.Gtk.Interfaces.Orientable#g:method:getOrientation"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getValue]("GI.Gtk.Objects.LevelBar#g:method:getValue"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setInverted]("GI.Gtk.Objects.LevelBar#g:method:setInverted"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMaxValue]("GI.Gtk.Objects.LevelBar#g:method:setMaxValue"), [setMinValue]("GI.Gtk.Objects.LevelBar#g:method:setMinValue"), [setMode]("GI.Gtk.Objects.LevelBar#g:method:setMode"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setOrientation]("GI.Gtk.Interfaces.Orientable#g:method:setOrientation"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setValue]("GI.Gtk.Objects.LevelBar#g:method:setValue"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveLevelBarMethod                   ,
#endif

-- ** addOffsetValue #method:addOffsetValue#

#if defined(ENABLE_OVERLOADING)
    LevelBarAddOffsetValueMethodInfo        ,
#endif
    levelBarAddOffsetValue                  ,


-- ** getInverted #method:getInverted#

#if defined(ENABLE_OVERLOADING)
    LevelBarGetInvertedMethodInfo           ,
#endif
    levelBarGetInverted                     ,


-- ** getMaxValue #method:getMaxValue#

#if defined(ENABLE_OVERLOADING)
    LevelBarGetMaxValueMethodInfo           ,
#endif
    levelBarGetMaxValue                     ,


-- ** getMinValue #method:getMinValue#

#if defined(ENABLE_OVERLOADING)
    LevelBarGetMinValueMethodInfo           ,
#endif
    levelBarGetMinValue                     ,


-- ** getMode #method:getMode#

#if defined(ENABLE_OVERLOADING)
    LevelBarGetModeMethodInfo               ,
#endif
    levelBarGetMode                         ,


-- ** getOffsetValue #method:getOffsetValue#

#if defined(ENABLE_OVERLOADING)
    LevelBarGetOffsetValueMethodInfo        ,
#endif
    levelBarGetOffsetValue                  ,


-- ** getValue #method:getValue#

#if defined(ENABLE_OVERLOADING)
    LevelBarGetValueMethodInfo              ,
#endif
    levelBarGetValue                        ,


-- ** new #method:new#

    levelBarNew                             ,


-- ** newForInterval #method:newForInterval#

    levelBarNewForInterval                  ,


-- ** removeOffsetValue #method:removeOffsetValue#

#if defined(ENABLE_OVERLOADING)
    LevelBarRemoveOffsetValueMethodInfo     ,
#endif
    levelBarRemoveOffsetValue               ,


-- ** setInverted #method:setInverted#

#if defined(ENABLE_OVERLOADING)
    LevelBarSetInvertedMethodInfo           ,
#endif
    levelBarSetInverted                     ,


-- ** setMaxValue #method:setMaxValue#

#if defined(ENABLE_OVERLOADING)
    LevelBarSetMaxValueMethodInfo           ,
#endif
    levelBarSetMaxValue                     ,


-- ** setMinValue #method:setMinValue#

#if defined(ENABLE_OVERLOADING)
    LevelBarSetMinValueMethodInfo           ,
#endif
    levelBarSetMinValue                     ,


-- ** setMode #method:setMode#

#if defined(ENABLE_OVERLOADING)
    LevelBarSetModeMethodInfo               ,
#endif
    levelBarSetMode                         ,


-- ** setValue #method:setValue#

#if defined(ENABLE_OVERLOADING)
    LevelBarSetValueMethodInfo              ,
#endif
    levelBarSetValue                        ,




 -- * Properties


-- ** inverted #attr:inverted#
-- | Level bars normally grow from top to bottom or left to right.
-- Inverted level bars grow in the opposite direction.
-- 
-- /Since: 3.8/

#if defined(ENABLE_OVERLOADING)
    LevelBarInvertedPropertyInfo            ,
#endif
    constructLevelBarInverted               ,
    getLevelBarInverted                     ,
#if defined(ENABLE_OVERLOADING)
    levelBarInverted                        ,
#endif
    setLevelBarInverted                     ,


-- ** maxValue #attr:maxValue#
-- | The [LevelBar:maxValue]("GI.Gtk.Objects.LevelBar#g:attr:maxValue") property determaxes the maximum value of
-- the interval that can be displayed by the bar.
-- 
-- /Since: 3.6/

#if defined(ENABLE_OVERLOADING)
    LevelBarMaxValuePropertyInfo            ,
#endif
    constructLevelBarMaxValue               ,
    getLevelBarMaxValue                     ,
#if defined(ENABLE_OVERLOADING)
    levelBarMaxValue                        ,
#endif
    setLevelBarMaxValue                     ,


-- ** minValue #attr:minValue#
-- | The [LevelBar:minValue]("GI.Gtk.Objects.LevelBar#g:attr:minValue") property determines the minimum value of
-- the interval that can be displayed by the bar.
-- 
-- /Since: 3.6/

#if defined(ENABLE_OVERLOADING)
    LevelBarMinValuePropertyInfo            ,
#endif
    constructLevelBarMinValue               ,
    getLevelBarMinValue                     ,
#if defined(ENABLE_OVERLOADING)
    levelBarMinValue                        ,
#endif
    setLevelBarMinValue                     ,


-- ** mode #attr:mode#
-- | The [LevelBar:mode]("GI.Gtk.Objects.LevelBar#g:attr:mode") property determines the way t'GI.Gtk.Objects.LevelBar.LevelBar'
-- interprets the value properties to draw the level fill area.
-- Specifically, when the value is @/GTK_LEVEL_BAR_MODE_CONTINUOUS/@,
-- t'GI.Gtk.Objects.LevelBar.LevelBar' will draw a single block representing the current value in
-- that area; when the value is @/GTK_LEVEL_BAR_MODE_DISCRETE/@,
-- the widget will draw a succession of separate blocks filling the
-- draw area, with the number of blocks being equal to the units separating
-- the integral roundings of [LevelBar:minValue]("GI.Gtk.Objects.LevelBar#g:attr:minValue") and [LevelBar:maxValue]("GI.Gtk.Objects.LevelBar#g:attr:maxValue").
-- 
-- /Since: 3.6/

#if defined(ENABLE_OVERLOADING)
    LevelBarModePropertyInfo                ,
#endif
    constructLevelBarMode                   ,
    getLevelBarMode                         ,
#if defined(ENABLE_OVERLOADING)
    levelBarMode                            ,
#endif
    setLevelBarMode                         ,


-- ** value #attr:value#
-- | The [LevelBar:value]("GI.Gtk.Objects.LevelBar#g:attr:value") property determines the currently
-- filled value of the level bar.
-- 
-- /Since: 3.6/

#if defined(ENABLE_OVERLOADING)
    LevelBarValuePropertyInfo               ,
#endif
    constructLevelBarValue                  ,
    getLevelBarValue                        ,
#if defined(ENABLE_OVERLOADING)
    levelBarValue                           ,
#endif
    setLevelBarValue                        ,




 -- * Signals


-- ** offsetChanged #signal:offsetChanged#

    LevelBarOffsetChangedCallback           ,
#if defined(ENABLE_OVERLOADING)
    LevelBarOffsetChangedSignalInfo         ,
#endif
    afterLevelBarOffsetChanged              ,
    onLevelBarOffsetChanged                 ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype LevelBar = LevelBar (SP.ManagedPtr LevelBar)
    deriving (Eq)

instance SP.ManagedPtrNewtype LevelBar where
    toManagedPtr (LevelBar p) = p

foreign import ccall "gtk_level_bar_get_type"
    c_gtk_level_bar_get_type :: IO B.Types.GType

instance B.Types.TypedObject LevelBar where
    glibType = c_gtk_level_bar_get_type

instance B.Types.GObject LevelBar

-- | Type class for types which can be safely cast to `LevelBar`, for instance with `toLevelBar`.
class (SP.GObject o, O.IsDescendantOf LevelBar o) => IsLevelBar o
instance (SP.GObject o, O.IsDescendantOf LevelBar o) => IsLevelBar o

instance O.HasParentTypes LevelBar
type instance O.ParentTypes LevelBar = '[Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable, Gtk.Orientable.Orientable]

-- | Cast to `LevelBar`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toLevelBar :: (MIO.MonadIO m, IsLevelBar o) => o -> m LevelBar
toLevelBar = MIO.liftIO . B.ManagedPtr.unsafeCastTo LevelBar

-- | Convert 'LevelBar' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe LevelBar) where
    gvalueGType_ = c_gtk_level_bar_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr LevelBar)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr LevelBar)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject LevelBar ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveLevelBarMethod (t :: Symbol) (o :: *) :: * where
    ResolveLevelBarMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveLevelBarMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveLevelBarMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveLevelBarMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveLevelBarMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveLevelBarMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveLevelBarMethod "addOffsetValue" o = LevelBarAddOffsetValueMethodInfo
    ResolveLevelBarMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveLevelBarMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveLevelBarMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveLevelBarMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveLevelBarMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveLevelBarMethod "childNotify" o = Gtk.Widget.WidgetChildNotifyMethodInfo
    ResolveLevelBarMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveLevelBarMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveLevelBarMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveLevelBarMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveLevelBarMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveLevelBarMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveLevelBarMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveLevelBarMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveLevelBarMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveLevelBarMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveLevelBarMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveLevelBarMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveLevelBarMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveLevelBarMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveLevelBarMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveLevelBarMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveLevelBarMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveLevelBarMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveLevelBarMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveLevelBarMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveLevelBarMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveLevelBarMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveLevelBarMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveLevelBarMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveLevelBarMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveLevelBarMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveLevelBarMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveLevelBarMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveLevelBarMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveLevelBarMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveLevelBarMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveLevelBarMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveLevelBarMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveLevelBarMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveLevelBarMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveLevelBarMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveLevelBarMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveLevelBarMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveLevelBarMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveLevelBarMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveLevelBarMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveLevelBarMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveLevelBarMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveLevelBarMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveLevelBarMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveLevelBarMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveLevelBarMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveLevelBarMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveLevelBarMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveLevelBarMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveLevelBarMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveLevelBarMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveLevelBarMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveLevelBarMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveLevelBarMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveLevelBarMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveLevelBarMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveLevelBarMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveLevelBarMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveLevelBarMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveLevelBarMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveLevelBarMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveLevelBarMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveLevelBarMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveLevelBarMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveLevelBarMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveLevelBarMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveLevelBarMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveLevelBarMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveLevelBarMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveLevelBarMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveLevelBarMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveLevelBarMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveLevelBarMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveLevelBarMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveLevelBarMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveLevelBarMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveLevelBarMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveLevelBarMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveLevelBarMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveLevelBarMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveLevelBarMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveLevelBarMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveLevelBarMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveLevelBarMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveLevelBarMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveLevelBarMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveLevelBarMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveLevelBarMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveLevelBarMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveLevelBarMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveLevelBarMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveLevelBarMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveLevelBarMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveLevelBarMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveLevelBarMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveLevelBarMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveLevelBarMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveLevelBarMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveLevelBarMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveLevelBarMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveLevelBarMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveLevelBarMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveLevelBarMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveLevelBarMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveLevelBarMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveLevelBarMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveLevelBarMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveLevelBarMethod "removeOffsetValue" o = LevelBarRemoveOffsetValueMethodInfo
    ResolveLevelBarMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveLevelBarMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveLevelBarMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveLevelBarMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveLevelBarMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveLevelBarMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveLevelBarMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveLevelBarMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveLevelBarMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveLevelBarMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveLevelBarMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveLevelBarMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveLevelBarMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveLevelBarMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveLevelBarMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveLevelBarMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveLevelBarMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveLevelBarMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveLevelBarMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveLevelBarMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveLevelBarMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveLevelBarMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveLevelBarMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveLevelBarMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveLevelBarMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveLevelBarMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveLevelBarMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveLevelBarMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveLevelBarMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveLevelBarMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveLevelBarMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveLevelBarMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveLevelBarMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveLevelBarMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveLevelBarMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveLevelBarMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveLevelBarMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveLevelBarMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveLevelBarMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveLevelBarMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveLevelBarMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveLevelBarMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveLevelBarMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveLevelBarMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveLevelBarMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveLevelBarMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveLevelBarMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveLevelBarMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveLevelBarMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveLevelBarMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveLevelBarMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveLevelBarMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveLevelBarMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveLevelBarMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveLevelBarMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveLevelBarMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveLevelBarMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveLevelBarMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveLevelBarMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveLevelBarMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveLevelBarMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveLevelBarMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveLevelBarMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveLevelBarMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveLevelBarMethod "getInverted" o = LevelBarGetInvertedMethodInfo
    ResolveLevelBarMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveLevelBarMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveLevelBarMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveLevelBarMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveLevelBarMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveLevelBarMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveLevelBarMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveLevelBarMethod "getMaxValue" o = LevelBarGetMaxValueMethodInfo
    ResolveLevelBarMethod "getMinValue" o = LevelBarGetMinValueMethodInfo
    ResolveLevelBarMethod "getMode" o = LevelBarGetModeMethodInfo
    ResolveLevelBarMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveLevelBarMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveLevelBarMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveLevelBarMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveLevelBarMethod "getOffsetValue" o = LevelBarGetOffsetValueMethodInfo
    ResolveLevelBarMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveLevelBarMethod "getOrientation" o = Gtk.Orientable.OrientableGetOrientationMethodInfo
    ResolveLevelBarMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveLevelBarMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveLevelBarMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveLevelBarMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveLevelBarMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveLevelBarMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveLevelBarMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveLevelBarMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveLevelBarMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveLevelBarMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveLevelBarMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveLevelBarMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveLevelBarMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveLevelBarMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveLevelBarMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveLevelBarMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveLevelBarMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveLevelBarMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveLevelBarMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveLevelBarMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveLevelBarMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveLevelBarMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveLevelBarMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveLevelBarMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveLevelBarMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveLevelBarMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveLevelBarMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveLevelBarMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveLevelBarMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveLevelBarMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveLevelBarMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveLevelBarMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveLevelBarMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveLevelBarMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveLevelBarMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveLevelBarMethod "getValue" o = LevelBarGetValueMethodInfo
    ResolveLevelBarMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveLevelBarMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveLevelBarMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveLevelBarMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveLevelBarMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveLevelBarMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveLevelBarMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveLevelBarMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveLevelBarMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveLevelBarMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveLevelBarMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveLevelBarMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveLevelBarMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveLevelBarMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveLevelBarMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveLevelBarMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveLevelBarMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveLevelBarMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveLevelBarMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveLevelBarMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveLevelBarMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveLevelBarMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveLevelBarMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveLevelBarMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveLevelBarMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveLevelBarMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveLevelBarMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveLevelBarMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveLevelBarMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveLevelBarMethod "setInverted" o = LevelBarSetInvertedMethodInfo
    ResolveLevelBarMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveLevelBarMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveLevelBarMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveLevelBarMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveLevelBarMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveLevelBarMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveLevelBarMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveLevelBarMethod "setMaxValue" o = LevelBarSetMaxValueMethodInfo
    ResolveLevelBarMethod "setMinValue" o = LevelBarSetMinValueMethodInfo
    ResolveLevelBarMethod "setMode" o = LevelBarSetModeMethodInfo
    ResolveLevelBarMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveLevelBarMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveLevelBarMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveLevelBarMethod "setOrientation" o = Gtk.Orientable.OrientableSetOrientationMethodInfo
    ResolveLevelBarMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveLevelBarMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveLevelBarMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveLevelBarMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveLevelBarMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveLevelBarMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveLevelBarMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveLevelBarMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveLevelBarMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveLevelBarMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveLevelBarMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveLevelBarMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveLevelBarMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveLevelBarMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveLevelBarMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveLevelBarMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveLevelBarMethod "setValue" o = LevelBarSetValueMethodInfo
    ResolveLevelBarMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveLevelBarMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveLevelBarMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveLevelBarMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveLevelBarMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveLevelBarMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveLevelBarMethod t LevelBar, O.OverloadedMethod info LevelBar p) => OL.IsLabel t (LevelBar -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveLevelBarMethod t LevelBar, O.OverloadedMethod info LevelBar p, R.HasField t LevelBar p) => R.HasField t LevelBar p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveLevelBarMethod t LevelBar, O.OverloadedMethodInfo info LevelBar) => OL.IsLabel t (O.MethodProxy info LevelBar) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal LevelBar::offset-changed
-- | Emitted when an offset specified on the bar changes value as an
-- effect to 'GI.Gtk.Objects.LevelBar.levelBarAddOffsetValue' being called.
-- 
-- The signal supports detailed connections; you can connect to the
-- detailed signal \"changed[x](#g:signal:x)\" in order to only receive callbacks when
-- the value of offset \"x\" changes.
-- 
-- /Since: 3.6/
type LevelBarOffsetChangedCallback =
    T.Text
    -- ^ /@name@/: the name of the offset that changed value
    -> IO ()

type C_LevelBarOffsetChangedCallback =
    Ptr LevelBar ->                         -- object
    CString ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_LevelBarOffsetChangedCallback`.
foreign import ccall "wrapper"
    mk_LevelBarOffsetChangedCallback :: C_LevelBarOffsetChangedCallback -> IO (FunPtr C_LevelBarOffsetChangedCallback)

wrap_LevelBarOffsetChangedCallback :: 
    GObject a => (a -> LevelBarOffsetChangedCallback) ->
    C_LevelBarOffsetChangedCallback
wrap_LevelBarOffsetChangedCallback gi'cb gi'selfPtr name _ = do
    name' <- cstringToText name
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  name'


-- | Connect a signal handler for the [offsetChanged](#signal:offsetChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' levelBar #offsetChanged callback
-- @
-- 
-- This signal admits a optional parameter @detail@.
-- If it's not @Nothing@, we will connect to “@offset-changed::detail@” instead.
-- 
onLevelBarOffsetChanged :: (IsLevelBar a, MonadIO m) => a -> P.Maybe T.Text -> ((?self :: a) => LevelBarOffsetChangedCallback) -> m SignalHandlerId
onLevelBarOffsetChanged obj detail cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_LevelBarOffsetChangedCallback wrapped
    wrapped'' <- mk_LevelBarOffsetChangedCallback wrapped'
    connectSignalFunPtr obj "offset-changed" wrapped'' SignalConnectBefore detail

-- | Connect a signal handler for the [offsetChanged](#signal:offsetChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' levelBar #offsetChanged callback
-- @
-- 
-- This signal admits a optional parameter @detail@.
-- If it's not @Nothing@, we will connect to “@offset-changed::detail@” instead.
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterLevelBarOffsetChanged :: (IsLevelBar a, MonadIO m) => a -> P.Maybe T.Text -> ((?self :: a) => LevelBarOffsetChangedCallback) -> m SignalHandlerId
afterLevelBarOffsetChanged obj detail cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_LevelBarOffsetChangedCallback wrapped
    wrapped'' <- mk_LevelBarOffsetChangedCallback wrapped'
    connectSignalFunPtr obj "offset-changed" wrapped'' SignalConnectAfter detail


#if defined(ENABLE_OVERLOADING)
data LevelBarOffsetChangedSignalInfo
instance SignalInfo LevelBarOffsetChangedSignalInfo where
    type HaskellCallbackType LevelBarOffsetChangedSignalInfo = LevelBarOffsetChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_LevelBarOffsetChangedCallback cb
        cb'' <- mk_LevelBarOffsetChangedCallback cb'
        connectSignalFunPtr obj "offset-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar::offset-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#g:signal:offsetChanged"})

#endif

-- VVV Prop "inverted"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@inverted@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' levelBar #inverted
-- @
getLevelBarInverted :: (MonadIO m, IsLevelBar o) => o -> m Bool
getLevelBarInverted obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "inverted"

-- | Set the value of the “@inverted@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' levelBar [ #inverted 'Data.GI.Base.Attributes.:=' value ]
-- @
setLevelBarInverted :: (MonadIO m, IsLevelBar o) => o -> Bool -> m ()
setLevelBarInverted obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "inverted" val

-- | Construct a `GValueConstruct` with valid value for the “@inverted@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLevelBarInverted :: (IsLevelBar o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructLevelBarInverted val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "inverted" val

#if defined(ENABLE_OVERLOADING)
data LevelBarInvertedPropertyInfo
instance AttrInfo LevelBarInvertedPropertyInfo where
    type AttrAllowedOps LevelBarInvertedPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LevelBarInvertedPropertyInfo = IsLevelBar
    type AttrSetTypeConstraint LevelBarInvertedPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint LevelBarInvertedPropertyInfo = (~) Bool
    type AttrTransferType LevelBarInvertedPropertyInfo = Bool
    type AttrGetType LevelBarInvertedPropertyInfo = Bool
    type AttrLabel LevelBarInvertedPropertyInfo = "inverted"
    type AttrOrigin LevelBarInvertedPropertyInfo = LevelBar
    attrGet = getLevelBarInverted
    attrSet = setLevelBarInverted
    attrTransfer _ v = do
        return v
    attrConstruct = constructLevelBarInverted
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.inverted"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#g:attr:inverted"
        })
#endif

-- VVV Prop "max-value"
   -- Type: TBasicType TDouble
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@max-value@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' levelBar #maxValue
-- @
getLevelBarMaxValue :: (MonadIO m, IsLevelBar o) => o -> m Double
getLevelBarMaxValue obj = MIO.liftIO $ B.Properties.getObjectPropertyDouble obj "max-value"

-- | Set the value of the “@max-value@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' levelBar [ #maxValue 'Data.GI.Base.Attributes.:=' value ]
-- @
setLevelBarMaxValue :: (MonadIO m, IsLevelBar o) => o -> Double -> m ()
setLevelBarMaxValue obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyDouble obj "max-value" val

-- | Construct a `GValueConstruct` with valid value for the “@max-value@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLevelBarMaxValue :: (IsLevelBar o, MIO.MonadIO m) => Double -> m (GValueConstruct o)
constructLevelBarMaxValue val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyDouble "max-value" val

#if defined(ENABLE_OVERLOADING)
data LevelBarMaxValuePropertyInfo
instance AttrInfo LevelBarMaxValuePropertyInfo where
    type AttrAllowedOps LevelBarMaxValuePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LevelBarMaxValuePropertyInfo = IsLevelBar
    type AttrSetTypeConstraint LevelBarMaxValuePropertyInfo = (~) Double
    type AttrTransferTypeConstraint LevelBarMaxValuePropertyInfo = (~) Double
    type AttrTransferType LevelBarMaxValuePropertyInfo = Double
    type AttrGetType LevelBarMaxValuePropertyInfo = Double
    type AttrLabel LevelBarMaxValuePropertyInfo = "max-value"
    type AttrOrigin LevelBarMaxValuePropertyInfo = LevelBar
    attrGet = getLevelBarMaxValue
    attrSet = setLevelBarMaxValue
    attrTransfer _ v = do
        return v
    attrConstruct = constructLevelBarMaxValue
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.maxValue"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#g:attr:maxValue"
        })
#endif

-- VVV Prop "min-value"
   -- Type: TBasicType TDouble
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@min-value@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' levelBar #minValue
-- @
getLevelBarMinValue :: (MonadIO m, IsLevelBar o) => o -> m Double
getLevelBarMinValue obj = MIO.liftIO $ B.Properties.getObjectPropertyDouble obj "min-value"

-- | Set the value of the “@min-value@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' levelBar [ #minValue 'Data.GI.Base.Attributes.:=' value ]
-- @
setLevelBarMinValue :: (MonadIO m, IsLevelBar o) => o -> Double -> m ()
setLevelBarMinValue obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyDouble obj "min-value" val

-- | Construct a `GValueConstruct` with valid value for the “@min-value@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLevelBarMinValue :: (IsLevelBar o, MIO.MonadIO m) => Double -> m (GValueConstruct o)
constructLevelBarMinValue val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyDouble "min-value" val

#if defined(ENABLE_OVERLOADING)
data LevelBarMinValuePropertyInfo
instance AttrInfo LevelBarMinValuePropertyInfo where
    type AttrAllowedOps LevelBarMinValuePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LevelBarMinValuePropertyInfo = IsLevelBar
    type AttrSetTypeConstraint LevelBarMinValuePropertyInfo = (~) Double
    type AttrTransferTypeConstraint LevelBarMinValuePropertyInfo = (~) Double
    type AttrTransferType LevelBarMinValuePropertyInfo = Double
    type AttrGetType LevelBarMinValuePropertyInfo = Double
    type AttrLabel LevelBarMinValuePropertyInfo = "min-value"
    type AttrOrigin LevelBarMinValuePropertyInfo = LevelBar
    attrGet = getLevelBarMinValue
    attrSet = setLevelBarMinValue
    attrTransfer _ v = do
        return v
    attrConstruct = constructLevelBarMinValue
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.minValue"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#g:attr:minValue"
        })
#endif

-- VVV Prop "mode"
   -- Type: TInterface (Name {namespace = "Gtk", name = "LevelBarMode"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' levelBar #mode
-- @
getLevelBarMode :: (MonadIO m, IsLevelBar o) => o -> m Gtk.Enums.LevelBarMode
getLevelBarMode obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "mode"

-- | Set the value of the “@mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' levelBar [ #mode 'Data.GI.Base.Attributes.:=' value ]
-- @
setLevelBarMode :: (MonadIO m, IsLevelBar o) => o -> Gtk.Enums.LevelBarMode -> m ()
setLevelBarMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "mode" val

-- | Construct a `GValueConstruct` with valid value for the “@mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLevelBarMode :: (IsLevelBar o, MIO.MonadIO m) => Gtk.Enums.LevelBarMode -> m (GValueConstruct o)
constructLevelBarMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "mode" val

#if defined(ENABLE_OVERLOADING)
data LevelBarModePropertyInfo
instance AttrInfo LevelBarModePropertyInfo where
    type AttrAllowedOps LevelBarModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LevelBarModePropertyInfo = IsLevelBar
    type AttrSetTypeConstraint LevelBarModePropertyInfo = (~) Gtk.Enums.LevelBarMode
    type AttrTransferTypeConstraint LevelBarModePropertyInfo = (~) Gtk.Enums.LevelBarMode
    type AttrTransferType LevelBarModePropertyInfo = Gtk.Enums.LevelBarMode
    type AttrGetType LevelBarModePropertyInfo = Gtk.Enums.LevelBarMode
    type AttrLabel LevelBarModePropertyInfo = "mode"
    type AttrOrigin LevelBarModePropertyInfo = LevelBar
    attrGet = getLevelBarMode
    attrSet = setLevelBarMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructLevelBarMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.mode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#g:attr:mode"
        })
#endif

-- VVV Prop "value"
   -- Type: TBasicType TDouble
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@value@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' levelBar #value
-- @
getLevelBarValue :: (MonadIO m, IsLevelBar o) => o -> m Double
getLevelBarValue obj = MIO.liftIO $ B.Properties.getObjectPropertyDouble obj "value"

-- | Set the value of the “@value@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' levelBar [ #value 'Data.GI.Base.Attributes.:=' value ]
-- @
setLevelBarValue :: (MonadIO m, IsLevelBar o) => o -> Double -> m ()
setLevelBarValue obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyDouble obj "value" val

-- | Construct a `GValueConstruct` with valid value for the “@value@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLevelBarValue :: (IsLevelBar o, MIO.MonadIO m) => Double -> m (GValueConstruct o)
constructLevelBarValue val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyDouble "value" val

#if defined(ENABLE_OVERLOADING)
data LevelBarValuePropertyInfo
instance AttrInfo LevelBarValuePropertyInfo where
    type AttrAllowedOps LevelBarValuePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LevelBarValuePropertyInfo = IsLevelBar
    type AttrSetTypeConstraint LevelBarValuePropertyInfo = (~) Double
    type AttrTransferTypeConstraint LevelBarValuePropertyInfo = (~) Double
    type AttrTransferType LevelBarValuePropertyInfo = Double
    type AttrGetType LevelBarValuePropertyInfo = Double
    type AttrLabel LevelBarValuePropertyInfo = "value"
    type AttrOrigin LevelBarValuePropertyInfo = LevelBar
    attrGet = getLevelBarValue
    attrSet = setLevelBarValue
    attrTransfer _ v = do
        return v
    attrConstruct = constructLevelBarValue
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.value"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#g:attr:value"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList LevelBar
type instance O.AttributeList LevelBar = LevelBarAttributeList
type LevelBarAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("inverted", LevelBarInvertedPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("maxValue", LevelBarMaxValuePropertyInfo), '("minValue", LevelBarMinValuePropertyInfo), '("mode", LevelBarModePropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("orientation", Gtk.Orientable.OrientableOrientationPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("value", LevelBarValuePropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
levelBarInverted :: AttrLabelProxy "inverted"
levelBarInverted = AttrLabelProxy

levelBarMaxValue :: AttrLabelProxy "maxValue"
levelBarMaxValue = AttrLabelProxy

levelBarMinValue :: AttrLabelProxy "minValue"
levelBarMinValue = AttrLabelProxy

levelBarMode :: AttrLabelProxy "mode"
levelBarMode = AttrLabelProxy

levelBarValue :: AttrLabelProxy "value"
levelBarValue = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList LevelBar = LevelBarSignalList
type LevelBarSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("offsetChanged", LevelBarOffsetChangedSignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method LevelBar::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "LevelBar" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_level_bar_new" gtk_level_bar_new :: 
    IO (Ptr LevelBar)

-- | Creates a new t'GI.Gtk.Objects.LevelBar.LevelBar'.
-- 
-- /Since: 3.6/
levelBarNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m LevelBar
    -- ^ __Returns:__ a t'GI.Gtk.Objects.LevelBar.LevelBar'.
levelBarNew  = liftIO $ do
    result <- gtk_level_bar_new
    checkUnexpectedReturnNULL "levelBarNew" result
    result' <- (newObject LevelBar) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method LevelBar::new_for_interval
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "min_value"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a positive value" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "max_value"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a positive value" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "LevelBar" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_level_bar_new_for_interval" gtk_level_bar_new_for_interval :: 
    CDouble ->                              -- min_value : TBasicType TDouble
    CDouble ->                              -- max_value : TBasicType TDouble
    IO (Ptr LevelBar)

-- | Utility constructor that creates a new t'GI.Gtk.Objects.LevelBar.LevelBar' for the specified
-- interval.
-- 
-- /Since: 3.6/
levelBarNewForInterval ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Double
    -- ^ /@minValue@/: a positive value
    -> Double
    -- ^ /@maxValue@/: a positive value
    -> m LevelBar
    -- ^ __Returns:__ a t'GI.Gtk.Objects.LevelBar.LevelBar'
levelBarNewForInterval minValue maxValue = liftIO $ do
    let minValue' = realToFrac minValue
    let maxValue' = realToFrac maxValue
    result <- gtk_level_bar_new_for_interval minValue' maxValue'
    checkUnexpectedReturnNULL "levelBarNewForInterval" result
    result' <- (newObject LevelBar) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method LevelBar::add_offset_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "LevelBar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLevelBar" , sinceVersion = Nothing }
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
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the new offset"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the value for the new offset"
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

foreign import ccall "gtk_level_bar_add_offset_value" gtk_level_bar_add_offset_value :: 
    Ptr LevelBar ->                         -- self : TInterface (Name {namespace = "Gtk", name = "LevelBar"})
    CString ->                              -- name : TBasicType TUTF8
    CDouble ->                              -- value : TBasicType TDouble
    IO ()

-- | Adds a new offset marker on /@self@/ at the position specified by /@value@/.
-- When the bar value is in the interval topped by /@value@/ (or between /@value@/
-- and [LevelBar:maxValue]("GI.Gtk.Objects.LevelBar#g:attr:maxValue") in case the offset is the last one on the bar)
-- a style class named @level-@/@name@/ will be applied
-- when rendering the level bar fill.
-- If another offset marker named /@name@/ exists, its value will be
-- replaced by /@value@/.
-- 
-- /Since: 3.6/
levelBarAddOffsetValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsLevelBar a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.LevelBar.LevelBar'
    -> T.Text
    -- ^ /@name@/: the name of the new offset
    -> Double
    -- ^ /@value@/: the value for the new offset
    -> m ()
levelBarAddOffsetValue self name value = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    name' <- textToCString name
    let value' = realToFrac value
    gtk_level_bar_add_offset_value self' name' value'
    touchManagedPtr self
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data LevelBarAddOffsetValueMethodInfo
instance (signature ~ (T.Text -> Double -> m ()), MonadIO m, IsLevelBar a) => O.OverloadedMethod LevelBarAddOffsetValueMethodInfo a signature where
    overloadedMethod = levelBarAddOffsetValue

instance O.OverloadedMethodInfo LevelBarAddOffsetValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.levelBarAddOffsetValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#v:levelBarAddOffsetValue"
        })


#endif

-- method LevelBar::get_inverted
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "LevelBar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLevelBar" , sinceVersion = Nothing }
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

foreign import ccall "gtk_level_bar_get_inverted" gtk_level_bar_get_inverted :: 
    Ptr LevelBar ->                         -- self : TInterface (Name {namespace = "Gtk", name = "LevelBar"})
    IO CInt

-- | Return the value of the [LevelBar:inverted]("GI.Gtk.Objects.LevelBar#g:attr:inverted") property.
-- 
-- /Since: 3.8/
levelBarGetInverted ::
    (B.CallStack.HasCallStack, MonadIO m, IsLevelBar a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.LevelBar.LevelBar'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the level bar is inverted
levelBarGetInverted self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_level_bar_get_inverted self'
    let result' = (/= 0) result
    touchManagedPtr self
    return result'

#if defined(ENABLE_OVERLOADING)
data LevelBarGetInvertedMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsLevelBar a) => O.OverloadedMethod LevelBarGetInvertedMethodInfo a signature where
    overloadedMethod = levelBarGetInverted

instance O.OverloadedMethodInfo LevelBarGetInvertedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.levelBarGetInverted",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#v:levelBarGetInverted"
        })


#endif

-- method LevelBar::get_max_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "LevelBar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLevelBar" , sinceVersion = Nothing }
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

foreign import ccall "gtk_level_bar_get_max_value" gtk_level_bar_get_max_value :: 
    Ptr LevelBar ->                         -- self : TInterface (Name {namespace = "Gtk", name = "LevelBar"})
    IO CDouble

-- | Returns the value of the [LevelBar:maxValue]("GI.Gtk.Objects.LevelBar#g:attr:maxValue") property.
-- 
-- /Since: 3.6/
levelBarGetMaxValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsLevelBar a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.LevelBar.LevelBar'
    -> m Double
    -- ^ __Returns:__ a positive value
levelBarGetMaxValue self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_level_bar_get_max_value self'
    let result' = realToFrac result
    touchManagedPtr self
    return result'

#if defined(ENABLE_OVERLOADING)
data LevelBarGetMaxValueMethodInfo
instance (signature ~ (m Double), MonadIO m, IsLevelBar a) => O.OverloadedMethod LevelBarGetMaxValueMethodInfo a signature where
    overloadedMethod = levelBarGetMaxValue

instance O.OverloadedMethodInfo LevelBarGetMaxValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.levelBarGetMaxValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#v:levelBarGetMaxValue"
        })


#endif

-- method LevelBar::get_min_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "LevelBar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLevelBar" , sinceVersion = Nothing }
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

foreign import ccall "gtk_level_bar_get_min_value" gtk_level_bar_get_min_value :: 
    Ptr LevelBar ->                         -- self : TInterface (Name {namespace = "Gtk", name = "LevelBar"})
    IO CDouble

-- | Returns the value of the [LevelBar:minValue]("GI.Gtk.Objects.LevelBar#g:attr:minValue") property.
-- 
-- /Since: 3.6/
levelBarGetMinValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsLevelBar a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.LevelBar.LevelBar'
    -> m Double
    -- ^ __Returns:__ a positive value
levelBarGetMinValue self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_level_bar_get_min_value self'
    let result' = realToFrac result
    touchManagedPtr self
    return result'

#if defined(ENABLE_OVERLOADING)
data LevelBarGetMinValueMethodInfo
instance (signature ~ (m Double), MonadIO m, IsLevelBar a) => O.OverloadedMethod LevelBarGetMinValueMethodInfo a signature where
    overloadedMethod = levelBarGetMinValue

instance O.OverloadedMethodInfo LevelBarGetMinValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.levelBarGetMinValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#v:levelBarGetMinValue"
        })


#endif

-- method LevelBar::get_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "LevelBar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLevelBar" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "LevelBarMode" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_level_bar_get_mode" gtk_level_bar_get_mode :: 
    Ptr LevelBar ->                         -- self : TInterface (Name {namespace = "Gtk", name = "LevelBar"})
    IO CUInt

-- | Returns the value of the [LevelBar:mode]("GI.Gtk.Objects.LevelBar#g:attr:mode") property.
-- 
-- /Since: 3.6/
levelBarGetMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsLevelBar a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.LevelBar.LevelBar'
    -> m Gtk.Enums.LevelBarMode
    -- ^ __Returns:__ a t'GI.Gtk.Enums.LevelBarMode'
levelBarGetMode self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_level_bar_get_mode self'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr self
    return result'

#if defined(ENABLE_OVERLOADING)
data LevelBarGetModeMethodInfo
instance (signature ~ (m Gtk.Enums.LevelBarMode), MonadIO m, IsLevelBar a) => O.OverloadedMethod LevelBarGetModeMethodInfo a signature where
    overloadedMethod = levelBarGetMode

instance O.OverloadedMethodInfo LevelBarGetModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.levelBarGetMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#v:levelBarGetMode"
        })


#endif

-- method LevelBar::get_offset_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "LevelBar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLevelBar" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the name of an offset in the bar"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location where to store the value"
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
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_level_bar_get_offset_value" gtk_level_bar_get_offset_value :: 
    Ptr LevelBar ->                         -- self : TInterface (Name {namespace = "Gtk", name = "LevelBar"})
    CString ->                              -- name : TBasicType TUTF8
    Ptr CDouble ->                          -- value : TBasicType TDouble
    IO CInt

-- | Fetches the value specified for the offset marker /@name@/ in /@self@/,
-- returning 'P.True' in case an offset named /@name@/ was found.
-- 
-- /Since: 3.6/
levelBarGetOffsetValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsLevelBar a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.LevelBar.LevelBar'
    -> Maybe (T.Text)
    -- ^ /@name@/: the name of an offset in the bar
    -> m ((Bool, Double))
    -- ^ __Returns:__ 'P.True' if the specified offset is found
levelBarGetOffsetValue self name = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    maybeName <- case name of
        Nothing -> return nullPtr
        Just jName -> do
            jName' <- textToCString jName
            return jName'
    value <- allocMem :: IO (Ptr CDouble)
    result <- gtk_level_bar_get_offset_value self' maybeName value
    let result' = (/= 0) result
    value' <- peek value
    let value'' = realToFrac value'
    touchManagedPtr self
    freeMem maybeName
    freeMem value
    return (result', value'')

#if defined(ENABLE_OVERLOADING)
data LevelBarGetOffsetValueMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ((Bool, Double))), MonadIO m, IsLevelBar a) => O.OverloadedMethod LevelBarGetOffsetValueMethodInfo a signature where
    overloadedMethod = levelBarGetOffsetValue

instance O.OverloadedMethodInfo LevelBarGetOffsetValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.levelBarGetOffsetValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#v:levelBarGetOffsetValue"
        })


#endif

-- method LevelBar::get_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "LevelBar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLevelBar" , sinceVersion = Nothing }
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

foreign import ccall "gtk_level_bar_get_value" gtk_level_bar_get_value :: 
    Ptr LevelBar ->                         -- self : TInterface (Name {namespace = "Gtk", name = "LevelBar"})
    IO CDouble

-- | Returns the value of the [LevelBar:value]("GI.Gtk.Objects.LevelBar#g:attr:value") property.
-- 
-- /Since: 3.6/
levelBarGetValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsLevelBar a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.LevelBar.LevelBar'
    -> m Double
    -- ^ __Returns:__ a value in the interval between
    --     [LevelBar:minValue]("GI.Gtk.Objects.LevelBar#g:attr:minValue") and [LevelBar:maxValue]("GI.Gtk.Objects.LevelBar#g:attr:maxValue")
levelBarGetValue self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_level_bar_get_value self'
    let result' = realToFrac result
    touchManagedPtr self
    return result'

#if defined(ENABLE_OVERLOADING)
data LevelBarGetValueMethodInfo
instance (signature ~ (m Double), MonadIO m, IsLevelBar a) => O.OverloadedMethod LevelBarGetValueMethodInfo a signature where
    overloadedMethod = levelBarGetValue

instance O.OverloadedMethodInfo LevelBarGetValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.levelBarGetValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#v:levelBarGetValue"
        })


#endif

-- method LevelBar::remove_offset_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "LevelBar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLevelBar" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the name of an offset in the bar"
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

foreign import ccall "gtk_level_bar_remove_offset_value" gtk_level_bar_remove_offset_value :: 
    Ptr LevelBar ->                         -- self : TInterface (Name {namespace = "Gtk", name = "LevelBar"})
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Removes an offset marker previously added with
-- 'GI.Gtk.Objects.LevelBar.levelBarAddOffsetValue'.
-- 
-- /Since: 3.6/
levelBarRemoveOffsetValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsLevelBar a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.LevelBar.LevelBar'
    -> Maybe (T.Text)
    -- ^ /@name@/: the name of an offset in the bar
    -> m ()
levelBarRemoveOffsetValue self name = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    maybeName <- case name of
        Nothing -> return nullPtr
        Just jName -> do
            jName' <- textToCString jName
            return jName'
    gtk_level_bar_remove_offset_value self' maybeName
    touchManagedPtr self
    freeMem maybeName
    return ()

#if defined(ENABLE_OVERLOADING)
data LevelBarRemoveOffsetValueMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsLevelBar a) => O.OverloadedMethod LevelBarRemoveOffsetValueMethodInfo a signature where
    overloadedMethod = levelBarRemoveOffsetValue

instance O.OverloadedMethodInfo LevelBarRemoveOffsetValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.levelBarRemoveOffsetValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#v:levelBarRemoveOffsetValue"
        })


#endif

-- method LevelBar::set_inverted
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "LevelBar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLevelBar" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "inverted"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to invert the level bar"
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

foreign import ccall "gtk_level_bar_set_inverted" gtk_level_bar_set_inverted :: 
    Ptr LevelBar ->                         -- self : TInterface (Name {namespace = "Gtk", name = "LevelBar"})
    CInt ->                                 -- inverted : TBasicType TBoolean
    IO ()

-- | Sets the value of the [LevelBar:inverted]("GI.Gtk.Objects.LevelBar#g:attr:inverted") property.
-- 
-- /Since: 3.8/
levelBarSetInverted ::
    (B.CallStack.HasCallStack, MonadIO m, IsLevelBar a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.LevelBar.LevelBar'
    -> Bool
    -- ^ /@inverted@/: 'P.True' to invert the level bar
    -> m ()
levelBarSetInverted self inverted = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    let inverted' = (fromIntegral . fromEnum) inverted
    gtk_level_bar_set_inverted self' inverted'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data LevelBarSetInvertedMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsLevelBar a) => O.OverloadedMethod LevelBarSetInvertedMethodInfo a signature where
    overloadedMethod = levelBarSetInverted

instance O.OverloadedMethodInfo LevelBarSetInvertedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.levelBarSetInverted",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#v:levelBarSetInverted"
        })


#endif

-- method LevelBar::set_max_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "LevelBar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLevelBar" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a positive value" , sinceVersion = Nothing }
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

foreign import ccall "gtk_level_bar_set_max_value" gtk_level_bar_set_max_value :: 
    Ptr LevelBar ->                         -- self : TInterface (Name {namespace = "Gtk", name = "LevelBar"})
    CDouble ->                              -- value : TBasicType TDouble
    IO ()

-- | Sets the value of the [LevelBar:maxValue]("GI.Gtk.Objects.LevelBar#g:attr:maxValue") property.
-- 
-- You probably want to update preexisting level offsets after calling
-- this function.
-- 
-- /Since: 3.6/
levelBarSetMaxValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsLevelBar a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.LevelBar.LevelBar'
    -> Double
    -- ^ /@value@/: a positive value
    -> m ()
levelBarSetMaxValue self value = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    let value' = realToFrac value
    gtk_level_bar_set_max_value self' value'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data LevelBarSetMaxValueMethodInfo
instance (signature ~ (Double -> m ()), MonadIO m, IsLevelBar a) => O.OverloadedMethod LevelBarSetMaxValueMethodInfo a signature where
    overloadedMethod = levelBarSetMaxValue

instance O.OverloadedMethodInfo LevelBarSetMaxValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.levelBarSetMaxValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#v:levelBarSetMaxValue"
        })


#endif

-- method LevelBar::set_min_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "LevelBar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLevelBar" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a positive value" , sinceVersion = Nothing }
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

foreign import ccall "gtk_level_bar_set_min_value" gtk_level_bar_set_min_value :: 
    Ptr LevelBar ->                         -- self : TInterface (Name {namespace = "Gtk", name = "LevelBar"})
    CDouble ->                              -- value : TBasicType TDouble
    IO ()

-- | Sets the value of the [LevelBar:minValue]("GI.Gtk.Objects.LevelBar#g:attr:minValue") property.
-- 
-- You probably want to update preexisting level offsets after calling
-- this function.
-- 
-- /Since: 3.6/
levelBarSetMinValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsLevelBar a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.LevelBar.LevelBar'
    -> Double
    -- ^ /@value@/: a positive value
    -> m ()
levelBarSetMinValue self value = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    let value' = realToFrac value
    gtk_level_bar_set_min_value self' value'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data LevelBarSetMinValueMethodInfo
instance (signature ~ (Double -> m ()), MonadIO m, IsLevelBar a) => O.OverloadedMethod LevelBarSetMinValueMethodInfo a signature where
    overloadedMethod = levelBarSetMinValue

instance O.OverloadedMethodInfo LevelBarSetMinValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.levelBarSetMinValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#v:levelBarSetMinValue"
        })


#endif

-- method LevelBar::set_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "LevelBar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLevelBar" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "mode"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "LevelBarMode" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLevelBarMode" , sinceVersion = Nothing }
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

foreign import ccall "gtk_level_bar_set_mode" gtk_level_bar_set_mode :: 
    Ptr LevelBar ->                         -- self : TInterface (Name {namespace = "Gtk", name = "LevelBar"})
    CUInt ->                                -- mode : TInterface (Name {namespace = "Gtk", name = "LevelBarMode"})
    IO ()

-- | Sets the value of the [LevelBar:mode]("GI.Gtk.Objects.LevelBar#g:attr:mode") property.
-- 
-- /Since: 3.6/
levelBarSetMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsLevelBar a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.LevelBar.LevelBar'
    -> Gtk.Enums.LevelBarMode
    -- ^ /@mode@/: a t'GI.Gtk.Enums.LevelBarMode'
    -> m ()
levelBarSetMode self mode = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    let mode' = (fromIntegral . fromEnum) mode
    gtk_level_bar_set_mode self' mode'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data LevelBarSetModeMethodInfo
instance (signature ~ (Gtk.Enums.LevelBarMode -> m ()), MonadIO m, IsLevelBar a) => O.OverloadedMethod LevelBarSetModeMethodInfo a signature where
    overloadedMethod = levelBarSetMode

instance O.OverloadedMethodInfo LevelBarSetModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.levelBarSetMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#v:levelBarSetMode"
        })


#endif

-- method LevelBar::set_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "LevelBar" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLevelBar" , sinceVersion = Nothing }
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
--                       "a value in the interval between\n    #GtkLevelBar:min-value and #GtkLevelBar:max-value"
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

foreign import ccall "gtk_level_bar_set_value" gtk_level_bar_set_value :: 
    Ptr LevelBar ->                         -- self : TInterface (Name {namespace = "Gtk", name = "LevelBar"})
    CDouble ->                              -- value : TBasicType TDouble
    IO ()

-- | Sets the value of the [LevelBar:value]("GI.Gtk.Objects.LevelBar#g:attr:value") property.
-- 
-- /Since: 3.6/
levelBarSetValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsLevelBar a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.LevelBar.LevelBar'
    -> Double
    -- ^ /@value@/: a value in the interval between
    --     [LevelBar:minValue]("GI.Gtk.Objects.LevelBar#g:attr:minValue") and [LevelBar:maxValue]("GI.Gtk.Objects.LevelBar#g:attr:maxValue")
    -> m ()
levelBarSetValue self value = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    let value' = realToFrac value
    gtk_level_bar_set_value self' value'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data LevelBarSetValueMethodInfo
instance (signature ~ (Double -> m ()), MonadIO m, IsLevelBar a) => O.OverloadedMethod LevelBarSetValueMethodInfo a signature where
    overloadedMethod = levelBarSetValue

instance O.OverloadedMethodInfo LevelBarSetValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.LevelBar.levelBarSetValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-LevelBar.html#v:levelBarSetValue"
        })


#endif


