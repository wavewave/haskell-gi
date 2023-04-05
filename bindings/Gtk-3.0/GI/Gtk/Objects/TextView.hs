{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- You may wish to begin by reading the
-- [text widget conceptual overview][TextWidget]
-- which gives an overview of all the objects and data
-- types related to the text widget and how they work together.
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >textview.view
-- >├── border.top
-- >├── border.left
-- >├── text
-- >│   ╰── [selection]
-- >├── border.right
-- >├── border.bottom
-- >╰── [window.popup]
-- 
-- 
-- GtkTextView has a main css node with name textview and style class .view,
-- and subnodes for each of the border windows, and the main text area,
-- with names border and text, respectively. The border nodes each get
-- one of the style classes .left, .right, .top or .bottom.
-- 
-- A node representing the selection will appear below the text node.
-- 
-- If a context menu is opened, the window node will appear as a subnode
-- of the main node.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.TextView
    ( 

-- * Exported types
    TextView(..)                            ,
    IsTextView                              ,
    toTextView                              ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addChildAtAnchor]("GI.Gtk.Objects.TextView#g:method:addChildAtAnchor"), [addChildInWindow]("GI.Gtk.Objects.TextView#g:method:addChildInWindow"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [backwardDisplayLine]("GI.Gtk.Objects.TextView#g:method:backwardDisplayLine"), [backwardDisplayLineStart]("GI.Gtk.Objects.TextView#g:method:backwardDisplayLineStart"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [bufferToWindowCoords]("GI.Gtk.Objects.TextView#g:method:bufferToWindowCoords"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [forwardDisplayLine]("GI.Gtk.Objects.TextView#g:method:forwardDisplayLine"), [forwardDisplayLineEnd]("GI.Gtk.Objects.TextView#g:method:forwardDisplayLineEnd"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [imContextFilterKeypress]("GI.Gtk.Objects.TextView#g:method:imContextFilterKeypress"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [moveChild]("GI.Gtk.Objects.TextView#g:method:moveChild"), [moveMarkOnscreen]("GI.Gtk.Objects.TextView#g:method:moveMarkOnscreen"), [moveVisually]("GI.Gtk.Objects.TextView#g:method:moveVisually"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [placeCursorOnscreen]("GI.Gtk.Objects.TextView#g:method:placeCursorOnscreen"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetCursorBlink]("GI.Gtk.Objects.TextView#g:method:resetCursorBlink"), [resetImContext]("GI.Gtk.Objects.TextView#g:method:resetImContext"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [scrollMarkOnscreen]("GI.Gtk.Objects.TextView#g:method:scrollMarkOnscreen"), [scrollToIter]("GI.Gtk.Objects.TextView#g:method:scrollToIter"), [scrollToMark]("GI.Gtk.Objects.TextView#g:method:scrollToMark"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [startsDisplayLine]("GI.Gtk.Objects.TextView#g:method:startsDisplayLine"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure"), [windowToBufferCoords]("GI.Gtk.Objects.TextView#g:method:windowToBufferCoords").
-- 
-- ==== Getters
-- [getAcceptsTab]("GI.Gtk.Objects.TextView#g:method:getAcceptsTab"), [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorder]("GI.Gtk.Interfaces.Scrollable#g:method:getBorder"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getBorderWindowSize]("GI.Gtk.Objects.TextView#g:method:getBorderWindowSize"), [getBottomMargin]("GI.Gtk.Objects.TextView#g:method:getBottomMargin"), [getBuffer]("GI.Gtk.Objects.TextView#g:method:getBuffer"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getCursorLocations]("GI.Gtk.Objects.TextView#g:method:getCursorLocations"), [getCursorVisible]("GI.Gtk.Objects.TextView#g:method:getCursorVisible"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDefaultAttributes]("GI.Gtk.Objects.TextView#g:method:getDefaultAttributes"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEditable]("GI.Gtk.Objects.TextView#g:method:getEditable"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHadjustment]("GI.Gtk.Objects.TextView#g:method:getHadjustment"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getHscrollPolicy]("GI.Gtk.Interfaces.Scrollable#g:method:getHscrollPolicy"), [getIndent]("GI.Gtk.Objects.TextView#g:method:getIndent"), [getInputHints]("GI.Gtk.Objects.TextView#g:method:getInputHints"), [getInputPurpose]("GI.Gtk.Objects.TextView#g:method:getInputPurpose"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getIterAtLocation]("GI.Gtk.Objects.TextView#g:method:getIterAtLocation"), [getIterAtPosition]("GI.Gtk.Objects.TextView#g:method:getIterAtPosition"), [getIterLocation]("GI.Gtk.Objects.TextView#g:method:getIterLocation"), [getJustification]("GI.Gtk.Objects.TextView#g:method:getJustification"), [getLeftMargin]("GI.Gtk.Objects.TextView#g:method:getLeftMargin"), [getLineAtY]("GI.Gtk.Objects.TextView#g:method:getLineAtY"), [getLineYrange]("GI.Gtk.Objects.TextView#g:method:getLineYrange"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getMonospace]("GI.Gtk.Objects.TextView#g:method:getMonospace"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getOverwrite]("GI.Gtk.Objects.TextView#g:method:getOverwrite"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPixelsAboveLines]("GI.Gtk.Objects.TextView#g:method:getPixelsAboveLines"), [getPixelsBelowLines]("GI.Gtk.Objects.TextView#g:method:getPixelsBelowLines"), [getPixelsInsideWrap]("GI.Gtk.Objects.TextView#g:method:getPixelsInsideWrap"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRightMargin]("GI.Gtk.Objects.TextView#g:method:getRightMargin"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTabs]("GI.Gtk.Objects.TextView#g:method:getTabs"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getTopMargin]("GI.Gtk.Objects.TextView#g:method:getTopMargin"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getVadjustment]("GI.Gtk.Objects.TextView#g:method:getVadjustment"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisibleRect]("GI.Gtk.Objects.TextView#g:method:getVisibleRect"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getVscrollPolicy]("GI.Gtk.Interfaces.Scrollable#g:method:getVscrollPolicy"), [getWindow]("GI.Gtk.Objects.TextView#g:method:getWindow"), [getWindowType]("GI.Gtk.Objects.TextView#g:method:getWindowType"), [getWrapMode]("GI.Gtk.Objects.TextView#g:method:getWrapMode").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAcceptsTab]("GI.Gtk.Objects.TextView#g:method:setAcceptsTab"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBorderWindowSize]("GI.Gtk.Objects.TextView#g:method:setBorderWindowSize"), [setBottomMargin]("GI.Gtk.Objects.TextView#g:method:setBottomMargin"), [setBuffer]("GI.Gtk.Objects.TextView#g:method:setBuffer"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setCursorVisible]("GI.Gtk.Objects.TextView#g:method:setCursorVisible"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEditable]("GI.Gtk.Objects.TextView#g:method:setEditable"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHadjustment]("GI.Gtk.Interfaces.Scrollable#g:method:setHadjustment"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setHscrollPolicy]("GI.Gtk.Interfaces.Scrollable#g:method:setHscrollPolicy"), [setIndent]("GI.Gtk.Objects.TextView#g:method:setIndent"), [setInputHints]("GI.Gtk.Objects.TextView#g:method:setInputHints"), [setInputPurpose]("GI.Gtk.Objects.TextView#g:method:setInputPurpose"), [setJustification]("GI.Gtk.Objects.TextView#g:method:setJustification"), [setLeftMargin]("GI.Gtk.Objects.TextView#g:method:setLeftMargin"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMonospace]("GI.Gtk.Objects.TextView#g:method:setMonospace"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setOverwrite]("GI.Gtk.Objects.TextView#g:method:setOverwrite"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPixelsAboveLines]("GI.Gtk.Objects.TextView#g:method:setPixelsAboveLines"), [setPixelsBelowLines]("GI.Gtk.Objects.TextView#g:method:setPixelsBelowLines"), [setPixelsInsideWrap]("GI.Gtk.Objects.TextView#g:method:setPixelsInsideWrap"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setRightMargin]("GI.Gtk.Objects.TextView#g:method:setRightMargin"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTabs]("GI.Gtk.Objects.TextView#g:method:setTabs"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setTopMargin]("GI.Gtk.Objects.TextView#g:method:setTopMargin"), [setVadjustment]("GI.Gtk.Interfaces.Scrollable#g:method:setVadjustment"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setVscrollPolicy]("GI.Gtk.Interfaces.Scrollable#g:method:setVscrollPolicy"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow"), [setWrapMode]("GI.Gtk.Objects.TextView#g:method:setWrapMode").

#if defined(ENABLE_OVERLOADING)
    ResolveTextViewMethod                   ,
#endif

-- ** addChildAtAnchor #method:addChildAtAnchor#

#if defined(ENABLE_OVERLOADING)
    TextViewAddChildAtAnchorMethodInfo      ,
#endif
    textViewAddChildAtAnchor                ,


-- ** addChildInWindow #method:addChildInWindow#

#if defined(ENABLE_OVERLOADING)
    TextViewAddChildInWindowMethodInfo      ,
#endif
    textViewAddChildInWindow                ,


-- ** backwardDisplayLine #method:backwardDisplayLine#

#if defined(ENABLE_OVERLOADING)
    TextViewBackwardDisplayLineMethodInfo   ,
#endif
    textViewBackwardDisplayLine             ,


-- ** backwardDisplayLineStart #method:backwardDisplayLineStart#

#if defined(ENABLE_OVERLOADING)
    TextViewBackwardDisplayLineStartMethodInfo,
#endif
    textViewBackwardDisplayLineStart        ,


-- ** bufferToWindowCoords #method:bufferToWindowCoords#

#if defined(ENABLE_OVERLOADING)
    TextViewBufferToWindowCoordsMethodInfo  ,
#endif
    textViewBufferToWindowCoords            ,


-- ** forwardDisplayLine #method:forwardDisplayLine#

#if defined(ENABLE_OVERLOADING)
    TextViewForwardDisplayLineMethodInfo    ,
#endif
    textViewForwardDisplayLine              ,


-- ** forwardDisplayLineEnd #method:forwardDisplayLineEnd#

#if defined(ENABLE_OVERLOADING)
    TextViewForwardDisplayLineEndMethodInfo ,
#endif
    textViewForwardDisplayLineEnd           ,


-- ** getAcceptsTab #method:getAcceptsTab#

#if defined(ENABLE_OVERLOADING)
    TextViewGetAcceptsTabMethodInfo         ,
#endif
    textViewGetAcceptsTab                   ,


-- ** getBorderWindowSize #method:getBorderWindowSize#

#if defined(ENABLE_OVERLOADING)
    TextViewGetBorderWindowSizeMethodInfo   ,
#endif
    textViewGetBorderWindowSize             ,


-- ** getBottomMargin #method:getBottomMargin#

#if defined(ENABLE_OVERLOADING)
    TextViewGetBottomMarginMethodInfo       ,
#endif
    textViewGetBottomMargin                 ,


-- ** getBuffer #method:getBuffer#

#if defined(ENABLE_OVERLOADING)
    TextViewGetBufferMethodInfo             ,
#endif
    textViewGetBuffer                       ,


-- ** getCursorLocations #method:getCursorLocations#

#if defined(ENABLE_OVERLOADING)
    TextViewGetCursorLocationsMethodInfo    ,
#endif
    textViewGetCursorLocations              ,


-- ** getCursorVisible #method:getCursorVisible#

#if defined(ENABLE_OVERLOADING)
    TextViewGetCursorVisibleMethodInfo      ,
#endif
    textViewGetCursorVisible                ,


-- ** getDefaultAttributes #method:getDefaultAttributes#

#if defined(ENABLE_OVERLOADING)
    TextViewGetDefaultAttributesMethodInfo  ,
#endif
    textViewGetDefaultAttributes            ,


-- ** getEditable #method:getEditable#

#if defined(ENABLE_OVERLOADING)
    TextViewGetEditableMethodInfo           ,
#endif
    textViewGetEditable                     ,


-- ** getHadjustment #method:getHadjustment#

#if defined(ENABLE_OVERLOADING)
    TextViewGetHadjustmentMethodInfo        ,
#endif
    textViewGetHadjustment                  ,


-- ** getIndent #method:getIndent#

#if defined(ENABLE_OVERLOADING)
    TextViewGetIndentMethodInfo             ,
#endif
    textViewGetIndent                       ,


-- ** getInputHints #method:getInputHints#

#if defined(ENABLE_OVERLOADING)
    TextViewGetInputHintsMethodInfo         ,
#endif
    textViewGetInputHints                   ,


-- ** getInputPurpose #method:getInputPurpose#

#if defined(ENABLE_OVERLOADING)
    TextViewGetInputPurposeMethodInfo       ,
#endif
    textViewGetInputPurpose                 ,


-- ** getIterAtLocation #method:getIterAtLocation#

#if defined(ENABLE_OVERLOADING)
    TextViewGetIterAtLocationMethodInfo     ,
#endif
    textViewGetIterAtLocation               ,


-- ** getIterAtPosition #method:getIterAtPosition#

#if defined(ENABLE_OVERLOADING)
    TextViewGetIterAtPositionMethodInfo     ,
#endif
    textViewGetIterAtPosition               ,


-- ** getIterLocation #method:getIterLocation#

#if defined(ENABLE_OVERLOADING)
    TextViewGetIterLocationMethodInfo       ,
#endif
    textViewGetIterLocation                 ,


-- ** getJustification #method:getJustification#

#if defined(ENABLE_OVERLOADING)
    TextViewGetJustificationMethodInfo      ,
#endif
    textViewGetJustification                ,


-- ** getLeftMargin #method:getLeftMargin#

#if defined(ENABLE_OVERLOADING)
    TextViewGetLeftMarginMethodInfo         ,
#endif
    textViewGetLeftMargin                   ,


-- ** getLineAtY #method:getLineAtY#

#if defined(ENABLE_OVERLOADING)
    TextViewGetLineAtYMethodInfo            ,
#endif
    textViewGetLineAtY                      ,


-- ** getLineYrange #method:getLineYrange#

#if defined(ENABLE_OVERLOADING)
    TextViewGetLineYrangeMethodInfo         ,
#endif
    textViewGetLineYrange                   ,


-- ** getMonospace #method:getMonospace#

#if defined(ENABLE_OVERLOADING)
    TextViewGetMonospaceMethodInfo          ,
#endif
    textViewGetMonospace                    ,


-- ** getOverwrite #method:getOverwrite#

#if defined(ENABLE_OVERLOADING)
    TextViewGetOverwriteMethodInfo          ,
#endif
    textViewGetOverwrite                    ,


-- ** getPixelsAboveLines #method:getPixelsAboveLines#

#if defined(ENABLE_OVERLOADING)
    TextViewGetPixelsAboveLinesMethodInfo   ,
#endif
    textViewGetPixelsAboveLines             ,


-- ** getPixelsBelowLines #method:getPixelsBelowLines#

#if defined(ENABLE_OVERLOADING)
    TextViewGetPixelsBelowLinesMethodInfo   ,
#endif
    textViewGetPixelsBelowLines             ,


-- ** getPixelsInsideWrap #method:getPixelsInsideWrap#

#if defined(ENABLE_OVERLOADING)
    TextViewGetPixelsInsideWrapMethodInfo   ,
#endif
    textViewGetPixelsInsideWrap             ,


-- ** getRightMargin #method:getRightMargin#

#if defined(ENABLE_OVERLOADING)
    TextViewGetRightMarginMethodInfo        ,
#endif
    textViewGetRightMargin                  ,


-- ** getTabs #method:getTabs#

#if defined(ENABLE_OVERLOADING)
    TextViewGetTabsMethodInfo               ,
#endif
    textViewGetTabs                         ,


-- ** getTopMargin #method:getTopMargin#

#if defined(ENABLE_OVERLOADING)
    TextViewGetTopMarginMethodInfo          ,
#endif
    textViewGetTopMargin                    ,


-- ** getVadjustment #method:getVadjustment#

#if defined(ENABLE_OVERLOADING)
    TextViewGetVadjustmentMethodInfo        ,
#endif
    textViewGetVadjustment                  ,


-- ** getVisibleRect #method:getVisibleRect#

#if defined(ENABLE_OVERLOADING)
    TextViewGetVisibleRectMethodInfo        ,
#endif
    textViewGetVisibleRect                  ,


-- ** getWindow #method:getWindow#

#if defined(ENABLE_OVERLOADING)
    TextViewGetWindowMethodInfo             ,
#endif
    textViewGetWindow                       ,


-- ** getWindowType #method:getWindowType#

#if defined(ENABLE_OVERLOADING)
    TextViewGetWindowTypeMethodInfo         ,
#endif
    textViewGetWindowType                   ,


-- ** getWrapMode #method:getWrapMode#

#if defined(ENABLE_OVERLOADING)
    TextViewGetWrapModeMethodInfo           ,
#endif
    textViewGetWrapMode                     ,


-- ** imContextFilterKeypress #method:imContextFilterKeypress#

#if defined(ENABLE_OVERLOADING)
    TextViewImContextFilterKeypressMethodInfo,
#endif
    textViewImContextFilterKeypress         ,


-- ** moveChild #method:moveChild#

#if defined(ENABLE_OVERLOADING)
    TextViewMoveChildMethodInfo             ,
#endif
    textViewMoveChild                       ,


-- ** moveMarkOnscreen #method:moveMarkOnscreen#

#if defined(ENABLE_OVERLOADING)
    TextViewMoveMarkOnscreenMethodInfo      ,
#endif
    textViewMoveMarkOnscreen                ,


-- ** moveVisually #method:moveVisually#

#if defined(ENABLE_OVERLOADING)
    TextViewMoveVisuallyMethodInfo          ,
#endif
    textViewMoveVisually                    ,


-- ** new #method:new#

    textViewNew                             ,


-- ** newWithBuffer #method:newWithBuffer#

    textViewNewWithBuffer                   ,


-- ** placeCursorOnscreen #method:placeCursorOnscreen#

#if defined(ENABLE_OVERLOADING)
    TextViewPlaceCursorOnscreenMethodInfo   ,
#endif
    textViewPlaceCursorOnscreen             ,


-- ** resetCursorBlink #method:resetCursorBlink#

#if defined(ENABLE_OVERLOADING)
    TextViewResetCursorBlinkMethodInfo      ,
#endif
    textViewResetCursorBlink                ,


-- ** resetImContext #method:resetImContext#

#if defined(ENABLE_OVERLOADING)
    TextViewResetImContextMethodInfo        ,
#endif
    textViewResetImContext                  ,


-- ** scrollMarkOnscreen #method:scrollMarkOnscreen#

#if defined(ENABLE_OVERLOADING)
    TextViewScrollMarkOnscreenMethodInfo    ,
#endif
    textViewScrollMarkOnscreen              ,


-- ** scrollToIter #method:scrollToIter#

#if defined(ENABLE_OVERLOADING)
    TextViewScrollToIterMethodInfo          ,
#endif
    textViewScrollToIter                    ,


-- ** scrollToMark #method:scrollToMark#

#if defined(ENABLE_OVERLOADING)
    TextViewScrollToMarkMethodInfo          ,
#endif
    textViewScrollToMark                    ,


-- ** setAcceptsTab #method:setAcceptsTab#

#if defined(ENABLE_OVERLOADING)
    TextViewSetAcceptsTabMethodInfo         ,
#endif
    textViewSetAcceptsTab                   ,


-- ** setBorderWindowSize #method:setBorderWindowSize#

#if defined(ENABLE_OVERLOADING)
    TextViewSetBorderWindowSizeMethodInfo   ,
#endif
    textViewSetBorderWindowSize             ,


-- ** setBottomMargin #method:setBottomMargin#

#if defined(ENABLE_OVERLOADING)
    TextViewSetBottomMarginMethodInfo       ,
#endif
    textViewSetBottomMargin                 ,


-- ** setBuffer #method:setBuffer#

#if defined(ENABLE_OVERLOADING)
    TextViewSetBufferMethodInfo             ,
#endif
    textViewSetBuffer                       ,


-- ** setCursorVisible #method:setCursorVisible#

#if defined(ENABLE_OVERLOADING)
    TextViewSetCursorVisibleMethodInfo      ,
#endif
    textViewSetCursorVisible                ,


-- ** setEditable #method:setEditable#

#if defined(ENABLE_OVERLOADING)
    TextViewSetEditableMethodInfo           ,
#endif
    textViewSetEditable                     ,


-- ** setIndent #method:setIndent#

#if defined(ENABLE_OVERLOADING)
    TextViewSetIndentMethodInfo             ,
#endif
    textViewSetIndent                       ,


-- ** setInputHints #method:setInputHints#

#if defined(ENABLE_OVERLOADING)
    TextViewSetInputHintsMethodInfo         ,
#endif
    textViewSetInputHints                   ,


-- ** setInputPurpose #method:setInputPurpose#

#if defined(ENABLE_OVERLOADING)
    TextViewSetInputPurposeMethodInfo       ,
#endif
    textViewSetInputPurpose                 ,


-- ** setJustification #method:setJustification#

#if defined(ENABLE_OVERLOADING)
    TextViewSetJustificationMethodInfo      ,
#endif
    textViewSetJustification                ,


-- ** setLeftMargin #method:setLeftMargin#

#if defined(ENABLE_OVERLOADING)
    TextViewSetLeftMarginMethodInfo         ,
#endif
    textViewSetLeftMargin                   ,


-- ** setMonospace #method:setMonospace#

#if defined(ENABLE_OVERLOADING)
    TextViewSetMonospaceMethodInfo          ,
#endif
    textViewSetMonospace                    ,


-- ** setOverwrite #method:setOverwrite#

#if defined(ENABLE_OVERLOADING)
    TextViewSetOverwriteMethodInfo          ,
#endif
    textViewSetOverwrite                    ,


-- ** setPixelsAboveLines #method:setPixelsAboveLines#

#if defined(ENABLE_OVERLOADING)
    TextViewSetPixelsAboveLinesMethodInfo   ,
#endif
    textViewSetPixelsAboveLines             ,


-- ** setPixelsBelowLines #method:setPixelsBelowLines#

#if defined(ENABLE_OVERLOADING)
    TextViewSetPixelsBelowLinesMethodInfo   ,
#endif
    textViewSetPixelsBelowLines             ,


-- ** setPixelsInsideWrap #method:setPixelsInsideWrap#

#if defined(ENABLE_OVERLOADING)
    TextViewSetPixelsInsideWrapMethodInfo   ,
#endif
    textViewSetPixelsInsideWrap             ,


-- ** setRightMargin #method:setRightMargin#

#if defined(ENABLE_OVERLOADING)
    TextViewSetRightMarginMethodInfo        ,
#endif
    textViewSetRightMargin                  ,


-- ** setTabs #method:setTabs#

#if defined(ENABLE_OVERLOADING)
    TextViewSetTabsMethodInfo               ,
#endif
    textViewSetTabs                         ,


-- ** setTopMargin #method:setTopMargin#

#if defined(ENABLE_OVERLOADING)
    TextViewSetTopMarginMethodInfo          ,
#endif
    textViewSetTopMargin                    ,


-- ** setWrapMode #method:setWrapMode#

#if defined(ENABLE_OVERLOADING)
    TextViewSetWrapModeMethodInfo           ,
#endif
    textViewSetWrapMode                     ,


-- ** startsDisplayLine #method:startsDisplayLine#

#if defined(ENABLE_OVERLOADING)
    TextViewStartsDisplayLineMethodInfo     ,
#endif
    textViewStartsDisplayLine               ,


-- ** windowToBufferCoords #method:windowToBufferCoords#

#if defined(ENABLE_OVERLOADING)
    TextViewWindowToBufferCoordsMethodInfo  ,
#endif
    textViewWindowToBufferCoords            ,




 -- * Properties


-- ** acceptsTab #attr:acceptsTab#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextViewAcceptsTabPropertyInfo          ,
#endif
    constructTextViewAcceptsTab             ,
    getTextViewAcceptsTab                   ,
    setTextViewAcceptsTab                   ,
#if defined(ENABLE_OVERLOADING)
    textViewAcceptsTab                      ,
#endif


-- ** bottomMargin #attr:bottomMargin#
-- | The bottom margin for text in the text view.
-- 
-- Note that this property is confusingly named. In CSS terms,
-- the value set here is padding, and it is applied in addition
-- to the padding from the theme.
-- 
-- Don\'t confuse this property with [Widget:marginBottom]("GI.Gtk.Objects.Widget#g:attr:marginBottom").
-- 
-- /Since: 3.18/

#if defined(ENABLE_OVERLOADING)
    TextViewBottomMarginPropertyInfo        ,
#endif
    constructTextViewBottomMargin           ,
    getTextViewBottomMargin                 ,
    setTextViewBottomMargin                 ,
#if defined(ENABLE_OVERLOADING)
    textViewBottomMargin                    ,
#endif


-- ** buffer #attr:buffer#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextViewBufferPropertyInfo              ,
#endif
    clearTextViewBuffer                     ,
    constructTextViewBuffer                 ,
    getTextViewBuffer                       ,
    setTextViewBuffer                       ,
#if defined(ENABLE_OVERLOADING)
    textViewBuffer                          ,
#endif


-- ** cursorVisible #attr:cursorVisible#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextViewCursorVisiblePropertyInfo       ,
#endif
    constructTextViewCursorVisible          ,
    getTextViewCursorVisible                ,
    setTextViewCursorVisible                ,
#if defined(ENABLE_OVERLOADING)
    textViewCursorVisible                   ,
#endif


-- ** editable #attr:editable#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextViewEditablePropertyInfo            ,
#endif
    constructTextViewEditable               ,
    getTextViewEditable                     ,
    setTextViewEditable                     ,
#if defined(ENABLE_OVERLOADING)
    textViewEditable                        ,
#endif


-- ** imModule #attr:imModule#
-- | Which IM (input method) module should be used for this text_view.
-- See t'GI.Gtk.Objects.IMContext.IMContext'.
-- 
-- Setting this to a non-'P.Nothing' value overrides the
-- system-wide IM module setting. See the GtkSettings
-- [Settings:gtkImModule]("GI.Gtk.Objects.Settings#g:attr:gtkImModule") property.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    TextViewImModulePropertyInfo            ,
#endif
    clearTextViewImModule                   ,
    constructTextViewImModule               ,
    getTextViewImModule                     ,
    setTextViewImModule                     ,
#if defined(ENABLE_OVERLOADING)
    textViewImModule                        ,
#endif


-- ** indent #attr:indent#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextViewIndentPropertyInfo              ,
#endif
    constructTextViewIndent                 ,
    getTextViewIndent                       ,
    setTextViewIndent                       ,
#if defined(ENABLE_OVERLOADING)
    textViewIndent                          ,
#endif


-- ** inputHints #attr:inputHints#
-- | Additional hints (beyond [TextView:inputPurpose]("GI.Gtk.Objects.TextView#g:attr:inputPurpose")) that
-- allow input methods to fine-tune their behaviour.
-- 
-- /Since: 3.6/

#if defined(ENABLE_OVERLOADING)
    TextViewInputHintsPropertyInfo          ,
#endif
    constructTextViewInputHints             ,
    getTextViewInputHints                   ,
    setTextViewInputHints                   ,
#if defined(ENABLE_OVERLOADING)
    textViewInputHints                      ,
#endif


-- ** inputPurpose #attr:inputPurpose#
-- | The purpose of this text field.
-- 
-- This property can be used by on-screen keyboards and other input
-- methods to adjust their behaviour.
-- 
-- /Since: 3.6/

#if defined(ENABLE_OVERLOADING)
    TextViewInputPurposePropertyInfo        ,
#endif
    constructTextViewInputPurpose           ,
    getTextViewInputPurpose                 ,
    setTextViewInputPurpose                 ,
#if defined(ENABLE_OVERLOADING)
    textViewInputPurpose                    ,
#endif


-- ** justification #attr:justification#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextViewJustificationPropertyInfo       ,
#endif
    constructTextViewJustification          ,
    getTextViewJustification                ,
    setTextViewJustification                ,
#if defined(ENABLE_OVERLOADING)
    textViewJustification                   ,
#endif


-- ** leftMargin #attr:leftMargin#
-- | The default left margin for text in the text view.
-- Tags in the buffer may override the default.
-- 
-- Note that this property is confusingly named. In CSS terms,
-- the value set here is padding, and it is applied in addition
-- to the padding from the theme.
-- 
-- Don\'t confuse this property with [Widget:marginLeft]("GI.Gtk.Objects.Widget#g:attr:marginLeft").

#if defined(ENABLE_OVERLOADING)
    TextViewLeftMarginPropertyInfo          ,
#endif
    constructTextViewLeftMargin             ,
    getTextViewLeftMargin                   ,
    setTextViewLeftMargin                   ,
#if defined(ENABLE_OVERLOADING)
    textViewLeftMargin                      ,
#endif


-- ** monospace #attr:monospace#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextViewMonospacePropertyInfo           ,
#endif
    constructTextViewMonospace              ,
    getTextViewMonospace                    ,
    setTextViewMonospace                    ,
#if defined(ENABLE_OVERLOADING)
    textViewMonospace                       ,
#endif


-- ** overwrite #attr:overwrite#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextViewOverwritePropertyInfo           ,
#endif
    constructTextViewOverwrite              ,
    getTextViewOverwrite                    ,
    setTextViewOverwrite                    ,
#if defined(ENABLE_OVERLOADING)
    textViewOverwrite                       ,
#endif


-- ** pixelsAboveLines #attr:pixelsAboveLines#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextViewPixelsAboveLinesPropertyInfo    ,
#endif
    constructTextViewPixelsAboveLines       ,
    getTextViewPixelsAboveLines             ,
    setTextViewPixelsAboveLines             ,
#if defined(ENABLE_OVERLOADING)
    textViewPixelsAboveLines                ,
#endif


-- ** pixelsBelowLines #attr:pixelsBelowLines#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextViewPixelsBelowLinesPropertyInfo    ,
#endif
    constructTextViewPixelsBelowLines       ,
    getTextViewPixelsBelowLines             ,
    setTextViewPixelsBelowLines             ,
#if defined(ENABLE_OVERLOADING)
    textViewPixelsBelowLines                ,
#endif


-- ** pixelsInsideWrap #attr:pixelsInsideWrap#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextViewPixelsInsideWrapPropertyInfo    ,
#endif
    constructTextViewPixelsInsideWrap       ,
    getTextViewPixelsInsideWrap             ,
    setTextViewPixelsInsideWrap             ,
#if defined(ENABLE_OVERLOADING)
    textViewPixelsInsideWrap                ,
#endif


-- ** populateAll #attr:populateAll#
-- | If :populate-all is 'P.True', the [TextView::populatePopup]("GI.Gtk.Objects.TextView#g:signal:populatePopup")
-- signal is also emitted for touch popups.
-- 
-- /Since: 3.8/

#if defined(ENABLE_OVERLOADING)
    TextViewPopulateAllPropertyInfo         ,
#endif
    constructTextViewPopulateAll            ,
    getTextViewPopulateAll                  ,
    setTextViewPopulateAll                  ,
#if defined(ENABLE_OVERLOADING)
    textViewPopulateAll                     ,
#endif


-- ** rightMargin #attr:rightMargin#
-- | The default right margin for text in the text view.
-- Tags in the buffer may override the default.
-- 
-- Note that this property is confusingly named. In CSS terms,
-- the value set here is padding, and it is applied in addition
-- to the padding from the theme.
-- 
-- Don\'t confuse this property with [Widget:marginRight]("GI.Gtk.Objects.Widget#g:attr:marginRight").

#if defined(ENABLE_OVERLOADING)
    TextViewRightMarginPropertyInfo         ,
#endif
    constructTextViewRightMargin            ,
    getTextViewRightMargin                  ,
    setTextViewRightMargin                  ,
#if defined(ENABLE_OVERLOADING)
    textViewRightMargin                     ,
#endif


-- ** tabs #attr:tabs#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextViewTabsPropertyInfo                ,
#endif
    constructTextViewTabs                   ,
    getTextViewTabs                         ,
    setTextViewTabs                         ,
#if defined(ENABLE_OVERLOADING)
    textViewTabs                            ,
#endif


-- ** topMargin #attr:topMargin#
-- | The top margin for text in the text view.
-- 
-- Note that this property is confusingly named. In CSS terms,
-- the value set here is padding, and it is applied in addition
-- to the padding from the theme.
-- 
-- Don\'t confuse this property with [Widget:marginTop]("GI.Gtk.Objects.Widget#g:attr:marginTop").
-- 
-- /Since: 3.18/

#if defined(ENABLE_OVERLOADING)
    TextViewTopMarginPropertyInfo           ,
#endif
    constructTextViewTopMargin              ,
    getTextViewTopMargin                    ,
    setTextViewTopMargin                    ,
#if defined(ENABLE_OVERLOADING)
    textViewTopMargin                       ,
#endif


-- ** wrapMode #attr:wrapMode#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TextViewWrapModePropertyInfo            ,
#endif
    constructTextViewWrapMode               ,
    getTextViewWrapMode                     ,
    setTextViewWrapMode                     ,
#if defined(ENABLE_OVERLOADING)
    textViewWrapMode                        ,
#endif




 -- * Signals


-- ** backspace #signal:backspace#

    TextViewBackspaceCallback               ,
#if defined(ENABLE_OVERLOADING)
    TextViewBackspaceSignalInfo             ,
#endif
    afterTextViewBackspace                  ,
    onTextViewBackspace                     ,


-- ** copyClipboard #signal:copyClipboard#

    TextViewCopyClipboardCallback           ,
#if defined(ENABLE_OVERLOADING)
    TextViewCopyClipboardSignalInfo         ,
#endif
    afterTextViewCopyClipboard              ,
    onTextViewCopyClipboard                 ,


-- ** cutClipboard #signal:cutClipboard#

    TextViewCutClipboardCallback            ,
#if defined(ENABLE_OVERLOADING)
    TextViewCutClipboardSignalInfo          ,
#endif
    afterTextViewCutClipboard               ,
    onTextViewCutClipboard                  ,


-- ** deleteFromCursor #signal:deleteFromCursor#

    TextViewDeleteFromCursorCallback        ,
#if defined(ENABLE_OVERLOADING)
    TextViewDeleteFromCursorSignalInfo      ,
#endif
    afterTextViewDeleteFromCursor           ,
    onTextViewDeleteFromCursor              ,


-- ** extendSelection #signal:extendSelection#

    TextViewExtendSelectionCallback         ,
#if defined(ENABLE_OVERLOADING)
    TextViewExtendSelectionSignalInfo       ,
#endif
    afterTextViewExtendSelection            ,
    onTextViewExtendSelection               ,


-- ** insertAtCursor #signal:insertAtCursor#

    TextViewInsertAtCursorCallback          ,
#if defined(ENABLE_OVERLOADING)
    TextViewInsertAtCursorSignalInfo        ,
#endif
    afterTextViewInsertAtCursor             ,
    onTextViewInsertAtCursor                ,


-- ** insertEmoji #signal:insertEmoji#

    TextViewInsertEmojiCallback             ,
#if defined(ENABLE_OVERLOADING)
    TextViewInsertEmojiSignalInfo           ,
#endif
    afterTextViewInsertEmoji                ,
    onTextViewInsertEmoji                   ,


-- ** moveCursor #signal:moveCursor#

    TextViewMoveCursorCallback              ,
#if defined(ENABLE_OVERLOADING)
    TextViewMoveCursorSignalInfo            ,
#endif
    afterTextViewMoveCursor                 ,
    onTextViewMoveCursor                    ,


-- ** moveViewport #signal:moveViewport#

    TextViewMoveViewportCallback            ,
#if defined(ENABLE_OVERLOADING)
    TextViewMoveViewportSignalInfo          ,
#endif
    afterTextViewMoveViewport               ,
    onTextViewMoveViewport                  ,


-- ** pasteClipboard #signal:pasteClipboard#

    TextViewPasteClipboardCallback          ,
#if defined(ENABLE_OVERLOADING)
    TextViewPasteClipboardSignalInfo        ,
#endif
    afterTextViewPasteClipboard             ,
    onTextViewPasteClipboard                ,


-- ** populatePopup #signal:populatePopup#

    TextViewPopulatePopupCallback           ,
#if defined(ENABLE_OVERLOADING)
    TextViewPopulatePopupSignalInfo         ,
#endif
    afterTextViewPopulatePopup              ,
    onTextViewPopulatePopup                 ,


-- ** preeditChanged #signal:preeditChanged#

    TextViewPreeditChangedCallback          ,
#if defined(ENABLE_OVERLOADING)
    TextViewPreeditChangedSignalInfo        ,
#endif
    afterTextViewPreeditChanged             ,
    onTextViewPreeditChanged                ,


-- ** selectAll #signal:selectAll#

    TextViewSelectAllCallback               ,
#if defined(ENABLE_OVERLOADING)
    TextViewSelectAllSignalInfo             ,
#endif
    afterTextViewSelectAll                  ,
    onTextViewSelectAll                     ,


-- ** setAnchor #signal:setAnchor#

    TextViewSetAnchorCallback               ,
#if defined(ENABLE_OVERLOADING)
    TextViewSetAnchorSignalInfo             ,
#endif
    afterTextViewSetAnchor                  ,
    onTextViewSetAnchor                     ,


-- ** toggleCursorVisible #signal:toggleCursorVisible#

    TextViewToggleCursorVisibleCallback     ,
#if defined(ENABLE_OVERLOADING)
    TextViewToggleCursorVisibleSignalInfo   ,
#endif
    afterTextViewToggleCursorVisible        ,
    onTextViewToggleCursorVisible           ,


-- ** toggleOverwrite #signal:toggleOverwrite#

    TextViewToggleOverwriteCallback         ,
#if defined(ENABLE_OVERLOADING)
    TextViewToggleOverwriteSignalInfo       ,
#endif
    afterTextViewToggleOverwrite            ,
    onTextViewToggleOverwrite               ,




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
import qualified GI.Gdk.Objects.Window as Gdk.Window
import qualified GI.Gdk.Structs.EventKey as Gdk.EventKey
import qualified GI.Gdk.Structs.Rectangle as Gdk.Rectangle
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Scrollable as Gtk.Scrollable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Adjustment as Gtk.Adjustment
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.TextBuffer as Gtk.TextBuffer
import {-# SOURCE #-} qualified GI.Gtk.Objects.TextChildAnchor as Gtk.TextChildAnchor
import {-# SOURCE #-} qualified GI.Gtk.Objects.TextMark as Gtk.TextMark
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Structs.TextAttributes as Gtk.TextAttributes
import {-# SOURCE #-} qualified GI.Gtk.Structs.TextIter as Gtk.TextIter
import qualified GI.Pango.Structs.TabArray as Pango.TabArray

-- | Memory-managed wrapper type.
newtype TextView = TextView (SP.ManagedPtr TextView)
    deriving (Eq)

instance SP.ManagedPtrNewtype TextView where
    toManagedPtr (TextView p) = p

foreign import ccall "gtk_text_view_get_type"
    c_gtk_text_view_get_type :: IO B.Types.GType

instance B.Types.TypedObject TextView where
    glibType = c_gtk_text_view_get_type

instance B.Types.GObject TextView

-- | Type class for types which can be safely cast to `TextView`, for instance with `toTextView`.
class (SP.GObject o, O.IsDescendantOf TextView o) => IsTextView o
instance (SP.GObject o, O.IsDescendantOf TextView o) => IsTextView o

instance O.HasParentTypes TextView
type instance O.ParentTypes TextView = '[Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable, Gtk.Scrollable.Scrollable]

-- | Cast to `TextView`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTextView :: (MIO.MonadIO m, IsTextView o) => o -> m TextView
toTextView = MIO.liftIO . B.ManagedPtr.unsafeCastTo TextView

-- | Convert 'TextView' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TextView) where
    gvalueGType_ = c_gtk_text_view_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr TextView)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr TextView)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject TextView ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveTextViewMethod (t :: Symbol) (o :: *) :: * where
    ResolveTextViewMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveTextViewMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveTextViewMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveTextViewMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveTextViewMethod "addChildAtAnchor" o = TextViewAddChildAtAnchorMethodInfo
    ResolveTextViewMethod "addChildInWindow" o = TextViewAddChildInWindowMethodInfo
    ResolveTextViewMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveTextViewMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveTextViewMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveTextViewMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveTextViewMethod "backwardDisplayLine" o = TextViewBackwardDisplayLineMethodInfo
    ResolveTextViewMethod "backwardDisplayLineStart" o = TextViewBackwardDisplayLineStartMethodInfo
    ResolveTextViewMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTextViewMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTextViewMethod "bufferToWindowCoords" o = TextViewBufferToWindowCoordsMethodInfo
    ResolveTextViewMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveTextViewMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveTextViewMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveTextViewMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveTextViewMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveTextViewMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveTextViewMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveTextViewMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveTextViewMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveTextViewMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveTextViewMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveTextViewMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveTextViewMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveTextViewMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveTextViewMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveTextViewMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveTextViewMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveTextViewMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveTextViewMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveTextViewMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveTextViewMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveTextViewMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveTextViewMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveTextViewMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveTextViewMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveTextViewMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveTextViewMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveTextViewMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveTextViewMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveTextViewMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveTextViewMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveTextViewMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveTextViewMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveTextViewMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveTextViewMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveTextViewMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveTextViewMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveTextViewMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveTextViewMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveTextViewMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveTextViewMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveTextViewMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveTextViewMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveTextViewMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveTextViewMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveTextViewMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveTextViewMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveTextViewMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveTextViewMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveTextViewMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveTextViewMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveTextViewMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveTextViewMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTextViewMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveTextViewMethod "forwardDisplayLine" o = TextViewForwardDisplayLineMethodInfo
    ResolveTextViewMethod "forwardDisplayLineEnd" o = TextViewForwardDisplayLineEndMethodInfo
    ResolveTextViewMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveTextViewMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTextViewMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTextViewMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveTextViewMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveTextViewMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveTextViewMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveTextViewMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveTextViewMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveTextViewMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveTextViewMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveTextViewMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveTextViewMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveTextViewMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveTextViewMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveTextViewMethod "imContextFilterKeypress" o = TextViewImContextFilterKeypressMethodInfo
    ResolveTextViewMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveTextViewMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveTextViewMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveTextViewMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveTextViewMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveTextViewMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveTextViewMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveTextViewMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveTextViewMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTextViewMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveTextViewMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveTextViewMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveTextViewMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveTextViewMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveTextViewMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveTextViewMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveTextViewMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveTextViewMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveTextViewMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveTextViewMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveTextViewMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveTextViewMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveTextViewMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveTextViewMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveTextViewMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveTextViewMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveTextViewMethod "moveChild" o = TextViewMoveChildMethodInfo
    ResolveTextViewMethod "moveMarkOnscreen" o = TextViewMoveMarkOnscreenMethodInfo
    ResolveTextViewMethod "moveVisually" o = TextViewMoveVisuallyMethodInfo
    ResolveTextViewMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTextViewMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTextViewMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveTextViewMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveTextViewMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveTextViewMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveTextViewMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveTextViewMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveTextViewMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveTextViewMethod "placeCursorOnscreen" o = TextViewPlaceCursorOnscreenMethodInfo
    ResolveTextViewMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveTextViewMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveTextViewMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveTextViewMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveTextViewMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveTextViewMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveTextViewMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveTextViewMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveTextViewMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveTextViewMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTextViewMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTextViewMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveTextViewMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveTextViewMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveTextViewMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveTextViewMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveTextViewMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveTextViewMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveTextViewMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveTextViewMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveTextViewMethod "resetCursorBlink" o = TextViewResetCursorBlinkMethodInfo
    ResolveTextViewMethod "resetImContext" o = TextViewResetImContextMethodInfo
    ResolveTextViewMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveTextViewMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveTextViewMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveTextViewMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTextViewMethod "scrollMarkOnscreen" o = TextViewScrollMarkOnscreenMethodInfo
    ResolveTextViewMethod "scrollToIter" o = TextViewScrollToIterMethodInfo
    ResolveTextViewMethod "scrollToMark" o = TextViewScrollToMarkMethodInfo
    ResolveTextViewMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveTextViewMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveTextViewMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveTextViewMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveTextViewMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveTextViewMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveTextViewMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveTextViewMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveTextViewMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveTextViewMethod "startsDisplayLine" o = TextViewStartsDisplayLineMethodInfo
    ResolveTextViewMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTextViewMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTextViewMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveTextViewMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveTextViewMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveTextViewMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTextViewMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveTextViewMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveTextViewMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveTextViewMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveTextViewMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveTextViewMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTextViewMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveTextViewMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveTextViewMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveTextViewMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTextViewMethod "windowToBufferCoords" o = TextViewWindowToBufferCoordsMethodInfo
    ResolveTextViewMethod "getAcceptsTab" o = TextViewGetAcceptsTabMethodInfo
    ResolveTextViewMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveTextViewMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveTextViewMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveTextViewMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveTextViewMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveTextViewMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveTextViewMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveTextViewMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveTextViewMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveTextViewMethod "getBorder" o = Gtk.Scrollable.ScrollableGetBorderMethodInfo
    ResolveTextViewMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveTextViewMethod "getBorderWindowSize" o = TextViewGetBorderWindowSizeMethodInfo
    ResolveTextViewMethod "getBottomMargin" o = TextViewGetBottomMarginMethodInfo
    ResolveTextViewMethod "getBuffer" o = TextViewGetBufferMethodInfo
    ResolveTextViewMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveTextViewMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveTextViewMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveTextViewMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveTextViewMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveTextViewMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveTextViewMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveTextViewMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveTextViewMethod "getCursorLocations" o = TextViewGetCursorLocationsMethodInfo
    ResolveTextViewMethod "getCursorVisible" o = TextViewGetCursorVisibleMethodInfo
    ResolveTextViewMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTextViewMethod "getDefaultAttributes" o = TextViewGetDefaultAttributesMethodInfo
    ResolveTextViewMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveTextViewMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveTextViewMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveTextViewMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveTextViewMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveTextViewMethod "getEditable" o = TextViewGetEditableMethodInfo
    ResolveTextViewMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveTextViewMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveTextViewMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveTextViewMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveTextViewMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveTextViewMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveTextViewMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveTextViewMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveTextViewMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveTextViewMethod "getHadjustment" o = TextViewGetHadjustmentMethodInfo
    ResolveTextViewMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveTextViewMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveTextViewMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveTextViewMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveTextViewMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveTextViewMethod "getHscrollPolicy" o = Gtk.Scrollable.ScrollableGetHscrollPolicyMethodInfo
    ResolveTextViewMethod "getIndent" o = TextViewGetIndentMethodInfo
    ResolveTextViewMethod "getInputHints" o = TextViewGetInputHintsMethodInfo
    ResolveTextViewMethod "getInputPurpose" o = TextViewGetInputPurposeMethodInfo
    ResolveTextViewMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveTextViewMethod "getIterAtLocation" o = TextViewGetIterAtLocationMethodInfo
    ResolveTextViewMethod "getIterAtPosition" o = TextViewGetIterAtPositionMethodInfo
    ResolveTextViewMethod "getIterLocation" o = TextViewGetIterLocationMethodInfo
    ResolveTextViewMethod "getJustification" o = TextViewGetJustificationMethodInfo
    ResolveTextViewMethod "getLeftMargin" o = TextViewGetLeftMarginMethodInfo
    ResolveTextViewMethod "getLineAtY" o = TextViewGetLineAtYMethodInfo
    ResolveTextViewMethod "getLineYrange" o = TextViewGetLineYrangeMethodInfo
    ResolveTextViewMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveTextViewMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveTextViewMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveTextViewMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveTextViewMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveTextViewMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveTextViewMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveTextViewMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveTextViewMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveTextViewMethod "getMonospace" o = TextViewGetMonospaceMethodInfo
    ResolveTextViewMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveTextViewMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveTextViewMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveTextViewMethod "getOverwrite" o = TextViewGetOverwriteMethodInfo
    ResolveTextViewMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveTextViewMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveTextViewMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveTextViewMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveTextViewMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveTextViewMethod "getPixelsAboveLines" o = TextViewGetPixelsAboveLinesMethodInfo
    ResolveTextViewMethod "getPixelsBelowLines" o = TextViewGetPixelsBelowLinesMethodInfo
    ResolveTextViewMethod "getPixelsInsideWrap" o = TextViewGetPixelsInsideWrapMethodInfo
    ResolveTextViewMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveTextViewMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveTextViewMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveTextViewMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveTextViewMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveTextViewMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveTextViewMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveTextViewMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTextViewMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTextViewMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveTextViewMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveTextViewMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveTextViewMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveTextViewMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveTextViewMethod "getRightMargin" o = TextViewGetRightMarginMethodInfo
    ResolveTextViewMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveTextViewMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveTextViewMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveTextViewMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveTextViewMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveTextViewMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveTextViewMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveTextViewMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveTextViewMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveTextViewMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveTextViewMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveTextViewMethod "getTabs" o = TextViewGetTabsMethodInfo
    ResolveTextViewMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveTextViewMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveTextViewMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveTextViewMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveTextViewMethod "getTopMargin" o = TextViewGetTopMarginMethodInfo
    ResolveTextViewMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveTextViewMethod "getVadjustment" o = TextViewGetVadjustmentMethodInfo
    ResolveTextViewMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveTextViewMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveTextViewMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveTextViewMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveTextViewMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveTextViewMethod "getVisibleRect" o = TextViewGetVisibleRectMethodInfo
    ResolveTextViewMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveTextViewMethod "getVscrollPolicy" o = Gtk.Scrollable.ScrollableGetVscrollPolicyMethodInfo
    ResolveTextViewMethod "getWindow" o = TextViewGetWindowMethodInfo
    ResolveTextViewMethod "getWindowType" o = TextViewGetWindowTypeMethodInfo
    ResolveTextViewMethod "getWrapMode" o = TextViewGetWrapModeMethodInfo
    ResolveTextViewMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveTextViewMethod "setAcceptsTab" o = TextViewSetAcceptsTabMethodInfo
    ResolveTextViewMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveTextViewMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveTextViewMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveTextViewMethod "setBorderWindowSize" o = TextViewSetBorderWindowSizeMethodInfo
    ResolveTextViewMethod "setBottomMargin" o = TextViewSetBottomMarginMethodInfo
    ResolveTextViewMethod "setBuffer" o = TextViewSetBufferMethodInfo
    ResolveTextViewMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveTextViewMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveTextViewMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveTextViewMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveTextViewMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveTextViewMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveTextViewMethod "setCursorVisible" o = TextViewSetCursorVisibleMethodInfo
    ResolveTextViewMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTextViewMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTextViewMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveTextViewMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveTextViewMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveTextViewMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveTextViewMethod "setEditable" o = TextViewSetEditableMethodInfo
    ResolveTextViewMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveTextViewMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveTextViewMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveTextViewMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveTextViewMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveTextViewMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveTextViewMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveTextViewMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveTextViewMethod "setHadjustment" o = Gtk.Scrollable.ScrollableSetHadjustmentMethodInfo
    ResolveTextViewMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveTextViewMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveTextViewMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveTextViewMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveTextViewMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveTextViewMethod "setHscrollPolicy" o = Gtk.Scrollable.ScrollableSetHscrollPolicyMethodInfo
    ResolveTextViewMethod "setIndent" o = TextViewSetIndentMethodInfo
    ResolveTextViewMethod "setInputHints" o = TextViewSetInputHintsMethodInfo
    ResolveTextViewMethod "setInputPurpose" o = TextViewSetInputPurposeMethodInfo
    ResolveTextViewMethod "setJustification" o = TextViewSetJustificationMethodInfo
    ResolveTextViewMethod "setLeftMargin" o = TextViewSetLeftMarginMethodInfo
    ResolveTextViewMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveTextViewMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveTextViewMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveTextViewMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveTextViewMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveTextViewMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveTextViewMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveTextViewMethod "setMonospace" o = TextViewSetMonospaceMethodInfo
    ResolveTextViewMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveTextViewMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveTextViewMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveTextViewMethod "setOverwrite" o = TextViewSetOverwriteMethodInfo
    ResolveTextViewMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveTextViewMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveTextViewMethod "setPixelsAboveLines" o = TextViewSetPixelsAboveLinesMethodInfo
    ResolveTextViewMethod "setPixelsBelowLines" o = TextViewSetPixelsBelowLinesMethodInfo
    ResolveTextViewMethod "setPixelsInsideWrap" o = TextViewSetPixelsInsideWrapMethodInfo
    ResolveTextViewMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTextViewMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveTextViewMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveTextViewMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveTextViewMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveTextViewMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveTextViewMethod "setRightMargin" o = TextViewSetRightMarginMethodInfo
    ResolveTextViewMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveTextViewMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveTextViewMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveTextViewMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveTextViewMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveTextViewMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveTextViewMethod "setTabs" o = TextViewSetTabsMethodInfo
    ResolveTextViewMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveTextViewMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveTextViewMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveTextViewMethod "setTopMargin" o = TextViewSetTopMarginMethodInfo
    ResolveTextViewMethod "setVadjustment" o = Gtk.Scrollable.ScrollableSetVadjustmentMethodInfo
    ResolveTextViewMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveTextViewMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveTextViewMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveTextViewMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveTextViewMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveTextViewMethod "setVscrollPolicy" o = Gtk.Scrollable.ScrollableSetVscrollPolicyMethodInfo
    ResolveTextViewMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveTextViewMethod "setWrapMode" o = TextViewSetWrapModeMethodInfo
    ResolveTextViewMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTextViewMethod t TextView, O.OverloadedMethod info TextView p) => OL.IsLabel t (TextView -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTextViewMethod t TextView, O.OverloadedMethod info TextView p, R.HasField t TextView p) => R.HasField t TextView p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTextViewMethod t TextView, O.OverloadedMethodInfo info TextView) => OL.IsLabel t (O.MethodProxy info TextView) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal TextView::backspace
-- | The [backspace](#g:signal:backspace) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user asks for it.
-- 
-- The default bindings for this signal are
-- Backspace and Shift-Backspace.
type TextViewBackspaceCallback =
    IO ()

type C_TextViewBackspaceCallback =
    Ptr TextView ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextViewBackspaceCallback`.
foreign import ccall "wrapper"
    mk_TextViewBackspaceCallback :: C_TextViewBackspaceCallback -> IO (FunPtr C_TextViewBackspaceCallback)

wrap_TextViewBackspaceCallback :: 
    GObject a => (a -> TextViewBackspaceCallback) ->
    C_TextViewBackspaceCallback
wrap_TextViewBackspaceCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [backspace](#signal:backspace) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textView #backspace callback
-- @
-- 
-- 
onTextViewBackspace :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewBackspaceCallback) -> m SignalHandlerId
onTextViewBackspace obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewBackspaceCallback wrapped
    wrapped'' <- mk_TextViewBackspaceCallback wrapped'
    connectSignalFunPtr obj "backspace" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [backspace](#signal:backspace) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textView #backspace callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextViewBackspace :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewBackspaceCallback) -> m SignalHandlerId
afterTextViewBackspace obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewBackspaceCallback wrapped
    wrapped'' <- mk_TextViewBackspaceCallback wrapped'
    connectSignalFunPtr obj "backspace" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextViewBackspaceSignalInfo
instance SignalInfo TextViewBackspaceSignalInfo where
    type HaskellCallbackType TextViewBackspaceSignalInfo = TextViewBackspaceCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextViewBackspaceCallback cb
        cb'' <- mk_TextViewBackspaceCallback cb'
        connectSignalFunPtr obj "backspace" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView::backspace"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:signal:backspace"})

#endif

-- signal TextView::copy-clipboard
-- | The [copyClipboard](#g:signal:copyClipboard) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to copy the selection to the clipboard.
-- 
-- The default bindings for this signal are
-- Ctrl-c and Ctrl-Insert.
type TextViewCopyClipboardCallback =
    IO ()

type C_TextViewCopyClipboardCallback =
    Ptr TextView ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextViewCopyClipboardCallback`.
foreign import ccall "wrapper"
    mk_TextViewCopyClipboardCallback :: C_TextViewCopyClipboardCallback -> IO (FunPtr C_TextViewCopyClipboardCallback)

wrap_TextViewCopyClipboardCallback :: 
    GObject a => (a -> TextViewCopyClipboardCallback) ->
    C_TextViewCopyClipboardCallback
wrap_TextViewCopyClipboardCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [copyClipboard](#signal:copyClipboard) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textView #copyClipboard callback
-- @
-- 
-- 
onTextViewCopyClipboard :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewCopyClipboardCallback) -> m SignalHandlerId
onTextViewCopyClipboard obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewCopyClipboardCallback wrapped
    wrapped'' <- mk_TextViewCopyClipboardCallback wrapped'
    connectSignalFunPtr obj "copy-clipboard" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [copyClipboard](#signal:copyClipboard) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textView #copyClipboard callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextViewCopyClipboard :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewCopyClipboardCallback) -> m SignalHandlerId
afterTextViewCopyClipboard obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewCopyClipboardCallback wrapped
    wrapped'' <- mk_TextViewCopyClipboardCallback wrapped'
    connectSignalFunPtr obj "copy-clipboard" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextViewCopyClipboardSignalInfo
instance SignalInfo TextViewCopyClipboardSignalInfo where
    type HaskellCallbackType TextViewCopyClipboardSignalInfo = TextViewCopyClipboardCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextViewCopyClipboardCallback cb
        cb'' <- mk_TextViewCopyClipboardCallback cb'
        connectSignalFunPtr obj "copy-clipboard" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView::copy-clipboard"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:signal:copyClipboard"})

#endif

-- signal TextView::cut-clipboard
-- | The [cutClipboard](#g:signal:cutClipboard) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to cut the selection to the clipboard.
-- 
-- The default bindings for this signal are
-- Ctrl-x and Shift-Delete.
type TextViewCutClipboardCallback =
    IO ()

type C_TextViewCutClipboardCallback =
    Ptr TextView ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextViewCutClipboardCallback`.
foreign import ccall "wrapper"
    mk_TextViewCutClipboardCallback :: C_TextViewCutClipboardCallback -> IO (FunPtr C_TextViewCutClipboardCallback)

wrap_TextViewCutClipboardCallback :: 
    GObject a => (a -> TextViewCutClipboardCallback) ->
    C_TextViewCutClipboardCallback
wrap_TextViewCutClipboardCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [cutClipboard](#signal:cutClipboard) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textView #cutClipboard callback
-- @
-- 
-- 
onTextViewCutClipboard :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewCutClipboardCallback) -> m SignalHandlerId
onTextViewCutClipboard obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewCutClipboardCallback wrapped
    wrapped'' <- mk_TextViewCutClipboardCallback wrapped'
    connectSignalFunPtr obj "cut-clipboard" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [cutClipboard](#signal:cutClipboard) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textView #cutClipboard callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextViewCutClipboard :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewCutClipboardCallback) -> m SignalHandlerId
afterTextViewCutClipboard obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewCutClipboardCallback wrapped
    wrapped'' <- mk_TextViewCutClipboardCallback wrapped'
    connectSignalFunPtr obj "cut-clipboard" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextViewCutClipboardSignalInfo
instance SignalInfo TextViewCutClipboardSignalInfo where
    type HaskellCallbackType TextViewCutClipboardSignalInfo = TextViewCutClipboardCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextViewCutClipboardCallback cb
        cb'' <- mk_TextViewCutClipboardCallback cb'
        connectSignalFunPtr obj "cut-clipboard" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView::cut-clipboard"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:signal:cutClipboard"})

#endif

-- signal TextView::delete-from-cursor
-- | The [deleteFromCursor](#g:signal:deleteFromCursor) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user initiates a text deletion.
-- 
-- If the /@type@/ is 'GI.Gtk.Enums.DeleteTypeChars', GTK+ deletes the selection
-- if there is one, otherwise it deletes the requested number
-- of characters.
-- 
-- The default bindings for this signal are
-- Delete for deleting a character, Ctrl-Delete for
-- deleting a word and Ctrl-Backspace for deleting a word
-- backwords.
type TextViewDeleteFromCursorCallback =
    Gtk.Enums.DeleteType
    -- ^ /@type@/: the granularity of the deletion, as a t'GI.Gtk.Enums.DeleteType'
    -> Int32
    -- ^ /@count@/: the number of /@type@/ units to delete
    -> IO ()

type C_TextViewDeleteFromCursorCallback =
    Ptr TextView ->                         -- object
    CUInt ->
    Int32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextViewDeleteFromCursorCallback`.
foreign import ccall "wrapper"
    mk_TextViewDeleteFromCursorCallback :: C_TextViewDeleteFromCursorCallback -> IO (FunPtr C_TextViewDeleteFromCursorCallback)

wrap_TextViewDeleteFromCursorCallback :: 
    GObject a => (a -> TextViewDeleteFromCursorCallback) ->
    C_TextViewDeleteFromCursorCallback
wrap_TextViewDeleteFromCursorCallback gi'cb gi'selfPtr type_ count _ = do
    let type_' = (toEnum . fromIntegral) type_
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  type_' count


-- | Connect a signal handler for the [deleteFromCursor](#signal:deleteFromCursor) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textView #deleteFromCursor callback
-- @
-- 
-- 
onTextViewDeleteFromCursor :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewDeleteFromCursorCallback) -> m SignalHandlerId
onTextViewDeleteFromCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewDeleteFromCursorCallback wrapped
    wrapped'' <- mk_TextViewDeleteFromCursorCallback wrapped'
    connectSignalFunPtr obj "delete-from-cursor" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [deleteFromCursor](#signal:deleteFromCursor) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textView #deleteFromCursor callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextViewDeleteFromCursor :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewDeleteFromCursorCallback) -> m SignalHandlerId
afterTextViewDeleteFromCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewDeleteFromCursorCallback wrapped
    wrapped'' <- mk_TextViewDeleteFromCursorCallback wrapped'
    connectSignalFunPtr obj "delete-from-cursor" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextViewDeleteFromCursorSignalInfo
instance SignalInfo TextViewDeleteFromCursorSignalInfo where
    type HaskellCallbackType TextViewDeleteFromCursorSignalInfo = TextViewDeleteFromCursorCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextViewDeleteFromCursorCallback cb
        cb'' <- mk_TextViewDeleteFromCursorCallback cb'
        connectSignalFunPtr obj "delete-from-cursor" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView::delete-from-cursor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:signal:deleteFromCursor"})

#endif

-- signal TextView::extend-selection
-- | The [extendSelection](#g:signal:extendSelection) signal is emitted when the selection needs to be
-- extended at /@location@/.
-- 
-- /Since: 3.16/
type TextViewExtendSelectionCallback =
    Gtk.Enums.TextExtendSelection
    -- ^ /@granularity@/: the granularity type
    -> Gtk.TextIter.TextIter
    -- ^ /@location@/: the location where to extend the selection
    -> Gtk.TextIter.TextIter
    -- ^ /@start@/: where the selection should start
    -> Gtk.TextIter.TextIter
    -- ^ /@end@/: where the selection should end
    -> IO Bool
    -- ^ __Returns:__ 'GI.Gdk.Constants.EVENT_STOP' to stop other handlers from being invoked for the
    --   event. 'GI.Gdk.Constants.EVENT_PROPAGATE' to propagate the event further.

type C_TextViewExtendSelectionCallback =
    Ptr TextView ->                         -- object
    CUInt ->
    Ptr Gtk.TextIter.TextIter ->
    Ptr Gtk.TextIter.TextIter ->
    Ptr Gtk.TextIter.TextIter ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_TextViewExtendSelectionCallback`.
foreign import ccall "wrapper"
    mk_TextViewExtendSelectionCallback :: C_TextViewExtendSelectionCallback -> IO (FunPtr C_TextViewExtendSelectionCallback)

wrap_TextViewExtendSelectionCallback :: 
    GObject a => (a -> TextViewExtendSelectionCallback) ->
    C_TextViewExtendSelectionCallback
wrap_TextViewExtendSelectionCallback gi'cb gi'selfPtr granularity location start end _ = do
    let granularity' = (toEnum . fromIntegral) granularity
    B.ManagedPtr.withTransient  location $ \location' -> do
        B.ManagedPtr.withTransient  start $ \start' -> do
            B.ManagedPtr.withTransient  end $ \end' -> do
                result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  granularity' location' start' end'
                let result' = (fromIntegral . fromEnum) result
                return result'


-- | Connect a signal handler for the [extendSelection](#signal:extendSelection) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textView #extendSelection callback
-- @
-- 
-- 
onTextViewExtendSelection :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewExtendSelectionCallback) -> m SignalHandlerId
onTextViewExtendSelection obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewExtendSelectionCallback wrapped
    wrapped'' <- mk_TextViewExtendSelectionCallback wrapped'
    connectSignalFunPtr obj "extend-selection" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [extendSelection](#signal:extendSelection) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textView #extendSelection callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextViewExtendSelection :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewExtendSelectionCallback) -> m SignalHandlerId
afterTextViewExtendSelection obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewExtendSelectionCallback wrapped
    wrapped'' <- mk_TextViewExtendSelectionCallback wrapped'
    connectSignalFunPtr obj "extend-selection" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextViewExtendSelectionSignalInfo
instance SignalInfo TextViewExtendSelectionSignalInfo where
    type HaskellCallbackType TextViewExtendSelectionSignalInfo = TextViewExtendSelectionCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextViewExtendSelectionCallback cb
        cb'' <- mk_TextViewExtendSelectionCallback cb'
        connectSignalFunPtr obj "extend-selection" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView::extend-selection"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:signal:extendSelection"})

#endif

-- signal TextView::insert-at-cursor
-- | The [insertAtCursor](#g:signal:insertAtCursor) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user initiates the insertion of a
-- fixed string at the cursor.
-- 
-- This signal has no default bindings.
type TextViewInsertAtCursorCallback =
    T.Text
    -- ^ /@string@/: the string to insert
    -> IO ()

type C_TextViewInsertAtCursorCallback =
    Ptr TextView ->                         -- object
    CString ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextViewInsertAtCursorCallback`.
foreign import ccall "wrapper"
    mk_TextViewInsertAtCursorCallback :: C_TextViewInsertAtCursorCallback -> IO (FunPtr C_TextViewInsertAtCursorCallback)

wrap_TextViewInsertAtCursorCallback :: 
    GObject a => (a -> TextViewInsertAtCursorCallback) ->
    C_TextViewInsertAtCursorCallback
wrap_TextViewInsertAtCursorCallback gi'cb gi'selfPtr string _ = do
    string' <- cstringToText string
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  string'


-- | Connect a signal handler for the [insertAtCursor](#signal:insertAtCursor) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textView #insertAtCursor callback
-- @
-- 
-- 
onTextViewInsertAtCursor :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewInsertAtCursorCallback) -> m SignalHandlerId
onTextViewInsertAtCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewInsertAtCursorCallback wrapped
    wrapped'' <- mk_TextViewInsertAtCursorCallback wrapped'
    connectSignalFunPtr obj "insert-at-cursor" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [insertAtCursor](#signal:insertAtCursor) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textView #insertAtCursor callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextViewInsertAtCursor :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewInsertAtCursorCallback) -> m SignalHandlerId
afterTextViewInsertAtCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewInsertAtCursorCallback wrapped
    wrapped'' <- mk_TextViewInsertAtCursorCallback wrapped'
    connectSignalFunPtr obj "insert-at-cursor" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextViewInsertAtCursorSignalInfo
instance SignalInfo TextViewInsertAtCursorSignalInfo where
    type HaskellCallbackType TextViewInsertAtCursorSignalInfo = TextViewInsertAtCursorCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextViewInsertAtCursorCallback cb
        cb'' <- mk_TextViewInsertAtCursorCallback cb'
        connectSignalFunPtr obj "insert-at-cursor" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView::insert-at-cursor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:signal:insertAtCursor"})

#endif

-- signal TextView::insert-emoji
-- | The [insertEmoji](#g:signal:insertEmoji) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to present the Emoji chooser for the /@textView@/.
-- 
-- The default bindings for this signal are Ctrl-. and Ctrl-;
-- 
-- /Since: 3.22.27/
type TextViewInsertEmojiCallback =
    IO ()

type C_TextViewInsertEmojiCallback =
    Ptr TextView ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextViewInsertEmojiCallback`.
foreign import ccall "wrapper"
    mk_TextViewInsertEmojiCallback :: C_TextViewInsertEmojiCallback -> IO (FunPtr C_TextViewInsertEmojiCallback)

wrap_TextViewInsertEmojiCallback :: 
    GObject a => (a -> TextViewInsertEmojiCallback) ->
    C_TextViewInsertEmojiCallback
wrap_TextViewInsertEmojiCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [insertEmoji](#signal:insertEmoji) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textView #insertEmoji callback
-- @
-- 
-- 
onTextViewInsertEmoji :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewInsertEmojiCallback) -> m SignalHandlerId
onTextViewInsertEmoji obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewInsertEmojiCallback wrapped
    wrapped'' <- mk_TextViewInsertEmojiCallback wrapped'
    connectSignalFunPtr obj "insert-emoji" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [insertEmoji](#signal:insertEmoji) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textView #insertEmoji callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextViewInsertEmoji :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewInsertEmojiCallback) -> m SignalHandlerId
afterTextViewInsertEmoji obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewInsertEmojiCallback wrapped
    wrapped'' <- mk_TextViewInsertEmojiCallback wrapped'
    connectSignalFunPtr obj "insert-emoji" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextViewInsertEmojiSignalInfo
instance SignalInfo TextViewInsertEmojiSignalInfo where
    type HaskellCallbackType TextViewInsertEmojiSignalInfo = TextViewInsertEmojiCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextViewInsertEmojiCallback cb
        cb'' <- mk_TextViewInsertEmojiCallback cb'
        connectSignalFunPtr obj "insert-emoji" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView::insert-emoji"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:signal:insertEmoji"})

#endif

-- signal TextView::move-cursor
-- | The [moveCursor](#g:signal:moveCursor) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user initiates a cursor movement.
-- If the cursor is not visible in /@textView@/, this signal causes
-- the viewport to be moved instead.
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
-- * Arrow keys move by individual characters\/lines
-- * Ctrl-arrow key combinations move by words\/paragraphs
-- * Home\/End keys move to the ends of the buffer
-- * PageUp\/PageDown keys move vertically by pages
-- * Ctrl-PageUp\/PageDown keys move horizontally by pages
type TextViewMoveCursorCallback =
    Gtk.Enums.MovementStep
    -- ^ /@step@/: the granularity of the move, as a t'GI.Gtk.Enums.MovementStep'
    -> Int32
    -- ^ /@count@/: the number of /@step@/ units to move
    -> Bool
    -- ^ /@extendSelection@/: 'P.True' if the move should extend the selection
    -> IO ()

type C_TextViewMoveCursorCallback =
    Ptr TextView ->                         -- object
    CUInt ->
    Int32 ->
    CInt ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextViewMoveCursorCallback`.
foreign import ccall "wrapper"
    mk_TextViewMoveCursorCallback :: C_TextViewMoveCursorCallback -> IO (FunPtr C_TextViewMoveCursorCallback)

wrap_TextViewMoveCursorCallback :: 
    GObject a => (a -> TextViewMoveCursorCallback) ->
    C_TextViewMoveCursorCallback
wrap_TextViewMoveCursorCallback gi'cb gi'selfPtr step count extendSelection _ = do
    let step' = (toEnum . fromIntegral) step
    let extendSelection' = (/= 0) extendSelection
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  step' count extendSelection'


-- | Connect a signal handler for the [moveCursor](#signal:moveCursor) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textView #moveCursor callback
-- @
-- 
-- 
onTextViewMoveCursor :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewMoveCursorCallback) -> m SignalHandlerId
onTextViewMoveCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewMoveCursorCallback wrapped
    wrapped'' <- mk_TextViewMoveCursorCallback wrapped'
    connectSignalFunPtr obj "move-cursor" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [moveCursor](#signal:moveCursor) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textView #moveCursor callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextViewMoveCursor :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewMoveCursorCallback) -> m SignalHandlerId
afterTextViewMoveCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewMoveCursorCallback wrapped
    wrapped'' <- mk_TextViewMoveCursorCallback wrapped'
    connectSignalFunPtr obj "move-cursor" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextViewMoveCursorSignalInfo
instance SignalInfo TextViewMoveCursorSignalInfo where
    type HaskellCallbackType TextViewMoveCursorSignalInfo = TextViewMoveCursorCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextViewMoveCursorCallback cb
        cb'' <- mk_TextViewMoveCursorCallback cb'
        connectSignalFunPtr obj "move-cursor" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView::move-cursor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:signal:moveCursor"})

#endif

-- signal TextView::move-viewport
-- | The [moveViewport](#g:signal:moveViewport) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which can be bound to key combinations to allow the user
-- to move the viewport, i.e. change what part of the text view
-- is visible in a containing scrolled window.
-- 
-- There are no default bindings for this signal.
type TextViewMoveViewportCallback =
    Gtk.Enums.ScrollStep
    -- ^ /@step@/: the granularity of the movement, as a t'GI.Gtk.Enums.ScrollStep'
    -> Int32
    -- ^ /@count@/: the number of /@step@/ units to move
    -> IO ()

type C_TextViewMoveViewportCallback =
    Ptr TextView ->                         -- object
    CUInt ->
    Int32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextViewMoveViewportCallback`.
foreign import ccall "wrapper"
    mk_TextViewMoveViewportCallback :: C_TextViewMoveViewportCallback -> IO (FunPtr C_TextViewMoveViewportCallback)

wrap_TextViewMoveViewportCallback :: 
    GObject a => (a -> TextViewMoveViewportCallback) ->
    C_TextViewMoveViewportCallback
wrap_TextViewMoveViewportCallback gi'cb gi'selfPtr step count _ = do
    let step' = (toEnum . fromIntegral) step
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  step' count


-- | Connect a signal handler for the [moveViewport](#signal:moveViewport) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textView #moveViewport callback
-- @
-- 
-- 
onTextViewMoveViewport :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewMoveViewportCallback) -> m SignalHandlerId
onTextViewMoveViewport obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewMoveViewportCallback wrapped
    wrapped'' <- mk_TextViewMoveViewportCallback wrapped'
    connectSignalFunPtr obj "move-viewport" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [moveViewport](#signal:moveViewport) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textView #moveViewport callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextViewMoveViewport :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewMoveViewportCallback) -> m SignalHandlerId
afterTextViewMoveViewport obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewMoveViewportCallback wrapped
    wrapped'' <- mk_TextViewMoveViewportCallback wrapped'
    connectSignalFunPtr obj "move-viewport" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextViewMoveViewportSignalInfo
instance SignalInfo TextViewMoveViewportSignalInfo where
    type HaskellCallbackType TextViewMoveViewportSignalInfo = TextViewMoveViewportCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextViewMoveViewportCallback cb
        cb'' <- mk_TextViewMoveViewportCallback cb'
        connectSignalFunPtr obj "move-viewport" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView::move-viewport"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:signal:moveViewport"})

#endif

-- signal TextView::paste-clipboard
-- | The [pasteClipboard](#g:signal:pasteClipboard) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to paste the contents of the clipboard
-- into the text view.
-- 
-- The default bindings for this signal are
-- Ctrl-v and Shift-Insert.
type TextViewPasteClipboardCallback =
    IO ()

type C_TextViewPasteClipboardCallback =
    Ptr TextView ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextViewPasteClipboardCallback`.
foreign import ccall "wrapper"
    mk_TextViewPasteClipboardCallback :: C_TextViewPasteClipboardCallback -> IO (FunPtr C_TextViewPasteClipboardCallback)

wrap_TextViewPasteClipboardCallback :: 
    GObject a => (a -> TextViewPasteClipboardCallback) ->
    C_TextViewPasteClipboardCallback
wrap_TextViewPasteClipboardCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [pasteClipboard](#signal:pasteClipboard) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textView #pasteClipboard callback
-- @
-- 
-- 
onTextViewPasteClipboard :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewPasteClipboardCallback) -> m SignalHandlerId
onTextViewPasteClipboard obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewPasteClipboardCallback wrapped
    wrapped'' <- mk_TextViewPasteClipboardCallback wrapped'
    connectSignalFunPtr obj "paste-clipboard" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [pasteClipboard](#signal:pasteClipboard) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textView #pasteClipboard callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextViewPasteClipboard :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewPasteClipboardCallback) -> m SignalHandlerId
afterTextViewPasteClipboard obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewPasteClipboardCallback wrapped
    wrapped'' <- mk_TextViewPasteClipboardCallback wrapped'
    connectSignalFunPtr obj "paste-clipboard" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextViewPasteClipboardSignalInfo
instance SignalInfo TextViewPasteClipboardSignalInfo where
    type HaskellCallbackType TextViewPasteClipboardSignalInfo = TextViewPasteClipboardCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextViewPasteClipboardCallback cb
        cb'' <- mk_TextViewPasteClipboardCallback cb'
        connectSignalFunPtr obj "paste-clipboard" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView::paste-clipboard"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:signal:pasteClipboard"})

#endif

-- signal TextView::populate-popup
-- | The [populatePopup](#g:signal:populatePopup) signal gets emitted before showing the
-- context menu of the text view.
-- 
-- If you need to add items to the context menu, connect
-- to this signal and append your items to the /@popup@/, which
-- will be a t'GI.Gtk.Objects.Menu.Menu' in this case.
-- 
-- If [TextView:populateAll]("GI.Gtk.Objects.TextView#g:attr:populateAll") is 'P.True', this signal will
-- also be emitted to populate touch popups. In this case,
-- /@popup@/ will be a different container, e.g. a t'GI.Gtk.Objects.Toolbar.Toolbar'.
-- 
-- The signal handler should not make assumptions about the
-- type of /@widget@/, but check whether /@popup@/ is a t'GI.Gtk.Objects.Menu.Menu'
-- or t'GI.Gtk.Objects.Toolbar.Toolbar' or another kind of container.
type TextViewPopulatePopupCallback =
    Gtk.Widget.Widget
    -- ^ /@popup@/: the container that is being populated
    -> IO ()

type C_TextViewPopulatePopupCallback =
    Ptr TextView ->                         -- object
    Ptr Gtk.Widget.Widget ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextViewPopulatePopupCallback`.
foreign import ccall "wrapper"
    mk_TextViewPopulatePopupCallback :: C_TextViewPopulatePopupCallback -> IO (FunPtr C_TextViewPopulatePopupCallback)

wrap_TextViewPopulatePopupCallback :: 
    GObject a => (a -> TextViewPopulatePopupCallback) ->
    C_TextViewPopulatePopupCallback
wrap_TextViewPopulatePopupCallback gi'cb gi'selfPtr popup _ = do
    popup' <- (newObject Gtk.Widget.Widget) popup
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  popup'


-- | Connect a signal handler for the [populatePopup](#signal:populatePopup) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textView #populatePopup callback
-- @
-- 
-- 
onTextViewPopulatePopup :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewPopulatePopupCallback) -> m SignalHandlerId
onTextViewPopulatePopup obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewPopulatePopupCallback wrapped
    wrapped'' <- mk_TextViewPopulatePopupCallback wrapped'
    connectSignalFunPtr obj "populate-popup" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [populatePopup](#signal:populatePopup) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textView #populatePopup callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextViewPopulatePopup :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewPopulatePopupCallback) -> m SignalHandlerId
afterTextViewPopulatePopup obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewPopulatePopupCallback wrapped
    wrapped'' <- mk_TextViewPopulatePopupCallback wrapped'
    connectSignalFunPtr obj "populate-popup" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextViewPopulatePopupSignalInfo
instance SignalInfo TextViewPopulatePopupSignalInfo where
    type HaskellCallbackType TextViewPopulatePopupSignalInfo = TextViewPopulatePopupCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextViewPopulatePopupCallback cb
        cb'' <- mk_TextViewPopulatePopupCallback cb'
        connectSignalFunPtr obj "populate-popup" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView::populate-popup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:signal:populatePopup"})

#endif

-- signal TextView::preedit-changed
-- | If an input method is used, the typed text will not immediately
-- be committed to the buffer. So if you are interested in the text,
-- connect to this signal.
-- 
-- This signal is only emitted if the text at the given position
-- is actually editable.
-- 
-- /Since: 2.20/
type TextViewPreeditChangedCallback =
    T.Text
    -- ^ /@preedit@/: the current preedit string
    -> IO ()

type C_TextViewPreeditChangedCallback =
    Ptr TextView ->                         -- object
    CString ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextViewPreeditChangedCallback`.
foreign import ccall "wrapper"
    mk_TextViewPreeditChangedCallback :: C_TextViewPreeditChangedCallback -> IO (FunPtr C_TextViewPreeditChangedCallback)

wrap_TextViewPreeditChangedCallback :: 
    GObject a => (a -> TextViewPreeditChangedCallback) ->
    C_TextViewPreeditChangedCallback
wrap_TextViewPreeditChangedCallback gi'cb gi'selfPtr preedit _ = do
    preedit' <- cstringToText preedit
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  preedit'


-- | Connect a signal handler for the [preeditChanged](#signal:preeditChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textView #preeditChanged callback
-- @
-- 
-- 
onTextViewPreeditChanged :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewPreeditChangedCallback) -> m SignalHandlerId
onTextViewPreeditChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewPreeditChangedCallback wrapped
    wrapped'' <- mk_TextViewPreeditChangedCallback wrapped'
    connectSignalFunPtr obj "preedit-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [preeditChanged](#signal:preeditChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textView #preeditChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextViewPreeditChanged :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewPreeditChangedCallback) -> m SignalHandlerId
afterTextViewPreeditChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewPreeditChangedCallback wrapped
    wrapped'' <- mk_TextViewPreeditChangedCallback wrapped'
    connectSignalFunPtr obj "preedit-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextViewPreeditChangedSignalInfo
instance SignalInfo TextViewPreeditChangedSignalInfo where
    type HaskellCallbackType TextViewPreeditChangedSignalInfo = TextViewPreeditChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextViewPreeditChangedCallback cb
        cb'' <- mk_TextViewPreeditChangedCallback cb'
        connectSignalFunPtr obj "preedit-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView::preedit-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:signal:preeditChanged"})

#endif

-- signal TextView::select-all
-- | The [selectAll](#g:signal:selectAll) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to select or unselect the complete
-- contents of the text view.
-- 
-- The default bindings for this signal are Ctrl-a and Ctrl-\/
-- for selecting and Shift-Ctrl-a and Ctrl-\\ for unselecting.
type TextViewSelectAllCallback =
    Bool
    -- ^ /@select@/: 'P.True' to select, 'P.False' to unselect
    -> IO ()

type C_TextViewSelectAllCallback =
    Ptr TextView ->                         -- object
    CInt ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextViewSelectAllCallback`.
foreign import ccall "wrapper"
    mk_TextViewSelectAllCallback :: C_TextViewSelectAllCallback -> IO (FunPtr C_TextViewSelectAllCallback)

wrap_TextViewSelectAllCallback :: 
    GObject a => (a -> TextViewSelectAllCallback) ->
    C_TextViewSelectAllCallback
wrap_TextViewSelectAllCallback gi'cb gi'selfPtr select _ = do
    let select' = (/= 0) select
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  select'


-- | Connect a signal handler for the [selectAll](#signal:selectAll) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textView #selectAll callback
-- @
-- 
-- 
onTextViewSelectAll :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewSelectAllCallback) -> m SignalHandlerId
onTextViewSelectAll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewSelectAllCallback wrapped
    wrapped'' <- mk_TextViewSelectAllCallback wrapped'
    connectSignalFunPtr obj "select-all" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [selectAll](#signal:selectAll) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textView #selectAll callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextViewSelectAll :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewSelectAllCallback) -> m SignalHandlerId
afterTextViewSelectAll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewSelectAllCallback wrapped
    wrapped'' <- mk_TextViewSelectAllCallback wrapped'
    connectSignalFunPtr obj "select-all" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextViewSelectAllSignalInfo
instance SignalInfo TextViewSelectAllSignalInfo where
    type HaskellCallbackType TextViewSelectAllSignalInfo = TextViewSelectAllCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextViewSelectAllCallback cb
        cb'' <- mk_TextViewSelectAllCallback cb'
        connectSignalFunPtr obj "select-all" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView::select-all"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:signal:selectAll"})

#endif

-- signal TextView::set-anchor
-- | The [setAnchor](#g:signal:setAnchor) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user initiates setting the \"anchor\"
-- mark. The \"anchor\" mark gets placed at the same position as the
-- \"insert\" mark.
-- 
-- This signal has no default bindings.
type TextViewSetAnchorCallback =
    IO ()

type C_TextViewSetAnchorCallback =
    Ptr TextView ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextViewSetAnchorCallback`.
foreign import ccall "wrapper"
    mk_TextViewSetAnchorCallback :: C_TextViewSetAnchorCallback -> IO (FunPtr C_TextViewSetAnchorCallback)

wrap_TextViewSetAnchorCallback :: 
    GObject a => (a -> TextViewSetAnchorCallback) ->
    C_TextViewSetAnchorCallback
wrap_TextViewSetAnchorCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [setAnchor](#signal:setAnchor) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textView #setAnchor callback
-- @
-- 
-- 
onTextViewSetAnchor :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewSetAnchorCallback) -> m SignalHandlerId
onTextViewSetAnchor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewSetAnchorCallback wrapped
    wrapped'' <- mk_TextViewSetAnchorCallback wrapped'
    connectSignalFunPtr obj "set-anchor" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [setAnchor](#signal:setAnchor) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textView #setAnchor callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextViewSetAnchor :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewSetAnchorCallback) -> m SignalHandlerId
afterTextViewSetAnchor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewSetAnchorCallback wrapped
    wrapped'' <- mk_TextViewSetAnchorCallback wrapped'
    connectSignalFunPtr obj "set-anchor" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextViewSetAnchorSignalInfo
instance SignalInfo TextViewSetAnchorSignalInfo where
    type HaskellCallbackType TextViewSetAnchorSignalInfo = TextViewSetAnchorCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextViewSetAnchorCallback cb
        cb'' <- mk_TextViewSetAnchorCallback cb'
        connectSignalFunPtr obj "set-anchor" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView::set-anchor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:signal:setAnchor"})

#endif

-- signal TextView::toggle-cursor-visible
-- | The [toggleCursorVisible](#g:signal:toggleCursorVisible) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to toggle the [TextView:cursorVisible]("GI.Gtk.Objects.TextView#g:attr:cursorVisible")
-- property.
-- 
-- The default binding for this signal is F7.
type TextViewToggleCursorVisibleCallback =
    IO ()

type C_TextViewToggleCursorVisibleCallback =
    Ptr TextView ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextViewToggleCursorVisibleCallback`.
foreign import ccall "wrapper"
    mk_TextViewToggleCursorVisibleCallback :: C_TextViewToggleCursorVisibleCallback -> IO (FunPtr C_TextViewToggleCursorVisibleCallback)

wrap_TextViewToggleCursorVisibleCallback :: 
    GObject a => (a -> TextViewToggleCursorVisibleCallback) ->
    C_TextViewToggleCursorVisibleCallback
wrap_TextViewToggleCursorVisibleCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [toggleCursorVisible](#signal:toggleCursorVisible) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textView #toggleCursorVisible callback
-- @
-- 
-- 
onTextViewToggleCursorVisible :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewToggleCursorVisibleCallback) -> m SignalHandlerId
onTextViewToggleCursorVisible obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewToggleCursorVisibleCallback wrapped
    wrapped'' <- mk_TextViewToggleCursorVisibleCallback wrapped'
    connectSignalFunPtr obj "toggle-cursor-visible" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [toggleCursorVisible](#signal:toggleCursorVisible) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textView #toggleCursorVisible callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextViewToggleCursorVisible :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewToggleCursorVisibleCallback) -> m SignalHandlerId
afterTextViewToggleCursorVisible obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewToggleCursorVisibleCallback wrapped
    wrapped'' <- mk_TextViewToggleCursorVisibleCallback wrapped'
    connectSignalFunPtr obj "toggle-cursor-visible" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextViewToggleCursorVisibleSignalInfo
instance SignalInfo TextViewToggleCursorVisibleSignalInfo where
    type HaskellCallbackType TextViewToggleCursorVisibleSignalInfo = TextViewToggleCursorVisibleCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextViewToggleCursorVisibleCallback cb
        cb'' <- mk_TextViewToggleCursorVisibleCallback cb'
        connectSignalFunPtr obj "toggle-cursor-visible" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView::toggle-cursor-visible"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:signal:toggleCursorVisible"})

#endif

-- signal TextView::toggle-overwrite
-- | The [toggleOverwrite](#g:signal:toggleOverwrite) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to toggle the overwrite mode of the text view.
-- 
-- The default bindings for this signal is Insert.
type TextViewToggleOverwriteCallback =
    IO ()

type C_TextViewToggleOverwriteCallback =
    Ptr TextView ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TextViewToggleOverwriteCallback`.
foreign import ccall "wrapper"
    mk_TextViewToggleOverwriteCallback :: C_TextViewToggleOverwriteCallback -> IO (FunPtr C_TextViewToggleOverwriteCallback)

wrap_TextViewToggleOverwriteCallback :: 
    GObject a => (a -> TextViewToggleOverwriteCallback) ->
    C_TextViewToggleOverwriteCallback
wrap_TextViewToggleOverwriteCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [toggleOverwrite](#signal:toggleOverwrite) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' textView #toggleOverwrite callback
-- @
-- 
-- 
onTextViewToggleOverwrite :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewToggleOverwriteCallback) -> m SignalHandlerId
onTextViewToggleOverwrite obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewToggleOverwriteCallback wrapped
    wrapped'' <- mk_TextViewToggleOverwriteCallback wrapped'
    connectSignalFunPtr obj "toggle-overwrite" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [toggleOverwrite](#signal:toggleOverwrite) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' textView #toggleOverwrite callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTextViewToggleOverwrite :: (IsTextView a, MonadIO m) => a -> ((?self :: a) => TextViewToggleOverwriteCallback) -> m SignalHandlerId
afterTextViewToggleOverwrite obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TextViewToggleOverwriteCallback wrapped
    wrapped'' <- mk_TextViewToggleOverwriteCallback wrapped'
    connectSignalFunPtr obj "toggle-overwrite" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TextViewToggleOverwriteSignalInfo
instance SignalInfo TextViewToggleOverwriteSignalInfo where
    type HaskellCallbackType TextViewToggleOverwriteSignalInfo = TextViewToggleOverwriteCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TextViewToggleOverwriteCallback cb
        cb'' <- mk_TextViewToggleOverwriteCallback cb'
        connectSignalFunPtr obj "toggle-overwrite" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView::toggle-overwrite"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:signal:toggleOverwrite"})

#endif

-- VVV Prop "accepts-tab"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@accepts-tab@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #acceptsTab
-- @
getTextViewAcceptsTab :: (MonadIO m, IsTextView o) => o -> m Bool
getTextViewAcceptsTab obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "accepts-tab"

-- | Set the value of the “@accepts-tab@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #acceptsTab 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewAcceptsTab :: (MonadIO m, IsTextView o) => o -> Bool -> m ()
setTextViewAcceptsTab obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "accepts-tab" val

-- | Construct a `GValueConstruct` with valid value for the “@accepts-tab@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewAcceptsTab :: (IsTextView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextViewAcceptsTab val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "accepts-tab" val

#if defined(ENABLE_OVERLOADING)
data TextViewAcceptsTabPropertyInfo
instance AttrInfo TextViewAcceptsTabPropertyInfo where
    type AttrAllowedOps TextViewAcceptsTabPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewAcceptsTabPropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewAcceptsTabPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextViewAcceptsTabPropertyInfo = (~) Bool
    type AttrTransferType TextViewAcceptsTabPropertyInfo = Bool
    type AttrGetType TextViewAcceptsTabPropertyInfo = Bool
    type AttrLabel TextViewAcceptsTabPropertyInfo = "accepts-tab"
    type AttrOrigin TextViewAcceptsTabPropertyInfo = TextView
    attrGet = getTextViewAcceptsTab
    attrSet = setTextViewAcceptsTab
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewAcceptsTab
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.acceptsTab"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:acceptsTab"
        })
#endif

-- VVV Prop "bottom-margin"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@bottom-margin@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #bottomMargin
-- @
getTextViewBottomMargin :: (MonadIO m, IsTextView o) => o -> m Int32
getTextViewBottomMargin obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "bottom-margin"

-- | Set the value of the “@bottom-margin@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #bottomMargin 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewBottomMargin :: (MonadIO m, IsTextView o) => o -> Int32 -> m ()
setTextViewBottomMargin obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "bottom-margin" val

-- | Construct a `GValueConstruct` with valid value for the “@bottom-margin@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewBottomMargin :: (IsTextView o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextViewBottomMargin val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "bottom-margin" val

#if defined(ENABLE_OVERLOADING)
data TextViewBottomMarginPropertyInfo
instance AttrInfo TextViewBottomMarginPropertyInfo where
    type AttrAllowedOps TextViewBottomMarginPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewBottomMarginPropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewBottomMarginPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextViewBottomMarginPropertyInfo = (~) Int32
    type AttrTransferType TextViewBottomMarginPropertyInfo = Int32
    type AttrGetType TextViewBottomMarginPropertyInfo = Int32
    type AttrLabel TextViewBottomMarginPropertyInfo = "bottom-margin"
    type AttrOrigin TextViewBottomMarginPropertyInfo = TextView
    attrGet = getTextViewBottomMargin
    attrSet = setTextViewBottomMargin
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewBottomMargin
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.bottomMargin"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:bottomMargin"
        })
#endif

-- VVV Prop "buffer"
   -- Type: TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just True)

-- | Get the value of the “@buffer@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #buffer
-- @
getTextViewBuffer :: (MonadIO m, IsTextView o) => o -> m Gtk.TextBuffer.TextBuffer
getTextViewBuffer obj = MIO.liftIO $ checkUnexpectedNothing "getTextViewBuffer" $ B.Properties.getObjectPropertyObject obj "buffer" Gtk.TextBuffer.TextBuffer

-- | Set the value of the “@buffer@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #buffer 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewBuffer :: (MonadIO m, IsTextView o, Gtk.TextBuffer.IsTextBuffer a) => o -> a -> m ()
setTextViewBuffer obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "buffer" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@buffer@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewBuffer :: (IsTextView o, MIO.MonadIO m, Gtk.TextBuffer.IsTextBuffer a) => a -> m (GValueConstruct o)
constructTextViewBuffer val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "buffer" (P.Just val)

-- | Set the value of the “@buffer@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #buffer
-- @
clearTextViewBuffer :: (MonadIO m, IsTextView o) => o -> m ()
clearTextViewBuffer obj = liftIO $ B.Properties.setObjectPropertyObject obj "buffer" (Nothing :: Maybe Gtk.TextBuffer.TextBuffer)

#if defined(ENABLE_OVERLOADING)
data TextViewBufferPropertyInfo
instance AttrInfo TextViewBufferPropertyInfo where
    type AttrAllowedOps TextViewBufferPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextViewBufferPropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewBufferPropertyInfo = Gtk.TextBuffer.IsTextBuffer
    type AttrTransferTypeConstraint TextViewBufferPropertyInfo = Gtk.TextBuffer.IsTextBuffer
    type AttrTransferType TextViewBufferPropertyInfo = Gtk.TextBuffer.TextBuffer
    type AttrGetType TextViewBufferPropertyInfo = Gtk.TextBuffer.TextBuffer
    type AttrLabel TextViewBufferPropertyInfo = "buffer"
    type AttrOrigin TextViewBufferPropertyInfo = TextView
    attrGet = getTextViewBuffer
    attrSet = setTextViewBuffer
    attrTransfer _ v = do
        unsafeCastTo Gtk.TextBuffer.TextBuffer v
    attrConstruct = constructTextViewBuffer
    attrClear = clearTextViewBuffer
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.buffer"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:buffer"
        })
#endif

-- VVV Prop "cursor-visible"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@cursor-visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #cursorVisible
-- @
getTextViewCursorVisible :: (MonadIO m, IsTextView o) => o -> m Bool
getTextViewCursorVisible obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "cursor-visible"

-- | Set the value of the “@cursor-visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #cursorVisible 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewCursorVisible :: (MonadIO m, IsTextView o) => o -> Bool -> m ()
setTextViewCursorVisible obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "cursor-visible" val

-- | Construct a `GValueConstruct` with valid value for the “@cursor-visible@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewCursorVisible :: (IsTextView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextViewCursorVisible val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "cursor-visible" val

#if defined(ENABLE_OVERLOADING)
data TextViewCursorVisiblePropertyInfo
instance AttrInfo TextViewCursorVisiblePropertyInfo where
    type AttrAllowedOps TextViewCursorVisiblePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewCursorVisiblePropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewCursorVisiblePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextViewCursorVisiblePropertyInfo = (~) Bool
    type AttrTransferType TextViewCursorVisiblePropertyInfo = Bool
    type AttrGetType TextViewCursorVisiblePropertyInfo = Bool
    type AttrLabel TextViewCursorVisiblePropertyInfo = "cursor-visible"
    type AttrOrigin TextViewCursorVisiblePropertyInfo = TextView
    attrGet = getTextViewCursorVisible
    attrSet = setTextViewCursorVisible
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewCursorVisible
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.cursorVisible"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:cursorVisible"
        })
#endif

-- VVV Prop "editable"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@editable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #editable
-- @
getTextViewEditable :: (MonadIO m, IsTextView o) => o -> m Bool
getTextViewEditable obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "editable"

-- | Set the value of the “@editable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #editable 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewEditable :: (MonadIO m, IsTextView o) => o -> Bool -> m ()
setTextViewEditable obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "editable" val

-- | Construct a `GValueConstruct` with valid value for the “@editable@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewEditable :: (IsTextView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextViewEditable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "editable" val

#if defined(ENABLE_OVERLOADING)
data TextViewEditablePropertyInfo
instance AttrInfo TextViewEditablePropertyInfo where
    type AttrAllowedOps TextViewEditablePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewEditablePropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewEditablePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextViewEditablePropertyInfo = (~) Bool
    type AttrTransferType TextViewEditablePropertyInfo = Bool
    type AttrGetType TextViewEditablePropertyInfo = Bool
    type AttrLabel TextViewEditablePropertyInfo = "editable"
    type AttrOrigin TextViewEditablePropertyInfo = TextView
    attrGet = getTextViewEditable
    attrSet = setTextViewEditable
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewEditable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.editable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:editable"
        })
#endif

-- VVV Prop "im-module"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@im-module@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #imModule
-- @
getTextViewImModule :: (MonadIO m, IsTextView o) => o -> m (Maybe T.Text)
getTextViewImModule obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "im-module"

-- | Set the value of the “@im-module@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #imModule 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewImModule :: (MonadIO m, IsTextView o) => o -> T.Text -> m ()
setTextViewImModule obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "im-module" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@im-module@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewImModule :: (IsTextView o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructTextViewImModule val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "im-module" (P.Just val)

-- | Set the value of the “@im-module@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #imModule
-- @
clearTextViewImModule :: (MonadIO m, IsTextView o) => o -> m ()
clearTextViewImModule obj = liftIO $ B.Properties.setObjectPropertyString obj "im-module" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data TextViewImModulePropertyInfo
instance AttrInfo TextViewImModulePropertyInfo where
    type AttrAllowedOps TextViewImModulePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TextViewImModulePropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewImModulePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint TextViewImModulePropertyInfo = (~) T.Text
    type AttrTransferType TextViewImModulePropertyInfo = T.Text
    type AttrGetType TextViewImModulePropertyInfo = (Maybe T.Text)
    type AttrLabel TextViewImModulePropertyInfo = "im-module"
    type AttrOrigin TextViewImModulePropertyInfo = TextView
    attrGet = getTextViewImModule
    attrSet = setTextViewImModule
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewImModule
    attrClear = clearTextViewImModule
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.imModule"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:imModule"
        })
#endif

-- VVV Prop "indent"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@indent@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #indent
-- @
getTextViewIndent :: (MonadIO m, IsTextView o) => o -> m Int32
getTextViewIndent obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "indent"

-- | Set the value of the “@indent@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #indent 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewIndent :: (MonadIO m, IsTextView o) => o -> Int32 -> m ()
setTextViewIndent obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "indent" val

-- | Construct a `GValueConstruct` with valid value for the “@indent@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewIndent :: (IsTextView o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextViewIndent val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "indent" val

#if defined(ENABLE_OVERLOADING)
data TextViewIndentPropertyInfo
instance AttrInfo TextViewIndentPropertyInfo where
    type AttrAllowedOps TextViewIndentPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewIndentPropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewIndentPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextViewIndentPropertyInfo = (~) Int32
    type AttrTransferType TextViewIndentPropertyInfo = Int32
    type AttrGetType TextViewIndentPropertyInfo = Int32
    type AttrLabel TextViewIndentPropertyInfo = "indent"
    type AttrOrigin TextViewIndentPropertyInfo = TextView
    attrGet = getTextViewIndent
    attrSet = setTextViewIndent
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewIndent
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.indent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:indent"
        })
#endif

-- VVV Prop "input-hints"
   -- Type: TInterface (Name {namespace = "Gtk", name = "InputHints"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@input-hints@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #inputHints
-- @
getTextViewInputHints :: (MonadIO m, IsTextView o) => o -> m [Gtk.Flags.InputHints]
getTextViewInputHints obj = MIO.liftIO $ B.Properties.getObjectPropertyFlags obj "input-hints"

-- | Set the value of the “@input-hints@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #inputHints 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewInputHints :: (MonadIO m, IsTextView o) => o -> [Gtk.Flags.InputHints] -> m ()
setTextViewInputHints obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFlags obj "input-hints" val

-- | Construct a `GValueConstruct` with valid value for the “@input-hints@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewInputHints :: (IsTextView o, MIO.MonadIO m) => [Gtk.Flags.InputHints] -> m (GValueConstruct o)
constructTextViewInputHints val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFlags "input-hints" val

#if defined(ENABLE_OVERLOADING)
data TextViewInputHintsPropertyInfo
instance AttrInfo TextViewInputHintsPropertyInfo where
    type AttrAllowedOps TextViewInputHintsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewInputHintsPropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewInputHintsPropertyInfo = (~) [Gtk.Flags.InputHints]
    type AttrTransferTypeConstraint TextViewInputHintsPropertyInfo = (~) [Gtk.Flags.InputHints]
    type AttrTransferType TextViewInputHintsPropertyInfo = [Gtk.Flags.InputHints]
    type AttrGetType TextViewInputHintsPropertyInfo = [Gtk.Flags.InputHints]
    type AttrLabel TextViewInputHintsPropertyInfo = "input-hints"
    type AttrOrigin TextViewInputHintsPropertyInfo = TextView
    attrGet = getTextViewInputHints
    attrSet = setTextViewInputHints
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewInputHints
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.inputHints"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:inputHints"
        })
#endif

-- VVV Prop "input-purpose"
   -- Type: TInterface (Name {namespace = "Gtk", name = "InputPurpose"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@input-purpose@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #inputPurpose
-- @
getTextViewInputPurpose :: (MonadIO m, IsTextView o) => o -> m Gtk.Enums.InputPurpose
getTextViewInputPurpose obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "input-purpose"

-- | Set the value of the “@input-purpose@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #inputPurpose 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewInputPurpose :: (MonadIO m, IsTextView o) => o -> Gtk.Enums.InputPurpose -> m ()
setTextViewInputPurpose obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "input-purpose" val

-- | Construct a `GValueConstruct` with valid value for the “@input-purpose@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewInputPurpose :: (IsTextView o, MIO.MonadIO m) => Gtk.Enums.InputPurpose -> m (GValueConstruct o)
constructTextViewInputPurpose val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "input-purpose" val

#if defined(ENABLE_OVERLOADING)
data TextViewInputPurposePropertyInfo
instance AttrInfo TextViewInputPurposePropertyInfo where
    type AttrAllowedOps TextViewInputPurposePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewInputPurposePropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewInputPurposePropertyInfo = (~) Gtk.Enums.InputPurpose
    type AttrTransferTypeConstraint TextViewInputPurposePropertyInfo = (~) Gtk.Enums.InputPurpose
    type AttrTransferType TextViewInputPurposePropertyInfo = Gtk.Enums.InputPurpose
    type AttrGetType TextViewInputPurposePropertyInfo = Gtk.Enums.InputPurpose
    type AttrLabel TextViewInputPurposePropertyInfo = "input-purpose"
    type AttrOrigin TextViewInputPurposePropertyInfo = TextView
    attrGet = getTextViewInputPurpose
    attrSet = setTextViewInputPurpose
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewInputPurpose
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.inputPurpose"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:inputPurpose"
        })
#endif

-- VVV Prop "justification"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Justification"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@justification@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #justification
-- @
getTextViewJustification :: (MonadIO m, IsTextView o) => o -> m Gtk.Enums.Justification
getTextViewJustification obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "justification"

-- | Set the value of the “@justification@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #justification 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewJustification :: (MonadIO m, IsTextView o) => o -> Gtk.Enums.Justification -> m ()
setTextViewJustification obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "justification" val

-- | Construct a `GValueConstruct` with valid value for the “@justification@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewJustification :: (IsTextView o, MIO.MonadIO m) => Gtk.Enums.Justification -> m (GValueConstruct o)
constructTextViewJustification val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "justification" val

#if defined(ENABLE_OVERLOADING)
data TextViewJustificationPropertyInfo
instance AttrInfo TextViewJustificationPropertyInfo where
    type AttrAllowedOps TextViewJustificationPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewJustificationPropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewJustificationPropertyInfo = (~) Gtk.Enums.Justification
    type AttrTransferTypeConstraint TextViewJustificationPropertyInfo = (~) Gtk.Enums.Justification
    type AttrTransferType TextViewJustificationPropertyInfo = Gtk.Enums.Justification
    type AttrGetType TextViewJustificationPropertyInfo = Gtk.Enums.Justification
    type AttrLabel TextViewJustificationPropertyInfo = "justification"
    type AttrOrigin TextViewJustificationPropertyInfo = TextView
    attrGet = getTextViewJustification
    attrSet = setTextViewJustification
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewJustification
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.justification"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:justification"
        })
#endif

-- VVV Prop "left-margin"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@left-margin@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #leftMargin
-- @
getTextViewLeftMargin :: (MonadIO m, IsTextView o) => o -> m Int32
getTextViewLeftMargin obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "left-margin"

-- | Set the value of the “@left-margin@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #leftMargin 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewLeftMargin :: (MonadIO m, IsTextView o) => o -> Int32 -> m ()
setTextViewLeftMargin obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "left-margin" val

-- | Construct a `GValueConstruct` with valid value for the “@left-margin@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewLeftMargin :: (IsTextView o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextViewLeftMargin val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "left-margin" val

#if defined(ENABLE_OVERLOADING)
data TextViewLeftMarginPropertyInfo
instance AttrInfo TextViewLeftMarginPropertyInfo where
    type AttrAllowedOps TextViewLeftMarginPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewLeftMarginPropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewLeftMarginPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextViewLeftMarginPropertyInfo = (~) Int32
    type AttrTransferType TextViewLeftMarginPropertyInfo = Int32
    type AttrGetType TextViewLeftMarginPropertyInfo = Int32
    type AttrLabel TextViewLeftMarginPropertyInfo = "left-margin"
    type AttrOrigin TextViewLeftMarginPropertyInfo = TextView
    attrGet = getTextViewLeftMargin
    attrSet = setTextViewLeftMargin
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewLeftMargin
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.leftMargin"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:leftMargin"
        })
#endif

-- VVV Prop "monospace"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@monospace@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #monospace
-- @
getTextViewMonospace :: (MonadIO m, IsTextView o) => o -> m Bool
getTextViewMonospace obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "monospace"

-- | Set the value of the “@monospace@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #monospace 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewMonospace :: (MonadIO m, IsTextView o) => o -> Bool -> m ()
setTextViewMonospace obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "monospace" val

-- | Construct a `GValueConstruct` with valid value for the “@monospace@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewMonospace :: (IsTextView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextViewMonospace val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "monospace" val

#if defined(ENABLE_OVERLOADING)
data TextViewMonospacePropertyInfo
instance AttrInfo TextViewMonospacePropertyInfo where
    type AttrAllowedOps TextViewMonospacePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewMonospacePropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewMonospacePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextViewMonospacePropertyInfo = (~) Bool
    type AttrTransferType TextViewMonospacePropertyInfo = Bool
    type AttrGetType TextViewMonospacePropertyInfo = Bool
    type AttrLabel TextViewMonospacePropertyInfo = "monospace"
    type AttrOrigin TextViewMonospacePropertyInfo = TextView
    attrGet = getTextViewMonospace
    attrSet = setTextViewMonospace
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewMonospace
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.monospace"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:monospace"
        })
#endif

-- VVV Prop "overwrite"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@overwrite@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #overwrite
-- @
getTextViewOverwrite :: (MonadIO m, IsTextView o) => o -> m Bool
getTextViewOverwrite obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "overwrite"

-- | Set the value of the “@overwrite@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #overwrite 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewOverwrite :: (MonadIO m, IsTextView o) => o -> Bool -> m ()
setTextViewOverwrite obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "overwrite" val

-- | Construct a `GValueConstruct` with valid value for the “@overwrite@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewOverwrite :: (IsTextView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextViewOverwrite val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "overwrite" val

#if defined(ENABLE_OVERLOADING)
data TextViewOverwritePropertyInfo
instance AttrInfo TextViewOverwritePropertyInfo where
    type AttrAllowedOps TextViewOverwritePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewOverwritePropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewOverwritePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextViewOverwritePropertyInfo = (~) Bool
    type AttrTransferType TextViewOverwritePropertyInfo = Bool
    type AttrGetType TextViewOverwritePropertyInfo = Bool
    type AttrLabel TextViewOverwritePropertyInfo = "overwrite"
    type AttrOrigin TextViewOverwritePropertyInfo = TextView
    attrGet = getTextViewOverwrite
    attrSet = setTextViewOverwrite
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewOverwrite
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.overwrite"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:overwrite"
        })
#endif

-- VVV Prop "pixels-above-lines"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@pixels-above-lines@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #pixelsAboveLines
-- @
getTextViewPixelsAboveLines :: (MonadIO m, IsTextView o) => o -> m Int32
getTextViewPixelsAboveLines obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "pixels-above-lines"

-- | Set the value of the “@pixels-above-lines@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #pixelsAboveLines 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewPixelsAboveLines :: (MonadIO m, IsTextView o) => o -> Int32 -> m ()
setTextViewPixelsAboveLines obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "pixels-above-lines" val

-- | Construct a `GValueConstruct` with valid value for the “@pixels-above-lines@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewPixelsAboveLines :: (IsTextView o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextViewPixelsAboveLines val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "pixels-above-lines" val

#if defined(ENABLE_OVERLOADING)
data TextViewPixelsAboveLinesPropertyInfo
instance AttrInfo TextViewPixelsAboveLinesPropertyInfo where
    type AttrAllowedOps TextViewPixelsAboveLinesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewPixelsAboveLinesPropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewPixelsAboveLinesPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextViewPixelsAboveLinesPropertyInfo = (~) Int32
    type AttrTransferType TextViewPixelsAboveLinesPropertyInfo = Int32
    type AttrGetType TextViewPixelsAboveLinesPropertyInfo = Int32
    type AttrLabel TextViewPixelsAboveLinesPropertyInfo = "pixels-above-lines"
    type AttrOrigin TextViewPixelsAboveLinesPropertyInfo = TextView
    attrGet = getTextViewPixelsAboveLines
    attrSet = setTextViewPixelsAboveLines
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewPixelsAboveLines
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.pixelsAboveLines"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:pixelsAboveLines"
        })
#endif

-- VVV Prop "pixels-below-lines"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@pixels-below-lines@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #pixelsBelowLines
-- @
getTextViewPixelsBelowLines :: (MonadIO m, IsTextView o) => o -> m Int32
getTextViewPixelsBelowLines obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "pixels-below-lines"

-- | Set the value of the “@pixels-below-lines@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #pixelsBelowLines 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewPixelsBelowLines :: (MonadIO m, IsTextView o) => o -> Int32 -> m ()
setTextViewPixelsBelowLines obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "pixels-below-lines" val

-- | Construct a `GValueConstruct` with valid value for the “@pixels-below-lines@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewPixelsBelowLines :: (IsTextView o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextViewPixelsBelowLines val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "pixels-below-lines" val

#if defined(ENABLE_OVERLOADING)
data TextViewPixelsBelowLinesPropertyInfo
instance AttrInfo TextViewPixelsBelowLinesPropertyInfo where
    type AttrAllowedOps TextViewPixelsBelowLinesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewPixelsBelowLinesPropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewPixelsBelowLinesPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextViewPixelsBelowLinesPropertyInfo = (~) Int32
    type AttrTransferType TextViewPixelsBelowLinesPropertyInfo = Int32
    type AttrGetType TextViewPixelsBelowLinesPropertyInfo = Int32
    type AttrLabel TextViewPixelsBelowLinesPropertyInfo = "pixels-below-lines"
    type AttrOrigin TextViewPixelsBelowLinesPropertyInfo = TextView
    attrGet = getTextViewPixelsBelowLines
    attrSet = setTextViewPixelsBelowLines
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewPixelsBelowLines
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.pixelsBelowLines"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:pixelsBelowLines"
        })
#endif

-- VVV Prop "pixels-inside-wrap"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@pixels-inside-wrap@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #pixelsInsideWrap
-- @
getTextViewPixelsInsideWrap :: (MonadIO m, IsTextView o) => o -> m Int32
getTextViewPixelsInsideWrap obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "pixels-inside-wrap"

-- | Set the value of the “@pixels-inside-wrap@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #pixelsInsideWrap 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewPixelsInsideWrap :: (MonadIO m, IsTextView o) => o -> Int32 -> m ()
setTextViewPixelsInsideWrap obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "pixels-inside-wrap" val

-- | Construct a `GValueConstruct` with valid value for the “@pixels-inside-wrap@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewPixelsInsideWrap :: (IsTextView o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextViewPixelsInsideWrap val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "pixels-inside-wrap" val

#if defined(ENABLE_OVERLOADING)
data TextViewPixelsInsideWrapPropertyInfo
instance AttrInfo TextViewPixelsInsideWrapPropertyInfo where
    type AttrAllowedOps TextViewPixelsInsideWrapPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewPixelsInsideWrapPropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewPixelsInsideWrapPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextViewPixelsInsideWrapPropertyInfo = (~) Int32
    type AttrTransferType TextViewPixelsInsideWrapPropertyInfo = Int32
    type AttrGetType TextViewPixelsInsideWrapPropertyInfo = Int32
    type AttrLabel TextViewPixelsInsideWrapPropertyInfo = "pixels-inside-wrap"
    type AttrOrigin TextViewPixelsInsideWrapPropertyInfo = TextView
    attrGet = getTextViewPixelsInsideWrap
    attrSet = setTextViewPixelsInsideWrap
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewPixelsInsideWrap
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.pixelsInsideWrap"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:pixelsInsideWrap"
        })
#endif

-- VVV Prop "populate-all"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@populate-all@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #populateAll
-- @
getTextViewPopulateAll :: (MonadIO m, IsTextView o) => o -> m Bool
getTextViewPopulateAll obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "populate-all"

-- | Set the value of the “@populate-all@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #populateAll 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewPopulateAll :: (MonadIO m, IsTextView o) => o -> Bool -> m ()
setTextViewPopulateAll obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "populate-all" val

-- | Construct a `GValueConstruct` with valid value for the “@populate-all@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewPopulateAll :: (IsTextView o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructTextViewPopulateAll val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "populate-all" val

#if defined(ENABLE_OVERLOADING)
data TextViewPopulateAllPropertyInfo
instance AttrInfo TextViewPopulateAllPropertyInfo where
    type AttrAllowedOps TextViewPopulateAllPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewPopulateAllPropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewPopulateAllPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint TextViewPopulateAllPropertyInfo = (~) Bool
    type AttrTransferType TextViewPopulateAllPropertyInfo = Bool
    type AttrGetType TextViewPopulateAllPropertyInfo = Bool
    type AttrLabel TextViewPopulateAllPropertyInfo = "populate-all"
    type AttrOrigin TextViewPopulateAllPropertyInfo = TextView
    attrGet = getTextViewPopulateAll
    attrSet = setTextViewPopulateAll
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewPopulateAll
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.populateAll"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:populateAll"
        })
#endif

-- VVV Prop "right-margin"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@right-margin@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #rightMargin
-- @
getTextViewRightMargin :: (MonadIO m, IsTextView o) => o -> m Int32
getTextViewRightMargin obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "right-margin"

-- | Set the value of the “@right-margin@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #rightMargin 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewRightMargin :: (MonadIO m, IsTextView o) => o -> Int32 -> m ()
setTextViewRightMargin obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "right-margin" val

-- | Construct a `GValueConstruct` with valid value for the “@right-margin@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewRightMargin :: (IsTextView o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextViewRightMargin val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "right-margin" val

#if defined(ENABLE_OVERLOADING)
data TextViewRightMarginPropertyInfo
instance AttrInfo TextViewRightMarginPropertyInfo where
    type AttrAllowedOps TextViewRightMarginPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewRightMarginPropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewRightMarginPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextViewRightMarginPropertyInfo = (~) Int32
    type AttrTransferType TextViewRightMarginPropertyInfo = Int32
    type AttrGetType TextViewRightMarginPropertyInfo = Int32
    type AttrLabel TextViewRightMarginPropertyInfo = "right-margin"
    type AttrOrigin TextViewRightMarginPropertyInfo = TextView
    attrGet = getTextViewRightMargin
    attrSet = setTextViewRightMargin
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewRightMargin
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.rightMargin"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:rightMargin"
        })
#endif

-- VVV Prop "tabs"
   -- Type: TInterface (Name {namespace = "Pango", name = "TabArray"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Just False)

-- | Get the value of the “@tabs@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #tabs
-- @
getTextViewTabs :: (MonadIO m, IsTextView o) => o -> m (Maybe Pango.TabArray.TabArray)
getTextViewTabs obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "tabs" Pango.TabArray.TabArray

-- | Set the value of the “@tabs@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #tabs 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewTabs :: (MonadIO m, IsTextView o) => o -> Pango.TabArray.TabArray -> m ()
setTextViewTabs obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "tabs" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@tabs@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewTabs :: (IsTextView o, MIO.MonadIO m) => Pango.TabArray.TabArray -> m (GValueConstruct o)
constructTextViewTabs val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "tabs" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data TextViewTabsPropertyInfo
instance AttrInfo TextViewTabsPropertyInfo where
    type AttrAllowedOps TextViewTabsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewTabsPropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewTabsPropertyInfo = (~) Pango.TabArray.TabArray
    type AttrTransferTypeConstraint TextViewTabsPropertyInfo = (~) Pango.TabArray.TabArray
    type AttrTransferType TextViewTabsPropertyInfo = Pango.TabArray.TabArray
    type AttrGetType TextViewTabsPropertyInfo = (Maybe Pango.TabArray.TabArray)
    type AttrLabel TextViewTabsPropertyInfo = "tabs"
    type AttrOrigin TextViewTabsPropertyInfo = TextView
    attrGet = getTextViewTabs
    attrSet = setTextViewTabs
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewTabs
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.tabs"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:tabs"
        })
#endif

-- VVV Prop "top-margin"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@top-margin@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #topMargin
-- @
getTextViewTopMargin :: (MonadIO m, IsTextView o) => o -> m Int32
getTextViewTopMargin obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "top-margin"

-- | Set the value of the “@top-margin@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #topMargin 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewTopMargin :: (MonadIO m, IsTextView o) => o -> Int32 -> m ()
setTextViewTopMargin obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "top-margin" val

-- | Construct a `GValueConstruct` with valid value for the “@top-margin@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewTopMargin :: (IsTextView o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructTextViewTopMargin val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "top-margin" val

#if defined(ENABLE_OVERLOADING)
data TextViewTopMarginPropertyInfo
instance AttrInfo TextViewTopMarginPropertyInfo where
    type AttrAllowedOps TextViewTopMarginPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewTopMarginPropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewTopMarginPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint TextViewTopMarginPropertyInfo = (~) Int32
    type AttrTransferType TextViewTopMarginPropertyInfo = Int32
    type AttrGetType TextViewTopMarginPropertyInfo = Int32
    type AttrLabel TextViewTopMarginPropertyInfo = "top-margin"
    type AttrOrigin TextViewTopMarginPropertyInfo = TextView
    attrGet = getTextViewTopMargin
    attrSet = setTextViewTopMargin
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewTopMargin
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.topMargin"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:topMargin"
        })
#endif

-- VVV Prop "wrap-mode"
   -- Type: TInterface (Name {namespace = "Gtk", name = "WrapMode"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@wrap-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' textView #wrapMode
-- @
getTextViewWrapMode :: (MonadIO m, IsTextView o) => o -> m Gtk.Enums.WrapMode
getTextViewWrapMode obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "wrap-mode"

-- | Set the value of the “@wrap-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' textView [ #wrapMode 'Data.GI.Base.Attributes.:=' value ]
-- @
setTextViewWrapMode :: (MonadIO m, IsTextView o) => o -> Gtk.Enums.WrapMode -> m ()
setTextViewWrapMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "wrap-mode" val

-- | Construct a `GValueConstruct` with valid value for the “@wrap-mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTextViewWrapMode :: (IsTextView o, MIO.MonadIO m) => Gtk.Enums.WrapMode -> m (GValueConstruct o)
constructTextViewWrapMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "wrap-mode" val

#if defined(ENABLE_OVERLOADING)
data TextViewWrapModePropertyInfo
instance AttrInfo TextViewWrapModePropertyInfo where
    type AttrAllowedOps TextViewWrapModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint TextViewWrapModePropertyInfo = IsTextView
    type AttrSetTypeConstraint TextViewWrapModePropertyInfo = (~) Gtk.Enums.WrapMode
    type AttrTransferTypeConstraint TextViewWrapModePropertyInfo = (~) Gtk.Enums.WrapMode
    type AttrTransferType TextViewWrapModePropertyInfo = Gtk.Enums.WrapMode
    type AttrGetType TextViewWrapModePropertyInfo = Gtk.Enums.WrapMode
    type AttrLabel TextViewWrapModePropertyInfo = "wrap-mode"
    type AttrOrigin TextViewWrapModePropertyInfo = TextView
    attrGet = getTextViewWrapMode
    attrSet = setTextViewWrapMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructTextViewWrapMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.wrapMode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#g:attr:wrapMode"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TextView
type instance O.AttributeList TextView = TextViewAttributeList
type TextViewAttributeList = ('[ '("acceptsTab", TextViewAcceptsTabPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("bottomMargin", TextViewBottomMarginPropertyInfo), '("buffer", TextViewBufferPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("cursorVisible", TextViewCursorVisiblePropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("editable", TextViewEditablePropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("hadjustment", Gtk.Scrollable.ScrollableHadjustmentPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("hscrollPolicy", Gtk.Scrollable.ScrollableHscrollPolicyPropertyInfo), '("imModule", TextViewImModulePropertyInfo), '("indent", TextViewIndentPropertyInfo), '("inputHints", TextViewInputHintsPropertyInfo), '("inputPurpose", TextViewInputPurposePropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("justification", TextViewJustificationPropertyInfo), '("leftMargin", TextViewLeftMarginPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("monospace", TextViewMonospacePropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("overwrite", TextViewOverwritePropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("pixelsAboveLines", TextViewPixelsAboveLinesPropertyInfo), '("pixelsBelowLines", TextViewPixelsBelowLinesPropertyInfo), '("pixelsInsideWrap", TextViewPixelsInsideWrapPropertyInfo), '("populateAll", TextViewPopulateAllPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("rightMargin", TextViewRightMarginPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tabs", TextViewTabsPropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("topMargin", TextViewTopMarginPropertyInfo), '("vadjustment", Gtk.Scrollable.ScrollableVadjustmentPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("vscrollPolicy", Gtk.Scrollable.ScrollableVscrollPolicyPropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("wrapMode", TextViewWrapModePropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
textViewAcceptsTab :: AttrLabelProxy "acceptsTab"
textViewAcceptsTab = AttrLabelProxy

textViewBottomMargin :: AttrLabelProxy "bottomMargin"
textViewBottomMargin = AttrLabelProxy

textViewBuffer :: AttrLabelProxy "buffer"
textViewBuffer = AttrLabelProxy

textViewCursorVisible :: AttrLabelProxy "cursorVisible"
textViewCursorVisible = AttrLabelProxy

textViewEditable :: AttrLabelProxy "editable"
textViewEditable = AttrLabelProxy

textViewImModule :: AttrLabelProxy "imModule"
textViewImModule = AttrLabelProxy

textViewIndent :: AttrLabelProxy "indent"
textViewIndent = AttrLabelProxy

textViewInputHints :: AttrLabelProxy "inputHints"
textViewInputHints = AttrLabelProxy

textViewInputPurpose :: AttrLabelProxy "inputPurpose"
textViewInputPurpose = AttrLabelProxy

textViewJustification :: AttrLabelProxy "justification"
textViewJustification = AttrLabelProxy

textViewLeftMargin :: AttrLabelProxy "leftMargin"
textViewLeftMargin = AttrLabelProxy

textViewMonospace :: AttrLabelProxy "monospace"
textViewMonospace = AttrLabelProxy

textViewOverwrite :: AttrLabelProxy "overwrite"
textViewOverwrite = AttrLabelProxy

textViewPixelsAboveLines :: AttrLabelProxy "pixelsAboveLines"
textViewPixelsAboveLines = AttrLabelProxy

textViewPixelsBelowLines :: AttrLabelProxy "pixelsBelowLines"
textViewPixelsBelowLines = AttrLabelProxy

textViewPixelsInsideWrap :: AttrLabelProxy "pixelsInsideWrap"
textViewPixelsInsideWrap = AttrLabelProxy

textViewPopulateAll :: AttrLabelProxy "populateAll"
textViewPopulateAll = AttrLabelProxy

textViewRightMargin :: AttrLabelProxy "rightMargin"
textViewRightMargin = AttrLabelProxy

textViewTabs :: AttrLabelProxy "tabs"
textViewTabs = AttrLabelProxy

textViewTopMargin :: AttrLabelProxy "topMargin"
textViewTopMargin = AttrLabelProxy

textViewWrapMode :: AttrLabelProxy "wrapMode"
textViewWrapMode = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TextView = TextViewSignalList
type TextViewSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("backspace", TextViewBackspaceSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("copyClipboard", TextViewCopyClipboardSignalInfo), '("cutClipboard", TextViewCutClipboardSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("deleteFromCursor", TextViewDeleteFromCursorSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("extendSelection", TextViewExtendSelectionSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("insertAtCursor", TextViewInsertAtCursorSignalInfo), '("insertEmoji", TextViewInsertEmojiSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveCursor", TextViewMoveCursorSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("moveViewport", TextViewMoveViewportSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("pasteClipboard", TextViewPasteClipboardSignalInfo), '("populatePopup", TextViewPopulatePopupSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("preeditChanged", TextViewPreeditChangedSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectAll", TextViewSelectAllSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setAnchor", TextViewSetAnchorSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("toggleCursorVisible", TextViewToggleCursorVisibleSignalInfo), '("toggleOverwrite", TextViewToggleOverwriteSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method TextView::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TextView" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_view_new" gtk_text_view_new :: 
    IO (Ptr TextView)

-- | Creates a new t'GI.Gtk.Objects.TextView.TextView'. If you don’t call 'GI.Gtk.Objects.TextView.textViewSetBuffer'
-- before using the text view, an empty default buffer will be created
-- for you. Get the buffer with 'GI.Gtk.Objects.TextView.textViewGetBuffer'. If you want
-- to specify your own buffer, consider 'GI.Gtk.Objects.TextView.textViewNewWithBuffer'.
textViewNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m TextView
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.TextView.TextView'
textViewNew  = liftIO $ do
    result <- gtk_text_view_new
    checkUnexpectedReturnNULL "textViewNew" result
    result' <- (newObject TextView) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TextView::new_with_buffer
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextBuffer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TextView" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_view_new_with_buffer" gtk_text_view_new_with_buffer :: 
    Ptr Gtk.TextBuffer.TextBuffer ->        -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    IO (Ptr TextView)

-- | Creates a new t'GI.Gtk.Objects.TextView.TextView' widget displaying the buffer
-- /@buffer@/. One buffer can be shared among many widgets.
-- /@buffer@/ may be 'P.Nothing' to create a default buffer, in which case
-- this function is equivalent to 'GI.Gtk.Objects.TextView.textViewNew'. The
-- text view adds its own reference count to the buffer; it does not
-- take over an existing reference.
textViewNewWithBuffer ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TextBuffer.IsTextBuffer a) =>
    a
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m TextView
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.TextView.TextView'.
textViewNewWithBuffer buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_text_view_new_with_buffer buffer'
    checkUnexpectedReturnNULL "textViewNewWithBuffer" result
    result' <- (newObject TextView) result
    touchManagedPtr buffer
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TextView::add_child_at_anchor
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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
--       , Arg
--           { argCName = "anchor"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextChildAnchor" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkTextChildAnchor in the #GtkTextBuffer for @text_view"
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

foreign import ccall "gtk_text_view_add_child_at_anchor" gtk_text_view_add_child_at_anchor :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.TextChildAnchor.TextChildAnchor -> -- anchor : TInterface (Name {namespace = "Gtk", name = "TextChildAnchor"})
    IO ()

-- | Adds a child widget in the text buffer, at the given /@anchor@/.
textViewAddChildAtAnchor ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a, Gtk.Widget.IsWidget b, Gtk.TextChildAnchor.IsTextChildAnchor c) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> b
    -- ^ /@child@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> c
    -- ^ /@anchor@/: a t'GI.Gtk.Objects.TextChildAnchor.TextChildAnchor' in the t'GI.Gtk.Objects.TextBuffer.TextBuffer' for /@textView@/
    -> m ()
textViewAddChildAtAnchor textView child anchor = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    child' <- unsafeManagedPtrCastPtr child
    anchor' <- unsafeManagedPtrCastPtr anchor
    gtk_text_view_add_child_at_anchor textView' child' anchor'
    touchManagedPtr textView
    touchManagedPtr child
    touchManagedPtr anchor
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewAddChildAtAnchorMethodInfo
instance (signature ~ (b -> c -> m ()), MonadIO m, IsTextView a, Gtk.Widget.IsWidget b, Gtk.TextChildAnchor.IsTextChildAnchor c) => O.OverloadedMethod TextViewAddChildAtAnchorMethodInfo a signature where
    overloadedMethod = textViewAddChildAtAnchor

instance O.OverloadedMethodInfo TextViewAddChildAtAnchorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewAddChildAtAnchor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewAddChildAtAnchor"
        })


#endif

-- method TextView::add_child_in_window
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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
--       , Arg
--           { argCName = "which_window"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextWindowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "which window the child should appear in"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xpos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "X position of child in window coordinates"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "ypos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Y position of child in window coordinates"
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

foreign import ccall "gtk_text_view_add_child_in_window" gtk_text_view_add_child_in_window :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CUInt ->                                -- which_window : TInterface (Name {namespace = "Gtk", name = "TextWindowType"})
    Int32 ->                                -- xpos : TBasicType TInt
    Int32 ->                                -- ypos : TBasicType TInt
    IO ()

-- | Adds a child at fixed coordinates in one of the text widget\'s
-- windows.
-- 
-- The window must have nonzero size (see
-- 'GI.Gtk.Objects.TextView.textViewSetBorderWindowSize'). Note that the child
-- coordinates are given relative to scrolling. When
-- placing a child in @/GTK_TEXT_WINDOW_WIDGET/@, scrolling is
-- irrelevant, the child floats above all scrollable areas. But when
-- placing a child in one of the scrollable windows (border windows or
-- text window) it will move with the scrolling as needed.
textViewAddChildInWindow ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> b
    -- ^ /@child@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> Gtk.Enums.TextWindowType
    -- ^ /@whichWindow@/: which window the child should appear in
    -> Int32
    -- ^ /@xpos@/: X position of child in window coordinates
    -> Int32
    -- ^ /@ypos@/: Y position of child in window coordinates
    -> m ()
textViewAddChildInWindow textView child whichWindow xpos ypos = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    child' <- unsafeManagedPtrCastPtr child
    let whichWindow' = (fromIntegral . fromEnum) whichWindow
    gtk_text_view_add_child_in_window textView' child' whichWindow' xpos ypos
    touchManagedPtr textView
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewAddChildInWindowMethodInfo
instance (signature ~ (b -> Gtk.Enums.TextWindowType -> Int32 -> Int32 -> m ()), MonadIO m, IsTextView a, Gtk.Widget.IsWidget b) => O.OverloadedMethod TextViewAddChildInWindowMethodInfo a signature where
    overloadedMethod = textViewAddChildInWindow

instance O.OverloadedMethodInfo TextViewAddChildInWindowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewAddChildInWindow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewAddChildInWindow"
        })


#endif

-- method TextView::backward_display_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_backward_display_line" gtk_text_view_backward_display_line :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves the given /@iter@/ backward by one display (wrapped) line.
-- A display line is different from a paragraph. Paragraphs are
-- separated by newlines or other paragraph separator characters.
-- Display lines are created by line-wrapping a paragraph. If
-- wrapping is turned off, display lines and paragraphs will be the
-- same. Display lines are divided differently for each view, since
-- they depend on the view’s width; paragraphs are the same in all
-- views, since they depend on the contents of the t'GI.Gtk.Objects.TextBuffer.TextBuffer'.
textViewBackwardDisplayLine ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ was moved and is not on the end iterator
textViewBackwardDisplayLine textView iter = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_view_backward_display_line textView' iter'
    let result' = (/= 0) result
    touchManagedPtr textView
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewBackwardDisplayLineMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> m Bool), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewBackwardDisplayLineMethodInfo a signature where
    overloadedMethod = textViewBackwardDisplayLine

instance O.OverloadedMethodInfo TextViewBackwardDisplayLineMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewBackwardDisplayLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewBackwardDisplayLine"
        })


#endif

-- method TextView::backward_display_line_start
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_backward_display_line_start" gtk_text_view_backward_display_line_start :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves the given /@iter@/ backward to the next display line start.
-- A display line is different from a paragraph. Paragraphs are
-- separated by newlines or other paragraph separator characters.
-- Display lines are created by line-wrapping a paragraph. If
-- wrapping is turned off, display lines and paragraphs will be the
-- same. Display lines are divided differently for each view, since
-- they depend on the view’s width; paragraphs are the same in all
-- views, since they depend on the contents of the t'GI.Gtk.Objects.TextBuffer.TextBuffer'.
textViewBackwardDisplayLineStart ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ was moved and is not on the end iterator
textViewBackwardDisplayLineStart textView iter = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_view_backward_display_line_start textView' iter'
    let result' = (/= 0) result
    touchManagedPtr textView
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewBackwardDisplayLineStartMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> m Bool), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewBackwardDisplayLineStartMethodInfo a signature where
    overloadedMethod = textViewBackwardDisplayLineStart

instance O.OverloadedMethodInfo TextViewBackwardDisplayLineStartMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewBackwardDisplayLineStart",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewBackwardDisplayLineStart"
        })


#endif

-- method TextView::buffer_to_window_coords
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "win"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextWindowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkTextWindowType, except %GTK_TEXT_WINDOW_PRIVATE"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "buffer_x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "buffer x coordinate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "buffer_y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "buffer y coordinate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "window_x"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "window x coordinate return location or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "window_y"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "window y coordinate return location or %NULL"
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

foreign import ccall "gtk_text_view_buffer_to_window_coords" gtk_text_view_buffer_to_window_coords :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    CUInt ->                                -- win : TInterface (Name {namespace = "Gtk", name = "TextWindowType"})
    Int32 ->                                -- buffer_x : TBasicType TInt
    Int32 ->                                -- buffer_y : TBasicType TInt
    Ptr Int32 ->                            -- window_x : TBasicType TInt
    Ptr Int32 ->                            -- window_y : TBasicType TInt
    IO ()

-- | Converts coordinate (/@bufferX@/, /@bufferY@/) to coordinates for the window
-- /@win@/, and stores the result in (/@windowX@/, /@windowY@/).
-- 
-- Note that you can’t convert coordinates for a nonexisting window (see
-- 'GI.Gtk.Objects.TextView.textViewSetBorderWindowSize').
textViewBufferToWindowCoords ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.Enums.TextWindowType
    -- ^ /@win@/: a t'GI.Gtk.Enums.TextWindowType', except 'GI.Gtk.Enums.TextWindowTypePrivate'
    -> Int32
    -- ^ /@bufferX@/: buffer x coordinate
    -> Int32
    -- ^ /@bufferY@/: buffer y coordinate
    -> m ((Int32, Int32))
textViewBufferToWindowCoords textView win bufferX bufferY = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    let win' = (fromIntegral . fromEnum) win
    windowX <- allocMem :: IO (Ptr Int32)
    windowY <- allocMem :: IO (Ptr Int32)
    gtk_text_view_buffer_to_window_coords textView' win' bufferX bufferY windowX windowY
    windowX' <- peek windowX
    windowY' <- peek windowY
    touchManagedPtr textView
    freeMem windowX
    freeMem windowY
    return (windowX', windowY')

#if defined(ENABLE_OVERLOADING)
data TextViewBufferToWindowCoordsMethodInfo
instance (signature ~ (Gtk.Enums.TextWindowType -> Int32 -> Int32 -> m ((Int32, Int32))), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewBufferToWindowCoordsMethodInfo a signature where
    overloadedMethod = textViewBufferToWindowCoords

instance O.OverloadedMethodInfo TextViewBufferToWindowCoordsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewBufferToWindowCoords",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewBufferToWindowCoords"
        })


#endif

-- method TextView::forward_display_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_forward_display_line" gtk_text_view_forward_display_line :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves the given /@iter@/ forward by one display (wrapped) line.
-- A display line is different from a paragraph. Paragraphs are
-- separated by newlines or other paragraph separator characters.
-- Display lines are created by line-wrapping a paragraph. If
-- wrapping is turned off, display lines and paragraphs will be the
-- same. Display lines are divided differently for each view, since
-- they depend on the view’s width; paragraphs are the same in all
-- views, since they depend on the contents of the t'GI.Gtk.Objects.TextBuffer.TextBuffer'.
textViewForwardDisplayLine ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ was moved and is not on the end iterator
textViewForwardDisplayLine textView iter = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_view_forward_display_line textView' iter'
    let result' = (/= 0) result
    touchManagedPtr textView
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewForwardDisplayLineMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> m Bool), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewForwardDisplayLineMethodInfo a signature where
    overloadedMethod = textViewForwardDisplayLine

instance O.OverloadedMethodInfo TextViewForwardDisplayLineMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewForwardDisplayLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewForwardDisplayLine"
        })


#endif

-- method TextView::forward_display_line_end
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_forward_display_line_end" gtk_text_view_forward_display_line_end :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Moves the given /@iter@/ forward to the next display line end.
-- A display line is different from a paragraph. Paragraphs are
-- separated by newlines or other paragraph separator characters.
-- Display lines are created by line-wrapping a paragraph. If
-- wrapping is turned off, display lines and paragraphs will be the
-- same. Display lines are divided differently for each view, since
-- they depend on the view’s width; paragraphs are the same in all
-- views, since they depend on the contents of the t'GI.Gtk.Objects.TextBuffer.TextBuffer'.
textViewForwardDisplayLineEnd ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ was moved and is not on the end iterator
textViewForwardDisplayLineEnd textView iter = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_view_forward_display_line_end textView' iter'
    let result' = (/= 0) result
    touchManagedPtr textView
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewForwardDisplayLineEndMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> m Bool), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewForwardDisplayLineEndMethodInfo a signature where
    overloadedMethod = textViewForwardDisplayLineEnd

instance O.OverloadedMethodInfo TextViewForwardDisplayLineEndMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewForwardDisplayLineEnd",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewForwardDisplayLineEnd"
        })


#endif

-- method TextView::get_accepts_tab
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTextView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_get_accepts_tab" gtk_text_view_get_accepts_tab :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO CInt

-- | Returns whether pressing the Tab key inserts a tab characters.
-- 'GI.Gtk.Objects.TextView.textViewSetAcceptsTab'.
-- 
-- /Since: 2.4/
textViewGetAcceptsTab ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: A t'GI.Gtk.Objects.TextView.TextView'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if pressing the Tab key inserts a tab character,
    --   'P.False' if pressing the Tab key moves the keyboard focus.
textViewGetAcceptsTab textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_accepts_tab textView'
    let result' = (/= 0) result
    touchManagedPtr textView
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewGetAcceptsTabMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetAcceptsTabMethodInfo a signature where
    overloadedMethod = textViewGetAcceptsTab

instance O.OverloadedMethodInfo TextViewGetAcceptsTabMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetAcceptsTab",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetAcceptsTab"
        })


#endif

-- method TextView::get_border_window_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextWindowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "window to return size from"
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

foreign import ccall "gtk_text_view_get_border_window_size" gtk_text_view_get_border_window_size :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    CUInt ->                                -- type : TInterface (Name {namespace = "Gtk", name = "TextWindowType"})
    IO Int32

-- | Gets the width of the specified border window. See
-- 'GI.Gtk.Objects.TextView.textViewSetBorderWindowSize'.
textViewGetBorderWindowSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.Enums.TextWindowType
    -- ^ /@type@/: window to return size from
    -> m Int32
    -- ^ __Returns:__ width of window
textViewGetBorderWindowSize textView type_ = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    let type_' = (fromIntegral . fromEnum) type_
    result <- gtk_text_view_get_border_window_size textView' type_'
    touchManagedPtr textView
    return result

#if defined(ENABLE_OVERLOADING)
data TextViewGetBorderWindowSizeMethodInfo
instance (signature ~ (Gtk.Enums.TextWindowType -> m Int32), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetBorderWindowSizeMethodInfo a signature where
    overloadedMethod = textViewGetBorderWindowSize

instance O.OverloadedMethodInfo TextViewGetBorderWindowSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetBorderWindowSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetBorderWindowSize"
        })


#endif

-- method TextView::get_bottom_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_get_bottom_margin" gtk_text_view_get_bottom_margin :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO Int32

-- | Gets the bottom margin for text in the /@textView@/.
-- 
-- /Since: 3.18/
textViewGetBottomMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Int32
    -- ^ __Returns:__ bottom margin in pixels
textViewGetBottomMargin textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_bottom_margin textView'
    touchManagedPtr textView
    return result

#if defined(ENABLE_OVERLOADING)
data TextViewGetBottomMarginMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetBottomMarginMethodInfo a signature where
    overloadedMethod = textViewGetBottomMargin

instance O.OverloadedMethodInfo TextViewGetBottomMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetBottomMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetBottomMargin"
        })


#endif

-- method TextView::get_buffer
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TextBuffer" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_view_get_buffer" gtk_text_view_get_buffer :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO (Ptr Gtk.TextBuffer.TextBuffer)

-- | Returns the t'GI.Gtk.Objects.TextBuffer.TextBuffer' being displayed by this text view.
-- The reference count on the buffer is not incremented; the caller
-- of this function won’t own a new reference.
textViewGetBuffer ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Gtk.TextBuffer.TextBuffer
    -- ^ __Returns:__ a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
textViewGetBuffer textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_buffer textView'
    checkUnexpectedReturnNULL "textViewGetBuffer" result
    result' <- (newObject Gtk.TextBuffer.TextBuffer) result
    touchManagedPtr textView
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewGetBufferMethodInfo
instance (signature ~ (m Gtk.TextBuffer.TextBuffer), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetBufferMethodInfo a signature where
    overloadedMethod = textViewGetBuffer

instance O.OverloadedMethodInfo TextViewGetBufferMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetBuffer",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetBuffer"
        })


#endif

-- method TextView::get_cursor_locations
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "strong"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to store the strong\n    cursor position (may be %NULL)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "weak"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to store the weak\n    cursor position (may be %NULL)"
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_view_get_cursor_locations" gtk_text_view_get_cursor_locations :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gdk.Rectangle.Rectangle ->          -- strong : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    Ptr Gdk.Rectangle.Rectangle ->          -- weak : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO ()

-- | Given an /@iter@/ within a text layout, determine the positions of the
-- strong and weak cursors if the insertion point is at that
-- iterator. The position of each cursor is stored as a zero-width
-- rectangle. The strong cursor location is the location where
-- characters of the directionality equal to the base direction of the
-- paragraph are inserted.  The weak cursor location is the location
-- where characters of the directionality opposite to the base
-- direction of the paragraph are inserted.
-- 
-- If /@iter@/ is 'P.Nothing', the actual cursor position is used.
-- 
-- Note that if /@iter@/ happens to be the actual cursor position, and
-- there is currently an IM preedit sequence being entered, the
-- returned locations will be adjusted to account for the preedit
-- cursor’s offset within the preedit sequence.
-- 
-- The rectangle position is in buffer coordinates; use
-- 'GI.Gtk.Objects.TextView.textViewBufferToWindowCoords' to convert these
-- coordinates to coordinates for one of the windows in the text view.
-- 
-- /Since: 3.0/
textViewGetCursorLocations ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Maybe (Gtk.TextIter.TextIter)
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m ((Gdk.Rectangle.Rectangle, Gdk.Rectangle.Rectangle))
textViewGetCursorLocations textView iter = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    maybeIter <- case iter of
        Nothing -> return nullPtr
        Just jIter -> do
            jIter' <- unsafeManagedPtrGetPtr jIter
            return jIter'
    strong <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    weak <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    gtk_text_view_get_cursor_locations textView' maybeIter strong weak
    strong' <- (wrapBoxed Gdk.Rectangle.Rectangle) strong
    weak' <- (wrapBoxed Gdk.Rectangle.Rectangle) weak
    touchManagedPtr textView
    whenJust iter touchManagedPtr
    return (strong', weak')

#if defined(ENABLE_OVERLOADING)
data TextViewGetCursorLocationsMethodInfo
instance (signature ~ (Maybe (Gtk.TextIter.TextIter) -> m ((Gdk.Rectangle.Rectangle, Gdk.Rectangle.Rectangle))), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetCursorLocationsMethodInfo a signature where
    overloadedMethod = textViewGetCursorLocations

instance O.OverloadedMethodInfo TextViewGetCursorLocationsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetCursorLocations",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetCursorLocations"
        })


#endif

-- method TextView::get_cursor_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_get_cursor_visible" gtk_text_view_get_cursor_visible :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO CInt

-- | Find out whether the cursor should be displayed.
textViewGetCursorVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Bool
    -- ^ __Returns:__ whether the insertion mark is visible
textViewGetCursorVisible textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_cursor_visible textView'
    let result' = (/= 0) result
    touchManagedPtr textView
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewGetCursorVisibleMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetCursorVisibleMethodInfo a signature where
    overloadedMethod = textViewGetCursorVisible

instance O.OverloadedMethodInfo TextViewGetCursorVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetCursorVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetCursorVisible"
        })


#endif

-- method TextView::get_default_attributes
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "TextAttributes" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_view_get_default_attributes" gtk_text_view_get_default_attributes :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO (Ptr Gtk.TextAttributes.TextAttributes)

-- | Obtains a copy of the default text attributes. These are the
-- attributes used for text unless a tag overrides them.
-- You’d typically pass the default attributes in to
-- 'GI.Gtk.Structs.TextIter.textIterGetAttributes' in order to get the
-- attributes in effect at a given text position.
-- 
-- The return value is a copy owned by the caller of this function,
-- and should be freed with 'GI.Gtk.Structs.TextAttributes.textAttributesUnref'.
textViewGetDefaultAttributes ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Gtk.TextAttributes.TextAttributes
    -- ^ __Returns:__ a new t'GI.Gtk.Structs.TextAttributes.TextAttributes'
textViewGetDefaultAttributes textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_default_attributes textView'
    checkUnexpectedReturnNULL "textViewGetDefaultAttributes" result
    result' <- (wrapBoxed Gtk.TextAttributes.TextAttributes) result
    touchManagedPtr textView
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewGetDefaultAttributesMethodInfo
instance (signature ~ (m Gtk.TextAttributes.TextAttributes), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetDefaultAttributesMethodInfo a signature where
    overloadedMethod = textViewGetDefaultAttributes

instance O.OverloadedMethodInfo TextViewGetDefaultAttributesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetDefaultAttributes",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetDefaultAttributes"
        })


#endif

-- method TextView::get_editable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_get_editable" gtk_text_view_get_editable :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO CInt

-- | Returns the default editability of the t'GI.Gtk.Objects.TextView.TextView'. Tags in the
-- buffer may override this setting for some ranges of text.
textViewGetEditable ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Bool
    -- ^ __Returns:__ whether text is editable by default
textViewGetEditable textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_editable textView'
    let result' = (/= 0) result
    touchManagedPtr textView
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewGetEditableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetEditableMethodInfo a signature where
    overloadedMethod = textViewGetEditable

instance O.OverloadedMethodInfo TextViewGetEditableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetEditable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetEditable"
        })


#endif

-- method TextView::get_hadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Adjustment" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_view_get_hadjustment" gtk_text_view_get_hadjustment :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO (Ptr Gtk.Adjustment.Adjustment)

{-# DEPRECATED textViewGetHadjustment ["(Since version 3.0)","Use 'GI.Gtk.Interfaces.Scrollable.scrollableGetHadjustment'"] #-}
-- | Gets the horizontal-scrolling t'GI.Gtk.Objects.Adjustment.Adjustment'.
-- 
-- /Since: 2.22/
textViewGetHadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Gtk.Adjustment.Adjustment
    -- ^ __Returns:__ pointer to the horizontal t'GI.Gtk.Objects.Adjustment.Adjustment'
textViewGetHadjustment textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_hadjustment textView'
    checkUnexpectedReturnNULL "textViewGetHadjustment" result
    result' <- (newObject Gtk.Adjustment.Adjustment) result
    touchManagedPtr textView
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewGetHadjustmentMethodInfo
instance (signature ~ (m Gtk.Adjustment.Adjustment), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetHadjustmentMethodInfo a signature where
    overloadedMethod = textViewGetHadjustment

instance O.OverloadedMethodInfo TextViewGetHadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetHadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetHadjustment"
        })


#endif

-- method TextView::get_indent
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_get_indent" gtk_text_view_get_indent :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO Int32

-- | Gets the default indentation of paragraphs in /@textView@/.
-- Tags in the view’s buffer may override the default.
-- The indentation may be negative.
textViewGetIndent ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Int32
    -- ^ __Returns:__ number of pixels of indentation
textViewGetIndent textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_indent textView'
    touchManagedPtr textView
    return result

#if defined(ENABLE_OVERLOADING)
data TextViewGetIndentMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetIndentMethodInfo a signature where
    overloadedMethod = textViewGetIndent

instance O.OverloadedMethodInfo TextViewGetIndentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetIndent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetIndent"
        })


#endif

-- method TextView::get_input_hints
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "InputHints" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_view_get_input_hints" gtk_text_view_get_input_hints :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO CUInt

-- | Gets the value of the [TextView:inputHints]("GI.Gtk.Objects.TextView#g:attr:inputHints") property.
-- 
-- /Since: 3.6/
textViewGetInputHints ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m [Gtk.Flags.InputHints]
textViewGetInputHints textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_input_hints textView'
    let result' = wordToGFlags result
    touchManagedPtr textView
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewGetInputHintsMethodInfo
instance (signature ~ (m [Gtk.Flags.InputHints]), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetInputHintsMethodInfo a signature where
    overloadedMethod = textViewGetInputHints

instance O.OverloadedMethodInfo TextViewGetInputHintsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetInputHints",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetInputHints"
        })


#endif

-- method TextView::get_input_purpose
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "InputPurpose" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_view_get_input_purpose" gtk_text_view_get_input_purpose :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO CUInt

-- | Gets the value of the [TextView:inputPurpose]("GI.Gtk.Objects.TextView#g:attr:inputPurpose") property.
-- 
-- /Since: 3.6/
textViewGetInputPurpose ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Gtk.Enums.InputPurpose
textViewGetInputPurpose textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_input_purpose textView'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr textView
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewGetInputPurposeMethodInfo
instance (signature ~ (m Gtk.Enums.InputPurpose), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetInputPurposeMethodInfo a signature where
    overloadedMethod = textViewGetInputPurpose

instance O.OverloadedMethodInfo TextViewGetInputPurposeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetInputPurpose",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetInputPurpose"
        })


#endif

-- method TextView::get_iter_at_location
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "x position, in buffer coordinates"
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
--                 { rawDocText = Just "y position, in buffer coordinates"
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

foreign import ccall "gtk_text_view_get_iter_at_location" gtk_text_view_get_iter_at_location :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    IO CInt

-- | Retrieves the iterator at buffer coordinates /@x@/ and /@y@/. Buffer
-- coordinates are coordinates for the entire buffer, not just the
-- currently-displayed portion.  If you have coordinates from an
-- event, you have to convert those to buffer coordinates with
-- 'GI.Gtk.Objects.TextView.textViewWindowToBufferCoords'.
textViewGetIterAtLocation ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Int32
    -- ^ /@x@/: x position, in buffer coordinates
    -> Int32
    -- ^ /@y@/: y position, in buffer coordinates
    -> m ((Bool, Gtk.TextIter.TextIter))
    -- ^ __Returns:__ 'P.True' if the position is over text
textViewGetIterAtLocation textView x y = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    iter <- SP.callocBoxedBytes 80 :: IO (Ptr Gtk.TextIter.TextIter)
    result <- gtk_text_view_get_iter_at_location textView' iter x y
    let result' = (/= 0) result
    iter' <- (wrapBoxed Gtk.TextIter.TextIter) iter
    touchManagedPtr textView
    return (result', iter')

#if defined(ENABLE_OVERLOADING)
data TextViewGetIterAtLocationMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ((Bool, Gtk.TextIter.TextIter))), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetIterAtLocationMethodInfo a signature where
    overloadedMethod = textViewGetIterAtLocation

instance O.OverloadedMethodInfo TextViewGetIterAtLocationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetIterAtLocation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetIterAtLocation"
        })


#endif

-- method TextView::get_iter_at_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "trailing"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "if non-%NULL, location to store an integer indicating where\n   in the grapheme the user clicked. It will either be\n   zero, or the number of characters in the grapheme.\n   0 represents the trailing edge of the grapheme."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "x position, in buffer coordinates"
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
--                 { rawDocText = Just "y position, in buffer coordinates"
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

foreign import ccall "gtk_text_view_get_iter_at_position" gtk_text_view_get_iter_at_position :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Int32 ->                            -- trailing : TBasicType TInt
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    IO CInt

-- | Retrieves the iterator pointing to the character at buffer
-- coordinates /@x@/ and /@y@/. Buffer coordinates are coordinates for
-- the entire buffer, not just the currently-displayed portion.
-- If you have coordinates from an event, you have to convert
-- those to buffer coordinates with
-- 'GI.Gtk.Objects.TextView.textViewWindowToBufferCoords'.
-- 
-- Note that this is different from 'GI.Gtk.Objects.TextView.textViewGetIterAtLocation',
-- which returns cursor locations, i.e. positions between
-- characters.
-- 
-- /Since: 2.6/
textViewGetIterAtPosition ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Int32
    -- ^ /@x@/: x position, in buffer coordinates
    -> Int32
    -- ^ /@y@/: y position, in buffer coordinates
    -> m ((Bool, Gtk.TextIter.TextIter, Int32))
    -- ^ __Returns:__ 'P.True' if the position is over text
textViewGetIterAtPosition textView x y = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    iter <- SP.callocBoxedBytes 80 :: IO (Ptr Gtk.TextIter.TextIter)
    trailing <- allocMem :: IO (Ptr Int32)
    result <- gtk_text_view_get_iter_at_position textView' iter trailing x y
    let result' = (/= 0) result
    iter' <- (wrapBoxed Gtk.TextIter.TextIter) iter
    trailing' <- peek trailing
    touchManagedPtr textView
    freeMem trailing
    return (result', iter', trailing')

#if defined(ENABLE_OVERLOADING)
data TextViewGetIterAtPositionMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ((Bool, Gtk.TextIter.TextIter, Int32))), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetIterAtPositionMethodInfo a signature where
    overloadedMethod = textViewGetIterAtPosition

instance O.OverloadedMethodInfo TextViewGetIterAtPositionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetIterAtPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetIterAtPosition"
        })


#endif

-- method TextView::get_iter_location
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "location"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "bounds of the character at @iter"
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_view_get_iter_location" gtk_text_view_get_iter_location :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Gdk.Rectangle.Rectangle ->          -- location : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO ()

-- | Gets a rectangle which roughly contains the character at /@iter@/.
-- The rectangle position is in buffer coordinates; use
-- 'GI.Gtk.Objects.TextView.textViewBufferToWindowCoords' to convert these
-- coordinates to coordinates for one of the windows in the text view.
textViewGetIterLocation ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m (Gdk.Rectangle.Rectangle)
textViewGetIterLocation textView iter = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    iter' <- unsafeManagedPtrGetPtr iter
    location <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    gtk_text_view_get_iter_location textView' iter' location
    location' <- (wrapBoxed Gdk.Rectangle.Rectangle) location
    touchManagedPtr textView
    touchManagedPtr iter
    return location'

#if defined(ENABLE_OVERLOADING)
data TextViewGetIterLocationMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> m (Gdk.Rectangle.Rectangle)), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetIterLocationMethodInfo a signature where
    overloadedMethod = textViewGetIterLocation

instance O.OverloadedMethodInfo TextViewGetIterLocationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetIterLocation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetIterLocation"
        })


#endif

-- method TextView::get_justification
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "Justification" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_view_get_justification" gtk_text_view_get_justification :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO CUInt

-- | Gets the default justification of paragraphs in /@textView@/.
-- Tags in the buffer may override the default.
textViewGetJustification ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Gtk.Enums.Justification
    -- ^ __Returns:__ default justification
textViewGetJustification textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_justification textView'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr textView
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewGetJustificationMethodInfo
instance (signature ~ (m Gtk.Enums.Justification), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetJustificationMethodInfo a signature where
    overloadedMethod = textViewGetJustification

instance O.OverloadedMethodInfo TextViewGetJustificationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetJustification",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetJustification"
        })


#endif

-- method TextView::get_left_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_get_left_margin" gtk_text_view_get_left_margin :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO Int32

-- | Gets the default left margin size of paragraphs in the /@textView@/.
-- Tags in the buffer may override the default.
textViewGetLeftMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Int32
    -- ^ __Returns:__ left margin in pixels
textViewGetLeftMargin textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_left_margin textView'
    touchManagedPtr textView
    return result

#if defined(ENABLE_OVERLOADING)
data TextViewGetLeftMarginMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetLeftMarginMethodInfo a signature where
    overloadedMethod = textViewGetLeftMargin

instance O.OverloadedMethodInfo TextViewGetLeftMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetLeftMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetLeftMargin"
        })


#endif

-- method TextView::get_line_at_y
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "target_iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a y coordinate" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "line_top"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for top coordinate of the line"
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

foreign import ccall "gtk_text_view_get_line_at_y" gtk_text_view_get_line_at_y :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextIter.TextIter ->            -- target_iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- y : TBasicType TInt
    Ptr Int32 ->                            -- line_top : TBasicType TInt
    IO ()

-- | Gets the t'GI.Gtk.Structs.TextIter.TextIter' at the start of the line containing
-- the coordinate /@y@/. /@y@/ is in buffer coordinates, convert from
-- window coordinates with 'GI.Gtk.Objects.TextView.textViewWindowToBufferCoords'.
-- If non-'P.Nothing', /@lineTop@/ will be filled with the coordinate of the top
-- edge of the line.
textViewGetLineAtY ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Int32
    -- ^ /@y@/: a y coordinate
    -> m ((Gtk.TextIter.TextIter, Int32))
textViewGetLineAtY textView y = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    targetIter <- SP.callocBoxedBytes 80 :: IO (Ptr Gtk.TextIter.TextIter)
    lineTop <- allocMem :: IO (Ptr Int32)
    gtk_text_view_get_line_at_y textView' targetIter y lineTop
    targetIter' <- (wrapBoxed Gtk.TextIter.TextIter) targetIter
    lineTop' <- peek lineTop
    touchManagedPtr textView
    freeMem lineTop
    return (targetIter', lineTop')

#if defined(ENABLE_OVERLOADING)
data TextViewGetLineAtYMethodInfo
instance (signature ~ (Int32 -> m ((Gtk.TextIter.TextIter, Int32))), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetLineAtYMethodInfo a signature where
    overloadedMethod = textViewGetLineAtY

instance O.OverloadedMethodInfo TextViewGetLineAtYMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetLineAtY",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetLineAtY"
        })


#endif

-- method TextView::get_line_yrange
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for a y coordinate"
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
--                 { rawDocText = Just "return location for a height"
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

foreign import ccall "gtk_text_view_get_line_yrange" gtk_text_view_get_line_yrange :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Ptr Int32 ->                            -- y : TBasicType TInt
    Ptr Int32 ->                            -- height : TBasicType TInt
    IO ()

-- | Gets the y coordinate of the top of the line containing /@iter@/,
-- and the height of the line. The coordinate is a buffer coordinate;
-- convert to window coordinates with 'GI.Gtk.Objects.TextView.textViewBufferToWindowCoords'.
textViewGetLineYrange ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m ((Int32, Int32))
textViewGetLineYrange textView iter = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    iter' <- unsafeManagedPtrGetPtr iter
    y <- allocMem :: IO (Ptr Int32)
    height <- allocMem :: IO (Ptr Int32)
    gtk_text_view_get_line_yrange textView' iter' y height
    y' <- peek y
    height' <- peek height
    touchManagedPtr textView
    touchManagedPtr iter
    freeMem y
    freeMem height
    return (y', height')

#if defined(ENABLE_OVERLOADING)
data TextViewGetLineYrangeMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> m ((Int32, Int32))), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetLineYrangeMethodInfo a signature where
    overloadedMethod = textViewGetLineYrange

instance O.OverloadedMethodInfo TextViewGetLineYrangeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetLineYrange",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetLineYrange"
        })


#endif

-- method TextView::get_monospace
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_get_monospace" gtk_text_view_get_monospace :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO CInt

-- | Gets the value of the [TextView:monospace]("GI.Gtk.Objects.TextView#g:attr:monospace") property.
-- 
-- /Since: 3.16/
textViewGetMonospace ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if monospace fonts are desired
textViewGetMonospace textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_monospace textView'
    let result' = (/= 0) result
    touchManagedPtr textView
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewGetMonospaceMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetMonospaceMethodInfo a signature where
    overloadedMethod = textViewGetMonospace

instance O.OverloadedMethodInfo TextViewGetMonospaceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetMonospace",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetMonospace"
        })


#endif

-- method TextView::get_overwrite
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_get_overwrite" gtk_text_view_get_overwrite :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO CInt

-- | Returns whether the t'GI.Gtk.Objects.TextView.TextView' is in overwrite mode or not.
-- 
-- /Since: 2.4/
textViewGetOverwrite ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Bool
    -- ^ __Returns:__ whether /@textView@/ is in overwrite mode or not.
textViewGetOverwrite textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_overwrite textView'
    let result' = (/= 0) result
    touchManagedPtr textView
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewGetOverwriteMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetOverwriteMethodInfo a signature where
    overloadedMethod = textViewGetOverwrite

instance O.OverloadedMethodInfo TextViewGetOverwriteMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetOverwrite",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetOverwrite"
        })


#endif

-- method TextView::get_pixels_above_lines
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_get_pixels_above_lines" gtk_text_view_get_pixels_above_lines :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO Int32

-- | Gets the default number of pixels to put above paragraphs.
-- Adding this function with 'GI.Gtk.Objects.TextView.textViewGetPixelsBelowLines'
-- is equal to the line space between each paragraph.
textViewGetPixelsAboveLines ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Int32
    -- ^ __Returns:__ default number of pixels above paragraphs
textViewGetPixelsAboveLines textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_pixels_above_lines textView'
    touchManagedPtr textView
    return result

#if defined(ENABLE_OVERLOADING)
data TextViewGetPixelsAboveLinesMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetPixelsAboveLinesMethodInfo a signature where
    overloadedMethod = textViewGetPixelsAboveLines

instance O.OverloadedMethodInfo TextViewGetPixelsAboveLinesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetPixelsAboveLines",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetPixelsAboveLines"
        })


#endif

-- method TextView::get_pixels_below_lines
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_get_pixels_below_lines" gtk_text_view_get_pixels_below_lines :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO Int32

-- | Gets the value set by 'GI.Gtk.Objects.TextView.textViewSetPixelsBelowLines'.
-- 
-- The line space is the sum of the value returned by this function and the
-- value returned by 'GI.Gtk.Objects.TextView.textViewGetPixelsAboveLines'.
textViewGetPixelsBelowLines ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Int32
    -- ^ __Returns:__ default number of blank pixels below paragraphs
textViewGetPixelsBelowLines textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_pixels_below_lines textView'
    touchManagedPtr textView
    return result

#if defined(ENABLE_OVERLOADING)
data TextViewGetPixelsBelowLinesMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetPixelsBelowLinesMethodInfo a signature where
    overloadedMethod = textViewGetPixelsBelowLines

instance O.OverloadedMethodInfo TextViewGetPixelsBelowLinesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetPixelsBelowLines",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetPixelsBelowLines"
        })


#endif

-- method TextView::get_pixels_inside_wrap
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_get_pixels_inside_wrap" gtk_text_view_get_pixels_inside_wrap :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO Int32

-- | Gets the value set by 'GI.Gtk.Objects.TextView.textViewSetPixelsInsideWrap'.
textViewGetPixelsInsideWrap ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Int32
    -- ^ __Returns:__ default number of pixels of blank space between wrapped lines
textViewGetPixelsInsideWrap textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_pixels_inside_wrap textView'
    touchManagedPtr textView
    return result

#if defined(ENABLE_OVERLOADING)
data TextViewGetPixelsInsideWrapMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetPixelsInsideWrapMethodInfo a signature where
    overloadedMethod = textViewGetPixelsInsideWrap

instance O.OverloadedMethodInfo TextViewGetPixelsInsideWrapMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetPixelsInsideWrap",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetPixelsInsideWrap"
        })


#endif

-- method TextView::get_right_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_get_right_margin" gtk_text_view_get_right_margin :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO Int32

-- | Gets the default right margin for text in /@textView@/. Tags
-- in the buffer may override the default.
textViewGetRightMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Int32
    -- ^ __Returns:__ right margin in pixels
textViewGetRightMargin textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_right_margin textView'
    touchManagedPtr textView
    return result

#if defined(ENABLE_OVERLOADING)
data TextViewGetRightMarginMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetRightMarginMethodInfo a signature where
    overloadedMethod = textViewGetRightMargin

instance O.OverloadedMethodInfo TextViewGetRightMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetRightMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetRightMargin"
        })


#endif

-- method TextView::get_tabs
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Pango" , name = "TabArray" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_view_get_tabs" gtk_text_view_get_tabs :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO (Ptr Pango.TabArray.TabArray)

-- | Gets the default tabs for /@textView@/. Tags in the buffer may
-- override the defaults. The returned array will be 'P.Nothing' if
-- “standard” (8-space) tabs are used. Free the return value
-- with 'GI.Pango.Structs.TabArray.tabArrayFree'.
textViewGetTabs ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m (Maybe Pango.TabArray.TabArray)
    -- ^ __Returns:__ copy of default tab array, or 'P.Nothing' if
    --    “standard\" tabs are used; must be freed with 'GI.Pango.Structs.TabArray.tabArrayFree'.
textViewGetTabs textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_tabs textView'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapBoxed Pango.TabArray.TabArray) result'
        return result''
    touchManagedPtr textView
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data TextViewGetTabsMethodInfo
instance (signature ~ (m (Maybe Pango.TabArray.TabArray)), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetTabsMethodInfo a signature where
    overloadedMethod = textViewGetTabs

instance O.OverloadedMethodInfo TextViewGetTabsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetTabs",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetTabs"
        })


#endif

-- method TextView::get_top_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_get_top_margin" gtk_text_view_get_top_margin :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO Int32

-- | Gets the top margin for text in the /@textView@/.
-- 
-- /Since: 3.18/
textViewGetTopMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Int32
    -- ^ __Returns:__ top margin in pixels
textViewGetTopMargin textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_top_margin textView'
    touchManagedPtr textView
    return result

#if defined(ENABLE_OVERLOADING)
data TextViewGetTopMarginMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetTopMarginMethodInfo a signature where
    overloadedMethod = textViewGetTopMargin

instance O.OverloadedMethodInfo TextViewGetTopMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetTopMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetTopMargin"
        })


#endif

-- method TextView::get_vadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Adjustment" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_view_get_vadjustment" gtk_text_view_get_vadjustment :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO (Ptr Gtk.Adjustment.Adjustment)

{-# DEPRECATED textViewGetVadjustment ["(Since version 3.0)","Use 'GI.Gtk.Interfaces.Scrollable.scrollableGetVadjustment'"] #-}
-- | Gets the vertical-scrolling t'GI.Gtk.Objects.Adjustment.Adjustment'.
-- 
-- /Since: 2.22/
textViewGetVadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Gtk.Adjustment.Adjustment
    -- ^ __Returns:__ pointer to the vertical t'GI.Gtk.Objects.Adjustment.Adjustment'
textViewGetVadjustment textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_vadjustment textView'
    checkUnexpectedReturnNULL "textViewGetVadjustment" result
    result' <- (newObject Gtk.Adjustment.Adjustment) result
    touchManagedPtr textView
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewGetVadjustmentMethodInfo
instance (signature ~ (m Gtk.Adjustment.Adjustment), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetVadjustmentMethodInfo a signature where
    overloadedMethod = textViewGetVadjustment

instance O.OverloadedMethodInfo TextViewGetVadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetVadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetVadjustment"
        })


#endif

-- method TextView::get_visible_rect
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "visible_rect"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "rectangle to fill" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_view_get_visible_rect" gtk_text_view_get_visible_rect :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gdk.Rectangle.Rectangle ->          -- visible_rect : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO ()

-- | Fills /@visibleRect@/ with the currently-visible
-- region of the buffer, in buffer coordinates. Convert to window coordinates
-- with 'GI.Gtk.Objects.TextView.textViewBufferToWindowCoords'.
textViewGetVisibleRect ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m (Gdk.Rectangle.Rectangle)
textViewGetVisibleRect textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    visibleRect <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    gtk_text_view_get_visible_rect textView' visibleRect
    visibleRect' <- (wrapBoxed Gdk.Rectangle.Rectangle) visibleRect
    touchManagedPtr textView
    return visibleRect'

#if defined(ENABLE_OVERLOADING)
data TextViewGetVisibleRectMethodInfo
instance (signature ~ (m (Gdk.Rectangle.Rectangle)), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetVisibleRectMethodInfo a signature where
    overloadedMethod = textViewGetVisibleRect

instance O.OverloadedMethodInfo TextViewGetVisibleRectMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetVisibleRect",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetVisibleRect"
        })


#endif

-- method TextView::get_window
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "win"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextWindowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "window to get" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Window" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_view_get_window" gtk_text_view_get_window :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    CUInt ->                                -- win : TInterface (Name {namespace = "Gtk", name = "TextWindowType"})
    IO (Ptr Gdk.Window.Window)

-- | Retrieves the t'GI.Gdk.Objects.Window.Window' corresponding to an area of the text view;
-- possible windows include the overall widget window, child windows
-- on the left, right, top, bottom, and the window that displays the
-- text buffer. Windows are 'P.Nothing' and nonexistent if their width or
-- height is 0, and are nonexistent before the widget has been
-- realized.
textViewGetWindow ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.Enums.TextWindowType
    -- ^ /@win@/: window to get
    -> m (Maybe Gdk.Window.Window)
    -- ^ __Returns:__ a t'GI.Gdk.Objects.Window.Window', or 'P.Nothing'
textViewGetWindow textView win = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    let win' = (fromIntegral . fromEnum) win
    result <- gtk_text_view_get_window textView' win'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gdk.Window.Window) result'
        return result''
    touchManagedPtr textView
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data TextViewGetWindowMethodInfo
instance (signature ~ (Gtk.Enums.TextWindowType -> m (Maybe Gdk.Window.Window)), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetWindowMethodInfo a signature where
    overloadedMethod = textViewGetWindow

instance O.OverloadedMethodInfo TextViewGetWindowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetWindow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetWindow"
        })


#endif

-- method TextView::get_window_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a window type" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "TextWindowType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_view_get_window_type" gtk_text_view_get_window_type :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gdk.Window.Window ->                -- window : TInterface (Name {namespace = "Gdk", name = "Window"})
    IO CUInt

-- | Usually used to find out which window an event corresponds to.
-- 
-- If you connect to an event signal on /@textView@/, this function
-- should be called on @event->window@ to see which window it was.
textViewGetWindowType ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a, Gdk.Window.IsWindow b) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> b
    -- ^ /@window@/: a window type
    -> m Gtk.Enums.TextWindowType
    -- ^ __Returns:__ the window type.
textViewGetWindowType textView window = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    window' <- unsafeManagedPtrCastPtr window
    result <- gtk_text_view_get_window_type textView' window'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr textView
    touchManagedPtr window
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewGetWindowTypeMethodInfo
instance (signature ~ (b -> m Gtk.Enums.TextWindowType), MonadIO m, IsTextView a, Gdk.Window.IsWindow b) => O.OverloadedMethod TextViewGetWindowTypeMethodInfo a signature where
    overloadedMethod = textViewGetWindowType

instance O.OverloadedMethodInfo TextViewGetWindowTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetWindowType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetWindowType"
        })


#endif

-- method TextView::get_wrap_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "WrapMode" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_text_view_get_wrap_mode" gtk_text_view_get_wrap_mode :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO CUInt

-- | Gets the line wrapping for the view.
textViewGetWrapMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Gtk.Enums.WrapMode
    -- ^ __Returns:__ the line wrap setting
textViewGetWrapMode textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_get_wrap_mode textView'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr textView
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewGetWrapModeMethodInfo
instance (signature ~ (m Gtk.Enums.WrapMode), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewGetWrapModeMethodInfo a signature where
    overloadedMethod = textViewGetWrapMode

instance O.OverloadedMethodInfo TextViewGetWrapModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewGetWrapMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewGetWrapMode"
        })


#endif

-- method TextView::im_context_filter_keypress
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the key event" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_im_context_filter_keypress" gtk_text_view_im_context_filter_keypress :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gdk.EventKey.EventKey ->            -- event : TInterface (Name {namespace = "Gdk", name = "EventKey"})
    IO CInt

-- | Allow the t'GI.Gtk.Objects.TextView.TextView' input method to internally handle key press
-- and release events. If this function returns 'P.True', then no further
-- processing should be done for this key event. See
-- 'GI.Gtk.Objects.IMContext.iMContextFilterKeypress'.
-- 
-- Note that you are expected to call this function from your handler
-- when overriding key event handling. This is needed in the case when
-- you need to insert your own key handling between the input method
-- and the default key event handling of the t'GI.Gtk.Objects.TextView.TextView'.
-- 
-- 
-- === /C code/
-- >
-- >static gboolean
-- >gtk_foo_bar_key_press_event (GtkWidget   *widget,
-- >                             GdkEventKey *event)
-- >{
-- >  guint keyval;
-- >
-- >  gdk_event_get_keyval ((GdkEvent*)event, &keyval);
-- >
-- >  if (keyval == GDK_KEY_Return || keyval == GDK_KEY_KP_Enter)
-- >    {
-- >      if (gtk_text_view_im_context_filter_keypress (GTK_TEXT_VIEW (widget), event))
-- >        return TRUE;
-- >    }
-- >
-- >  // Do some stuff
-- >
-- >  return GTK_WIDGET_CLASS (gtk_foo_bar_parent_class)->key_press_event (widget, event);
-- >}
-- 
-- 
-- /Since: 2.22/
textViewImContextFilterKeypress ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gdk.EventKey.EventKey
    -- ^ /@event@/: the key event
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the input method handled the key event.
textViewImContextFilterKeypress textView event = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    event' <- unsafeManagedPtrGetPtr event
    result <- gtk_text_view_im_context_filter_keypress textView' event'
    let result' = (/= 0) result
    touchManagedPtr textView
    touchManagedPtr event
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewImContextFilterKeypressMethodInfo
instance (signature ~ (Gdk.EventKey.EventKey -> m Bool), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewImContextFilterKeypressMethodInfo a signature where
    overloadedMethod = textViewImContextFilterKeypress

instance O.OverloadedMethodInfo TextViewImContextFilterKeypressMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewImContextFilterKeypress",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewImContextFilterKeypress"
        })


#endif

-- method TextView::move_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "child widget already added to the text view"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xpos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "new X position in window coordinates"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "ypos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "new Y position in window coordinates"
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

foreign import ccall "gtk_text_view_move_child" gtk_text_view_move_child :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- xpos : TBasicType TInt
    Int32 ->                                -- ypos : TBasicType TInt
    IO ()

-- | Updates the position of a child, as for 'GI.Gtk.Objects.TextView.textViewAddChildInWindow'.
textViewMoveChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> b
    -- ^ /@child@/: child widget already added to the text view
    -> Int32
    -- ^ /@xpos@/: new X position in window coordinates
    -> Int32
    -- ^ /@ypos@/: new Y position in window coordinates
    -> m ()
textViewMoveChild textView child xpos ypos = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    child' <- unsafeManagedPtrCastPtr child
    gtk_text_view_move_child textView' child' xpos ypos
    touchManagedPtr textView
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewMoveChildMethodInfo
instance (signature ~ (b -> Int32 -> Int32 -> m ()), MonadIO m, IsTextView a, Gtk.Widget.IsWidget b) => O.OverloadedMethod TextViewMoveChildMethodInfo a signature where
    overloadedMethod = textViewMoveChild

instance O.OverloadedMethodInfo TextViewMoveChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewMoveChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewMoveChild"
        })


#endif

-- method TextView::move_mark_onscreen
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "mark"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextMark" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextMark" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_move_mark_onscreen" gtk_text_view_move_mark_onscreen :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextMark.TextMark ->            -- mark : TInterface (Name {namespace = "Gtk", name = "TextMark"})
    IO CInt

-- | Moves a mark within the buffer so that it\'s
-- located within the currently-visible text area.
textViewMoveMarkOnscreen ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a, Gtk.TextMark.IsTextMark b) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> b
    -- ^ /@mark@/: a t'GI.Gtk.Objects.TextMark.TextMark'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the mark moved (wasn’t already onscreen)
textViewMoveMarkOnscreen textView mark = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    mark' <- unsafeManagedPtrCastPtr mark
    result <- gtk_text_view_move_mark_onscreen textView' mark'
    let result' = (/= 0) result
    touchManagedPtr textView
    touchManagedPtr mark
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewMoveMarkOnscreenMethodInfo
instance (signature ~ (b -> m Bool), MonadIO m, IsTextView a, Gtk.TextMark.IsTextMark b) => O.OverloadedMethod TextViewMoveMarkOnscreenMethodInfo a signature where
    overloadedMethod = textViewMoveMarkOnscreen

instance O.OverloadedMethodInfo TextViewMoveMarkOnscreenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewMoveMarkOnscreen",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewMoveMarkOnscreen"
        })


#endif

-- method TextView::move_visually
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "number of characters to move (negative moves left,\n   positive moves right)"
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

foreign import ccall "gtk_text_view_move_visually" gtk_text_view_move_visually :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    Int32 ->                                -- count : TBasicType TInt
    IO CInt

-- | Move the iterator a given number of characters visually, treating
-- it as the strong cursor position. If /@count@/ is positive, then the
-- new strong cursor position will be /@count@/ positions to the right of
-- the old cursor position. If /@count@/ is negative then the new strong
-- cursor position will be /@count@/ positions to the left of the old
-- cursor position.
-- 
-- In the presence of bi-directional text, the correspondence
-- between logical and visual order will depend on the direction
-- of the current run, and there may be jumps when the cursor
-- is moved off of the end of a run.
textViewMoveVisually ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Int32
    -- ^ /@count@/: number of characters to move (negative moves left,
    --    positive moves right)
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ moved and is not on the end iterator
textViewMoveVisually textView iter count = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_view_move_visually textView' iter' count
    let result' = (/= 0) result
    touchManagedPtr textView
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewMoveVisuallyMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> Int32 -> m Bool), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewMoveVisuallyMethodInfo a signature where
    overloadedMethod = textViewMoveVisually

instance O.OverloadedMethodInfo TextViewMoveVisuallyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewMoveVisually",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewMoveVisually"
        })


#endif

-- method TextView::place_cursor_onscreen
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_place_cursor_onscreen" gtk_text_view_place_cursor_onscreen :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO CInt

-- | Moves the cursor to the currently visible region of the
-- buffer, it it isn’t there already.
textViewPlaceCursorOnscreen ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the cursor had to be moved.
textViewPlaceCursorOnscreen textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    result <- gtk_text_view_place_cursor_onscreen textView'
    let result' = (/= 0) result
    touchManagedPtr textView
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewPlaceCursorOnscreenMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewPlaceCursorOnscreenMethodInfo a signature where
    overloadedMethod = textViewPlaceCursorOnscreen

instance O.OverloadedMethodInfo TextViewPlaceCursorOnscreenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewPlaceCursorOnscreen",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewPlaceCursorOnscreen"
        })


#endif

-- method TextView::reset_cursor_blink
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_reset_cursor_blink" gtk_text_view_reset_cursor_blink :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO ()

-- | Ensures that the cursor is shown (i.e. not in an \'off\' blink
-- interval) and resets the time that it will stay blinking (or
-- visible, in case blinking is disabled).
-- 
-- This function should be called in response to user input
-- (e.g. from derived classes that override the textview\'s
-- [Widget::keyPressEvent]("GI.Gtk.Objects.Widget#g:signal:keyPressEvent") handler).
-- 
-- /Since: 3.20/
textViewResetCursorBlink ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m ()
textViewResetCursorBlink textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    gtk_text_view_reset_cursor_blink textView'
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewResetCursorBlinkMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewResetCursorBlinkMethodInfo a signature where
    overloadedMethod = textViewResetCursorBlink

instance O.OverloadedMethodInfo TextViewResetCursorBlinkMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewResetCursorBlink",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewResetCursorBlink"
        })


#endif

-- method TextView::reset_im_context
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_reset_im_context" gtk_text_view_reset_im_context :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    IO ()

-- | Reset the input method context of the text view if needed.
-- 
-- This can be necessary in the case where modifying the buffer
-- would confuse on-going input method behavior.
-- 
-- /Since: 2.22/
textViewResetImContext ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> m ()
textViewResetImContext textView = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    gtk_text_view_reset_im_context textView'
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewResetImContextMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewResetImContextMethodInfo a signature where
    overloadedMethod = textViewResetImContext

instance O.OverloadedMethodInfo TextViewResetImContextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewResetImContext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewResetImContext"
        })


#endif

-- method TextView::scroll_mark_onscreen
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "mark"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextMark" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a mark in the buffer for @text_view"
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

foreign import ccall "gtk_text_view_scroll_mark_onscreen" gtk_text_view_scroll_mark_onscreen :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextMark.TextMark ->            -- mark : TInterface (Name {namespace = "Gtk", name = "TextMark"})
    IO ()

-- | Scrolls /@textView@/ the minimum distance such that /@mark@/ is contained
-- within the visible area of the widget.
textViewScrollMarkOnscreen ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a, Gtk.TextMark.IsTextMark b) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> b
    -- ^ /@mark@/: a mark in the buffer for /@textView@/
    -> m ()
textViewScrollMarkOnscreen textView mark = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    mark' <- unsafeManagedPtrCastPtr mark
    gtk_text_view_scroll_mark_onscreen textView' mark'
    touchManagedPtr textView
    touchManagedPtr mark
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewScrollMarkOnscreenMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsTextView a, Gtk.TextMark.IsTextMark b) => O.OverloadedMethod TextViewScrollMarkOnscreenMethodInfo a signature where
    overloadedMethod = textViewScrollMarkOnscreen

instance O.OverloadedMethodInfo TextViewScrollMarkOnscreenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewScrollMarkOnscreen",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewScrollMarkOnscreen"
        })


#endif

-- method TextView::scroll_to_iter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "within_margin"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "margin as a [0.0,0.5) fraction of screen size"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "use_align"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "whether to use alignment arguments (if %FALSE,\n   just get the mark onscreen)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xalign"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "horizontal alignment of mark within visible area"
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
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "vertical alignment of mark within visible area"
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

foreign import ccall "gtk_text_view_scroll_to_iter" gtk_text_view_scroll_to_iter :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    CDouble ->                              -- within_margin : TBasicType TDouble
    CInt ->                                 -- use_align : TBasicType TBoolean
    CDouble ->                              -- xalign : TBasicType TDouble
    CDouble ->                              -- yalign : TBasicType TDouble
    IO CInt

-- | Scrolls /@textView@/ so that /@iter@/ is on the screen in the position
-- indicated by /@xalign@/ and /@yalign@/. An alignment of 0.0 indicates
-- left or top, 1.0 indicates right or bottom, 0.5 means center.
-- If /@useAlign@/ is 'P.False', the text scrolls the minimal distance to
-- get the mark onscreen, possibly not scrolling at all. The effective
-- screen for purposes of this function is reduced by a margin of size
-- /@withinMargin@/.
-- 
-- Note that this function uses the currently-computed height of the
-- lines in the text buffer. Line heights are computed in an idle
-- handler; so this function may not have the desired effect if it’s
-- called before the height computations. To avoid oddness, consider
-- using 'GI.Gtk.Objects.TextView.textViewScrollToMark' which saves a point to be
-- scrolled to after line validation.
textViewScrollToIter ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> Double
    -- ^ /@withinMargin@/: margin as a [0.0,0.5) fraction of screen size
    -> Bool
    -- ^ /@useAlign@/: whether to use alignment arguments (if 'P.False',
    --    just get the mark onscreen)
    -> Double
    -- ^ /@xalign@/: horizontal alignment of mark within visible area
    -> Double
    -- ^ /@yalign@/: vertical alignment of mark within visible area
    -> m Bool
    -- ^ __Returns:__ 'P.True' if scrolling occurred
textViewScrollToIter textView iter withinMargin useAlign xalign yalign = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    iter' <- unsafeManagedPtrGetPtr iter
    let withinMargin' = realToFrac withinMargin
    let useAlign' = (fromIntegral . fromEnum) useAlign
    let xalign' = realToFrac xalign
    let yalign' = realToFrac yalign
    result <- gtk_text_view_scroll_to_iter textView' iter' withinMargin' useAlign' xalign' yalign'
    let result' = (/= 0) result
    touchManagedPtr textView
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewScrollToIterMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> Double -> Bool -> Double -> Double -> m Bool), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewScrollToIterMethodInfo a signature where
    overloadedMethod = textViewScrollToIter

instance O.OverloadedMethodInfo TextViewScrollToIterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewScrollToIter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewScrollToIter"
        })


#endif

-- method TextView::scroll_to_mark
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "mark"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextMark" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextMark" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "within_margin"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "margin as a [0.0,0.5) fraction of screen size"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "use_align"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "whether to use alignment arguments (if %FALSE, just\n   get the mark onscreen)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xalign"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "horizontal alignment of mark within visible area"
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
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "vertical alignment of mark within visible area"
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

foreign import ccall "gtk_text_view_scroll_to_mark" gtk_text_view_scroll_to_mark :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextMark.TextMark ->            -- mark : TInterface (Name {namespace = "Gtk", name = "TextMark"})
    CDouble ->                              -- within_margin : TBasicType TDouble
    CInt ->                                 -- use_align : TBasicType TBoolean
    CDouble ->                              -- xalign : TBasicType TDouble
    CDouble ->                              -- yalign : TBasicType TDouble
    IO ()

-- | Scrolls /@textView@/ so that /@mark@/ is on the screen in the position
-- indicated by /@xalign@/ and /@yalign@/. An alignment of 0.0 indicates
-- left or top, 1.0 indicates right or bottom, 0.5 means center.
-- If /@useAlign@/ is 'P.False', the text scrolls the minimal distance to
-- get the mark onscreen, possibly not scrolling at all. The effective
-- screen for purposes of this function is reduced by a margin of size
-- /@withinMargin@/.
textViewScrollToMark ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a, Gtk.TextMark.IsTextMark b) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> b
    -- ^ /@mark@/: a t'GI.Gtk.Objects.TextMark.TextMark'
    -> Double
    -- ^ /@withinMargin@/: margin as a [0.0,0.5) fraction of screen size
    -> Bool
    -- ^ /@useAlign@/: whether to use alignment arguments (if 'P.False', just
    --    get the mark onscreen)
    -> Double
    -- ^ /@xalign@/: horizontal alignment of mark within visible area
    -> Double
    -- ^ /@yalign@/: vertical alignment of mark within visible area
    -> m ()
textViewScrollToMark textView mark withinMargin useAlign xalign yalign = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    mark' <- unsafeManagedPtrCastPtr mark
    let withinMargin' = realToFrac withinMargin
    let useAlign' = (fromIntegral . fromEnum) useAlign
    let xalign' = realToFrac xalign
    let yalign' = realToFrac yalign
    gtk_text_view_scroll_to_mark textView' mark' withinMargin' useAlign' xalign' yalign'
    touchManagedPtr textView
    touchManagedPtr mark
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewScrollToMarkMethodInfo
instance (signature ~ (b -> Double -> Bool -> Double -> Double -> m ()), MonadIO m, IsTextView a, Gtk.TextMark.IsTextMark b) => O.OverloadedMethod TextViewScrollToMarkMethodInfo a signature where
    overloadedMethod = textViewScrollToMark

instance O.OverloadedMethodInfo TextViewScrollToMarkMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewScrollToMark",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewScrollToMark"
        })


#endif

-- method TextView::set_accepts_tab
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "accepts_tab"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "%TRUE if pressing the Tab key should insert a tab\n   character, %FALSE, if pressing the Tab key should move the\n   keyboard focus."
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

foreign import ccall "gtk_text_view_set_accepts_tab" gtk_text_view_set_accepts_tab :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    CInt ->                                 -- accepts_tab : TBasicType TBoolean
    IO ()

-- | Sets the behavior of the text widget when the Tab key is pressed.
-- If /@acceptsTab@/ is 'P.True', a tab character is inserted. If /@acceptsTab@/
-- is 'P.False' the keyboard focus is moved to the next widget in the focus
-- chain.
-- 
-- /Since: 2.4/
textViewSetAcceptsTab ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: A t'GI.Gtk.Objects.TextView.TextView'
    -> Bool
    -- ^ /@acceptsTab@/: 'P.True' if pressing the Tab key should insert a tab
    --    character, 'P.False', if pressing the Tab key should move the
    --    keyboard focus.
    -> m ()
textViewSetAcceptsTab textView acceptsTab = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    let acceptsTab' = (fromIntegral . fromEnum) acceptsTab
    gtk_text_view_set_accepts_tab textView' acceptsTab'
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetAcceptsTabMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetAcceptsTabMethodInfo a signature where
    overloadedMethod = textViewSetAcceptsTab

instance O.OverloadedMethodInfo TextViewSetAcceptsTabMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetAcceptsTab",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetAcceptsTab"
        })


#endif

-- method TextView::set_border_window_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextWindowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "window to affect" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "size"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "width or height of the window"
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

foreign import ccall "gtk_text_view_set_border_window_size" gtk_text_view_set_border_window_size :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    CUInt ->                                -- type : TInterface (Name {namespace = "Gtk", name = "TextWindowType"})
    Int32 ->                                -- size : TBasicType TInt
    IO ()

-- | Sets the width of 'GI.Gtk.Enums.TextWindowTypeLeft' or 'GI.Gtk.Enums.TextWindowTypeRight',
-- or the height of 'GI.Gtk.Enums.TextWindowTypeTop' or 'GI.Gtk.Enums.TextWindowTypeBottom'.
-- Automatically destroys the corresponding window if the size is set
-- to 0, and creates the window if the size is set to non-zero.  This
-- function can only be used for the “border windows”, and it won’t
-- work with 'GI.Gtk.Enums.TextWindowTypeWidget', 'GI.Gtk.Enums.TextWindowTypeText', or
-- 'GI.Gtk.Enums.TextWindowTypePrivate'.
textViewSetBorderWindowSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.Enums.TextWindowType
    -- ^ /@type@/: window to affect
    -> Int32
    -- ^ /@size@/: width or height of the window
    -> m ()
textViewSetBorderWindowSize textView type_ size = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    let type_' = (fromIntegral . fromEnum) type_
    gtk_text_view_set_border_window_size textView' type_' size
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetBorderWindowSizeMethodInfo
instance (signature ~ (Gtk.Enums.TextWindowType -> Int32 -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetBorderWindowSizeMethodInfo a signature where
    overloadedMethod = textViewSetBorderWindowSize

instance O.OverloadedMethodInfo TextViewSetBorderWindowSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetBorderWindowSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetBorderWindowSize"
        })


#endif

-- method TextView::set_bottom_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "bottom_margin"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "bottom margin in pixels"
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

foreign import ccall "gtk_text_view_set_bottom_margin" gtk_text_view_set_bottom_margin :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Int32 ->                                -- bottom_margin : TBasicType TInt
    IO ()

-- | Sets the bottom margin for text in /@textView@/.
-- 
-- Note that this function is confusingly named.
-- In CSS terms, the value set here is padding.
-- 
-- /Since: 3.18/
textViewSetBottomMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Int32
    -- ^ /@bottomMargin@/: bottom margin in pixels
    -> m ()
textViewSetBottomMargin textView bottomMargin = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    gtk_text_view_set_bottom_margin textView' bottomMargin
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetBottomMarginMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetBottomMarginMethodInfo a signature where
    overloadedMethod = textViewSetBottomMargin

instance O.OverloadedMethodInfo TextViewSetBottomMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetBottomMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetBottomMargin"
        })


#endif

-- method TextView::set_buffer
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextBuffer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_set_buffer" gtk_text_view_set_buffer :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextBuffer.TextBuffer ->        -- buffer : TInterface (Name {namespace = "Gtk", name = "TextBuffer"})
    IO ()

-- | Sets /@buffer@/ as the buffer being displayed by /@textView@/. The previous
-- buffer displayed by the text view is unreferenced, and a reference is
-- added to /@buffer@/. If you owned a reference to /@buffer@/ before passing it
-- to this function, you must remove that reference yourself; t'GI.Gtk.Objects.TextView.TextView'
-- will not “adopt” it.
textViewSetBuffer ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a, Gtk.TextBuffer.IsTextBuffer b) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Maybe (b)
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.TextBuffer.TextBuffer'
    -> m ()
textViewSetBuffer textView buffer = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    maybeBuffer <- case buffer of
        Nothing -> return nullPtr
        Just jBuffer -> do
            jBuffer' <- unsafeManagedPtrCastPtr jBuffer
            return jBuffer'
    gtk_text_view_set_buffer textView' maybeBuffer
    touchManagedPtr textView
    whenJust buffer touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetBufferMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsTextView a, Gtk.TextBuffer.IsTextBuffer b) => O.OverloadedMethod TextViewSetBufferMethodInfo a signature where
    overloadedMethod = textViewSetBuffer

instance O.OverloadedMethodInfo TextViewSetBufferMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetBuffer",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetBuffer"
        })


#endif

-- method TextView::set_cursor_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "whether to show the insertion cursor"
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

foreign import ccall "gtk_text_view_set_cursor_visible" gtk_text_view_set_cursor_visible :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Toggles whether the insertion point should be displayed. A buffer with
-- no editable text probably shouldn’t have a visible cursor, so you may
-- want to turn the cursor off.
-- 
-- Note that this property may be overridden by the
-- t'GI.Gtk.Objects.Settings.Settings':@/gtk-keynave-use-caret/@ settings.
textViewSetCursorVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Bool
    -- ^ /@setting@/: whether to show the insertion cursor
    -> m ()
textViewSetCursorVisible textView setting = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    let setting' = (fromIntegral . fromEnum) setting
    gtk_text_view_set_cursor_visible textView' setting'
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetCursorVisibleMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetCursorVisibleMethodInfo a signature where
    overloadedMethod = textViewSetCursorVisible

instance O.OverloadedMethodInfo TextViewSetCursorVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetCursorVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetCursorVisible"
        })


#endif

-- method TextView::set_editable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "whether it\8217s editable"
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

foreign import ccall "gtk_text_view_set_editable" gtk_text_view_set_editable :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Sets the default editability of the t'GI.Gtk.Objects.TextView.TextView'. You can override
-- this default setting with tags in the buffer, using the “editable”
-- attribute of tags.
textViewSetEditable ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Bool
    -- ^ /@setting@/: whether it’s editable
    -> m ()
textViewSetEditable textView setting = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    let setting' = (fromIntegral . fromEnum) setting
    gtk_text_view_set_editable textView' setting'
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetEditableMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetEditableMethodInfo a signature where
    overloadedMethod = textViewSetEditable

instance O.OverloadedMethodInfo TextViewSetEditableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetEditable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetEditable"
        })


#endif

-- method TextView::set_indent
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "indent"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "indentation in pixels"
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

foreign import ccall "gtk_text_view_set_indent" gtk_text_view_set_indent :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Int32 ->                                -- indent : TBasicType TInt
    IO ()

-- | Sets the default indentation for paragraphs in /@textView@/.
-- Tags in the buffer may override the default.
textViewSetIndent ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Int32
    -- ^ /@indent@/: indentation in pixels
    -> m ()
textViewSetIndent textView indent = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    gtk_text_view_set_indent textView' indent
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetIndentMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetIndentMethodInfo a signature where
    overloadedMethod = textViewSetIndent

instance O.OverloadedMethodInfo TextViewSetIndentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetIndent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetIndent"
        })


#endif

-- method TextView::set_input_hints
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "hints"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "InputHints" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the hints" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_set_input_hints" gtk_text_view_set_input_hints :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    CUInt ->                                -- hints : TInterface (Name {namespace = "Gtk", name = "InputHints"})
    IO ()

-- | Sets the [TextView:inputHints]("GI.Gtk.Objects.TextView#g:attr:inputHints") property, which
-- allows input methods to fine-tune their behaviour.
-- 
-- /Since: 3.6/
textViewSetInputHints ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> [Gtk.Flags.InputHints]
    -- ^ /@hints@/: the hints
    -> m ()
textViewSetInputHints textView hints = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    let hints' = gflagsToWord hints
    gtk_text_view_set_input_hints textView' hints'
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetInputHintsMethodInfo
instance (signature ~ ([Gtk.Flags.InputHints] -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetInputHintsMethodInfo a signature where
    overloadedMethod = textViewSetInputHints

instance O.OverloadedMethodInfo TextViewSetInputHintsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetInputHints",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetInputHints"
        })


#endif

-- method TextView::set_input_purpose
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "purpose"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "InputPurpose" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the purpose" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_set_input_purpose" gtk_text_view_set_input_purpose :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    CUInt ->                                -- purpose : TInterface (Name {namespace = "Gtk", name = "InputPurpose"})
    IO ()

-- | Sets the [TextView:inputPurpose]("GI.Gtk.Objects.TextView#g:attr:inputPurpose") property which
-- can be used by on-screen keyboards and other input
-- methods to adjust their behaviour.
-- 
-- /Since: 3.6/
textViewSetInputPurpose ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.Enums.InputPurpose
    -- ^ /@purpose@/: the purpose
    -> m ()
textViewSetInputPurpose textView purpose = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    let purpose' = (fromIntegral . fromEnum) purpose
    gtk_text_view_set_input_purpose textView' purpose'
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetInputPurposeMethodInfo
instance (signature ~ (Gtk.Enums.InputPurpose -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetInputPurposeMethodInfo a signature where
    overloadedMethod = textViewSetInputPurpose

instance O.OverloadedMethodInfo TextViewSetInputPurposeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetInputPurpose",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetInputPurpose"
        })


#endif

-- method TextView::set_justification
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "justification"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Justification" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "justification" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_set_justification" gtk_text_view_set_justification :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    CUInt ->                                -- justification : TInterface (Name {namespace = "Gtk", name = "Justification"})
    IO ()

-- | Sets the default justification of text in /@textView@/.
-- Tags in the view’s buffer may override the default.
textViewSetJustification ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.Enums.Justification
    -- ^ /@justification@/: justification
    -> m ()
textViewSetJustification textView justification = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    let justification' = (fromIntegral . fromEnum) justification
    gtk_text_view_set_justification textView' justification'
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetJustificationMethodInfo
instance (signature ~ (Gtk.Enums.Justification -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetJustificationMethodInfo a signature where
    overloadedMethod = textViewSetJustification

instance O.OverloadedMethodInfo TextViewSetJustificationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetJustification",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetJustification"
        })


#endif

-- method TextView::set_left_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "left_margin"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "left margin in pixels"
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

foreign import ccall "gtk_text_view_set_left_margin" gtk_text_view_set_left_margin :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Int32 ->                                -- left_margin : TBasicType TInt
    IO ()

-- | Sets the default left margin for text in /@textView@/.
-- Tags in the buffer may override the default.
-- 
-- Note that this function is confusingly named.
-- In CSS terms, the value set here is padding.
textViewSetLeftMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Int32
    -- ^ /@leftMargin@/: left margin in pixels
    -> m ()
textViewSetLeftMargin textView leftMargin = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    gtk_text_view_set_left_margin textView' leftMargin
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetLeftMarginMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetLeftMarginMethodInfo a signature where
    overloadedMethod = textViewSetLeftMargin

instance O.OverloadedMethodInfo TextViewSetLeftMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetLeftMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetLeftMargin"
        })


#endif

-- method TextView::set_monospace
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "monospace"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to request monospace styling"
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

foreign import ccall "gtk_text_view_set_monospace" gtk_text_view_set_monospace :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    CInt ->                                 -- monospace : TBasicType TBoolean
    IO ()

-- | Sets the [TextView:monospace]("GI.Gtk.Objects.TextView#g:attr:monospace") property, which
-- indicates that the text view should use monospace
-- fonts.
-- 
-- /Since: 3.16/
textViewSetMonospace ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Bool
    -- ^ /@monospace@/: 'P.True' to request monospace styling
    -> m ()
textViewSetMonospace textView monospace = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    let monospace' = (fromIntegral . fromEnum) monospace
    gtk_text_view_set_monospace textView' monospace'
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetMonospaceMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetMonospaceMethodInfo a signature where
    overloadedMethod = textViewSetMonospace

instance O.OverloadedMethodInfo TextViewSetMonospaceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetMonospace",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetMonospace"
        })


#endif

-- method TextView::set_overwrite
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "overwrite"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE to turn on overwrite mode, %FALSE to turn it off"
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

foreign import ccall "gtk_text_view_set_overwrite" gtk_text_view_set_overwrite :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    CInt ->                                 -- overwrite : TBasicType TBoolean
    IO ()

-- | Changes the t'GI.Gtk.Objects.TextView.TextView' overwrite mode.
-- 
-- /Since: 2.4/
textViewSetOverwrite ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Bool
    -- ^ /@overwrite@/: 'P.True' to turn on overwrite mode, 'P.False' to turn it off
    -> m ()
textViewSetOverwrite textView overwrite = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    let overwrite' = (fromIntegral . fromEnum) overwrite
    gtk_text_view_set_overwrite textView' overwrite'
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetOverwriteMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetOverwriteMethodInfo a signature where
    overloadedMethod = textViewSetOverwrite

instance O.OverloadedMethodInfo TextViewSetOverwriteMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetOverwrite",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetOverwrite"
        })


#endif

-- method TextView::set_pixels_above_lines
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pixels_above_lines"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "pixels above paragraphs"
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

foreign import ccall "gtk_text_view_set_pixels_above_lines" gtk_text_view_set_pixels_above_lines :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Int32 ->                                -- pixels_above_lines : TBasicType TInt
    IO ()

-- | Sets the default number of blank pixels above paragraphs in /@textView@/.
-- Tags in the buffer for /@textView@/ may override the defaults.
textViewSetPixelsAboveLines ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Int32
    -- ^ /@pixelsAboveLines@/: pixels above paragraphs
    -> m ()
textViewSetPixelsAboveLines textView pixelsAboveLines = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    gtk_text_view_set_pixels_above_lines textView' pixelsAboveLines
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetPixelsAboveLinesMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetPixelsAboveLinesMethodInfo a signature where
    overloadedMethod = textViewSetPixelsAboveLines

instance O.OverloadedMethodInfo TextViewSetPixelsAboveLinesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetPixelsAboveLines",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetPixelsAboveLines"
        })


#endif

-- method TextView::set_pixels_below_lines
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pixels_below_lines"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "pixels below paragraphs"
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

foreign import ccall "gtk_text_view_set_pixels_below_lines" gtk_text_view_set_pixels_below_lines :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Int32 ->                                -- pixels_below_lines : TBasicType TInt
    IO ()

-- | Sets the default number of pixels of blank space
-- to put below paragraphs in /@textView@/. May be overridden
-- by tags applied to /@textView@/’s buffer.
textViewSetPixelsBelowLines ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Int32
    -- ^ /@pixelsBelowLines@/: pixels below paragraphs
    -> m ()
textViewSetPixelsBelowLines textView pixelsBelowLines = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    gtk_text_view_set_pixels_below_lines textView' pixelsBelowLines
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetPixelsBelowLinesMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetPixelsBelowLinesMethodInfo a signature where
    overloadedMethod = textViewSetPixelsBelowLines

instance O.OverloadedMethodInfo TextViewSetPixelsBelowLinesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetPixelsBelowLines",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetPixelsBelowLines"
        })


#endif

-- method TextView::set_pixels_inside_wrap
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pixels_inside_wrap"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "default number of pixels between wrapped lines"
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

foreign import ccall "gtk_text_view_set_pixels_inside_wrap" gtk_text_view_set_pixels_inside_wrap :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Int32 ->                                -- pixels_inside_wrap : TBasicType TInt
    IO ()

-- | Sets the default number of pixels of blank space to leave between
-- display\/wrapped lines within a paragraph. May be overridden by
-- tags in /@textView@/’s buffer.
textViewSetPixelsInsideWrap ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Int32
    -- ^ /@pixelsInsideWrap@/: default number of pixels between wrapped lines
    -> m ()
textViewSetPixelsInsideWrap textView pixelsInsideWrap = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    gtk_text_view_set_pixels_inside_wrap textView' pixelsInsideWrap
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetPixelsInsideWrapMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetPixelsInsideWrapMethodInfo a signature where
    overloadedMethod = textViewSetPixelsInsideWrap

instance O.OverloadedMethodInfo TextViewSetPixelsInsideWrapMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetPixelsInsideWrap",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetPixelsInsideWrap"
        })


#endif

-- method TextView::set_right_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "right_margin"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "right margin in pixels"
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

foreign import ccall "gtk_text_view_set_right_margin" gtk_text_view_set_right_margin :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Int32 ->                                -- right_margin : TBasicType TInt
    IO ()

-- | Sets the default right margin for text in the text view.
-- Tags in the buffer may override the default.
-- 
-- Note that this function is confusingly named.
-- In CSS terms, the value set here is padding.
textViewSetRightMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Int32
    -- ^ /@rightMargin@/: right margin in pixels
    -> m ()
textViewSetRightMargin textView rightMargin = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    gtk_text_view_set_right_margin textView' rightMargin
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetRightMarginMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetRightMarginMethodInfo a signature where
    overloadedMethod = textViewSetRightMargin

instance O.OverloadedMethodInfo TextViewSetRightMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetRightMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetRightMargin"
        })


#endif

-- method TextView::set_tabs
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tabs"
--           , argType =
--               TInterface Name { namespace = "Pango" , name = "TabArray" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "tabs as a #PangoTabArray"
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

foreign import ccall "gtk_text_view_set_tabs" gtk_text_view_set_tabs :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Pango.TabArray.TabArray ->          -- tabs : TInterface (Name {namespace = "Pango", name = "TabArray"})
    IO ()

-- | Sets the default tab stops for paragraphs in /@textView@/.
-- Tags in the buffer may override the default.
textViewSetTabs ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Pango.TabArray.TabArray
    -- ^ /@tabs@/: tabs as a t'GI.Pango.Structs.TabArray.TabArray'
    -> m ()
textViewSetTabs textView tabs = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    tabs' <- unsafeManagedPtrGetPtr tabs
    gtk_text_view_set_tabs textView' tabs'
    touchManagedPtr textView
    touchManagedPtr tabs
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetTabsMethodInfo
instance (signature ~ (Pango.TabArray.TabArray -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetTabsMethodInfo a signature where
    overloadedMethod = textViewSetTabs

instance O.OverloadedMethodInfo TextViewSetTabsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetTabs",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetTabs"
        })


#endif

-- method TextView::set_top_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "top_margin"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "top margin in pixels"
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

foreign import ccall "gtk_text_view_set_top_margin" gtk_text_view_set_top_margin :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Int32 ->                                -- top_margin : TBasicType TInt
    IO ()

-- | Sets the top margin for text in /@textView@/.
-- 
-- Note that this function is confusingly named.
-- In CSS terms, the value set here is padding.
-- 
-- /Since: 3.18/
textViewSetTopMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Int32
    -- ^ /@topMargin@/: top margin in pixels
    -> m ()
textViewSetTopMargin textView topMargin = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    gtk_text_view_set_top_margin textView' topMargin
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetTopMarginMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetTopMarginMethodInfo a signature where
    overloadedMethod = textViewSetTopMargin

instance O.OverloadedMethodInfo TextViewSetTopMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetTopMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetTopMargin"
        })


#endif

-- method TextView::set_wrap_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "wrap_mode"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WrapMode" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWrapMode" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_set_wrap_mode" gtk_text_view_set_wrap_mode :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    CUInt ->                                -- wrap_mode : TInterface (Name {namespace = "Gtk", name = "WrapMode"})
    IO ()

-- | Sets the line wrapping for the view.
textViewSetWrapMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.Enums.WrapMode
    -- ^ /@wrapMode@/: a t'GI.Gtk.Enums.WrapMode'
    -> m ()
textViewSetWrapMode textView wrapMode = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    let wrapMode' = (fromIntegral . fromEnum) wrapMode
    gtk_text_view_set_wrap_mode textView' wrapMode'
    touchManagedPtr textView
    return ()

#if defined(ENABLE_OVERLOADING)
data TextViewSetWrapModeMethodInfo
instance (signature ~ (Gtk.Enums.WrapMode -> m ()), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewSetWrapModeMethodInfo a signature where
    overloadedMethod = textViewSetWrapMode

instance O.OverloadedMethodInfo TextViewSetWrapModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewSetWrapMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewSetWrapMode"
        })


#endif

-- method TextView::starts_display_line
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_text_view_starts_display_line" gtk_text_view_starts_display_line :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    Ptr Gtk.TextIter.TextIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TextIter"})
    IO CInt

-- | Determines whether /@iter@/ is at the start of a display line.
-- See 'GI.Gtk.Objects.TextView.textViewForwardDisplayLine' for an explanation of
-- display lines vs. paragraphs.
textViewStartsDisplayLine ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.TextIter.TextIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TextIter.TextIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ begins a wrapped line
textViewStartsDisplayLine textView iter = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_text_view_starts_display_line textView' iter'
    let result' = (/= 0) result
    touchManagedPtr textView
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TextViewStartsDisplayLineMethodInfo
instance (signature ~ (Gtk.TextIter.TextIter -> m Bool), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewStartsDisplayLineMethodInfo a signature where
    overloadedMethod = textViewStartsDisplayLine

instance O.OverloadedMethodInfo TextViewStartsDisplayLineMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewStartsDisplayLine",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewStartsDisplayLine"
        })


#endif

-- method TextView::window_to_buffer_coords
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "text_view"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextView" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTextView" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "win"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextWindowType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkTextWindowType except %GTK_TEXT_WINDOW_PRIVATE"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "window_x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "window x coordinate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "window_y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "window y coordinate"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "buffer_x"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "buffer x coordinate return location or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "buffer_y"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "buffer y coordinate return location or %NULL"
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

foreign import ccall "gtk_text_view_window_to_buffer_coords" gtk_text_view_window_to_buffer_coords :: 
    Ptr TextView ->                         -- text_view : TInterface (Name {namespace = "Gtk", name = "TextView"})
    CUInt ->                                -- win : TInterface (Name {namespace = "Gtk", name = "TextWindowType"})
    Int32 ->                                -- window_x : TBasicType TInt
    Int32 ->                                -- window_y : TBasicType TInt
    Ptr Int32 ->                            -- buffer_x : TBasicType TInt
    Ptr Int32 ->                            -- buffer_y : TBasicType TInt
    IO ()

-- | Converts coordinates on the window identified by /@win@/ to buffer
-- coordinates, storing the result in (/@bufferX@/,/@bufferY@/).
-- 
-- Note that you can’t convert coordinates for a nonexisting window (see
-- 'GI.Gtk.Objects.TextView.textViewSetBorderWindowSize').
textViewWindowToBufferCoords ::
    (B.CallStack.HasCallStack, MonadIO m, IsTextView a) =>
    a
    -- ^ /@textView@/: a t'GI.Gtk.Objects.TextView.TextView'
    -> Gtk.Enums.TextWindowType
    -- ^ /@win@/: a t'GI.Gtk.Enums.TextWindowType' except 'GI.Gtk.Enums.TextWindowTypePrivate'
    -> Int32
    -- ^ /@windowX@/: window x coordinate
    -> Int32
    -- ^ /@windowY@/: window y coordinate
    -> m ((Int32, Int32))
textViewWindowToBufferCoords textView win windowX windowY = liftIO $ do
    textView' <- unsafeManagedPtrCastPtr textView
    let win' = (fromIntegral . fromEnum) win
    bufferX <- allocMem :: IO (Ptr Int32)
    bufferY <- allocMem :: IO (Ptr Int32)
    gtk_text_view_window_to_buffer_coords textView' win' windowX windowY bufferX bufferY
    bufferX' <- peek bufferX
    bufferY' <- peek bufferY
    touchManagedPtr textView
    freeMem bufferX
    freeMem bufferY
    return (bufferX', bufferY')

#if defined(ENABLE_OVERLOADING)
data TextViewWindowToBufferCoordsMethodInfo
instance (signature ~ (Gtk.Enums.TextWindowType -> Int32 -> Int32 -> m ((Int32, Int32))), MonadIO m, IsTextView a) => O.OverloadedMethod TextViewWindowToBufferCoordsMethodInfo a signature where
    overloadedMethod = textViewWindowToBufferCoords

instance O.OverloadedMethodInfo TextViewWindowToBufferCoordsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TextView.textViewWindowToBufferCoords",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TextView.html#v:textViewWindowToBufferCoords"
        })


#endif


