{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.SearchEntry.SearchEntry' is a subclass of t'GI.Gtk.Objects.Entry.Entry' that has been
-- tailored for use as a search entry.
-- 
-- It will show an inactive symbolic “find” icon when the search
-- entry is empty, and a symbolic “clear” icon when there is text.
-- Clicking on the “clear” icon will empty the search entry.
-- 
-- Note that the search\/clear icon is shown using a secondary
-- icon, and thus does not work if you are using the secondary
-- icon position for some other purpose.
-- 
-- To make filtering appear more reactive, it is a good idea to
-- not react to every change in the entry text immediately, but
-- only after a short delay. To support this, t'GI.Gtk.Objects.SearchEntry.SearchEntry'
-- emits the [SearchEntry::searchChanged]("GI.Gtk.Objects.SearchEntry#g:signal:searchChanged") signal which can
-- be used instead of the [Editable::changed]("GI.Gtk.Interfaces.Editable#g:signal:changed") signal.
-- 
-- The [SearchEntry::previousMatch]("GI.Gtk.Objects.SearchEntry#g:signal:previousMatch"), [SearchEntry::nextMatch]("GI.Gtk.Objects.SearchEntry#g:signal:nextMatch")
-- and [SearchEntry::stopSearch]("GI.Gtk.Objects.SearchEntry#g:signal:stopSearch") signals can be used to implement
-- moving between search results and ending the search.
-- 
-- Often, GtkSearchEntry will be fed events by means of being
-- placed inside a t'GI.Gtk.Objects.SearchBar.SearchBar'. If that is not the case,
-- you can use 'GI.Gtk.Objects.SearchEntry.searchEntryHandleEvent' to pass events.
-- 
-- /Since: 3.6/

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.SearchEntry
    ( 

-- * Exported types
    SearchEntry(..)                         ,
    IsSearchEntry                           ,
    toSearchEntry                           ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childNotify]("GI.Gtk.Objects.Widget#g:method:childNotify"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [copyClipboard]("GI.Gtk.Interfaces.Editable#g:method:copyClipboard"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [cutClipboard]("GI.Gtk.Interfaces.Editable#g:method:cutClipboard"), [deleteSelection]("GI.Gtk.Interfaces.Editable#g:method:deleteSelection"), [deleteText]("GI.Gtk.Interfaces.Editable#g:method:deleteText"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [editingDone]("GI.Gtk.Interfaces.CellEditable#g:method:editingDone"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabFocusWithoutSelecting]("GI.Gtk.Objects.Entry#g:method:grabFocusWithoutSelecting"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [handleEvent]("GI.Gtk.Objects.SearchEntry#g:method:handleEvent"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [imContextFilterKeypress]("GI.Gtk.Objects.Entry#g:method:imContextFilterKeypress"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [insertText]("GI.Gtk.Interfaces.Editable#g:method:insertText"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [layoutIndexToTextIndex]("GI.Gtk.Objects.Entry#g:method:layoutIndexToTextIndex"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [pasteClipboard]("GI.Gtk.Interfaces.Editable#g:method:pasteClipboard"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [progressPulse]("GI.Gtk.Objects.Entry#g:method:progressPulse"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [removeWidget]("GI.Gtk.Interfaces.CellEditable#g:method:removeWidget"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetImContext]("GI.Gtk.Objects.Entry#g:method:resetImContext"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [selectRegion]("GI.Gtk.Interfaces.Editable#g:method:selectRegion"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [startEditing]("GI.Gtk.Interfaces.CellEditable#g:method:startEditing"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [textIndexToLayoutIndex]("GI.Gtk.Objects.Entry#g:method:textIndexToLayoutIndex"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetInvisibleChar]("GI.Gtk.Objects.Entry#g:method:unsetInvisibleChar"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActivatesDefault]("GI.Gtk.Objects.Entry#g:method:getActivatesDefault"), [getAlignment]("GI.Gtk.Objects.Entry#g:method:getAlignment"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getAttributes]("GI.Gtk.Objects.Entry#g:method:getAttributes"), [getBuffer]("GI.Gtk.Objects.Entry#g:method:getBuffer"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChars]("GI.Gtk.Interfaces.Editable#g:method:getChars"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompletion]("GI.Gtk.Objects.Entry#g:method:getCompletion"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getCurrentIconDragSource]("GI.Gtk.Objects.Entry#g:method:getCurrentIconDragSource"), [getCursorHadjustment]("GI.Gtk.Objects.Entry#g:method:getCursorHadjustment"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEditable]("GI.Gtk.Interfaces.Editable#g:method:getEditable"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasFrame]("GI.Gtk.Objects.Entry#g:method:getHasFrame"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getIconActivatable]("GI.Gtk.Objects.Entry#g:method:getIconActivatable"), [getIconArea]("GI.Gtk.Objects.Entry#g:method:getIconArea"), [getIconAtPos]("GI.Gtk.Objects.Entry#g:method:getIconAtPos"), [getIconGicon]("GI.Gtk.Objects.Entry#g:method:getIconGicon"), [getIconName]("GI.Gtk.Objects.Entry#g:method:getIconName"), [getIconPixbuf]("GI.Gtk.Objects.Entry#g:method:getIconPixbuf"), [getIconSensitive]("GI.Gtk.Objects.Entry#g:method:getIconSensitive"), [getIconStock]("GI.Gtk.Objects.Entry#g:method:getIconStock"), [getIconStorageType]("GI.Gtk.Objects.Entry#g:method:getIconStorageType"), [getIconTooltipMarkup]("GI.Gtk.Objects.Entry#g:method:getIconTooltipMarkup"), [getIconTooltipText]("GI.Gtk.Objects.Entry#g:method:getIconTooltipText"), [getInnerBorder]("GI.Gtk.Objects.Entry#g:method:getInnerBorder"), [getInputHints]("GI.Gtk.Objects.Entry#g:method:getInputHints"), [getInputPurpose]("GI.Gtk.Objects.Entry#g:method:getInputPurpose"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getInvisibleChar]("GI.Gtk.Objects.Entry#g:method:getInvisibleChar"), [getLayout]("GI.Gtk.Objects.Entry#g:method:getLayout"), [getLayoutOffsets]("GI.Gtk.Objects.Entry#g:method:getLayoutOffsets"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMaxLength]("GI.Gtk.Objects.Entry#g:method:getMaxLength"), [getMaxWidthChars]("GI.Gtk.Objects.Entry#g:method:getMaxWidthChars"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getOverwriteMode]("GI.Gtk.Objects.Entry#g:method:getOverwriteMode"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPlaceholderText]("GI.Gtk.Objects.Entry#g:method:getPlaceholderText"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPosition]("GI.Gtk.Interfaces.Editable#g:method:getPosition"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProgressFraction]("GI.Gtk.Objects.Entry#g:method:getProgressFraction"), [getProgressPulseStep]("GI.Gtk.Objects.Entry#g:method:getProgressPulseStep"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSelectionBounds]("GI.Gtk.Interfaces.Editable#g:method:getSelectionBounds"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTabs]("GI.Gtk.Objects.Entry#g:method:getTabs"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getText]("GI.Gtk.Objects.Entry#g:method:getText"), [getTextArea]("GI.Gtk.Objects.Entry#g:method:getTextArea"), [getTextLength]("GI.Gtk.Objects.Entry#g:method:getTextLength"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisibility]("GI.Gtk.Objects.Entry#g:method:getVisibility"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWidthChars]("GI.Gtk.Objects.Entry#g:method:getWidthChars"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActivatesDefault]("GI.Gtk.Objects.Entry#g:method:setActivatesDefault"), [setAlignment]("GI.Gtk.Objects.Entry#g:method:setAlignment"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setAttributes]("GI.Gtk.Objects.Entry#g:method:setAttributes"), [setBuffer]("GI.Gtk.Objects.Entry#g:method:setBuffer"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompletion]("GI.Gtk.Objects.Entry#g:method:setCompletion"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setCursorHadjustment]("GI.Gtk.Objects.Entry#g:method:setCursorHadjustment"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEditable]("GI.Gtk.Interfaces.Editable#g:method:setEditable"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasFrame]("GI.Gtk.Objects.Entry#g:method:setHasFrame"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setIconActivatable]("GI.Gtk.Objects.Entry#g:method:setIconActivatable"), [setIconDragSource]("GI.Gtk.Objects.Entry#g:method:setIconDragSource"), [setIconFromGicon]("GI.Gtk.Objects.Entry#g:method:setIconFromGicon"), [setIconFromIconName]("GI.Gtk.Objects.Entry#g:method:setIconFromIconName"), [setIconFromPixbuf]("GI.Gtk.Objects.Entry#g:method:setIconFromPixbuf"), [setIconFromStock]("GI.Gtk.Objects.Entry#g:method:setIconFromStock"), [setIconSensitive]("GI.Gtk.Objects.Entry#g:method:setIconSensitive"), [setIconTooltipMarkup]("GI.Gtk.Objects.Entry#g:method:setIconTooltipMarkup"), [setIconTooltipText]("GI.Gtk.Objects.Entry#g:method:setIconTooltipText"), [setInnerBorder]("GI.Gtk.Objects.Entry#g:method:setInnerBorder"), [setInputHints]("GI.Gtk.Objects.Entry#g:method:setInputHints"), [setInputPurpose]("GI.Gtk.Objects.Entry#g:method:setInputPurpose"), [setInvisibleChar]("GI.Gtk.Objects.Entry#g:method:setInvisibleChar"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMaxLength]("GI.Gtk.Objects.Entry#g:method:setMaxLength"), [setMaxWidthChars]("GI.Gtk.Objects.Entry#g:method:setMaxWidthChars"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setOverwriteMode]("GI.Gtk.Objects.Entry#g:method:setOverwriteMode"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPlaceholderText]("GI.Gtk.Objects.Entry#g:method:setPlaceholderText"), [setPosition]("GI.Gtk.Interfaces.Editable#g:method:setPosition"), [setProgressFraction]("GI.Gtk.Objects.Entry#g:method:setProgressFraction"), [setProgressPulseStep]("GI.Gtk.Objects.Entry#g:method:setProgressPulseStep"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTabs]("GI.Gtk.Objects.Entry#g:method:setTabs"), [setText]("GI.Gtk.Objects.Entry#g:method:setText"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisibility]("GI.Gtk.Objects.Entry#g:method:setVisibility"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWidthChars]("GI.Gtk.Objects.Entry#g:method:setWidthChars"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveSearchEntryMethod                ,
#endif

-- ** handleEvent #method:handleEvent#

#if defined(ENABLE_OVERLOADING)
    SearchEntryHandleEventMethodInfo        ,
#endif
    searchEntryHandleEvent                  ,


-- ** new #method:new#

    searchEntryNew                          ,




 -- * Signals


-- ** nextMatch #signal:nextMatch#

    SearchEntryNextMatchCallback            ,
#if defined(ENABLE_OVERLOADING)
    SearchEntryNextMatchSignalInfo          ,
#endif
    afterSearchEntryNextMatch               ,
    onSearchEntryNextMatch                  ,


-- ** previousMatch #signal:previousMatch#

    SearchEntryPreviousMatchCallback        ,
#if defined(ENABLE_OVERLOADING)
    SearchEntryPreviousMatchSignalInfo      ,
#endif
    afterSearchEntryPreviousMatch           ,
    onSearchEntryPreviousMatch              ,


-- ** searchChanged #signal:searchChanged#

    SearchEntrySearchChangedCallback        ,
#if defined(ENABLE_OVERLOADING)
    SearchEntrySearchChangedSignalInfo      ,
#endif
    afterSearchEntrySearchChanged           ,
    onSearchEntrySearchChanged              ,


-- ** stopSearch #signal:stopSearch#

    SearchEntryStopSearchCallback           ,
#if defined(ENABLE_OVERLOADING)
    SearchEntryStopSearchSignalInfo         ,
#endif
    afterSearchEntryStopSearch              ,
    onSearchEntryStopSearch                 ,




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
import qualified GI.Gdk.Unions.Event as Gdk.Event
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.CellEditable as Gtk.CellEditable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Editable as Gtk.Editable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Entry as Gtk.Entry
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype SearchEntry = SearchEntry (SP.ManagedPtr SearchEntry)
    deriving (Eq)

instance SP.ManagedPtrNewtype SearchEntry where
    toManagedPtr (SearchEntry p) = p

foreign import ccall "gtk_search_entry_get_type"
    c_gtk_search_entry_get_type :: IO B.Types.GType

instance B.Types.TypedObject SearchEntry where
    glibType = c_gtk_search_entry_get_type

instance B.Types.GObject SearchEntry

-- | Type class for types which can be safely cast to `SearchEntry`, for instance with `toSearchEntry`.
class (SP.GObject o, O.IsDescendantOf SearchEntry o) => IsSearchEntry o
instance (SP.GObject o, O.IsDescendantOf SearchEntry o) => IsSearchEntry o

instance O.HasParentTypes SearchEntry
type instance O.ParentTypes SearchEntry = '[Gtk.Entry.Entry, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable, Gtk.CellEditable.CellEditable, Gtk.Editable.Editable]

-- | Cast to `SearchEntry`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toSearchEntry :: (MIO.MonadIO m, IsSearchEntry o) => o -> m SearchEntry
toSearchEntry = MIO.liftIO . B.ManagedPtr.unsafeCastTo SearchEntry

-- | Convert 'SearchEntry' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe SearchEntry) where
    gvalueGType_ = c_gtk_search_entry_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr SearchEntry)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr SearchEntry)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject SearchEntry ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveSearchEntryMethod (t :: Symbol) (o :: *) :: * where
    ResolveSearchEntryMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveSearchEntryMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveSearchEntryMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveSearchEntryMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveSearchEntryMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveSearchEntryMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveSearchEntryMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveSearchEntryMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveSearchEntryMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveSearchEntryMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveSearchEntryMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveSearchEntryMethod "childNotify" o = Gtk.Widget.WidgetChildNotifyMethodInfo
    ResolveSearchEntryMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveSearchEntryMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveSearchEntryMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveSearchEntryMethod "copyClipboard" o = Gtk.Editable.EditableCopyClipboardMethodInfo
    ResolveSearchEntryMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveSearchEntryMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveSearchEntryMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveSearchEntryMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveSearchEntryMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveSearchEntryMethod "cutClipboard" o = Gtk.Editable.EditableCutClipboardMethodInfo
    ResolveSearchEntryMethod "deleteSelection" o = Gtk.Editable.EditableDeleteSelectionMethodInfo
    ResolveSearchEntryMethod "deleteText" o = Gtk.Editable.EditableDeleteTextMethodInfo
    ResolveSearchEntryMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveSearchEntryMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveSearchEntryMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveSearchEntryMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveSearchEntryMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveSearchEntryMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveSearchEntryMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveSearchEntryMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveSearchEntryMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveSearchEntryMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveSearchEntryMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveSearchEntryMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveSearchEntryMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveSearchEntryMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveSearchEntryMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveSearchEntryMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveSearchEntryMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveSearchEntryMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveSearchEntryMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveSearchEntryMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveSearchEntryMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveSearchEntryMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveSearchEntryMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveSearchEntryMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveSearchEntryMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveSearchEntryMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveSearchEntryMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveSearchEntryMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveSearchEntryMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveSearchEntryMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveSearchEntryMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveSearchEntryMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveSearchEntryMethod "editingDone" o = Gtk.CellEditable.CellEditableEditingDoneMethodInfo
    ResolveSearchEntryMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveSearchEntryMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveSearchEntryMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveSearchEntryMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveSearchEntryMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveSearchEntryMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveSearchEntryMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveSearchEntryMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveSearchEntryMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveSearchEntryMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveSearchEntryMethod "grabFocusWithoutSelecting" o = Gtk.Entry.EntryGrabFocusWithoutSelectingMethodInfo
    ResolveSearchEntryMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveSearchEntryMethod "handleEvent" o = SearchEntryHandleEventMethodInfo
    ResolveSearchEntryMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveSearchEntryMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveSearchEntryMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveSearchEntryMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveSearchEntryMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveSearchEntryMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveSearchEntryMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveSearchEntryMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveSearchEntryMethod "imContextFilterKeypress" o = Gtk.Entry.EntryImContextFilterKeypressMethodInfo
    ResolveSearchEntryMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveSearchEntryMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveSearchEntryMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveSearchEntryMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveSearchEntryMethod "insertText" o = Gtk.Editable.EditableInsertTextMethodInfo
    ResolveSearchEntryMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveSearchEntryMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveSearchEntryMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveSearchEntryMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveSearchEntryMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveSearchEntryMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveSearchEntryMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveSearchEntryMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveSearchEntryMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveSearchEntryMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveSearchEntryMethod "layoutIndexToTextIndex" o = Gtk.Entry.EntryLayoutIndexToTextIndexMethodInfo
    ResolveSearchEntryMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveSearchEntryMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveSearchEntryMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveSearchEntryMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveSearchEntryMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveSearchEntryMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveSearchEntryMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveSearchEntryMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveSearchEntryMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveSearchEntryMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveSearchEntryMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveSearchEntryMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveSearchEntryMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveSearchEntryMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveSearchEntryMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveSearchEntryMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveSearchEntryMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveSearchEntryMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveSearchEntryMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveSearchEntryMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveSearchEntryMethod "pasteClipboard" o = Gtk.Editable.EditablePasteClipboardMethodInfo
    ResolveSearchEntryMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveSearchEntryMethod "progressPulse" o = Gtk.Entry.EntryProgressPulseMethodInfo
    ResolveSearchEntryMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveSearchEntryMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveSearchEntryMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveSearchEntryMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveSearchEntryMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveSearchEntryMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveSearchEntryMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveSearchEntryMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveSearchEntryMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveSearchEntryMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveSearchEntryMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveSearchEntryMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveSearchEntryMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveSearchEntryMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveSearchEntryMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveSearchEntryMethod "removeWidget" o = Gtk.CellEditable.CellEditableRemoveWidgetMethodInfo
    ResolveSearchEntryMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveSearchEntryMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveSearchEntryMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveSearchEntryMethod "resetImContext" o = Gtk.Entry.EntryResetImContextMethodInfo
    ResolveSearchEntryMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveSearchEntryMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveSearchEntryMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveSearchEntryMethod "selectRegion" o = Gtk.Editable.EditableSelectRegionMethodInfo
    ResolveSearchEntryMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveSearchEntryMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveSearchEntryMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveSearchEntryMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveSearchEntryMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveSearchEntryMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveSearchEntryMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveSearchEntryMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveSearchEntryMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveSearchEntryMethod "startEditing" o = Gtk.CellEditable.CellEditableStartEditingMethodInfo
    ResolveSearchEntryMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveSearchEntryMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveSearchEntryMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveSearchEntryMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveSearchEntryMethod "textIndexToLayoutIndex" o = Gtk.Entry.EntryTextIndexToLayoutIndexMethodInfo
    ResolveSearchEntryMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveSearchEntryMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveSearchEntryMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveSearchEntryMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveSearchEntryMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveSearchEntryMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveSearchEntryMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveSearchEntryMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveSearchEntryMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveSearchEntryMethod "unsetInvisibleChar" o = Gtk.Entry.EntryUnsetInvisibleCharMethodInfo
    ResolveSearchEntryMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveSearchEntryMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveSearchEntryMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveSearchEntryMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveSearchEntryMethod "getActivatesDefault" o = Gtk.Entry.EntryGetActivatesDefaultMethodInfo
    ResolveSearchEntryMethod "getAlignment" o = Gtk.Entry.EntryGetAlignmentMethodInfo
    ResolveSearchEntryMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveSearchEntryMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveSearchEntryMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveSearchEntryMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveSearchEntryMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveSearchEntryMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveSearchEntryMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveSearchEntryMethod "getAttributes" o = Gtk.Entry.EntryGetAttributesMethodInfo
    ResolveSearchEntryMethod "getBuffer" o = Gtk.Entry.EntryGetBufferMethodInfo
    ResolveSearchEntryMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveSearchEntryMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveSearchEntryMethod "getChars" o = Gtk.Editable.EditableGetCharsMethodInfo
    ResolveSearchEntryMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveSearchEntryMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveSearchEntryMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveSearchEntryMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveSearchEntryMethod "getCompletion" o = Gtk.Entry.EntryGetCompletionMethodInfo
    ResolveSearchEntryMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveSearchEntryMethod "getCurrentIconDragSource" o = Gtk.Entry.EntryGetCurrentIconDragSourceMethodInfo
    ResolveSearchEntryMethod "getCursorHadjustment" o = Gtk.Entry.EntryGetCursorHadjustmentMethodInfo
    ResolveSearchEntryMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveSearchEntryMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveSearchEntryMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveSearchEntryMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveSearchEntryMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveSearchEntryMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveSearchEntryMethod "getEditable" o = Gtk.Editable.EditableGetEditableMethodInfo
    ResolveSearchEntryMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveSearchEntryMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveSearchEntryMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveSearchEntryMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveSearchEntryMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveSearchEntryMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveSearchEntryMethod "getHasFrame" o = Gtk.Entry.EntryGetHasFrameMethodInfo
    ResolveSearchEntryMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveSearchEntryMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveSearchEntryMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveSearchEntryMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveSearchEntryMethod "getIconActivatable" o = Gtk.Entry.EntryGetIconActivatableMethodInfo
    ResolveSearchEntryMethod "getIconArea" o = Gtk.Entry.EntryGetIconAreaMethodInfo
    ResolveSearchEntryMethod "getIconAtPos" o = Gtk.Entry.EntryGetIconAtPosMethodInfo
    ResolveSearchEntryMethod "getIconGicon" o = Gtk.Entry.EntryGetIconGiconMethodInfo
    ResolveSearchEntryMethod "getIconName" o = Gtk.Entry.EntryGetIconNameMethodInfo
    ResolveSearchEntryMethod "getIconPixbuf" o = Gtk.Entry.EntryGetIconPixbufMethodInfo
    ResolveSearchEntryMethod "getIconSensitive" o = Gtk.Entry.EntryGetIconSensitiveMethodInfo
    ResolveSearchEntryMethod "getIconStock" o = Gtk.Entry.EntryGetIconStockMethodInfo
    ResolveSearchEntryMethod "getIconStorageType" o = Gtk.Entry.EntryGetIconStorageTypeMethodInfo
    ResolveSearchEntryMethod "getIconTooltipMarkup" o = Gtk.Entry.EntryGetIconTooltipMarkupMethodInfo
    ResolveSearchEntryMethod "getIconTooltipText" o = Gtk.Entry.EntryGetIconTooltipTextMethodInfo
    ResolveSearchEntryMethod "getInnerBorder" o = Gtk.Entry.EntryGetInnerBorderMethodInfo
    ResolveSearchEntryMethod "getInputHints" o = Gtk.Entry.EntryGetInputHintsMethodInfo
    ResolveSearchEntryMethod "getInputPurpose" o = Gtk.Entry.EntryGetInputPurposeMethodInfo
    ResolveSearchEntryMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveSearchEntryMethod "getInvisibleChar" o = Gtk.Entry.EntryGetInvisibleCharMethodInfo
    ResolveSearchEntryMethod "getLayout" o = Gtk.Entry.EntryGetLayoutMethodInfo
    ResolveSearchEntryMethod "getLayoutOffsets" o = Gtk.Entry.EntryGetLayoutOffsetsMethodInfo
    ResolveSearchEntryMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveSearchEntryMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveSearchEntryMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveSearchEntryMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveSearchEntryMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveSearchEntryMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveSearchEntryMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveSearchEntryMethod "getMaxLength" o = Gtk.Entry.EntryGetMaxLengthMethodInfo
    ResolveSearchEntryMethod "getMaxWidthChars" o = Gtk.Entry.EntryGetMaxWidthCharsMethodInfo
    ResolveSearchEntryMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveSearchEntryMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveSearchEntryMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveSearchEntryMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveSearchEntryMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveSearchEntryMethod "getOverwriteMode" o = Gtk.Entry.EntryGetOverwriteModeMethodInfo
    ResolveSearchEntryMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveSearchEntryMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveSearchEntryMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveSearchEntryMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveSearchEntryMethod "getPlaceholderText" o = Gtk.Entry.EntryGetPlaceholderTextMethodInfo
    ResolveSearchEntryMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveSearchEntryMethod "getPosition" o = Gtk.Editable.EditableGetPositionMethodInfo
    ResolveSearchEntryMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveSearchEntryMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveSearchEntryMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveSearchEntryMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveSearchEntryMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveSearchEntryMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveSearchEntryMethod "getProgressFraction" o = Gtk.Entry.EntryGetProgressFractionMethodInfo
    ResolveSearchEntryMethod "getProgressPulseStep" o = Gtk.Entry.EntryGetProgressPulseStepMethodInfo
    ResolveSearchEntryMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveSearchEntryMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveSearchEntryMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveSearchEntryMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveSearchEntryMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveSearchEntryMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveSearchEntryMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveSearchEntryMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveSearchEntryMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveSearchEntryMethod "getSelectionBounds" o = Gtk.Editable.EditableGetSelectionBoundsMethodInfo
    ResolveSearchEntryMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveSearchEntryMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveSearchEntryMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveSearchEntryMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveSearchEntryMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveSearchEntryMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveSearchEntryMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveSearchEntryMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveSearchEntryMethod "getTabs" o = Gtk.Entry.EntryGetTabsMethodInfo
    ResolveSearchEntryMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveSearchEntryMethod "getText" o = Gtk.Entry.EntryGetTextMethodInfo
    ResolveSearchEntryMethod "getTextArea" o = Gtk.Entry.EntryGetTextAreaMethodInfo
    ResolveSearchEntryMethod "getTextLength" o = Gtk.Entry.EntryGetTextLengthMethodInfo
    ResolveSearchEntryMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveSearchEntryMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveSearchEntryMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveSearchEntryMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveSearchEntryMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveSearchEntryMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveSearchEntryMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveSearchEntryMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveSearchEntryMethod "getVisibility" o = Gtk.Entry.EntryGetVisibilityMethodInfo
    ResolveSearchEntryMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveSearchEntryMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveSearchEntryMethod "getWidthChars" o = Gtk.Entry.EntryGetWidthCharsMethodInfo
    ResolveSearchEntryMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveSearchEntryMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveSearchEntryMethod "setActivatesDefault" o = Gtk.Entry.EntrySetActivatesDefaultMethodInfo
    ResolveSearchEntryMethod "setAlignment" o = Gtk.Entry.EntrySetAlignmentMethodInfo
    ResolveSearchEntryMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveSearchEntryMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveSearchEntryMethod "setAttributes" o = Gtk.Entry.EntrySetAttributesMethodInfo
    ResolveSearchEntryMethod "setBuffer" o = Gtk.Entry.EntrySetBufferMethodInfo
    ResolveSearchEntryMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveSearchEntryMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveSearchEntryMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveSearchEntryMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveSearchEntryMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveSearchEntryMethod "setCompletion" o = Gtk.Entry.EntrySetCompletionMethodInfo
    ResolveSearchEntryMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveSearchEntryMethod "setCursorHadjustment" o = Gtk.Entry.EntrySetCursorHadjustmentMethodInfo
    ResolveSearchEntryMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveSearchEntryMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveSearchEntryMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveSearchEntryMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveSearchEntryMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveSearchEntryMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveSearchEntryMethod "setEditable" o = Gtk.Editable.EditableSetEditableMethodInfo
    ResolveSearchEntryMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveSearchEntryMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveSearchEntryMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveSearchEntryMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveSearchEntryMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveSearchEntryMethod "setHasFrame" o = Gtk.Entry.EntrySetHasFrameMethodInfo
    ResolveSearchEntryMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveSearchEntryMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveSearchEntryMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveSearchEntryMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveSearchEntryMethod "setIconActivatable" o = Gtk.Entry.EntrySetIconActivatableMethodInfo
    ResolveSearchEntryMethod "setIconDragSource" o = Gtk.Entry.EntrySetIconDragSourceMethodInfo
    ResolveSearchEntryMethod "setIconFromGicon" o = Gtk.Entry.EntrySetIconFromGiconMethodInfo
    ResolveSearchEntryMethod "setIconFromIconName" o = Gtk.Entry.EntrySetIconFromIconNameMethodInfo
    ResolveSearchEntryMethod "setIconFromPixbuf" o = Gtk.Entry.EntrySetIconFromPixbufMethodInfo
    ResolveSearchEntryMethod "setIconFromStock" o = Gtk.Entry.EntrySetIconFromStockMethodInfo
    ResolveSearchEntryMethod "setIconSensitive" o = Gtk.Entry.EntrySetIconSensitiveMethodInfo
    ResolveSearchEntryMethod "setIconTooltipMarkup" o = Gtk.Entry.EntrySetIconTooltipMarkupMethodInfo
    ResolveSearchEntryMethod "setIconTooltipText" o = Gtk.Entry.EntrySetIconTooltipTextMethodInfo
    ResolveSearchEntryMethod "setInnerBorder" o = Gtk.Entry.EntrySetInnerBorderMethodInfo
    ResolveSearchEntryMethod "setInputHints" o = Gtk.Entry.EntrySetInputHintsMethodInfo
    ResolveSearchEntryMethod "setInputPurpose" o = Gtk.Entry.EntrySetInputPurposeMethodInfo
    ResolveSearchEntryMethod "setInvisibleChar" o = Gtk.Entry.EntrySetInvisibleCharMethodInfo
    ResolveSearchEntryMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveSearchEntryMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveSearchEntryMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveSearchEntryMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveSearchEntryMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveSearchEntryMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveSearchEntryMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveSearchEntryMethod "setMaxLength" o = Gtk.Entry.EntrySetMaxLengthMethodInfo
    ResolveSearchEntryMethod "setMaxWidthChars" o = Gtk.Entry.EntrySetMaxWidthCharsMethodInfo
    ResolveSearchEntryMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveSearchEntryMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveSearchEntryMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveSearchEntryMethod "setOverwriteMode" o = Gtk.Entry.EntrySetOverwriteModeMethodInfo
    ResolveSearchEntryMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveSearchEntryMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveSearchEntryMethod "setPlaceholderText" o = Gtk.Entry.EntrySetPlaceholderTextMethodInfo
    ResolveSearchEntryMethod "setPosition" o = Gtk.Editable.EditableSetPositionMethodInfo
    ResolveSearchEntryMethod "setProgressFraction" o = Gtk.Entry.EntrySetProgressFractionMethodInfo
    ResolveSearchEntryMethod "setProgressPulseStep" o = Gtk.Entry.EntrySetProgressPulseStepMethodInfo
    ResolveSearchEntryMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveSearchEntryMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveSearchEntryMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveSearchEntryMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveSearchEntryMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveSearchEntryMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveSearchEntryMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveSearchEntryMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveSearchEntryMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveSearchEntryMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveSearchEntryMethod "setTabs" o = Gtk.Entry.EntrySetTabsMethodInfo
    ResolveSearchEntryMethod "setText" o = Gtk.Entry.EntrySetTextMethodInfo
    ResolveSearchEntryMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveSearchEntryMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveSearchEntryMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveSearchEntryMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveSearchEntryMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveSearchEntryMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveSearchEntryMethod "setVisibility" o = Gtk.Entry.EntrySetVisibilityMethodInfo
    ResolveSearchEntryMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveSearchEntryMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveSearchEntryMethod "setWidthChars" o = Gtk.Entry.EntrySetWidthCharsMethodInfo
    ResolveSearchEntryMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveSearchEntryMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveSearchEntryMethod t SearchEntry, O.OverloadedMethod info SearchEntry p) => OL.IsLabel t (SearchEntry -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveSearchEntryMethod t SearchEntry, O.OverloadedMethod info SearchEntry p, R.HasField t SearchEntry p) => R.HasField t SearchEntry p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveSearchEntryMethod t SearchEntry, O.OverloadedMethodInfo info SearchEntry) => OL.IsLabel t (O.MethodProxy info SearchEntry) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal SearchEntry::next-match
-- | The [nextMatch](#g:signal:nextMatch) signal is a [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user initiates a move to the next match
-- for the current search string.
-- 
-- Applications should connect to it, to implement moving between
-- matches.
-- 
-- The default bindings for this signal is Ctrl-g.
-- 
-- /Since: 3.16/
type SearchEntryNextMatchCallback =
    IO ()

type C_SearchEntryNextMatchCallback =
    Ptr SearchEntry ->                      -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_SearchEntryNextMatchCallback`.
foreign import ccall "wrapper"
    mk_SearchEntryNextMatchCallback :: C_SearchEntryNextMatchCallback -> IO (FunPtr C_SearchEntryNextMatchCallback)

wrap_SearchEntryNextMatchCallback :: 
    GObject a => (a -> SearchEntryNextMatchCallback) ->
    C_SearchEntryNextMatchCallback
wrap_SearchEntryNextMatchCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [nextMatch](#signal:nextMatch) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' searchEntry #nextMatch callback
-- @
-- 
-- 
onSearchEntryNextMatch :: (IsSearchEntry a, MonadIO m) => a -> ((?self :: a) => SearchEntryNextMatchCallback) -> m SignalHandlerId
onSearchEntryNextMatch obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SearchEntryNextMatchCallback wrapped
    wrapped'' <- mk_SearchEntryNextMatchCallback wrapped'
    connectSignalFunPtr obj "next-match" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [nextMatch](#signal:nextMatch) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' searchEntry #nextMatch callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterSearchEntryNextMatch :: (IsSearchEntry a, MonadIO m) => a -> ((?self :: a) => SearchEntryNextMatchCallback) -> m SignalHandlerId
afterSearchEntryNextMatch obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SearchEntryNextMatchCallback wrapped
    wrapped'' <- mk_SearchEntryNextMatchCallback wrapped'
    connectSignalFunPtr obj "next-match" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data SearchEntryNextMatchSignalInfo
instance SignalInfo SearchEntryNextMatchSignalInfo where
    type HaskellCallbackType SearchEntryNextMatchSignalInfo = SearchEntryNextMatchCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_SearchEntryNextMatchCallback cb
        cb'' <- mk_SearchEntryNextMatchCallback cb'
        connectSignalFunPtr obj "next-match" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SearchEntry::next-match"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SearchEntry.html#g:signal:nextMatch"})

#endif

-- signal SearchEntry::previous-match
-- | The [previousMatch](#g:signal:previousMatch) signal is a [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user initiates a move to the previous match
-- for the current search string.
-- 
-- Applications should connect to it, to implement moving between
-- matches.
-- 
-- The default bindings for this signal is Ctrl-Shift-g.
-- 
-- /Since: 3.16/
type SearchEntryPreviousMatchCallback =
    IO ()

type C_SearchEntryPreviousMatchCallback =
    Ptr SearchEntry ->                      -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_SearchEntryPreviousMatchCallback`.
foreign import ccall "wrapper"
    mk_SearchEntryPreviousMatchCallback :: C_SearchEntryPreviousMatchCallback -> IO (FunPtr C_SearchEntryPreviousMatchCallback)

wrap_SearchEntryPreviousMatchCallback :: 
    GObject a => (a -> SearchEntryPreviousMatchCallback) ->
    C_SearchEntryPreviousMatchCallback
wrap_SearchEntryPreviousMatchCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [previousMatch](#signal:previousMatch) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' searchEntry #previousMatch callback
-- @
-- 
-- 
onSearchEntryPreviousMatch :: (IsSearchEntry a, MonadIO m) => a -> ((?self :: a) => SearchEntryPreviousMatchCallback) -> m SignalHandlerId
onSearchEntryPreviousMatch obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SearchEntryPreviousMatchCallback wrapped
    wrapped'' <- mk_SearchEntryPreviousMatchCallback wrapped'
    connectSignalFunPtr obj "previous-match" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [previousMatch](#signal:previousMatch) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' searchEntry #previousMatch callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterSearchEntryPreviousMatch :: (IsSearchEntry a, MonadIO m) => a -> ((?self :: a) => SearchEntryPreviousMatchCallback) -> m SignalHandlerId
afterSearchEntryPreviousMatch obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SearchEntryPreviousMatchCallback wrapped
    wrapped'' <- mk_SearchEntryPreviousMatchCallback wrapped'
    connectSignalFunPtr obj "previous-match" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data SearchEntryPreviousMatchSignalInfo
instance SignalInfo SearchEntryPreviousMatchSignalInfo where
    type HaskellCallbackType SearchEntryPreviousMatchSignalInfo = SearchEntryPreviousMatchCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_SearchEntryPreviousMatchCallback cb
        cb'' <- mk_SearchEntryPreviousMatchCallback cb'
        connectSignalFunPtr obj "previous-match" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SearchEntry::previous-match"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SearchEntry.html#g:signal:previousMatch"})

#endif

-- signal SearchEntry::search-changed
-- | The [SearchEntry::searchChanged]("GI.Gtk.Objects.SearchEntry#g:signal:searchChanged") signal is emitted with a short
-- delay of 150 milliseconds after the last change to the entry text.
-- 
-- /Since: 3.10/
type SearchEntrySearchChangedCallback =
    IO ()

type C_SearchEntrySearchChangedCallback =
    Ptr SearchEntry ->                      -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_SearchEntrySearchChangedCallback`.
foreign import ccall "wrapper"
    mk_SearchEntrySearchChangedCallback :: C_SearchEntrySearchChangedCallback -> IO (FunPtr C_SearchEntrySearchChangedCallback)

wrap_SearchEntrySearchChangedCallback :: 
    GObject a => (a -> SearchEntrySearchChangedCallback) ->
    C_SearchEntrySearchChangedCallback
wrap_SearchEntrySearchChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [searchChanged](#signal:searchChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' searchEntry #searchChanged callback
-- @
-- 
-- 
onSearchEntrySearchChanged :: (IsSearchEntry a, MonadIO m) => a -> ((?self :: a) => SearchEntrySearchChangedCallback) -> m SignalHandlerId
onSearchEntrySearchChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SearchEntrySearchChangedCallback wrapped
    wrapped'' <- mk_SearchEntrySearchChangedCallback wrapped'
    connectSignalFunPtr obj "search-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [searchChanged](#signal:searchChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' searchEntry #searchChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterSearchEntrySearchChanged :: (IsSearchEntry a, MonadIO m) => a -> ((?self :: a) => SearchEntrySearchChangedCallback) -> m SignalHandlerId
afterSearchEntrySearchChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SearchEntrySearchChangedCallback wrapped
    wrapped'' <- mk_SearchEntrySearchChangedCallback wrapped'
    connectSignalFunPtr obj "search-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data SearchEntrySearchChangedSignalInfo
instance SignalInfo SearchEntrySearchChangedSignalInfo where
    type HaskellCallbackType SearchEntrySearchChangedSignalInfo = SearchEntrySearchChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_SearchEntrySearchChangedCallback cb
        cb'' <- mk_SearchEntrySearchChangedCallback cb'
        connectSignalFunPtr obj "search-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SearchEntry::search-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SearchEntry.html#g:signal:searchChanged"})

#endif

-- signal SearchEntry::stop-search
-- | The [stopSearch](#g:signal:stopSearch) signal is a [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user stops a search via keyboard input.
-- 
-- Applications should connect to it, to implement hiding the search
-- entry in this case.
-- 
-- The default bindings for this signal is Escape.
-- 
-- /Since: 3.16/
type SearchEntryStopSearchCallback =
    IO ()

type C_SearchEntryStopSearchCallback =
    Ptr SearchEntry ->                      -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_SearchEntryStopSearchCallback`.
foreign import ccall "wrapper"
    mk_SearchEntryStopSearchCallback :: C_SearchEntryStopSearchCallback -> IO (FunPtr C_SearchEntryStopSearchCallback)

wrap_SearchEntryStopSearchCallback :: 
    GObject a => (a -> SearchEntryStopSearchCallback) ->
    C_SearchEntryStopSearchCallback
wrap_SearchEntryStopSearchCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [stopSearch](#signal:stopSearch) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' searchEntry #stopSearch callback
-- @
-- 
-- 
onSearchEntryStopSearch :: (IsSearchEntry a, MonadIO m) => a -> ((?self :: a) => SearchEntryStopSearchCallback) -> m SignalHandlerId
onSearchEntryStopSearch obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SearchEntryStopSearchCallback wrapped
    wrapped'' <- mk_SearchEntryStopSearchCallback wrapped'
    connectSignalFunPtr obj "stop-search" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [stopSearch](#signal:stopSearch) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' searchEntry #stopSearch callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterSearchEntryStopSearch :: (IsSearchEntry a, MonadIO m) => a -> ((?self :: a) => SearchEntryStopSearchCallback) -> m SignalHandlerId
afterSearchEntryStopSearch obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_SearchEntryStopSearchCallback wrapped
    wrapped'' <- mk_SearchEntryStopSearchCallback wrapped'
    connectSignalFunPtr obj "stop-search" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data SearchEntryStopSearchSignalInfo
instance SignalInfo SearchEntryStopSearchSignalInfo where
    type HaskellCallbackType SearchEntryStopSearchSignalInfo = SearchEntryStopSearchCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_SearchEntryStopSearchCallback cb
        cb'' <- mk_SearchEntryStopSearchCallback cb'
        connectSignalFunPtr obj "stop-search" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SearchEntry::stop-search"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SearchEntry.html#g:signal:stopSearch"})

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList SearchEntry
type instance O.AttributeList SearchEntry = SearchEntryAttributeList
type SearchEntryAttributeList = ('[ '("activatesDefault", Gtk.Entry.EntryActivatesDefaultPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("attributes", Gtk.Entry.EntryAttributesPropertyInfo), '("buffer", Gtk.Entry.EntryBufferPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("capsLockWarning", Gtk.Entry.EntryCapsLockWarningPropertyInfo), '("completion", Gtk.Entry.EntryCompletionPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("cursorPosition", Gtk.Entry.EntryCursorPositionPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("editable", Gtk.Entry.EntryEditablePropertyInfo), '("editingCanceled", Gtk.CellEditable.CellEditableEditingCanceledPropertyInfo), '("enableEmojiCompletion", Gtk.Entry.EntryEnableEmojiCompletionPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasFrame", Gtk.Entry.EntryHasFramePropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("imModule", Gtk.Entry.EntryImModulePropertyInfo), '("innerBorder", Gtk.Entry.EntryInnerBorderPropertyInfo), '("inputHints", Gtk.Entry.EntryInputHintsPropertyInfo), '("inputPurpose", Gtk.Entry.EntryInputPurposePropertyInfo), '("invisibleChar", Gtk.Entry.EntryInvisibleCharPropertyInfo), '("invisibleCharSet", Gtk.Entry.EntryInvisibleCharSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("maxLength", Gtk.Entry.EntryMaxLengthPropertyInfo), '("maxWidthChars", Gtk.Entry.EntryMaxWidthCharsPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("overwriteMode", Gtk.Entry.EntryOverwriteModePropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("placeholderText", Gtk.Entry.EntryPlaceholderTextPropertyInfo), '("populateAll", Gtk.Entry.EntryPopulateAllPropertyInfo), '("primaryIconActivatable", Gtk.Entry.EntryPrimaryIconActivatablePropertyInfo), '("primaryIconGicon", Gtk.Entry.EntryPrimaryIconGiconPropertyInfo), '("primaryIconName", Gtk.Entry.EntryPrimaryIconNamePropertyInfo), '("primaryIconPixbuf", Gtk.Entry.EntryPrimaryIconPixbufPropertyInfo), '("primaryIconSensitive", Gtk.Entry.EntryPrimaryIconSensitivePropertyInfo), '("primaryIconStock", Gtk.Entry.EntryPrimaryIconStockPropertyInfo), '("primaryIconStorageType", Gtk.Entry.EntryPrimaryIconStorageTypePropertyInfo), '("primaryIconTooltipMarkup", Gtk.Entry.EntryPrimaryIconTooltipMarkupPropertyInfo), '("primaryIconTooltipText", Gtk.Entry.EntryPrimaryIconTooltipTextPropertyInfo), '("progressFraction", Gtk.Entry.EntryProgressFractionPropertyInfo), '("progressPulseStep", Gtk.Entry.EntryProgressPulseStepPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("scrollOffset", Gtk.Entry.EntryScrollOffsetPropertyInfo), '("secondaryIconActivatable", Gtk.Entry.EntrySecondaryIconActivatablePropertyInfo), '("secondaryIconGicon", Gtk.Entry.EntrySecondaryIconGiconPropertyInfo), '("secondaryIconName", Gtk.Entry.EntrySecondaryIconNamePropertyInfo), '("secondaryIconPixbuf", Gtk.Entry.EntrySecondaryIconPixbufPropertyInfo), '("secondaryIconSensitive", Gtk.Entry.EntrySecondaryIconSensitivePropertyInfo), '("secondaryIconStock", Gtk.Entry.EntrySecondaryIconStockPropertyInfo), '("secondaryIconStorageType", Gtk.Entry.EntrySecondaryIconStorageTypePropertyInfo), '("secondaryIconTooltipMarkup", Gtk.Entry.EntrySecondaryIconTooltipMarkupPropertyInfo), '("secondaryIconTooltipText", Gtk.Entry.EntrySecondaryIconTooltipTextPropertyInfo), '("selectionBound", Gtk.Entry.EntrySelectionBoundPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("shadowType", Gtk.Entry.EntryShadowTypePropertyInfo), '("showEmojiIcon", Gtk.Entry.EntryShowEmojiIconPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tabs", Gtk.Entry.EntryTabsPropertyInfo), '("text", Gtk.Entry.EntryTextPropertyInfo), '("textLength", Gtk.Entry.EntryTextLengthPropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("truncateMultiline", Gtk.Entry.EntryTruncateMultilinePropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visibility", Gtk.Entry.EntryVisibilityPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthChars", Gtk.Entry.EntryWidthCharsPropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("xalign", Gtk.Entry.EntryXalignPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList SearchEntry = SearchEntrySignalList
type SearchEntrySignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activate", Gtk.Entry.EntryActivateSignalInfo), '("backspace", Gtk.Entry.EntryBackspaceSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("changed", Gtk.Editable.EditableChangedSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("copyClipboard", Gtk.Entry.EntryCopyClipboardSignalInfo), '("cutClipboard", Gtk.Entry.EntryCutClipboardSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("deleteFromCursor", Gtk.Entry.EntryDeleteFromCursorSignalInfo), '("deleteText", Gtk.Editable.EditableDeleteTextSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("editingDone", Gtk.CellEditable.CellEditableEditingDoneSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("iconPress", Gtk.Entry.EntryIconPressSignalInfo), '("iconRelease", Gtk.Entry.EntryIconReleaseSignalInfo), '("insertAtCursor", Gtk.Entry.EntryInsertAtCursorSignalInfo), '("insertEmoji", Gtk.Entry.EntryInsertEmojiSignalInfo), '("insertText", Gtk.Editable.EditableInsertTextSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveCursor", Gtk.Entry.EntryMoveCursorSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("nextMatch", SearchEntryNextMatchSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("pasteClipboard", Gtk.Entry.EntryPasteClipboardSignalInfo), '("populatePopup", Gtk.Entry.EntryPopulatePopupSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("preeditChanged", Gtk.Entry.EntryPreeditChangedSignalInfo), '("previousMatch", SearchEntryPreviousMatchSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("removeWidget", Gtk.CellEditable.CellEditableRemoveWidgetSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("searchChanged", SearchEntrySearchChangedSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("stopSearch", SearchEntryStopSearchSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("toggleOverwrite", Gtk.Entry.EntryToggleOverwriteSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method SearchEntry::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "SearchEntry" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_search_entry_new" gtk_search_entry_new :: 
    IO (Ptr SearchEntry)

-- | Creates a t'GI.Gtk.Objects.SearchEntry.SearchEntry', with a find icon when the search field is
-- empty, and a clear icon when it isn\'t.
-- 
-- /Since: 3.6/
searchEntryNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m SearchEntry
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.SearchEntry.SearchEntry'
searchEntryNew  = liftIO $ do
    result <- gtk_search_entry_new
    checkUnexpectedReturnNULL "searchEntryNew" result
    result' <- (newObject SearchEntry) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method SearchEntry::handle_event
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SearchEntry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSearchEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "event"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Event" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a key event" , sinceVersion = Nothing }
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

foreign import ccall "gtk_search_entry_handle_event" gtk_search_entry_handle_event :: 
    Ptr SearchEntry ->                      -- entry : TInterface (Name {namespace = "Gtk", name = "SearchEntry"})
    Ptr Gdk.Event.Event ->                  -- event : TInterface (Name {namespace = "Gdk", name = "Event"})
    IO CInt

-- | This function should be called when the top-level window
-- which contains the search entry received a key event. If
-- the entry is part of a t'GI.Gtk.Objects.SearchBar.SearchBar', it is preferable
-- to call 'GI.Gtk.Objects.SearchBar.searchBarHandleEvent' instead, which will
-- reveal the entry in addition to passing the event to this
-- function.
-- 
-- If the key event is handled by the search entry and starts
-- or continues a search, 'GI.Gdk.Constants.EVENT_STOP' will be returned.
-- The caller should ensure that the entry is shown in this
-- case, and not propagate the event further.
-- 
-- /Since: 3.16/
searchEntryHandleEvent ::
    (B.CallStack.HasCallStack, MonadIO m, IsSearchEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.SearchEntry.SearchEntry'
    -> Gdk.Event.Event
    -- ^ /@event@/: a key event
    -> m Bool
    -- ^ __Returns:__ 'GI.Gdk.Constants.EVENT_STOP' if the key press event resulted
    --     in a search beginning or continuing, 'GI.Gdk.Constants.EVENT_PROPAGATE'
    --     otherwise.
searchEntryHandleEvent entry event = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    event' <- unsafeManagedPtrGetPtr event
    result <- gtk_search_entry_handle_event entry' event'
    let result' = (/= 0) result
    touchManagedPtr entry
    touchManagedPtr event
    return result'

#if defined(ENABLE_OVERLOADING)
data SearchEntryHandleEventMethodInfo
instance (signature ~ (Gdk.Event.Event -> m Bool), MonadIO m, IsSearchEntry a) => O.OverloadedMethod SearchEntryHandleEventMethodInfo a signature where
    overloadedMethod = searchEntryHandleEvent

instance O.OverloadedMethodInfo SearchEntryHandleEventMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.SearchEntry.searchEntryHandleEvent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-SearchEntry.html#v:searchEntryHandleEvent"
        })


#endif


