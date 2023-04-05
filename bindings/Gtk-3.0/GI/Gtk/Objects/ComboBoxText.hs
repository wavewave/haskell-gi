{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A GtkComboBoxText is a simple variant of t'GI.Gtk.Objects.ComboBox.ComboBox' that hides
-- the model-view complexity for simple text-only use cases.
-- 
-- To create a GtkComboBoxText, use 'GI.Gtk.Objects.ComboBoxText.comboBoxTextNew' or
-- 'GI.Gtk.Objects.ComboBoxText.comboBoxTextNewWithEntry'.
-- 
-- You can add items to a GtkComboBoxText with
-- 'GI.Gtk.Objects.ComboBoxText.comboBoxTextAppendText', 'GI.Gtk.Objects.ComboBoxText.comboBoxTextInsertText'
-- or 'GI.Gtk.Objects.ComboBoxText.comboBoxTextPrependText' and remove options with
-- 'GI.Gtk.Objects.ComboBoxText.comboBoxTextRemove'.
-- 
-- If the GtkComboBoxText contains an entry (via the “has-entry” property),
-- its contents can be retrieved using 'GI.Gtk.Objects.ComboBoxText.comboBoxTextGetActiveText'.
-- The entry itself can be accessed by calling 'GI.Gtk.Objects.Bin.binGetChild' on the
-- combo box.
-- 
-- You should not call 'GI.Gtk.Objects.ComboBox.comboBoxSetModel' or attempt to pack more cells
-- into this combo box via its GtkCellLayout interface.
-- 
-- = GtkComboBoxText as GtkBuildable
-- 
-- The GtkComboBoxText implementation of the GtkBuildable interface supports
-- adding items directly using the @\<items>@ element and specifying @\<item>@
-- elements for each item. Each @\<item>@ element can specify the “id”
-- corresponding to the appended text and also supports the regular
-- translation attributes “translatable”, “context” and “comments”.
-- 
-- Here is a UI definition fragment specifying GtkComboBoxText items:
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkComboBoxText">
-- >  <items>
-- >    <item translatable="yes" id="factory">Factory</item>
-- >    <item translatable="yes" id="home">Home</item>
-- >    <item translatable="yes" id="subway">Subway</item>
-- >  </items>
-- ></object>
-- 
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >combobox
-- >╰── box.linked
-- >    ├── entry.combo
-- >    ├── button.combo
-- >    ╰── window.popup
-- 
-- 
-- GtkComboBoxText has a single CSS node with name combobox. It adds
-- the style class .combo to the main CSS nodes of its entry and button
-- children, and the .linked class to the node of its internal box.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.ComboBoxText
    ( 

-- * Exported types
    ComboBoxText(..)                        ,
    IsComboBoxText                          ,
    toComboBoxText                          ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addAttribute]("GI.Gtk.Interfaces.CellLayout#g:method:addAttribute"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [append]("GI.Gtk.Objects.ComboBoxText#g:method:append"), [appendText]("GI.Gtk.Objects.ComboBoxText#g:method:appendText"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [clear]("GI.Gtk.Interfaces.CellLayout#g:method:clear"), [clearAttributes]("GI.Gtk.Interfaces.CellLayout#g:method:clearAttributes"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [editingDone]("GI.Gtk.Interfaces.CellEditable#g:method:editingDone"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insert]("GI.Gtk.Objects.ComboBoxText#g:method:insert"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [insertText]("GI.Gtk.Objects.ComboBoxText#g:method:insertText"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [packEnd]("GI.Gtk.Interfaces.CellLayout#g:method:packEnd"), [packStart]("GI.Gtk.Interfaces.CellLayout#g:method:packStart"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [popdown]("GI.Gtk.Objects.ComboBox#g:method:popdown"), [popup]("GI.Gtk.Objects.ComboBox#g:method:popup"), [popupForDevice]("GI.Gtk.Objects.ComboBox#g:method:popupForDevice"), [prepend]("GI.Gtk.Objects.ComboBoxText#g:method:prepend"), [prependText]("GI.Gtk.Objects.ComboBoxText#g:method:prependText"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.ComboBoxText#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeAll]("GI.Gtk.Objects.ComboBoxText#g:method:removeAll"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [removeWidget]("GI.Gtk.Interfaces.CellEditable#g:method:removeWidget"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reorder]("GI.Gtk.Interfaces.CellLayout#g:method:reorder"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [startEditing]("GI.Gtk.Interfaces.CellEditable#g:method:startEditing"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActive]("GI.Gtk.Objects.ComboBox#g:method:getActive"), [getActiveId]("GI.Gtk.Objects.ComboBox#g:method:getActiveId"), [getActiveIter]("GI.Gtk.Objects.ComboBox#g:method:getActiveIter"), [getActiveText]("GI.Gtk.Objects.ComboBoxText#g:method:getActiveText"), [getAddTearoffs]("GI.Gtk.Objects.ComboBox#g:method:getAddTearoffs"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getArea]("GI.Gtk.Interfaces.CellLayout#g:method:getArea"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getButtonSensitivity]("GI.Gtk.Objects.ComboBox#g:method:getButtonSensitivity"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getCells]("GI.Gtk.Interfaces.CellLayout#g:method:getCells"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getColumnSpanColumn]("GI.Gtk.Objects.ComboBox#g:method:getColumnSpanColumn"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEntryTextColumn]("GI.Gtk.Objects.ComboBox#g:method:getEntryTextColumn"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.ComboBox#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasEntry]("GI.Gtk.Objects.ComboBox#g:method:getHasEntry"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getIdColumn]("GI.Gtk.Objects.ComboBox#g:method:getIdColumn"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModel]("GI.Gtk.Objects.ComboBox#g:method:getModel"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPopupAccessible]("GI.Gtk.Objects.ComboBox#g:method:getPopupAccessible"), [getPopupFixedWidth]("GI.Gtk.Objects.ComboBox#g:method:getPopupFixedWidth"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getRowSpanColumn]("GI.Gtk.Objects.ComboBox#g:method:getRowSpanColumn"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTitle]("GI.Gtk.Objects.ComboBox#g:method:getTitle"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow"), [getWrapWidth]("GI.Gtk.Objects.ComboBox#g:method:getWrapWidth").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActive]("GI.Gtk.Objects.ComboBox#g:method:setActive"), [setActiveId]("GI.Gtk.Objects.ComboBox#g:method:setActiveId"), [setActiveIter]("GI.Gtk.Objects.ComboBox#g:method:setActiveIter"), [setAddTearoffs]("GI.Gtk.Objects.ComboBox#g:method:setAddTearoffs"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setButtonSensitivity]("GI.Gtk.Objects.ComboBox#g:method:setButtonSensitivity"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setCellDataFunc]("GI.Gtk.Interfaces.CellLayout#g:method:setCellDataFunc"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setColumnSpanColumn]("GI.Gtk.Objects.ComboBox#g:method:setColumnSpanColumn"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEntryTextColumn]("GI.Gtk.Objects.ComboBox#g:method:setEntryTextColumn"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.ComboBox#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setIdColumn]("GI.Gtk.Objects.ComboBox#g:method:setIdColumn"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setModel]("GI.Gtk.Objects.ComboBox#g:method:setModel"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPopupFixedWidth]("GI.Gtk.Objects.ComboBox#g:method:setPopupFixedWidth"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setRowSeparatorFunc]("GI.Gtk.Objects.ComboBox#g:method:setRowSeparatorFunc"), [setRowSpanColumn]("GI.Gtk.Objects.ComboBox#g:method:setRowSpanColumn"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTitle]("GI.Gtk.Objects.ComboBox#g:method:setTitle"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow"), [setWrapWidth]("GI.Gtk.Objects.ComboBox#g:method:setWrapWidth").

#if defined(ENABLE_OVERLOADING)
    ResolveComboBoxTextMethod               ,
#endif

-- ** append #method:append#

#if defined(ENABLE_OVERLOADING)
    ComboBoxTextAppendMethodInfo            ,
#endif
    comboBoxTextAppend                      ,


-- ** appendText #method:appendText#

#if defined(ENABLE_OVERLOADING)
    ComboBoxTextAppendTextMethodInfo        ,
#endif
    comboBoxTextAppendText                  ,


-- ** getActiveText #method:getActiveText#

#if defined(ENABLE_OVERLOADING)
    ComboBoxTextGetActiveTextMethodInfo     ,
#endif
    comboBoxTextGetActiveText               ,


-- ** insert #method:insert#

#if defined(ENABLE_OVERLOADING)
    ComboBoxTextInsertMethodInfo            ,
#endif
    comboBoxTextInsert                      ,


-- ** insertText #method:insertText#

#if defined(ENABLE_OVERLOADING)
    ComboBoxTextInsertTextMethodInfo        ,
#endif
    comboBoxTextInsertText                  ,


-- ** new #method:new#

    comboBoxTextNew                         ,


-- ** newWithEntry #method:newWithEntry#

    comboBoxTextNewWithEntry                ,


-- ** prepend #method:prepend#

#if defined(ENABLE_OVERLOADING)
    ComboBoxTextPrependMethodInfo           ,
#endif
    comboBoxTextPrepend                     ,


-- ** prependText #method:prependText#

#if defined(ENABLE_OVERLOADING)
    ComboBoxTextPrependTextMethodInfo       ,
#endif
    comboBoxTextPrependText                 ,


-- ** remove #method:remove#

#if defined(ENABLE_OVERLOADING)
    ComboBoxTextRemoveMethodInfo            ,
#endif
    comboBoxTextRemove                      ,


-- ** removeAll #method:removeAll#

#if defined(ENABLE_OVERLOADING)
    ComboBoxTextRemoveAllMethodInfo         ,
#endif
    comboBoxTextRemoveAll                   ,




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
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.CellEditable as Gtk.CellEditable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.CellLayout as Gtk.CellLayout
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.ComboBox as Gtk.ComboBox
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype ComboBoxText = ComboBoxText (SP.ManagedPtr ComboBoxText)
    deriving (Eq)

instance SP.ManagedPtrNewtype ComboBoxText where
    toManagedPtr (ComboBoxText p) = p

foreign import ccall "gtk_combo_box_text_get_type"
    c_gtk_combo_box_text_get_type :: IO B.Types.GType

instance B.Types.TypedObject ComboBoxText where
    glibType = c_gtk_combo_box_text_get_type

instance B.Types.GObject ComboBoxText

-- | Type class for types which can be safely cast to `ComboBoxText`, for instance with `toComboBoxText`.
class (SP.GObject o, O.IsDescendantOf ComboBoxText o) => IsComboBoxText o
instance (SP.GObject o, O.IsDescendantOf ComboBoxText o) => IsComboBoxText o

instance O.HasParentTypes ComboBoxText
type instance O.ParentTypes ComboBoxText = '[Gtk.ComboBox.ComboBox, Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable, Gtk.CellEditable.CellEditable, Gtk.CellLayout.CellLayout]

-- | Cast to `ComboBoxText`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toComboBoxText :: (MIO.MonadIO m, IsComboBoxText o) => o -> m ComboBoxText
toComboBoxText = MIO.liftIO . B.ManagedPtr.unsafeCastTo ComboBoxText

-- | Convert 'ComboBoxText' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe ComboBoxText) where
    gvalueGType_ = c_gtk_combo_box_text_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr ComboBoxText)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr ComboBoxText)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject ComboBoxText ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveComboBoxTextMethod (t :: Symbol) (o :: *) :: * where
    ResolveComboBoxTextMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveComboBoxTextMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveComboBoxTextMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveComboBoxTextMethod "addAttribute" o = Gtk.CellLayout.CellLayoutAddAttributeMethodInfo
    ResolveComboBoxTextMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveComboBoxTextMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveComboBoxTextMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveComboBoxTextMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveComboBoxTextMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveComboBoxTextMethod "append" o = ComboBoxTextAppendMethodInfo
    ResolveComboBoxTextMethod "appendText" o = ComboBoxTextAppendTextMethodInfo
    ResolveComboBoxTextMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveComboBoxTextMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveComboBoxTextMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveComboBoxTextMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveComboBoxTextMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveComboBoxTextMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveComboBoxTextMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveComboBoxTextMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveComboBoxTextMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveComboBoxTextMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveComboBoxTextMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveComboBoxTextMethod "clear" o = Gtk.CellLayout.CellLayoutClearMethodInfo
    ResolveComboBoxTextMethod "clearAttributes" o = Gtk.CellLayout.CellLayoutClearAttributesMethodInfo
    ResolveComboBoxTextMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveComboBoxTextMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveComboBoxTextMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveComboBoxTextMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveComboBoxTextMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveComboBoxTextMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveComboBoxTextMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveComboBoxTextMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveComboBoxTextMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveComboBoxTextMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveComboBoxTextMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveComboBoxTextMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveComboBoxTextMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveComboBoxTextMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveComboBoxTextMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveComboBoxTextMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveComboBoxTextMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveComboBoxTextMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveComboBoxTextMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveComboBoxTextMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveComboBoxTextMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveComboBoxTextMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveComboBoxTextMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveComboBoxTextMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveComboBoxTextMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveComboBoxTextMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveComboBoxTextMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveComboBoxTextMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveComboBoxTextMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveComboBoxTextMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveComboBoxTextMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveComboBoxTextMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveComboBoxTextMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveComboBoxTextMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveComboBoxTextMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveComboBoxTextMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveComboBoxTextMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveComboBoxTextMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveComboBoxTextMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveComboBoxTextMethod "editingDone" o = Gtk.CellEditable.CellEditableEditingDoneMethodInfo
    ResolveComboBoxTextMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveComboBoxTextMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveComboBoxTextMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveComboBoxTextMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveComboBoxTextMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveComboBoxTextMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveComboBoxTextMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveComboBoxTextMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveComboBoxTextMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveComboBoxTextMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveComboBoxTextMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveComboBoxTextMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveComboBoxTextMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveComboBoxTextMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveComboBoxTextMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveComboBoxTextMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveComboBoxTextMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveComboBoxTextMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveComboBoxTextMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveComboBoxTextMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveComboBoxTextMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveComboBoxTextMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveComboBoxTextMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveComboBoxTextMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveComboBoxTextMethod "insert" o = ComboBoxTextInsertMethodInfo
    ResolveComboBoxTextMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveComboBoxTextMethod "insertText" o = ComboBoxTextInsertTextMethodInfo
    ResolveComboBoxTextMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveComboBoxTextMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveComboBoxTextMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveComboBoxTextMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveComboBoxTextMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveComboBoxTextMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveComboBoxTextMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveComboBoxTextMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveComboBoxTextMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveComboBoxTextMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveComboBoxTextMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveComboBoxTextMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveComboBoxTextMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveComboBoxTextMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveComboBoxTextMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveComboBoxTextMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveComboBoxTextMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveComboBoxTextMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveComboBoxTextMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveComboBoxTextMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveComboBoxTextMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveComboBoxTextMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveComboBoxTextMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveComboBoxTextMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveComboBoxTextMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveComboBoxTextMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveComboBoxTextMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveComboBoxTextMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveComboBoxTextMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveComboBoxTextMethod "packEnd" o = Gtk.CellLayout.CellLayoutPackEndMethodInfo
    ResolveComboBoxTextMethod "packStart" o = Gtk.CellLayout.CellLayoutPackStartMethodInfo
    ResolveComboBoxTextMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveComboBoxTextMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveComboBoxTextMethod "popdown" o = Gtk.ComboBox.ComboBoxPopdownMethodInfo
    ResolveComboBoxTextMethod "popup" o = Gtk.ComboBox.ComboBoxPopupMethodInfo
    ResolveComboBoxTextMethod "popupForDevice" o = Gtk.ComboBox.ComboBoxPopupForDeviceMethodInfo
    ResolveComboBoxTextMethod "prepend" o = ComboBoxTextPrependMethodInfo
    ResolveComboBoxTextMethod "prependText" o = ComboBoxTextPrependTextMethodInfo
    ResolveComboBoxTextMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveComboBoxTextMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveComboBoxTextMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveComboBoxTextMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveComboBoxTextMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveComboBoxTextMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveComboBoxTextMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveComboBoxTextMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveComboBoxTextMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveComboBoxTextMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveComboBoxTextMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveComboBoxTextMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveComboBoxTextMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveComboBoxTextMethod "remove" o = ComboBoxTextRemoveMethodInfo
    ResolveComboBoxTextMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveComboBoxTextMethod "removeAll" o = ComboBoxTextRemoveAllMethodInfo
    ResolveComboBoxTextMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveComboBoxTextMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveComboBoxTextMethod "removeWidget" o = Gtk.CellEditable.CellEditableRemoveWidgetMethodInfo
    ResolveComboBoxTextMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveComboBoxTextMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveComboBoxTextMethod "reorder" o = Gtk.CellLayout.CellLayoutReorderMethodInfo
    ResolveComboBoxTextMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveComboBoxTextMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveComboBoxTextMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveComboBoxTextMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveComboBoxTextMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveComboBoxTextMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveComboBoxTextMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveComboBoxTextMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveComboBoxTextMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveComboBoxTextMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveComboBoxTextMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveComboBoxTextMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveComboBoxTextMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveComboBoxTextMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveComboBoxTextMethod "startEditing" o = Gtk.CellEditable.CellEditableStartEditingMethodInfo
    ResolveComboBoxTextMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveComboBoxTextMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveComboBoxTextMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveComboBoxTextMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveComboBoxTextMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveComboBoxTextMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveComboBoxTextMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveComboBoxTextMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveComboBoxTextMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveComboBoxTextMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveComboBoxTextMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveComboBoxTextMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveComboBoxTextMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveComboBoxTextMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveComboBoxTextMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveComboBoxTextMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveComboBoxTextMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveComboBoxTextMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveComboBoxTextMethod "getActive" o = Gtk.ComboBox.ComboBoxGetActiveMethodInfo
    ResolveComboBoxTextMethod "getActiveId" o = Gtk.ComboBox.ComboBoxGetActiveIdMethodInfo
    ResolveComboBoxTextMethod "getActiveIter" o = Gtk.ComboBox.ComboBoxGetActiveIterMethodInfo
    ResolveComboBoxTextMethod "getActiveText" o = ComboBoxTextGetActiveTextMethodInfo
    ResolveComboBoxTextMethod "getAddTearoffs" o = Gtk.ComboBox.ComboBoxGetAddTearoffsMethodInfo
    ResolveComboBoxTextMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveComboBoxTextMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveComboBoxTextMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveComboBoxTextMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveComboBoxTextMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveComboBoxTextMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveComboBoxTextMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveComboBoxTextMethod "getArea" o = Gtk.CellLayout.CellLayoutGetAreaMethodInfo
    ResolveComboBoxTextMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveComboBoxTextMethod "getButtonSensitivity" o = Gtk.ComboBox.ComboBoxGetButtonSensitivityMethodInfo
    ResolveComboBoxTextMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveComboBoxTextMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveComboBoxTextMethod "getCells" o = Gtk.CellLayout.CellLayoutGetCellsMethodInfo
    ResolveComboBoxTextMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveComboBoxTextMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveComboBoxTextMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveComboBoxTextMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveComboBoxTextMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveComboBoxTextMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveComboBoxTextMethod "getColumnSpanColumn" o = Gtk.ComboBox.ComboBoxGetColumnSpanColumnMethodInfo
    ResolveComboBoxTextMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveComboBoxTextMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveComboBoxTextMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveComboBoxTextMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveComboBoxTextMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveComboBoxTextMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveComboBoxTextMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveComboBoxTextMethod "getEntryTextColumn" o = Gtk.ComboBox.ComboBoxGetEntryTextColumnMethodInfo
    ResolveComboBoxTextMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveComboBoxTextMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveComboBoxTextMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveComboBoxTextMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveComboBoxTextMethod "getFocusOnClick" o = Gtk.ComboBox.ComboBoxGetFocusOnClickMethodInfo
    ResolveComboBoxTextMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveComboBoxTextMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveComboBoxTextMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveComboBoxTextMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveComboBoxTextMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveComboBoxTextMethod "getHasEntry" o = Gtk.ComboBox.ComboBoxGetHasEntryMethodInfo
    ResolveComboBoxTextMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveComboBoxTextMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveComboBoxTextMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveComboBoxTextMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveComboBoxTextMethod "getIdColumn" o = Gtk.ComboBox.ComboBoxGetIdColumnMethodInfo
    ResolveComboBoxTextMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveComboBoxTextMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveComboBoxTextMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveComboBoxTextMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveComboBoxTextMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveComboBoxTextMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveComboBoxTextMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveComboBoxTextMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveComboBoxTextMethod "getModel" o = Gtk.ComboBox.ComboBoxGetModelMethodInfo
    ResolveComboBoxTextMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveComboBoxTextMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveComboBoxTextMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveComboBoxTextMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveComboBoxTextMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveComboBoxTextMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveComboBoxTextMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveComboBoxTextMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveComboBoxTextMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveComboBoxTextMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveComboBoxTextMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveComboBoxTextMethod "getPopupAccessible" o = Gtk.ComboBox.ComboBoxGetPopupAccessibleMethodInfo
    ResolveComboBoxTextMethod "getPopupFixedWidth" o = Gtk.ComboBox.ComboBoxGetPopupFixedWidthMethodInfo
    ResolveComboBoxTextMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveComboBoxTextMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveComboBoxTextMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveComboBoxTextMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveComboBoxTextMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveComboBoxTextMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveComboBoxTextMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveComboBoxTextMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveComboBoxTextMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveComboBoxTextMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveComboBoxTextMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveComboBoxTextMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveComboBoxTextMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveComboBoxTextMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveComboBoxTextMethod "getRowSpanColumn" o = Gtk.ComboBox.ComboBoxGetRowSpanColumnMethodInfo
    ResolveComboBoxTextMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveComboBoxTextMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveComboBoxTextMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveComboBoxTextMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveComboBoxTextMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveComboBoxTextMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveComboBoxTextMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveComboBoxTextMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveComboBoxTextMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveComboBoxTextMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveComboBoxTextMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveComboBoxTextMethod "getTitle" o = Gtk.ComboBox.ComboBoxGetTitleMethodInfo
    ResolveComboBoxTextMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveComboBoxTextMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveComboBoxTextMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveComboBoxTextMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveComboBoxTextMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveComboBoxTextMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveComboBoxTextMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveComboBoxTextMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveComboBoxTextMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveComboBoxTextMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveComboBoxTextMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveComboBoxTextMethod "getWrapWidth" o = Gtk.ComboBox.ComboBoxGetWrapWidthMethodInfo
    ResolveComboBoxTextMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveComboBoxTextMethod "setActive" o = Gtk.ComboBox.ComboBoxSetActiveMethodInfo
    ResolveComboBoxTextMethod "setActiveId" o = Gtk.ComboBox.ComboBoxSetActiveIdMethodInfo
    ResolveComboBoxTextMethod "setActiveIter" o = Gtk.ComboBox.ComboBoxSetActiveIterMethodInfo
    ResolveComboBoxTextMethod "setAddTearoffs" o = Gtk.ComboBox.ComboBoxSetAddTearoffsMethodInfo
    ResolveComboBoxTextMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveComboBoxTextMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveComboBoxTextMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveComboBoxTextMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveComboBoxTextMethod "setButtonSensitivity" o = Gtk.ComboBox.ComboBoxSetButtonSensitivityMethodInfo
    ResolveComboBoxTextMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveComboBoxTextMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveComboBoxTextMethod "setCellDataFunc" o = Gtk.CellLayout.CellLayoutSetCellDataFuncMethodInfo
    ResolveComboBoxTextMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveComboBoxTextMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveComboBoxTextMethod "setColumnSpanColumn" o = Gtk.ComboBox.ComboBoxSetColumnSpanColumnMethodInfo
    ResolveComboBoxTextMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveComboBoxTextMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveComboBoxTextMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveComboBoxTextMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveComboBoxTextMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveComboBoxTextMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveComboBoxTextMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveComboBoxTextMethod "setEntryTextColumn" o = Gtk.ComboBox.ComboBoxSetEntryTextColumnMethodInfo
    ResolveComboBoxTextMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveComboBoxTextMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveComboBoxTextMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveComboBoxTextMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveComboBoxTextMethod "setFocusOnClick" o = Gtk.ComboBox.ComboBoxSetFocusOnClickMethodInfo
    ResolveComboBoxTextMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveComboBoxTextMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveComboBoxTextMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveComboBoxTextMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveComboBoxTextMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveComboBoxTextMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveComboBoxTextMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveComboBoxTextMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveComboBoxTextMethod "setIdColumn" o = Gtk.ComboBox.ComboBoxSetIdColumnMethodInfo
    ResolveComboBoxTextMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveComboBoxTextMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveComboBoxTextMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveComboBoxTextMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveComboBoxTextMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveComboBoxTextMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveComboBoxTextMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveComboBoxTextMethod "setModel" o = Gtk.ComboBox.ComboBoxSetModelMethodInfo
    ResolveComboBoxTextMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveComboBoxTextMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveComboBoxTextMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveComboBoxTextMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveComboBoxTextMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveComboBoxTextMethod "setPopupFixedWidth" o = Gtk.ComboBox.ComboBoxSetPopupFixedWidthMethodInfo
    ResolveComboBoxTextMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveComboBoxTextMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveComboBoxTextMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveComboBoxTextMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveComboBoxTextMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveComboBoxTextMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveComboBoxTextMethod "setRowSeparatorFunc" o = Gtk.ComboBox.ComboBoxSetRowSeparatorFuncMethodInfo
    ResolveComboBoxTextMethod "setRowSpanColumn" o = Gtk.ComboBox.ComboBoxSetRowSpanColumnMethodInfo
    ResolveComboBoxTextMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveComboBoxTextMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveComboBoxTextMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveComboBoxTextMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveComboBoxTextMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveComboBoxTextMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveComboBoxTextMethod "setTitle" o = Gtk.ComboBox.ComboBoxSetTitleMethodInfo
    ResolveComboBoxTextMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveComboBoxTextMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveComboBoxTextMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveComboBoxTextMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveComboBoxTextMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveComboBoxTextMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveComboBoxTextMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveComboBoxTextMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveComboBoxTextMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveComboBoxTextMethod "setWrapWidth" o = Gtk.ComboBox.ComboBoxSetWrapWidthMethodInfo
    ResolveComboBoxTextMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveComboBoxTextMethod t ComboBoxText, O.OverloadedMethod info ComboBoxText p) => OL.IsLabel t (ComboBoxText -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveComboBoxTextMethod t ComboBoxText, O.OverloadedMethod info ComboBoxText p, R.HasField t ComboBoxText p) => R.HasField t ComboBoxText p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveComboBoxTextMethod t ComboBoxText, O.OverloadedMethodInfo info ComboBoxText) => OL.IsLabel t (O.MethodProxy info ComboBoxText) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ComboBoxText
type instance O.AttributeList ComboBoxText = ComboBoxTextAttributeList
type ComboBoxTextAttributeList = ('[ '("active", Gtk.ComboBox.ComboBoxActivePropertyInfo), '("activeId", Gtk.ComboBox.ComboBoxActiveIdPropertyInfo), '("addTearoffs", Gtk.ComboBox.ComboBoxAddTearoffsPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("buttonSensitivity", Gtk.ComboBox.ComboBoxButtonSensitivityPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("cellArea", Gtk.ComboBox.ComboBoxCellAreaPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("columnSpanColumn", Gtk.ComboBox.ComboBoxColumnSpanColumnPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("editingCanceled", Gtk.CellEditable.CellEditableEditingCanceledPropertyInfo), '("entryTextColumn", Gtk.ComboBox.ComboBoxEntryTextColumnPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasEntry", Gtk.ComboBox.ComboBoxHasEntryPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasFrame", Gtk.ComboBox.ComboBoxHasFramePropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("idColumn", Gtk.ComboBox.ComboBoxIdColumnPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("model", Gtk.ComboBox.ComboBoxModelPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("popupFixedWidth", Gtk.ComboBox.ComboBoxPopupFixedWidthPropertyInfo), '("popupShown", Gtk.ComboBox.ComboBoxPopupShownPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("rowSpanColumn", Gtk.ComboBox.ComboBoxRowSpanColumnPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tearoffTitle", Gtk.ComboBox.ComboBoxTearoffTitlePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("wrapWidth", Gtk.ComboBox.ComboBoxWrapWidthPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList ComboBoxText = ComboBoxTextSignalList
type ComboBoxTextSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("changed", Gtk.ComboBox.ComboBoxChangedSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("editingDone", Gtk.CellEditable.CellEditableEditingDoneSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("formatEntryText", Gtk.ComboBox.ComboBoxFormatEntryTextSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveActive", Gtk.ComboBox.ComboBoxMoveActiveSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popdown", Gtk.ComboBox.ComboBoxPopdownSignalInfo), '("popup", Gtk.ComboBox.ComboBoxPopupSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("removeWidget", Gtk.CellEditable.CellEditableRemoveWidgetSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method ComboBoxText::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "ComboBoxText" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_combo_box_text_new" gtk_combo_box_text_new :: 
    IO (Ptr ComboBoxText)

-- | Creates a new t'GI.Gtk.Objects.ComboBoxText.ComboBoxText', which is a t'GI.Gtk.Objects.ComboBox.ComboBox' just displaying
-- strings.
-- 
-- /Since: 2.24/
comboBoxTextNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m ComboBoxText
    -- ^ __Returns:__ A new t'GI.Gtk.Objects.ComboBoxText.ComboBoxText'
comboBoxTextNew  = liftIO $ do
    result <- gtk_combo_box_text_new
    checkUnexpectedReturnNULL "comboBoxTextNew" result
    result' <- (newObject ComboBoxText) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ComboBoxText::new_with_entry
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "ComboBoxText" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_combo_box_text_new_with_entry" gtk_combo_box_text_new_with_entry :: 
    IO (Ptr ComboBoxText)

-- | Creates a new t'GI.Gtk.Objects.ComboBoxText.ComboBoxText', which is a t'GI.Gtk.Objects.ComboBox.ComboBox' just displaying
-- strings. The combo box created by this function has an entry.
-- 
-- /Since: 2.24/
comboBoxTextNewWithEntry ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m ComboBoxText
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.ComboBoxText.ComboBoxText'
comboBoxTextNewWithEntry  = liftIO $ do
    result <- gtk_combo_box_text_new_with_entry
    checkUnexpectedReturnNULL "comboBoxTextNewWithEntry" result
    result' <- (newObject ComboBoxText) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ComboBoxText::append
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBoxText" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkComboBoxText" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a string ID for this value, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A string" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_text_append" gtk_combo_box_text_append :: 
    Ptr ComboBoxText ->                     -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBoxText"})
    CString ->                              -- id : TBasicType TUTF8
    CString ->                              -- text : TBasicType TUTF8
    IO ()

-- | Appends /@text@/ to the list of strings stored in /@comboBox@/.
-- If /@id@/ is non-'P.Nothing' then it is used as the ID of the row.
-- 
-- This is the same as calling 'GI.Gtk.Objects.ComboBoxText.comboBoxTextInsert' with a
-- position of -1.
-- 
-- /Since: 2.24/
comboBoxTextAppend ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBoxText a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBoxText.ComboBoxText'
    -> Maybe (T.Text)
    -- ^ /@id@/: a string ID for this value, or 'P.Nothing'
    -> T.Text
    -- ^ /@text@/: A string
    -> m ()
comboBoxTextAppend comboBox id text = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    maybeId <- case id of
        Nothing -> return nullPtr
        Just jId -> do
            jId' <- textToCString jId
            return jId'
    text' <- textToCString text
    gtk_combo_box_text_append comboBox' maybeId text'
    touchManagedPtr comboBox
    freeMem maybeId
    freeMem text'
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxTextAppendMethodInfo
instance (signature ~ (Maybe (T.Text) -> T.Text -> m ()), MonadIO m, IsComboBoxText a) => O.OverloadedMethod ComboBoxTextAppendMethodInfo a signature where
    overloadedMethod = comboBoxTextAppend

instance O.OverloadedMethodInfo ComboBoxTextAppendMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBoxText.comboBoxTextAppend",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBoxText.html#v:comboBoxTextAppend"
        })


#endif

-- method ComboBoxText::append_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBoxText" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkComboBoxText" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A string" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_text_append_text" gtk_combo_box_text_append_text :: 
    Ptr ComboBoxText ->                     -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBoxText"})
    CString ->                              -- text : TBasicType TUTF8
    IO ()

-- | Appends /@text@/ to the list of strings stored in /@comboBox@/.
-- 
-- This is the same as calling 'GI.Gtk.Objects.ComboBoxText.comboBoxTextInsertText' with a
-- position of -1.
-- 
-- /Since: 2.24/
comboBoxTextAppendText ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBoxText a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBoxText.ComboBoxText'
    -> T.Text
    -- ^ /@text@/: A string
    -> m ()
comboBoxTextAppendText comboBox text = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    text' <- textToCString text
    gtk_combo_box_text_append_text comboBox' text'
    touchManagedPtr comboBox
    freeMem text'
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxTextAppendTextMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsComboBoxText a) => O.OverloadedMethod ComboBoxTextAppendTextMethodInfo a signature where
    overloadedMethod = comboBoxTextAppendText

instance O.OverloadedMethodInfo ComboBoxTextAppendTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBoxText.comboBoxTextAppendText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBoxText.html#v:comboBoxTextAppendText"
        })


#endif

-- method ComboBoxText::get_active_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBoxText" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkComboBoxText" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_text_get_active_text" gtk_combo_box_text_get_active_text :: 
    Ptr ComboBoxText ->                     -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBoxText"})
    IO CString

-- | Returns the currently active string in /@comboBox@/, or 'P.Nothing'
-- if none is selected. If /@comboBox@/ contains an entry, this
-- function will return its contents (which will not necessarily
-- be an item from the list).
-- 
-- /Since: 2.24/
comboBoxTextGetActiveText ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBoxText a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBoxText.ComboBoxText'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ a newly allocated string containing the
    --     currently active text. Must be freed with 'GI.GLib.Functions.free'.
comboBoxTextGetActiveText comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    result <- gtk_combo_box_text_get_active_text comboBox'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        freeMem result'
        return result''
    touchManagedPtr comboBox
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ComboBoxTextGetActiveTextMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsComboBoxText a) => O.OverloadedMethod ComboBoxTextGetActiveTextMethodInfo a signature where
    overloadedMethod = comboBoxTextGetActiveText

instance O.OverloadedMethodInfo ComboBoxTextGetActiveTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBoxText.comboBoxTextGetActiveText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBoxText.html#v:comboBoxTextGetActiveText"
        })


#endif

-- method ComboBoxText::insert
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBoxText" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkComboBoxText" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "An index to insert @text"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a string ID for this value, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A string to display"
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

foreign import ccall "gtk_combo_box_text_insert" gtk_combo_box_text_insert :: 
    Ptr ComboBoxText ->                     -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBoxText"})
    Int32 ->                                -- position : TBasicType TInt
    CString ->                              -- id : TBasicType TUTF8
    CString ->                              -- text : TBasicType TUTF8
    IO ()

-- | Inserts /@text@/ at /@position@/ in the list of strings stored in /@comboBox@/.
-- If /@id@/ is non-'P.Nothing' then it is used as the ID of the row.  See
-- [ComboBox:idColumn]("GI.Gtk.Objects.ComboBox#g:attr:idColumn").
-- 
-- If /@position@/ is negative then /@text@/ is appended.
-- 
-- /Since: 3.0/
comboBoxTextInsert ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBoxText a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBoxText.ComboBoxText'
    -> Int32
    -- ^ /@position@/: An index to insert /@text@/
    -> Maybe (T.Text)
    -- ^ /@id@/: a string ID for this value, or 'P.Nothing'
    -> T.Text
    -- ^ /@text@/: A string to display
    -> m ()
comboBoxTextInsert comboBox position id text = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    maybeId <- case id of
        Nothing -> return nullPtr
        Just jId -> do
            jId' <- textToCString jId
            return jId'
    text' <- textToCString text
    gtk_combo_box_text_insert comboBox' position maybeId text'
    touchManagedPtr comboBox
    freeMem maybeId
    freeMem text'
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxTextInsertMethodInfo
instance (signature ~ (Int32 -> Maybe (T.Text) -> T.Text -> m ()), MonadIO m, IsComboBoxText a) => O.OverloadedMethod ComboBoxTextInsertMethodInfo a signature where
    overloadedMethod = comboBoxTextInsert

instance O.OverloadedMethodInfo ComboBoxTextInsertMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBoxText.comboBoxTextInsert",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBoxText.html#v:comboBoxTextInsert"
        })


#endif

-- method ComboBoxText::insert_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBoxText" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkComboBoxText" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "An index to insert @text"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A string" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_text_insert_text" gtk_combo_box_text_insert_text :: 
    Ptr ComboBoxText ->                     -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBoxText"})
    Int32 ->                                -- position : TBasicType TInt
    CString ->                              -- text : TBasicType TUTF8
    IO ()

-- | Inserts /@text@/ at /@position@/ in the list of strings stored in /@comboBox@/.
-- 
-- If /@position@/ is negative then /@text@/ is appended.
-- 
-- This is the same as calling 'GI.Gtk.Objects.ComboBoxText.comboBoxTextInsert' with a 'P.Nothing'
-- ID string.
-- 
-- /Since: 2.24/
comboBoxTextInsertText ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBoxText a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBoxText.ComboBoxText'
    -> Int32
    -- ^ /@position@/: An index to insert /@text@/
    -> T.Text
    -- ^ /@text@/: A string
    -> m ()
comboBoxTextInsertText comboBox position text = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    text' <- textToCString text
    gtk_combo_box_text_insert_text comboBox' position text'
    touchManagedPtr comboBox
    freeMem text'
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxTextInsertTextMethodInfo
instance (signature ~ (Int32 -> T.Text -> m ()), MonadIO m, IsComboBoxText a) => O.OverloadedMethod ComboBoxTextInsertTextMethodInfo a signature where
    overloadedMethod = comboBoxTextInsertText

instance O.OverloadedMethodInfo ComboBoxTextInsertTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBoxText.comboBoxTextInsertText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBoxText.html#v:comboBoxTextInsertText"
        })


#endif

-- method ComboBoxText::prepend
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBoxText" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkComboBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a string ID for this value, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a string" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_text_prepend" gtk_combo_box_text_prepend :: 
    Ptr ComboBoxText ->                     -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBoxText"})
    CString ->                              -- id : TBasicType TUTF8
    CString ->                              -- text : TBasicType TUTF8
    IO ()

-- | Prepends /@text@/ to the list of strings stored in /@comboBox@/.
-- If /@id@/ is non-'P.Nothing' then it is used as the ID of the row.
-- 
-- This is the same as calling 'GI.Gtk.Objects.ComboBoxText.comboBoxTextInsert' with a
-- position of 0.
-- 
-- /Since: 2.24/
comboBoxTextPrepend ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBoxText a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> Maybe (T.Text)
    -- ^ /@id@/: a string ID for this value, or 'P.Nothing'
    -> T.Text
    -- ^ /@text@/: a string
    -> m ()
comboBoxTextPrepend comboBox id text = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    maybeId <- case id of
        Nothing -> return nullPtr
        Just jId -> do
            jId' <- textToCString jId
            return jId'
    text' <- textToCString text
    gtk_combo_box_text_prepend comboBox' maybeId text'
    touchManagedPtr comboBox
    freeMem maybeId
    freeMem text'
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxTextPrependMethodInfo
instance (signature ~ (Maybe (T.Text) -> T.Text -> m ()), MonadIO m, IsComboBoxText a) => O.OverloadedMethod ComboBoxTextPrependMethodInfo a signature where
    overloadedMethod = comboBoxTextPrepend

instance O.OverloadedMethodInfo ComboBoxTextPrependMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBoxText.comboBoxTextPrepend",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBoxText.html#v:comboBoxTextPrepend"
        })


#endif

-- method ComboBoxText::prepend_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBoxText" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkComboBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A string" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_text_prepend_text" gtk_combo_box_text_prepend_text :: 
    Ptr ComboBoxText ->                     -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBoxText"})
    CString ->                              -- text : TBasicType TUTF8
    IO ()

-- | Prepends /@text@/ to the list of strings stored in /@comboBox@/.
-- 
-- This is the same as calling 'GI.Gtk.Objects.ComboBoxText.comboBoxTextInsertText' with a
-- position of 0.
-- 
-- /Since: 2.24/
comboBoxTextPrependText ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBoxText a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> T.Text
    -- ^ /@text@/: A string
    -> m ()
comboBoxTextPrependText comboBox text = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    text' <- textToCString text
    gtk_combo_box_text_prepend_text comboBox' text'
    touchManagedPtr comboBox
    freeMem text'
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxTextPrependTextMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsComboBoxText a) => O.OverloadedMethod ComboBoxTextPrependTextMethodInfo a signature where
    overloadedMethod = comboBoxTextPrependText

instance O.OverloadedMethodInfo ComboBoxTextPrependTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBoxText.comboBoxTextPrependText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBoxText.html#v:comboBoxTextPrependText"
        })


#endif

-- method ComboBoxText::remove
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBoxText" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkComboBox" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "Index of the item to remove"
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

foreign import ccall "gtk_combo_box_text_remove" gtk_combo_box_text_remove :: 
    Ptr ComboBoxText ->                     -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBoxText"})
    Int32 ->                                -- position : TBasicType TInt
    IO ()

-- | Removes the string at /@position@/ from /@comboBox@/.
-- 
-- /Since: 2.24/
comboBoxTextRemove ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBoxText a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> Int32
    -- ^ /@position@/: Index of the item to remove
    -> m ()
comboBoxTextRemove comboBox position = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    gtk_combo_box_text_remove comboBox' position
    touchManagedPtr comboBox
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxTextRemoveMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsComboBoxText a) => O.OverloadedMethod ComboBoxTextRemoveMethodInfo a signature where
    overloadedMethod = comboBoxTextRemove

instance O.OverloadedMethodInfo ComboBoxTextRemoveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBoxText.comboBoxTextRemove",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBoxText.html#v:comboBoxTextRemove"
        })


#endif

-- method ComboBoxText::remove_all
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBoxText" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkComboBoxText" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_text_remove_all" gtk_combo_box_text_remove_all :: 
    Ptr ComboBoxText ->                     -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBoxText"})
    IO ()

-- | Removes all the text entries from the combo box.
-- 
-- /Since: 3.0/
comboBoxTextRemoveAll ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBoxText a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBoxText.ComboBoxText'
    -> m ()
comboBoxTextRemoveAll comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    gtk_combo_box_text_remove_all comboBox'
    touchManagedPtr comboBox
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxTextRemoveAllMethodInfo
instance (signature ~ (m ()), MonadIO m, IsComboBoxText a) => O.OverloadedMethod ComboBoxTextRemoveAllMethodInfo a signature where
    overloadedMethod = comboBoxTextRemoveAll

instance O.OverloadedMethodInfo ComboBoxTextRemoveAllMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBoxText.comboBoxTextRemoveAll",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBoxText.html#v:comboBoxTextRemoveAll"
        })


#endif


