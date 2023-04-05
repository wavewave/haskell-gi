{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A GtkComboBox is a widget that allows the user to choose from a list of
-- valid choices. The GtkComboBox displays the selected choice. When
-- activated, the GtkComboBox displays a popup which allows the user to
-- make a new choice. The style in which the selected value is displayed,
-- and the style of the popup is determined by the current theme. It may
-- be similar to a Windows-style combo box.
-- 
-- The GtkComboBox uses the model-view pattern; the list of valid choices
-- is specified in the form of a tree model, and the display of the choices
-- can be adapted to the data in the model by using cell renderers, as you
-- would in a tree view. This is possible since GtkComboBox implements the
-- t'GI.Gtk.Interfaces.CellLayout.CellLayout' interface. The tree model holding the valid choices is
-- not restricted to a flat list, it can be a real tree, and the popup will
-- reflect the tree structure.
-- 
-- To allow the user to enter values not in the model, the “has-entry”
-- property allows the GtkComboBox to contain a t'GI.Gtk.Objects.Entry.Entry'. This entry
-- can be accessed by calling 'GI.Gtk.Objects.Bin.binGetChild' on the combo box.
-- 
-- For a simple list of textual choices, the model-view API of GtkComboBox
-- can be a bit overwhelming. In this case, t'GI.Gtk.Objects.ComboBoxText.ComboBoxText' offers a
-- simple alternative. Both GtkComboBox and t'GI.Gtk.Objects.ComboBoxText.ComboBoxText' can contain
-- an entry.
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >combobox
-- >├── box.linked
-- >│   ╰── button.combo
-- >│       ╰── box
-- >│           ├── cellview
-- >│           ╰── arrow
-- >╰── window.popup
-- 
-- 
-- A normal combobox contains a box with the .linked class, a button
-- with the .combo class and inside those buttons, there are a cellview and
-- an arrow.
-- 
-- 
-- === /plain code/
-- >
-- >combobox
-- >├── box.linked
-- >│   ├── entry.combo
-- >│   ╰── button.combo
-- >│       ╰── box
-- >│           ╰── arrow
-- >╰── window.popup
-- 
-- 
-- A GtkComboBox with an entry has a single CSS node with name combobox. It
-- contains a box with the .linked class. That box contains an entry and a
-- button, both with the .combo class added.
-- The button also contains another node with name arrow.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.ComboBox
    ( 

-- * Exported types
    ComboBox(..)                            ,
    IsComboBox                              ,
    toComboBox                              ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addAttribute]("GI.Gtk.Interfaces.CellLayout#g:method:addAttribute"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [clear]("GI.Gtk.Interfaces.CellLayout#g:method:clear"), [clearAttributes]("GI.Gtk.Interfaces.CellLayout#g:method:clearAttributes"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [editingDone]("GI.Gtk.Interfaces.CellEditable#g:method:editingDone"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [packEnd]("GI.Gtk.Interfaces.CellLayout#g:method:packEnd"), [packStart]("GI.Gtk.Interfaces.CellLayout#g:method:packStart"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [popdown]("GI.Gtk.Objects.ComboBox#g:method:popdown"), [popup]("GI.Gtk.Objects.ComboBox#g:method:popup"), [popupForDevice]("GI.Gtk.Objects.ComboBox#g:method:popupForDevice"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [removeWidget]("GI.Gtk.Interfaces.CellEditable#g:method:removeWidget"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reorder]("GI.Gtk.Interfaces.CellLayout#g:method:reorder"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [startEditing]("GI.Gtk.Interfaces.CellEditable#g:method:startEditing"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActive]("GI.Gtk.Objects.ComboBox#g:method:getActive"), [getActiveId]("GI.Gtk.Objects.ComboBox#g:method:getActiveId"), [getActiveIter]("GI.Gtk.Objects.ComboBox#g:method:getActiveIter"), [getAddTearoffs]("GI.Gtk.Objects.ComboBox#g:method:getAddTearoffs"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getArea]("GI.Gtk.Interfaces.CellLayout#g:method:getArea"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getButtonSensitivity]("GI.Gtk.Objects.ComboBox#g:method:getButtonSensitivity"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getCells]("GI.Gtk.Interfaces.CellLayout#g:method:getCells"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getColumnSpanColumn]("GI.Gtk.Objects.ComboBox#g:method:getColumnSpanColumn"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEntryTextColumn]("GI.Gtk.Objects.ComboBox#g:method:getEntryTextColumn"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.ComboBox#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasEntry]("GI.Gtk.Objects.ComboBox#g:method:getHasEntry"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getIdColumn]("GI.Gtk.Objects.ComboBox#g:method:getIdColumn"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModel]("GI.Gtk.Objects.ComboBox#g:method:getModel"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPopupAccessible]("GI.Gtk.Objects.ComboBox#g:method:getPopupAccessible"), [getPopupFixedWidth]("GI.Gtk.Objects.ComboBox#g:method:getPopupFixedWidth"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getRowSpanColumn]("GI.Gtk.Objects.ComboBox#g:method:getRowSpanColumn"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTitle]("GI.Gtk.Objects.ComboBox#g:method:getTitle"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow"), [getWrapWidth]("GI.Gtk.Objects.ComboBox#g:method:getWrapWidth").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActive]("GI.Gtk.Objects.ComboBox#g:method:setActive"), [setActiveId]("GI.Gtk.Objects.ComboBox#g:method:setActiveId"), [setActiveIter]("GI.Gtk.Objects.ComboBox#g:method:setActiveIter"), [setAddTearoffs]("GI.Gtk.Objects.ComboBox#g:method:setAddTearoffs"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setButtonSensitivity]("GI.Gtk.Objects.ComboBox#g:method:setButtonSensitivity"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setCellDataFunc]("GI.Gtk.Interfaces.CellLayout#g:method:setCellDataFunc"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setColumnSpanColumn]("GI.Gtk.Objects.ComboBox#g:method:setColumnSpanColumn"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEntryTextColumn]("GI.Gtk.Objects.ComboBox#g:method:setEntryTextColumn"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.ComboBox#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setIdColumn]("GI.Gtk.Objects.ComboBox#g:method:setIdColumn"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setModel]("GI.Gtk.Objects.ComboBox#g:method:setModel"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPopupFixedWidth]("GI.Gtk.Objects.ComboBox#g:method:setPopupFixedWidth"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setRowSeparatorFunc]("GI.Gtk.Objects.ComboBox#g:method:setRowSeparatorFunc"), [setRowSpanColumn]("GI.Gtk.Objects.ComboBox#g:method:setRowSpanColumn"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTitle]("GI.Gtk.Objects.ComboBox#g:method:setTitle"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow"), [setWrapWidth]("GI.Gtk.Objects.ComboBox#g:method:setWrapWidth").

#if defined(ENABLE_OVERLOADING)
    ResolveComboBoxMethod                   ,
#endif

-- ** getActive #method:getActive#

#if defined(ENABLE_OVERLOADING)
    ComboBoxGetActiveMethodInfo             ,
#endif
    comboBoxGetActive                       ,


-- ** getActiveId #method:getActiveId#

#if defined(ENABLE_OVERLOADING)
    ComboBoxGetActiveIdMethodInfo           ,
#endif
    comboBoxGetActiveId                     ,


-- ** getActiveIter #method:getActiveIter#

#if defined(ENABLE_OVERLOADING)
    ComboBoxGetActiveIterMethodInfo         ,
#endif
    comboBoxGetActiveIter                   ,


-- ** getAddTearoffs #method:getAddTearoffs#

#if defined(ENABLE_OVERLOADING)
    ComboBoxGetAddTearoffsMethodInfo        ,
#endif
    comboBoxGetAddTearoffs                  ,


-- ** getButtonSensitivity #method:getButtonSensitivity#

#if defined(ENABLE_OVERLOADING)
    ComboBoxGetButtonSensitivityMethodInfo  ,
#endif
    comboBoxGetButtonSensitivity            ,


-- ** getColumnSpanColumn #method:getColumnSpanColumn#

#if defined(ENABLE_OVERLOADING)
    ComboBoxGetColumnSpanColumnMethodInfo   ,
#endif
    comboBoxGetColumnSpanColumn             ,


-- ** getEntryTextColumn #method:getEntryTextColumn#

#if defined(ENABLE_OVERLOADING)
    ComboBoxGetEntryTextColumnMethodInfo    ,
#endif
    comboBoxGetEntryTextColumn              ,


-- ** getFocusOnClick #method:getFocusOnClick#

#if defined(ENABLE_OVERLOADING)
    ComboBoxGetFocusOnClickMethodInfo       ,
#endif
    comboBoxGetFocusOnClick                 ,


-- ** getHasEntry #method:getHasEntry#

#if defined(ENABLE_OVERLOADING)
    ComboBoxGetHasEntryMethodInfo           ,
#endif
    comboBoxGetHasEntry                     ,


-- ** getIdColumn #method:getIdColumn#

#if defined(ENABLE_OVERLOADING)
    ComboBoxGetIdColumnMethodInfo           ,
#endif
    comboBoxGetIdColumn                     ,


-- ** getModel #method:getModel#

#if defined(ENABLE_OVERLOADING)
    ComboBoxGetModelMethodInfo              ,
#endif
    comboBoxGetModel                        ,


-- ** getPopupAccessible #method:getPopupAccessible#

#if defined(ENABLE_OVERLOADING)
    ComboBoxGetPopupAccessibleMethodInfo    ,
#endif
    comboBoxGetPopupAccessible              ,


-- ** getPopupFixedWidth #method:getPopupFixedWidth#

#if defined(ENABLE_OVERLOADING)
    ComboBoxGetPopupFixedWidthMethodInfo    ,
#endif
    comboBoxGetPopupFixedWidth              ,


-- ** getRowSpanColumn #method:getRowSpanColumn#

#if defined(ENABLE_OVERLOADING)
    ComboBoxGetRowSpanColumnMethodInfo      ,
#endif
    comboBoxGetRowSpanColumn                ,


-- ** getTitle #method:getTitle#

#if defined(ENABLE_OVERLOADING)
    ComboBoxGetTitleMethodInfo              ,
#endif
    comboBoxGetTitle                        ,


-- ** getWrapWidth #method:getWrapWidth#

#if defined(ENABLE_OVERLOADING)
    ComboBoxGetWrapWidthMethodInfo          ,
#endif
    comboBoxGetWrapWidth                    ,


-- ** new #method:new#

    comboBoxNew                             ,


-- ** newWithArea #method:newWithArea#

    comboBoxNewWithArea                     ,


-- ** newWithAreaAndEntry #method:newWithAreaAndEntry#

    comboBoxNewWithAreaAndEntry             ,


-- ** newWithEntry #method:newWithEntry#

    comboBoxNewWithEntry                    ,


-- ** newWithModel #method:newWithModel#

    comboBoxNewWithModel                    ,


-- ** newWithModelAndEntry #method:newWithModelAndEntry#

    comboBoxNewWithModelAndEntry            ,


-- ** popdown #method:popdown#

#if defined(ENABLE_OVERLOADING)
    ComboBoxPopdownMethodInfo               ,
#endif
    comboBoxPopdown                         ,


-- ** popup #method:popup#

#if defined(ENABLE_OVERLOADING)
    ComboBoxPopupMethodInfo                 ,
#endif
    comboBoxPopup                           ,


-- ** popupForDevice #method:popupForDevice#

#if defined(ENABLE_OVERLOADING)
    ComboBoxPopupForDeviceMethodInfo        ,
#endif
    comboBoxPopupForDevice                  ,


-- ** setActive #method:setActive#

#if defined(ENABLE_OVERLOADING)
    ComboBoxSetActiveMethodInfo             ,
#endif
    comboBoxSetActive                       ,


-- ** setActiveId #method:setActiveId#

#if defined(ENABLE_OVERLOADING)
    ComboBoxSetActiveIdMethodInfo           ,
#endif
    comboBoxSetActiveId                     ,


-- ** setActiveIter #method:setActiveIter#

#if defined(ENABLE_OVERLOADING)
    ComboBoxSetActiveIterMethodInfo         ,
#endif
    comboBoxSetActiveIter                   ,


-- ** setAddTearoffs #method:setAddTearoffs#

#if defined(ENABLE_OVERLOADING)
    ComboBoxSetAddTearoffsMethodInfo        ,
#endif
    comboBoxSetAddTearoffs                  ,


-- ** setButtonSensitivity #method:setButtonSensitivity#

#if defined(ENABLE_OVERLOADING)
    ComboBoxSetButtonSensitivityMethodInfo  ,
#endif
    comboBoxSetButtonSensitivity            ,


-- ** setColumnSpanColumn #method:setColumnSpanColumn#

#if defined(ENABLE_OVERLOADING)
    ComboBoxSetColumnSpanColumnMethodInfo   ,
#endif
    comboBoxSetColumnSpanColumn             ,


-- ** setEntryTextColumn #method:setEntryTextColumn#

#if defined(ENABLE_OVERLOADING)
    ComboBoxSetEntryTextColumnMethodInfo    ,
#endif
    comboBoxSetEntryTextColumn              ,


-- ** setFocusOnClick #method:setFocusOnClick#

#if defined(ENABLE_OVERLOADING)
    ComboBoxSetFocusOnClickMethodInfo       ,
#endif
    comboBoxSetFocusOnClick                 ,


-- ** setIdColumn #method:setIdColumn#

#if defined(ENABLE_OVERLOADING)
    ComboBoxSetIdColumnMethodInfo           ,
#endif
    comboBoxSetIdColumn                     ,


-- ** setModel #method:setModel#

#if defined(ENABLE_OVERLOADING)
    ComboBoxSetModelMethodInfo              ,
#endif
    comboBoxSetModel                        ,


-- ** setPopupFixedWidth #method:setPopupFixedWidth#

#if defined(ENABLE_OVERLOADING)
    ComboBoxSetPopupFixedWidthMethodInfo    ,
#endif
    comboBoxSetPopupFixedWidth              ,


-- ** setRowSeparatorFunc #method:setRowSeparatorFunc#

#if defined(ENABLE_OVERLOADING)
    ComboBoxSetRowSeparatorFuncMethodInfo   ,
#endif
    comboBoxSetRowSeparatorFunc             ,


-- ** setRowSpanColumn #method:setRowSpanColumn#

#if defined(ENABLE_OVERLOADING)
    ComboBoxSetRowSpanColumnMethodInfo      ,
#endif
    comboBoxSetRowSpanColumn                ,


-- ** setTitle #method:setTitle#

#if defined(ENABLE_OVERLOADING)
    ComboBoxSetTitleMethodInfo              ,
#endif
    comboBoxSetTitle                        ,


-- ** setWrapWidth #method:setWrapWidth#

#if defined(ENABLE_OVERLOADING)
    ComboBoxSetWrapWidthMethodInfo          ,
#endif
    comboBoxSetWrapWidth                    ,




 -- * Properties


-- ** active #attr:active#
-- | The item which is currently active. If the model is a non-flat treemodel,
-- and the active item is not an immediate child of the root of the tree,
-- this property has the value
-- @gtk_tree_path_get_indices (path)[0]@,
-- where @path@ is the t'GI.Gtk.Structs.TreePath.TreePath' of the active item.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    ComboBoxActivePropertyInfo              ,
#endif
#if defined(ENABLE_OVERLOADING)
    comboBoxActive                          ,
#endif
    constructComboBoxActive                 ,
    getComboBoxActive                       ,
    setComboBoxActive                       ,


-- ** activeId #attr:activeId#
-- | The value of the ID column of the active row.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    ComboBoxActiveIdPropertyInfo            ,
#endif
    clearComboBoxActiveId                   ,
#if defined(ENABLE_OVERLOADING)
    comboBoxActiveId                        ,
#endif
    constructComboBoxActiveId               ,
    getComboBoxActiveId                     ,
    setComboBoxActiveId                     ,


-- ** addTearoffs #attr:addTearoffs#
-- | The add-tearoffs property controls whether generated menus
-- have tearoff menu items.
-- 
-- Note that this only affects menu style combo boxes.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    ComboBoxAddTearoffsPropertyInfo         ,
#endif
#if defined(ENABLE_OVERLOADING)
    comboBoxAddTearoffs                     ,
#endif
    constructComboBoxAddTearoffs            ,
    getComboBoxAddTearoffs                  ,
    setComboBoxAddTearoffs                  ,


-- ** buttonSensitivity #attr:buttonSensitivity#
-- | Whether the dropdown button is sensitive when
-- the model is empty.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    ComboBoxButtonSensitivityPropertyInfo   ,
#endif
#if defined(ENABLE_OVERLOADING)
    comboBoxButtonSensitivity               ,
#endif
    constructComboBoxButtonSensitivity      ,
    getComboBoxButtonSensitivity            ,
    setComboBoxButtonSensitivity            ,


-- ** cellArea #attr:cellArea#
-- | The t'GI.Gtk.Objects.CellArea.CellArea' used to layout cell renderers for this combo box.
-- 
-- If no area is specified when creating the combo box with 'GI.Gtk.Objects.ComboBox.comboBoxNewWithArea'
-- a horizontally oriented t'GI.Gtk.Objects.CellAreaBox.CellAreaBox' will be used.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    ComboBoxCellAreaPropertyInfo            ,
#endif
#if defined(ENABLE_OVERLOADING)
    comboBoxCellArea                        ,
#endif
    constructComboBoxCellArea               ,
    getComboBoxCellArea                     ,


-- ** columnSpanColumn #attr:columnSpanColumn#
-- | If this is set to a non-negative value, it must be the index of a column
-- of type @/G_TYPE_INT/@ in the model. The value in that column for each item
-- will determine how many columns that item will span in the popup.
-- Therefore, values in this column must be greater than zero, and the sum of
-- an item’s column position + span should not exceed [ComboBox:wrapWidth]("GI.Gtk.Objects.ComboBox#g:attr:wrapWidth").
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    ComboBoxColumnSpanColumnPropertyInfo    ,
#endif
#if defined(ENABLE_OVERLOADING)
    comboBoxColumnSpanColumn                ,
#endif
    constructComboBoxColumnSpanColumn       ,
    getComboBoxColumnSpanColumn             ,
    setComboBoxColumnSpanColumn             ,


-- ** entryTextColumn #attr:entryTextColumn#
-- | The column in the combo box\'s model to associate with strings from the entry
-- if the combo was created with [ComboBox:hasEntry]("GI.Gtk.Objects.ComboBox#g:attr:hasEntry") = 'P.True'.
-- 
-- /Since: 2.24/

#if defined(ENABLE_OVERLOADING)
    ComboBoxEntryTextColumnPropertyInfo     ,
#endif
#if defined(ENABLE_OVERLOADING)
    comboBoxEntryTextColumn                 ,
#endif
    constructComboBoxEntryTextColumn        ,
    getComboBoxEntryTextColumn              ,
    setComboBoxEntryTextColumn              ,


-- ** hasEntry #attr:hasEntry#
-- | Whether the combo box has an entry.
-- 
-- /Since: 2.24/

#if defined(ENABLE_OVERLOADING)
    ComboBoxHasEntryPropertyInfo            ,
#endif
#if defined(ENABLE_OVERLOADING)
    comboBoxHasEntry                        ,
#endif
    constructComboBoxHasEntry               ,
    getComboBoxHasEntry                     ,


-- ** hasFrame #attr:hasFrame#
-- | The has-frame property controls whether a frame
-- is drawn around the entry.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    ComboBoxHasFramePropertyInfo            ,
#endif
#if defined(ENABLE_OVERLOADING)
    comboBoxHasFrame                        ,
#endif
    constructComboBoxHasFrame               ,
    getComboBoxHasFrame                     ,
    setComboBoxHasFrame                     ,


-- ** idColumn #attr:idColumn#
-- | The column in the combo box\'s model that provides string
-- IDs for the values in the model, if != -1.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    ComboBoxIdColumnPropertyInfo            ,
#endif
#if defined(ENABLE_OVERLOADING)
    comboBoxIdColumn                        ,
#endif
    constructComboBoxIdColumn               ,
    getComboBoxIdColumn                     ,
    setComboBoxIdColumn                     ,


-- ** model #attr:model#
-- | The model from which the combo box takes the values shown
-- in the list.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    ComboBoxModelPropertyInfo               ,
#endif
    clearComboBoxModel                      ,
#if defined(ENABLE_OVERLOADING)
    comboBoxModel                           ,
#endif
    constructComboBoxModel                  ,
    getComboBoxModel                        ,
    setComboBoxModel                        ,


-- ** popupFixedWidth #attr:popupFixedWidth#
-- | Whether the popup\'s width should be a fixed width matching the
-- allocated width of the combo box.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    ComboBoxPopupFixedWidthPropertyInfo     ,
#endif
#if defined(ENABLE_OVERLOADING)
    comboBoxPopupFixedWidth                 ,
#endif
    constructComboBoxPopupFixedWidth        ,
    getComboBoxPopupFixedWidth              ,
    setComboBoxPopupFixedWidth              ,


-- ** popupShown #attr:popupShown#
-- | Whether the combo boxes dropdown is popped up.
-- Note that this property is mainly useful, because
-- it allows you to connect to notify[popupShown](#g:signal:popupShown).
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    ComboBoxPopupShownPropertyInfo          ,
#endif
#if defined(ENABLE_OVERLOADING)
    comboBoxPopupShown                      ,
#endif
    getComboBoxPopupShown                   ,


-- ** rowSpanColumn #attr:rowSpanColumn#
-- | If this is set to a non-negative value, it must be the index of a column
-- of type @/G_TYPE_INT/@ in the model. The value in that column for each item
-- will determine how many rows that item will span in the popup. Therefore,
-- values in this column must be greater than zero.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    ComboBoxRowSpanColumnPropertyInfo       ,
#endif
#if defined(ENABLE_OVERLOADING)
    comboBoxRowSpanColumn                   ,
#endif
    constructComboBoxRowSpanColumn          ,
    getComboBoxRowSpanColumn                ,
    setComboBoxRowSpanColumn                ,


-- ** tearoffTitle #attr:tearoffTitle#
-- | A title that may be displayed by the window manager
-- when the popup is torn-off.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    ComboBoxTearoffTitlePropertyInfo        ,
#endif
    clearComboBoxTearoffTitle               ,
#if defined(ENABLE_OVERLOADING)
    comboBoxTearoffTitle                    ,
#endif
    constructComboBoxTearoffTitle           ,
    getComboBoxTearoffTitle                 ,
    setComboBoxTearoffTitle                 ,


-- ** wrapWidth #attr:wrapWidth#
-- | If wrap-width is set to a positive value, items in the popup will be laid
-- out along multiple columns, starting a new row on reaching the wrap width.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    ComboBoxWrapWidthPropertyInfo           ,
#endif
#if defined(ENABLE_OVERLOADING)
    comboBoxWrapWidth                       ,
#endif
    constructComboBoxWrapWidth              ,
    getComboBoxWrapWidth                    ,
    setComboBoxWrapWidth                    ,




 -- * Signals


-- ** changed #signal:changed#

    ComboBoxChangedCallback                 ,
#if defined(ENABLE_OVERLOADING)
    ComboBoxChangedSignalInfo               ,
#endif
    afterComboBoxChanged                    ,
    onComboBoxChanged                       ,


-- ** formatEntryText #signal:formatEntryText#

    ComboBoxFormatEntryTextCallback         ,
#if defined(ENABLE_OVERLOADING)
    ComboBoxFormatEntryTextSignalInfo       ,
#endif
    afterComboBoxFormatEntryText            ,
    onComboBoxFormatEntryText               ,


-- ** moveActive #signal:moveActive#

    ComboBoxMoveActiveCallback              ,
#if defined(ENABLE_OVERLOADING)
    ComboBoxMoveActiveSignalInfo            ,
#endif
    afterComboBoxMoveActive                 ,
    onComboBoxMoveActive                    ,


-- ** popdown #signal:popdown#

    ComboBoxPopdownCallback                 ,
#if defined(ENABLE_OVERLOADING)
    ComboBoxPopdownSignalInfo               ,
#endif
    afterComboBoxPopdown                    ,
    onComboBoxPopdown                       ,


-- ** popup #signal:popup#

    ComboBoxPopupCallback                   ,
#if defined(ENABLE_OVERLOADING)
    ComboBoxPopupSignalInfo                 ,
#endif
    afterComboBoxPopup                      ,
    onComboBoxPopup                         ,




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
import qualified GI.Atk.Objects.Object as Atk.Object
import qualified GI.GLib.Callbacks as GLib.Callbacks
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gdk.Objects.Device as Gdk.Device
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.CellEditable as Gtk.CellEditable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.CellLayout as Gtk.CellLayout
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeModel as Gtk.TreeModel
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellArea as Gtk.CellArea
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreeIter as Gtk.TreeIter

-- | Memory-managed wrapper type.
newtype ComboBox = ComboBox (SP.ManagedPtr ComboBox)
    deriving (Eq)

instance SP.ManagedPtrNewtype ComboBox where
    toManagedPtr (ComboBox p) = p

foreign import ccall "gtk_combo_box_get_type"
    c_gtk_combo_box_get_type :: IO B.Types.GType

instance B.Types.TypedObject ComboBox where
    glibType = c_gtk_combo_box_get_type

instance B.Types.GObject ComboBox

-- | Type class for types which can be safely cast to `ComboBox`, for instance with `toComboBox`.
class (SP.GObject o, O.IsDescendantOf ComboBox o) => IsComboBox o
instance (SP.GObject o, O.IsDescendantOf ComboBox o) => IsComboBox o

instance O.HasParentTypes ComboBox
type instance O.ParentTypes ComboBox = '[Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable, Gtk.CellEditable.CellEditable, Gtk.CellLayout.CellLayout]

-- | Cast to `ComboBox`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toComboBox :: (MIO.MonadIO m, IsComboBox o) => o -> m ComboBox
toComboBox = MIO.liftIO . B.ManagedPtr.unsafeCastTo ComboBox

-- | Convert 'ComboBox' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe ComboBox) where
    gvalueGType_ = c_gtk_combo_box_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr ComboBox)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr ComboBox)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject ComboBox ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveComboBoxMethod (t :: Symbol) (o :: *) :: * where
    ResolveComboBoxMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveComboBoxMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveComboBoxMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveComboBoxMethod "addAttribute" o = Gtk.CellLayout.CellLayoutAddAttributeMethodInfo
    ResolveComboBoxMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveComboBoxMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveComboBoxMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveComboBoxMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveComboBoxMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveComboBoxMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveComboBoxMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveComboBoxMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveComboBoxMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveComboBoxMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveComboBoxMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveComboBoxMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveComboBoxMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveComboBoxMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveComboBoxMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveComboBoxMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveComboBoxMethod "clear" o = Gtk.CellLayout.CellLayoutClearMethodInfo
    ResolveComboBoxMethod "clearAttributes" o = Gtk.CellLayout.CellLayoutClearAttributesMethodInfo
    ResolveComboBoxMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveComboBoxMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveComboBoxMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveComboBoxMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveComboBoxMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveComboBoxMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveComboBoxMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveComboBoxMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveComboBoxMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveComboBoxMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveComboBoxMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveComboBoxMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveComboBoxMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveComboBoxMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveComboBoxMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveComboBoxMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveComboBoxMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveComboBoxMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveComboBoxMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveComboBoxMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveComboBoxMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveComboBoxMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveComboBoxMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveComboBoxMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveComboBoxMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveComboBoxMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveComboBoxMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveComboBoxMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveComboBoxMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveComboBoxMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveComboBoxMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveComboBoxMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveComboBoxMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveComboBoxMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveComboBoxMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveComboBoxMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveComboBoxMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveComboBoxMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveComboBoxMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveComboBoxMethod "editingDone" o = Gtk.CellEditable.CellEditableEditingDoneMethodInfo
    ResolveComboBoxMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveComboBoxMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveComboBoxMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveComboBoxMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveComboBoxMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveComboBoxMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveComboBoxMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveComboBoxMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveComboBoxMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveComboBoxMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveComboBoxMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveComboBoxMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveComboBoxMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveComboBoxMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveComboBoxMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveComboBoxMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveComboBoxMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveComboBoxMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveComboBoxMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveComboBoxMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveComboBoxMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveComboBoxMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveComboBoxMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveComboBoxMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveComboBoxMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveComboBoxMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveComboBoxMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveComboBoxMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveComboBoxMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveComboBoxMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveComboBoxMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveComboBoxMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveComboBoxMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveComboBoxMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveComboBoxMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveComboBoxMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveComboBoxMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveComboBoxMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveComboBoxMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveComboBoxMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveComboBoxMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveComboBoxMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveComboBoxMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveComboBoxMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveComboBoxMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveComboBoxMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveComboBoxMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveComboBoxMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveComboBoxMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveComboBoxMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveComboBoxMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveComboBoxMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveComboBoxMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveComboBoxMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveComboBoxMethod "packEnd" o = Gtk.CellLayout.CellLayoutPackEndMethodInfo
    ResolveComboBoxMethod "packStart" o = Gtk.CellLayout.CellLayoutPackStartMethodInfo
    ResolveComboBoxMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveComboBoxMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveComboBoxMethod "popdown" o = ComboBoxPopdownMethodInfo
    ResolveComboBoxMethod "popup" o = ComboBoxPopupMethodInfo
    ResolveComboBoxMethod "popupForDevice" o = ComboBoxPopupForDeviceMethodInfo
    ResolveComboBoxMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveComboBoxMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveComboBoxMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveComboBoxMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveComboBoxMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveComboBoxMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveComboBoxMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveComboBoxMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveComboBoxMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveComboBoxMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveComboBoxMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveComboBoxMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveComboBoxMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveComboBoxMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveComboBoxMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveComboBoxMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveComboBoxMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveComboBoxMethod "removeWidget" o = Gtk.CellEditable.CellEditableRemoveWidgetMethodInfo
    ResolveComboBoxMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveComboBoxMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveComboBoxMethod "reorder" o = Gtk.CellLayout.CellLayoutReorderMethodInfo
    ResolveComboBoxMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveComboBoxMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveComboBoxMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveComboBoxMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveComboBoxMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveComboBoxMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveComboBoxMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveComboBoxMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveComboBoxMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveComboBoxMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveComboBoxMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveComboBoxMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveComboBoxMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveComboBoxMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveComboBoxMethod "startEditing" o = Gtk.CellEditable.CellEditableStartEditingMethodInfo
    ResolveComboBoxMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveComboBoxMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveComboBoxMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveComboBoxMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveComboBoxMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveComboBoxMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveComboBoxMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveComboBoxMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveComboBoxMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveComboBoxMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveComboBoxMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveComboBoxMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveComboBoxMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveComboBoxMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveComboBoxMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveComboBoxMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveComboBoxMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveComboBoxMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveComboBoxMethod "getActive" o = ComboBoxGetActiveMethodInfo
    ResolveComboBoxMethod "getActiveId" o = ComboBoxGetActiveIdMethodInfo
    ResolveComboBoxMethod "getActiveIter" o = ComboBoxGetActiveIterMethodInfo
    ResolveComboBoxMethod "getAddTearoffs" o = ComboBoxGetAddTearoffsMethodInfo
    ResolveComboBoxMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveComboBoxMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveComboBoxMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveComboBoxMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveComboBoxMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveComboBoxMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveComboBoxMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveComboBoxMethod "getArea" o = Gtk.CellLayout.CellLayoutGetAreaMethodInfo
    ResolveComboBoxMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveComboBoxMethod "getButtonSensitivity" o = ComboBoxGetButtonSensitivityMethodInfo
    ResolveComboBoxMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveComboBoxMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveComboBoxMethod "getCells" o = Gtk.CellLayout.CellLayoutGetCellsMethodInfo
    ResolveComboBoxMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveComboBoxMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveComboBoxMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveComboBoxMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveComboBoxMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveComboBoxMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveComboBoxMethod "getColumnSpanColumn" o = ComboBoxGetColumnSpanColumnMethodInfo
    ResolveComboBoxMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveComboBoxMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveComboBoxMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveComboBoxMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveComboBoxMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveComboBoxMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveComboBoxMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveComboBoxMethod "getEntryTextColumn" o = ComboBoxGetEntryTextColumnMethodInfo
    ResolveComboBoxMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveComboBoxMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveComboBoxMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveComboBoxMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveComboBoxMethod "getFocusOnClick" o = ComboBoxGetFocusOnClickMethodInfo
    ResolveComboBoxMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveComboBoxMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveComboBoxMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveComboBoxMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveComboBoxMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveComboBoxMethod "getHasEntry" o = ComboBoxGetHasEntryMethodInfo
    ResolveComboBoxMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveComboBoxMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveComboBoxMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveComboBoxMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveComboBoxMethod "getIdColumn" o = ComboBoxGetIdColumnMethodInfo
    ResolveComboBoxMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveComboBoxMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveComboBoxMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveComboBoxMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveComboBoxMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveComboBoxMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveComboBoxMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveComboBoxMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveComboBoxMethod "getModel" o = ComboBoxGetModelMethodInfo
    ResolveComboBoxMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveComboBoxMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveComboBoxMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveComboBoxMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveComboBoxMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveComboBoxMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveComboBoxMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveComboBoxMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveComboBoxMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveComboBoxMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveComboBoxMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveComboBoxMethod "getPopupAccessible" o = ComboBoxGetPopupAccessibleMethodInfo
    ResolveComboBoxMethod "getPopupFixedWidth" o = ComboBoxGetPopupFixedWidthMethodInfo
    ResolveComboBoxMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveComboBoxMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveComboBoxMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveComboBoxMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveComboBoxMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveComboBoxMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveComboBoxMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveComboBoxMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveComboBoxMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveComboBoxMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveComboBoxMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveComboBoxMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveComboBoxMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveComboBoxMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveComboBoxMethod "getRowSpanColumn" o = ComboBoxGetRowSpanColumnMethodInfo
    ResolveComboBoxMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveComboBoxMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveComboBoxMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveComboBoxMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveComboBoxMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveComboBoxMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveComboBoxMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveComboBoxMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveComboBoxMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveComboBoxMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveComboBoxMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveComboBoxMethod "getTitle" o = ComboBoxGetTitleMethodInfo
    ResolveComboBoxMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveComboBoxMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveComboBoxMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveComboBoxMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveComboBoxMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveComboBoxMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveComboBoxMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveComboBoxMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveComboBoxMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveComboBoxMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveComboBoxMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveComboBoxMethod "getWrapWidth" o = ComboBoxGetWrapWidthMethodInfo
    ResolveComboBoxMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveComboBoxMethod "setActive" o = ComboBoxSetActiveMethodInfo
    ResolveComboBoxMethod "setActiveId" o = ComboBoxSetActiveIdMethodInfo
    ResolveComboBoxMethod "setActiveIter" o = ComboBoxSetActiveIterMethodInfo
    ResolveComboBoxMethod "setAddTearoffs" o = ComboBoxSetAddTearoffsMethodInfo
    ResolveComboBoxMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveComboBoxMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveComboBoxMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveComboBoxMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveComboBoxMethod "setButtonSensitivity" o = ComboBoxSetButtonSensitivityMethodInfo
    ResolveComboBoxMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveComboBoxMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveComboBoxMethod "setCellDataFunc" o = Gtk.CellLayout.CellLayoutSetCellDataFuncMethodInfo
    ResolveComboBoxMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveComboBoxMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveComboBoxMethod "setColumnSpanColumn" o = ComboBoxSetColumnSpanColumnMethodInfo
    ResolveComboBoxMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveComboBoxMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveComboBoxMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveComboBoxMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveComboBoxMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveComboBoxMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveComboBoxMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveComboBoxMethod "setEntryTextColumn" o = ComboBoxSetEntryTextColumnMethodInfo
    ResolveComboBoxMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveComboBoxMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveComboBoxMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveComboBoxMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveComboBoxMethod "setFocusOnClick" o = ComboBoxSetFocusOnClickMethodInfo
    ResolveComboBoxMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveComboBoxMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveComboBoxMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveComboBoxMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveComboBoxMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveComboBoxMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveComboBoxMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveComboBoxMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveComboBoxMethod "setIdColumn" o = ComboBoxSetIdColumnMethodInfo
    ResolveComboBoxMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveComboBoxMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveComboBoxMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveComboBoxMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveComboBoxMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveComboBoxMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveComboBoxMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveComboBoxMethod "setModel" o = ComboBoxSetModelMethodInfo
    ResolveComboBoxMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveComboBoxMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveComboBoxMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveComboBoxMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveComboBoxMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveComboBoxMethod "setPopupFixedWidth" o = ComboBoxSetPopupFixedWidthMethodInfo
    ResolveComboBoxMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveComboBoxMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveComboBoxMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveComboBoxMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveComboBoxMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveComboBoxMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveComboBoxMethod "setRowSeparatorFunc" o = ComboBoxSetRowSeparatorFuncMethodInfo
    ResolveComboBoxMethod "setRowSpanColumn" o = ComboBoxSetRowSpanColumnMethodInfo
    ResolveComboBoxMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveComboBoxMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveComboBoxMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveComboBoxMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveComboBoxMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveComboBoxMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveComboBoxMethod "setTitle" o = ComboBoxSetTitleMethodInfo
    ResolveComboBoxMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveComboBoxMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveComboBoxMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveComboBoxMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveComboBoxMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveComboBoxMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveComboBoxMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveComboBoxMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveComboBoxMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveComboBoxMethod "setWrapWidth" o = ComboBoxSetWrapWidthMethodInfo
    ResolveComboBoxMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveComboBoxMethod t ComboBox, O.OverloadedMethod info ComboBox p) => OL.IsLabel t (ComboBox -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveComboBoxMethod t ComboBox, O.OverloadedMethod info ComboBox p, R.HasField t ComboBox p) => R.HasField t ComboBox p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveComboBoxMethod t ComboBox, O.OverloadedMethodInfo info ComboBox) => OL.IsLabel t (O.MethodProxy info ComboBox) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal ComboBox::changed
-- | The changed signal is emitted when the active
-- item is changed. The can be due to the user selecting
-- a different item from the list, or due to a
-- call to 'GI.Gtk.Objects.ComboBox.comboBoxSetActiveIter'.
-- It will also be emitted while typing into the entry of a combo box
-- with an entry.
-- 
-- /Since: 2.4/
type ComboBoxChangedCallback =
    IO ()

type C_ComboBoxChangedCallback =
    Ptr ComboBox ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ComboBoxChangedCallback`.
foreign import ccall "wrapper"
    mk_ComboBoxChangedCallback :: C_ComboBoxChangedCallback -> IO (FunPtr C_ComboBoxChangedCallback)

wrap_ComboBoxChangedCallback :: 
    GObject a => (a -> ComboBoxChangedCallback) ->
    C_ComboBoxChangedCallback
wrap_ComboBoxChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' comboBox #changed callback
-- @
-- 
-- 
onComboBoxChanged :: (IsComboBox a, MonadIO m) => a -> ((?self :: a) => ComboBoxChangedCallback) -> m SignalHandlerId
onComboBoxChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ComboBoxChangedCallback wrapped
    wrapped'' <- mk_ComboBoxChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' comboBox #changed callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterComboBoxChanged :: (IsComboBox a, MonadIO m) => a -> ((?self :: a) => ComboBoxChangedCallback) -> m SignalHandlerId
afterComboBoxChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ComboBoxChangedCallback wrapped
    wrapped'' <- mk_ComboBoxChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ComboBoxChangedSignalInfo
instance SignalInfo ComboBoxChangedSignalInfo where
    type HaskellCallbackType ComboBoxChangedSignalInfo = ComboBoxChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ComboBoxChangedCallback cb
        cb'' <- mk_ComboBoxChangedCallback cb'
        connectSignalFunPtr obj "changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox::changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:signal:changed"})

#endif

-- signal ComboBox::format-entry-text
-- | For combo boxes that are created with an entry (See GtkComboBox:has-entry).
-- 
-- A signal which allows you to change how the text displayed in a combo box\'s
-- entry is displayed.
-- 
-- Connect a signal handler which returns an allocated string representing
-- /@path@/. That string will then be used to set the text in the combo box\'s entry.
-- The default signal handler uses the text from the GtkComboBox[entryTextColumn](#g:signal:entryTextColumn)
-- model column.
-- 
-- Here\'s an example signal handler which fetches data from the model and
-- displays it in the entry.
-- 
-- === /C code/
-- >
-- >static gchar*
-- >format_entry_text_callback (GtkComboBox *combo,
-- >                            const gchar *path,
-- >                            gpointer     user_data)
-- >{
-- >  GtkTreeIter iter;
-- >  GtkTreeModel model;
-- >  gdouble      value;
-- >
-- >  model = gtk_combo_box_get_model (combo);
-- >
-- >  gtk_tree_model_get_iter_from_string (model, &iter, path);
-- >  gtk_tree_model_get (model, &iter,
-- >                      THE_DOUBLE_VALUE_COLUMN, &value,
-- >                      -1);
-- >
-- >  return g_strdup_printf ("%g", value);
-- >}
-- 
-- 
-- /Since: 3.4/
type ComboBoxFormatEntryTextCallback =
    T.Text
    -- ^ /@path@/: the GtkTreePath string from the combo box\'s current model to format text for
    -> IO T.Text
    -- ^ __Returns:__ a newly allocated string representing /@path@/
    -- for the current GtkComboBox model.

type C_ComboBoxFormatEntryTextCallback =
    Ptr ComboBox ->                         -- object
    CString ->
    Ptr () ->                               -- user_data
    IO CString

-- | Generate a function pointer callable from C code, from a `C_ComboBoxFormatEntryTextCallback`.
foreign import ccall "wrapper"
    mk_ComboBoxFormatEntryTextCallback :: C_ComboBoxFormatEntryTextCallback -> IO (FunPtr C_ComboBoxFormatEntryTextCallback)

wrap_ComboBoxFormatEntryTextCallback :: 
    GObject a => (a -> ComboBoxFormatEntryTextCallback) ->
    C_ComboBoxFormatEntryTextCallback
wrap_ComboBoxFormatEntryTextCallback gi'cb gi'selfPtr path _ = do
    path' <- cstringToText path
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  path'
    result' <- textToCString result
    return result'


-- | Connect a signal handler for the [formatEntryText](#signal:formatEntryText) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' comboBox #formatEntryText callback
-- @
-- 
-- 
onComboBoxFormatEntryText :: (IsComboBox a, MonadIO m) => a -> ((?self :: a) => ComboBoxFormatEntryTextCallback) -> m SignalHandlerId
onComboBoxFormatEntryText obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ComboBoxFormatEntryTextCallback wrapped
    wrapped'' <- mk_ComboBoxFormatEntryTextCallback wrapped'
    connectSignalFunPtr obj "format-entry-text" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [formatEntryText](#signal:formatEntryText) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' comboBox #formatEntryText callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterComboBoxFormatEntryText :: (IsComboBox a, MonadIO m) => a -> ((?self :: a) => ComboBoxFormatEntryTextCallback) -> m SignalHandlerId
afterComboBoxFormatEntryText obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ComboBoxFormatEntryTextCallback wrapped
    wrapped'' <- mk_ComboBoxFormatEntryTextCallback wrapped'
    connectSignalFunPtr obj "format-entry-text" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ComboBoxFormatEntryTextSignalInfo
instance SignalInfo ComboBoxFormatEntryTextSignalInfo where
    type HaskellCallbackType ComboBoxFormatEntryTextSignalInfo = ComboBoxFormatEntryTextCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ComboBoxFormatEntryTextCallback cb
        cb'' <- mk_ComboBoxFormatEntryTextCallback cb'
        connectSignalFunPtr obj "format-entry-text" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox::format-entry-text"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:signal:formatEntryText"})

#endif

-- signal ComboBox::move-active
-- | The [moveActive](#g:signal:moveActive) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to move the active selection.
-- 
-- /Since: 2.12/
type ComboBoxMoveActiveCallback =
    Gtk.Enums.ScrollType
    -- ^ /@scrollType@/: a t'GI.Gtk.Enums.ScrollType'
    -> IO ()

type C_ComboBoxMoveActiveCallback =
    Ptr ComboBox ->                         -- object
    CUInt ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ComboBoxMoveActiveCallback`.
foreign import ccall "wrapper"
    mk_ComboBoxMoveActiveCallback :: C_ComboBoxMoveActiveCallback -> IO (FunPtr C_ComboBoxMoveActiveCallback)

wrap_ComboBoxMoveActiveCallback :: 
    GObject a => (a -> ComboBoxMoveActiveCallback) ->
    C_ComboBoxMoveActiveCallback
wrap_ComboBoxMoveActiveCallback gi'cb gi'selfPtr scrollType _ = do
    let scrollType' = (toEnum . fromIntegral) scrollType
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  scrollType'


-- | Connect a signal handler for the [moveActive](#signal:moveActive) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' comboBox #moveActive callback
-- @
-- 
-- 
onComboBoxMoveActive :: (IsComboBox a, MonadIO m) => a -> ((?self :: a) => ComboBoxMoveActiveCallback) -> m SignalHandlerId
onComboBoxMoveActive obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ComboBoxMoveActiveCallback wrapped
    wrapped'' <- mk_ComboBoxMoveActiveCallback wrapped'
    connectSignalFunPtr obj "move-active" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [moveActive](#signal:moveActive) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' comboBox #moveActive callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterComboBoxMoveActive :: (IsComboBox a, MonadIO m) => a -> ((?self :: a) => ComboBoxMoveActiveCallback) -> m SignalHandlerId
afterComboBoxMoveActive obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ComboBoxMoveActiveCallback wrapped
    wrapped'' <- mk_ComboBoxMoveActiveCallback wrapped'
    connectSignalFunPtr obj "move-active" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ComboBoxMoveActiveSignalInfo
instance SignalInfo ComboBoxMoveActiveSignalInfo where
    type HaskellCallbackType ComboBoxMoveActiveSignalInfo = ComboBoxMoveActiveCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ComboBoxMoveActiveCallback cb
        cb'' <- mk_ComboBoxMoveActiveCallback cb'
        connectSignalFunPtr obj "move-active" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox::move-active"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:signal:moveActive"})

#endif

-- signal ComboBox::popdown
-- | The [popdown](#g:signal:popdown) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to popdown the combo box list.
-- 
-- The default bindings for this signal are Alt+Up and Escape.
-- 
-- /Since: 2.12/
type ComboBoxPopdownCallback =
    IO Bool

type C_ComboBoxPopdownCallback =
    Ptr ComboBox ->                         -- object
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_ComboBoxPopdownCallback`.
foreign import ccall "wrapper"
    mk_ComboBoxPopdownCallback :: C_ComboBoxPopdownCallback -> IO (FunPtr C_ComboBoxPopdownCallback)

wrap_ComboBoxPopdownCallback :: 
    GObject a => (a -> ComboBoxPopdownCallback) ->
    C_ComboBoxPopdownCallback
wrap_ComboBoxPopdownCallback gi'cb gi'selfPtr _ = do
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [popdown](#signal:popdown) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' comboBox #popdown callback
-- @
-- 
-- 
onComboBoxPopdown :: (IsComboBox a, MonadIO m) => a -> ((?self :: a) => ComboBoxPopdownCallback) -> m SignalHandlerId
onComboBoxPopdown obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ComboBoxPopdownCallback wrapped
    wrapped'' <- mk_ComboBoxPopdownCallback wrapped'
    connectSignalFunPtr obj "popdown" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [popdown](#signal:popdown) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' comboBox #popdown callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterComboBoxPopdown :: (IsComboBox a, MonadIO m) => a -> ((?self :: a) => ComboBoxPopdownCallback) -> m SignalHandlerId
afterComboBoxPopdown obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ComboBoxPopdownCallback wrapped
    wrapped'' <- mk_ComboBoxPopdownCallback wrapped'
    connectSignalFunPtr obj "popdown" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ComboBoxPopdownSignalInfo
instance SignalInfo ComboBoxPopdownSignalInfo where
    type HaskellCallbackType ComboBoxPopdownSignalInfo = ComboBoxPopdownCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ComboBoxPopdownCallback cb
        cb'' <- mk_ComboBoxPopdownCallback cb'
        connectSignalFunPtr obj "popdown" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox::popdown"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:signal:popdown"})

#endif

-- signal ComboBox::popup
-- | The [popup](#g:signal:popup) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to popup the combo box list.
-- 
-- The default binding for this signal is Alt+Down.
-- 
-- /Since: 2.12/
type ComboBoxPopupCallback =
    IO ()

type C_ComboBoxPopupCallback =
    Ptr ComboBox ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ComboBoxPopupCallback`.
foreign import ccall "wrapper"
    mk_ComboBoxPopupCallback :: C_ComboBoxPopupCallback -> IO (FunPtr C_ComboBoxPopupCallback)

wrap_ComboBoxPopupCallback :: 
    GObject a => (a -> ComboBoxPopupCallback) ->
    C_ComboBoxPopupCallback
wrap_ComboBoxPopupCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [popup](#signal:popup) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' comboBox #popup callback
-- @
-- 
-- 
onComboBoxPopup :: (IsComboBox a, MonadIO m) => a -> ((?self :: a) => ComboBoxPopupCallback) -> m SignalHandlerId
onComboBoxPopup obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ComboBoxPopupCallback wrapped
    wrapped'' <- mk_ComboBoxPopupCallback wrapped'
    connectSignalFunPtr obj "popup" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [popup](#signal:popup) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' comboBox #popup callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterComboBoxPopup :: (IsComboBox a, MonadIO m) => a -> ((?self :: a) => ComboBoxPopupCallback) -> m SignalHandlerId
afterComboBoxPopup obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ComboBoxPopupCallback wrapped
    wrapped'' <- mk_ComboBoxPopupCallback wrapped'
    connectSignalFunPtr obj "popup" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ComboBoxPopupSignalInfo
instance SignalInfo ComboBoxPopupSignalInfo where
    type HaskellCallbackType ComboBoxPopupSignalInfo = ComboBoxPopupCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ComboBoxPopupCallback cb
        cb'' <- mk_ComboBoxPopupCallback cb'
        connectSignalFunPtr obj "popup" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox::popup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:signal:popup"})

#endif

-- VVV Prop "active"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@active@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' comboBox #active
-- @
getComboBoxActive :: (MonadIO m, IsComboBox o) => o -> m Int32
getComboBoxActive obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "active"

-- | Set the value of the “@active@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' comboBox [ #active 'Data.GI.Base.Attributes.:=' value ]
-- @
setComboBoxActive :: (MonadIO m, IsComboBox o) => o -> Int32 -> m ()
setComboBoxActive obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "active" val

-- | Construct a `GValueConstruct` with valid value for the “@active@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructComboBoxActive :: (IsComboBox o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructComboBoxActive val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "active" val

#if defined(ENABLE_OVERLOADING)
data ComboBoxActivePropertyInfo
instance AttrInfo ComboBoxActivePropertyInfo where
    type AttrAllowedOps ComboBoxActivePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ComboBoxActivePropertyInfo = IsComboBox
    type AttrSetTypeConstraint ComboBoxActivePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint ComboBoxActivePropertyInfo = (~) Int32
    type AttrTransferType ComboBoxActivePropertyInfo = Int32
    type AttrGetType ComboBoxActivePropertyInfo = Int32
    type AttrLabel ComboBoxActivePropertyInfo = "active"
    type AttrOrigin ComboBoxActivePropertyInfo = ComboBox
    attrGet = getComboBoxActive
    attrSet = setComboBoxActive
    attrTransfer _ v = do
        return v
    attrConstruct = constructComboBoxActive
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.active"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:attr:active"
        })
#endif

-- VVV Prop "active-id"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Nothing)

-- | Get the value of the “@active-id@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' comboBox #activeId
-- @
getComboBoxActiveId :: (MonadIO m, IsComboBox o) => o -> m (Maybe T.Text)
getComboBoxActiveId obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "active-id"

-- | Set the value of the “@active-id@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' comboBox [ #activeId 'Data.GI.Base.Attributes.:=' value ]
-- @
setComboBoxActiveId :: (MonadIO m, IsComboBox o) => o -> T.Text -> m ()
setComboBoxActiveId obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "active-id" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@active-id@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructComboBoxActiveId :: (IsComboBox o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructComboBoxActiveId val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "active-id" (P.Just val)

-- | Set the value of the “@active-id@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #activeId
-- @
clearComboBoxActiveId :: (MonadIO m, IsComboBox o) => o -> m ()
clearComboBoxActiveId obj = liftIO $ B.Properties.setObjectPropertyString obj "active-id" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data ComboBoxActiveIdPropertyInfo
instance AttrInfo ComboBoxActiveIdPropertyInfo where
    type AttrAllowedOps ComboBoxActiveIdPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ComboBoxActiveIdPropertyInfo = IsComboBox
    type AttrSetTypeConstraint ComboBoxActiveIdPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint ComboBoxActiveIdPropertyInfo = (~) T.Text
    type AttrTransferType ComboBoxActiveIdPropertyInfo = T.Text
    type AttrGetType ComboBoxActiveIdPropertyInfo = (Maybe T.Text)
    type AttrLabel ComboBoxActiveIdPropertyInfo = "active-id"
    type AttrOrigin ComboBoxActiveIdPropertyInfo = ComboBox
    attrGet = getComboBoxActiveId
    attrSet = setComboBoxActiveId
    attrTransfer _ v = do
        return v
    attrConstruct = constructComboBoxActiveId
    attrClear = clearComboBoxActiveId
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.activeId"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:attr:activeId"
        })
#endif

-- VVV Prop "add-tearoffs"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@add-tearoffs@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' comboBox #addTearoffs
-- @
getComboBoxAddTearoffs :: (MonadIO m, IsComboBox o) => o -> m Bool
getComboBoxAddTearoffs obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "add-tearoffs"

-- | Set the value of the “@add-tearoffs@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' comboBox [ #addTearoffs 'Data.GI.Base.Attributes.:=' value ]
-- @
setComboBoxAddTearoffs :: (MonadIO m, IsComboBox o) => o -> Bool -> m ()
setComboBoxAddTearoffs obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "add-tearoffs" val

-- | Construct a `GValueConstruct` with valid value for the “@add-tearoffs@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructComboBoxAddTearoffs :: (IsComboBox o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructComboBoxAddTearoffs val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "add-tearoffs" val

#if defined(ENABLE_OVERLOADING)
data ComboBoxAddTearoffsPropertyInfo
instance AttrInfo ComboBoxAddTearoffsPropertyInfo where
    type AttrAllowedOps ComboBoxAddTearoffsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ComboBoxAddTearoffsPropertyInfo = IsComboBox
    type AttrSetTypeConstraint ComboBoxAddTearoffsPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ComboBoxAddTearoffsPropertyInfo = (~) Bool
    type AttrTransferType ComboBoxAddTearoffsPropertyInfo = Bool
    type AttrGetType ComboBoxAddTearoffsPropertyInfo = Bool
    type AttrLabel ComboBoxAddTearoffsPropertyInfo = "add-tearoffs"
    type AttrOrigin ComboBoxAddTearoffsPropertyInfo = ComboBox
    attrGet = getComboBoxAddTearoffs
    attrSet = setComboBoxAddTearoffs
    attrTransfer _ v = do
        return v
    attrConstruct = constructComboBoxAddTearoffs
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.addTearoffs"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:attr:addTearoffs"
        })
#endif

-- VVV Prop "button-sensitivity"
   -- Type: TInterface (Name {namespace = "Gtk", name = "SensitivityType"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@button-sensitivity@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' comboBox #buttonSensitivity
-- @
getComboBoxButtonSensitivity :: (MonadIO m, IsComboBox o) => o -> m Gtk.Enums.SensitivityType
getComboBoxButtonSensitivity obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "button-sensitivity"

-- | Set the value of the “@button-sensitivity@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' comboBox [ #buttonSensitivity 'Data.GI.Base.Attributes.:=' value ]
-- @
setComboBoxButtonSensitivity :: (MonadIO m, IsComboBox o) => o -> Gtk.Enums.SensitivityType -> m ()
setComboBoxButtonSensitivity obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "button-sensitivity" val

-- | Construct a `GValueConstruct` with valid value for the “@button-sensitivity@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructComboBoxButtonSensitivity :: (IsComboBox o, MIO.MonadIO m) => Gtk.Enums.SensitivityType -> m (GValueConstruct o)
constructComboBoxButtonSensitivity val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "button-sensitivity" val

#if defined(ENABLE_OVERLOADING)
data ComboBoxButtonSensitivityPropertyInfo
instance AttrInfo ComboBoxButtonSensitivityPropertyInfo where
    type AttrAllowedOps ComboBoxButtonSensitivityPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ComboBoxButtonSensitivityPropertyInfo = IsComboBox
    type AttrSetTypeConstraint ComboBoxButtonSensitivityPropertyInfo = (~) Gtk.Enums.SensitivityType
    type AttrTransferTypeConstraint ComboBoxButtonSensitivityPropertyInfo = (~) Gtk.Enums.SensitivityType
    type AttrTransferType ComboBoxButtonSensitivityPropertyInfo = Gtk.Enums.SensitivityType
    type AttrGetType ComboBoxButtonSensitivityPropertyInfo = Gtk.Enums.SensitivityType
    type AttrLabel ComboBoxButtonSensitivityPropertyInfo = "button-sensitivity"
    type AttrOrigin ComboBoxButtonSensitivityPropertyInfo = ComboBox
    attrGet = getComboBoxButtonSensitivity
    attrSet = setComboBoxButtonSensitivity
    attrTransfer _ v = do
        return v
    attrConstruct = constructComboBoxButtonSensitivity
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.buttonSensitivity"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:attr:buttonSensitivity"
        })
#endif

-- VVV Prop "cell-area"
   -- Type: TInterface (Name {namespace = "Gtk", name = "CellArea"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@cell-area@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' comboBox #cellArea
-- @
getComboBoxCellArea :: (MonadIO m, IsComboBox o) => o -> m (Maybe Gtk.CellArea.CellArea)
getComboBoxCellArea obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "cell-area" Gtk.CellArea.CellArea

-- | Construct a `GValueConstruct` with valid value for the “@cell-area@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructComboBoxCellArea :: (IsComboBox o, MIO.MonadIO m, Gtk.CellArea.IsCellArea a) => a -> m (GValueConstruct o)
constructComboBoxCellArea val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "cell-area" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data ComboBoxCellAreaPropertyInfo
instance AttrInfo ComboBoxCellAreaPropertyInfo where
    type AttrAllowedOps ComboBoxCellAreaPropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ComboBoxCellAreaPropertyInfo = IsComboBox
    type AttrSetTypeConstraint ComboBoxCellAreaPropertyInfo = Gtk.CellArea.IsCellArea
    type AttrTransferTypeConstraint ComboBoxCellAreaPropertyInfo = Gtk.CellArea.IsCellArea
    type AttrTransferType ComboBoxCellAreaPropertyInfo = Gtk.CellArea.CellArea
    type AttrGetType ComboBoxCellAreaPropertyInfo = (Maybe Gtk.CellArea.CellArea)
    type AttrLabel ComboBoxCellAreaPropertyInfo = "cell-area"
    type AttrOrigin ComboBoxCellAreaPropertyInfo = ComboBox
    attrGet = getComboBoxCellArea
    attrSet = undefined
    attrTransfer _ v = do
        unsafeCastTo Gtk.CellArea.CellArea v
    attrConstruct = constructComboBoxCellArea
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.cellArea"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:attr:cellArea"
        })
#endif

-- VVV Prop "column-span-column"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@column-span-column@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' comboBox #columnSpanColumn
-- @
getComboBoxColumnSpanColumn :: (MonadIO m, IsComboBox o) => o -> m Int32
getComboBoxColumnSpanColumn obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "column-span-column"

-- | Set the value of the “@column-span-column@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' comboBox [ #columnSpanColumn 'Data.GI.Base.Attributes.:=' value ]
-- @
setComboBoxColumnSpanColumn :: (MonadIO m, IsComboBox o) => o -> Int32 -> m ()
setComboBoxColumnSpanColumn obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "column-span-column" val

-- | Construct a `GValueConstruct` with valid value for the “@column-span-column@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructComboBoxColumnSpanColumn :: (IsComboBox o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructComboBoxColumnSpanColumn val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "column-span-column" val

#if defined(ENABLE_OVERLOADING)
data ComboBoxColumnSpanColumnPropertyInfo
instance AttrInfo ComboBoxColumnSpanColumnPropertyInfo where
    type AttrAllowedOps ComboBoxColumnSpanColumnPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ComboBoxColumnSpanColumnPropertyInfo = IsComboBox
    type AttrSetTypeConstraint ComboBoxColumnSpanColumnPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint ComboBoxColumnSpanColumnPropertyInfo = (~) Int32
    type AttrTransferType ComboBoxColumnSpanColumnPropertyInfo = Int32
    type AttrGetType ComboBoxColumnSpanColumnPropertyInfo = Int32
    type AttrLabel ComboBoxColumnSpanColumnPropertyInfo = "column-span-column"
    type AttrOrigin ComboBoxColumnSpanColumnPropertyInfo = ComboBox
    attrGet = getComboBoxColumnSpanColumn
    attrSet = setComboBoxColumnSpanColumn
    attrTransfer _ v = do
        return v
    attrConstruct = constructComboBoxColumnSpanColumn
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.columnSpanColumn"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:attr:columnSpanColumn"
        })
#endif

-- VVV Prop "entry-text-column"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@entry-text-column@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' comboBox #entryTextColumn
-- @
getComboBoxEntryTextColumn :: (MonadIO m, IsComboBox o) => o -> m Int32
getComboBoxEntryTextColumn obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "entry-text-column"

-- | Set the value of the “@entry-text-column@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' comboBox [ #entryTextColumn 'Data.GI.Base.Attributes.:=' value ]
-- @
setComboBoxEntryTextColumn :: (MonadIO m, IsComboBox o) => o -> Int32 -> m ()
setComboBoxEntryTextColumn obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "entry-text-column" val

-- | Construct a `GValueConstruct` with valid value for the “@entry-text-column@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructComboBoxEntryTextColumn :: (IsComboBox o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructComboBoxEntryTextColumn val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "entry-text-column" val

#if defined(ENABLE_OVERLOADING)
data ComboBoxEntryTextColumnPropertyInfo
instance AttrInfo ComboBoxEntryTextColumnPropertyInfo where
    type AttrAllowedOps ComboBoxEntryTextColumnPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ComboBoxEntryTextColumnPropertyInfo = IsComboBox
    type AttrSetTypeConstraint ComboBoxEntryTextColumnPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint ComboBoxEntryTextColumnPropertyInfo = (~) Int32
    type AttrTransferType ComboBoxEntryTextColumnPropertyInfo = Int32
    type AttrGetType ComboBoxEntryTextColumnPropertyInfo = Int32
    type AttrLabel ComboBoxEntryTextColumnPropertyInfo = "entry-text-column"
    type AttrOrigin ComboBoxEntryTextColumnPropertyInfo = ComboBox
    attrGet = getComboBoxEntryTextColumn
    attrSet = setComboBoxEntryTextColumn
    attrTransfer _ v = do
        return v
    attrConstruct = constructComboBoxEntryTextColumn
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.entryTextColumn"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:attr:entryTextColumn"
        })
#endif

-- VVV Prop "has-entry"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@has-entry@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' comboBox #hasEntry
-- @
getComboBoxHasEntry :: (MonadIO m, IsComboBox o) => o -> m Bool
getComboBoxHasEntry obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "has-entry"

-- | Construct a `GValueConstruct` with valid value for the “@has-entry@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructComboBoxHasEntry :: (IsComboBox o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructComboBoxHasEntry val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "has-entry" val

#if defined(ENABLE_OVERLOADING)
data ComboBoxHasEntryPropertyInfo
instance AttrInfo ComboBoxHasEntryPropertyInfo where
    type AttrAllowedOps ComboBoxHasEntryPropertyInfo = '[ 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ComboBoxHasEntryPropertyInfo = IsComboBox
    type AttrSetTypeConstraint ComboBoxHasEntryPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ComboBoxHasEntryPropertyInfo = (~) Bool
    type AttrTransferType ComboBoxHasEntryPropertyInfo = Bool
    type AttrGetType ComboBoxHasEntryPropertyInfo = Bool
    type AttrLabel ComboBoxHasEntryPropertyInfo = "has-entry"
    type AttrOrigin ComboBoxHasEntryPropertyInfo = ComboBox
    attrGet = getComboBoxHasEntry
    attrSet = undefined
    attrTransfer _ v = do
        return v
    attrConstruct = constructComboBoxHasEntry
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.hasEntry"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:attr:hasEntry"
        })
#endif

-- VVV Prop "has-frame"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@has-frame@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' comboBox #hasFrame
-- @
getComboBoxHasFrame :: (MonadIO m, IsComboBox o) => o -> m Bool
getComboBoxHasFrame obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "has-frame"

-- | Set the value of the “@has-frame@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' comboBox [ #hasFrame 'Data.GI.Base.Attributes.:=' value ]
-- @
setComboBoxHasFrame :: (MonadIO m, IsComboBox o) => o -> Bool -> m ()
setComboBoxHasFrame obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "has-frame" val

-- | Construct a `GValueConstruct` with valid value for the “@has-frame@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructComboBoxHasFrame :: (IsComboBox o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructComboBoxHasFrame val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "has-frame" val

#if defined(ENABLE_OVERLOADING)
data ComboBoxHasFramePropertyInfo
instance AttrInfo ComboBoxHasFramePropertyInfo where
    type AttrAllowedOps ComboBoxHasFramePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ComboBoxHasFramePropertyInfo = IsComboBox
    type AttrSetTypeConstraint ComboBoxHasFramePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ComboBoxHasFramePropertyInfo = (~) Bool
    type AttrTransferType ComboBoxHasFramePropertyInfo = Bool
    type AttrGetType ComboBoxHasFramePropertyInfo = Bool
    type AttrLabel ComboBoxHasFramePropertyInfo = "has-frame"
    type AttrOrigin ComboBoxHasFramePropertyInfo = ComboBox
    attrGet = getComboBoxHasFrame
    attrSet = setComboBoxHasFrame
    attrTransfer _ v = do
        return v
    attrConstruct = constructComboBoxHasFrame
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.hasFrame"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:attr:hasFrame"
        })
#endif

-- VVV Prop "id-column"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@id-column@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' comboBox #idColumn
-- @
getComboBoxIdColumn :: (MonadIO m, IsComboBox o) => o -> m Int32
getComboBoxIdColumn obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "id-column"

-- | Set the value of the “@id-column@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' comboBox [ #idColumn 'Data.GI.Base.Attributes.:=' value ]
-- @
setComboBoxIdColumn :: (MonadIO m, IsComboBox o) => o -> Int32 -> m ()
setComboBoxIdColumn obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "id-column" val

-- | Construct a `GValueConstruct` with valid value for the “@id-column@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructComboBoxIdColumn :: (IsComboBox o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructComboBoxIdColumn val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "id-column" val

#if defined(ENABLE_OVERLOADING)
data ComboBoxIdColumnPropertyInfo
instance AttrInfo ComboBoxIdColumnPropertyInfo where
    type AttrAllowedOps ComboBoxIdColumnPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ComboBoxIdColumnPropertyInfo = IsComboBox
    type AttrSetTypeConstraint ComboBoxIdColumnPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint ComboBoxIdColumnPropertyInfo = (~) Int32
    type AttrTransferType ComboBoxIdColumnPropertyInfo = Int32
    type AttrGetType ComboBoxIdColumnPropertyInfo = Int32
    type AttrLabel ComboBoxIdColumnPropertyInfo = "id-column"
    type AttrOrigin ComboBoxIdColumnPropertyInfo = ComboBox
    attrGet = getComboBoxIdColumn
    attrSet = setComboBoxIdColumn
    attrTransfer _ v = do
        return v
    attrConstruct = constructComboBoxIdColumn
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.idColumn"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:attr:idColumn"
        })
#endif

-- VVV Prop "model"
   -- Type: TInterface (Name {namespace = "Gtk", name = "TreeModel"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just True)

-- | Get the value of the “@model@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' comboBox #model
-- @
getComboBoxModel :: (MonadIO m, IsComboBox o) => o -> m Gtk.TreeModel.TreeModel
getComboBoxModel obj = MIO.liftIO $ checkUnexpectedNothing "getComboBoxModel" $ B.Properties.getObjectPropertyObject obj "model" Gtk.TreeModel.TreeModel

-- | Set the value of the “@model@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' comboBox [ #model 'Data.GI.Base.Attributes.:=' value ]
-- @
setComboBoxModel :: (MonadIO m, IsComboBox o, Gtk.TreeModel.IsTreeModel a) => o -> a -> m ()
setComboBoxModel obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "model" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@model@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructComboBoxModel :: (IsComboBox o, MIO.MonadIO m, Gtk.TreeModel.IsTreeModel a) => a -> m (GValueConstruct o)
constructComboBoxModel val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "model" (P.Just val)

-- | Set the value of the “@model@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #model
-- @
clearComboBoxModel :: (MonadIO m, IsComboBox o) => o -> m ()
clearComboBoxModel obj = liftIO $ B.Properties.setObjectPropertyObject obj "model" (Nothing :: Maybe Gtk.TreeModel.TreeModel)

#if defined(ENABLE_OVERLOADING)
data ComboBoxModelPropertyInfo
instance AttrInfo ComboBoxModelPropertyInfo where
    type AttrAllowedOps ComboBoxModelPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ComboBoxModelPropertyInfo = IsComboBox
    type AttrSetTypeConstraint ComboBoxModelPropertyInfo = Gtk.TreeModel.IsTreeModel
    type AttrTransferTypeConstraint ComboBoxModelPropertyInfo = Gtk.TreeModel.IsTreeModel
    type AttrTransferType ComboBoxModelPropertyInfo = Gtk.TreeModel.TreeModel
    type AttrGetType ComboBoxModelPropertyInfo = Gtk.TreeModel.TreeModel
    type AttrLabel ComboBoxModelPropertyInfo = "model"
    type AttrOrigin ComboBoxModelPropertyInfo = ComboBox
    attrGet = getComboBoxModel
    attrSet = setComboBoxModel
    attrTransfer _ v = do
        unsafeCastTo Gtk.TreeModel.TreeModel v
    attrConstruct = constructComboBoxModel
    attrClear = clearComboBoxModel
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.model"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:attr:model"
        })
#endif

-- VVV Prop "popup-fixed-width"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@popup-fixed-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' comboBox #popupFixedWidth
-- @
getComboBoxPopupFixedWidth :: (MonadIO m, IsComboBox o) => o -> m Bool
getComboBoxPopupFixedWidth obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "popup-fixed-width"

-- | Set the value of the “@popup-fixed-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' comboBox [ #popupFixedWidth 'Data.GI.Base.Attributes.:=' value ]
-- @
setComboBoxPopupFixedWidth :: (MonadIO m, IsComboBox o) => o -> Bool -> m ()
setComboBoxPopupFixedWidth obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "popup-fixed-width" val

-- | Construct a `GValueConstruct` with valid value for the “@popup-fixed-width@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructComboBoxPopupFixedWidth :: (IsComboBox o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructComboBoxPopupFixedWidth val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "popup-fixed-width" val

#if defined(ENABLE_OVERLOADING)
data ComboBoxPopupFixedWidthPropertyInfo
instance AttrInfo ComboBoxPopupFixedWidthPropertyInfo where
    type AttrAllowedOps ComboBoxPopupFixedWidthPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ComboBoxPopupFixedWidthPropertyInfo = IsComboBox
    type AttrSetTypeConstraint ComboBoxPopupFixedWidthPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint ComboBoxPopupFixedWidthPropertyInfo = (~) Bool
    type AttrTransferType ComboBoxPopupFixedWidthPropertyInfo = Bool
    type AttrGetType ComboBoxPopupFixedWidthPropertyInfo = Bool
    type AttrLabel ComboBoxPopupFixedWidthPropertyInfo = "popup-fixed-width"
    type AttrOrigin ComboBoxPopupFixedWidthPropertyInfo = ComboBox
    attrGet = getComboBoxPopupFixedWidth
    attrSet = setComboBoxPopupFixedWidth
    attrTransfer _ v = do
        return v
    attrConstruct = constructComboBoxPopupFixedWidth
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.popupFixedWidth"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:attr:popupFixedWidth"
        })
#endif

-- VVV Prop "popup-shown"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@popup-shown@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' comboBox #popupShown
-- @
getComboBoxPopupShown :: (MonadIO m, IsComboBox o) => o -> m Bool
getComboBoxPopupShown obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "popup-shown"

#if defined(ENABLE_OVERLOADING)
data ComboBoxPopupShownPropertyInfo
instance AttrInfo ComboBoxPopupShownPropertyInfo where
    type AttrAllowedOps ComboBoxPopupShownPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint ComboBoxPopupShownPropertyInfo = IsComboBox
    type AttrSetTypeConstraint ComboBoxPopupShownPropertyInfo = (~) ()
    type AttrTransferTypeConstraint ComboBoxPopupShownPropertyInfo = (~) ()
    type AttrTransferType ComboBoxPopupShownPropertyInfo = ()
    type AttrGetType ComboBoxPopupShownPropertyInfo = Bool
    type AttrLabel ComboBoxPopupShownPropertyInfo = "popup-shown"
    type AttrOrigin ComboBoxPopupShownPropertyInfo = ComboBox
    attrGet = getComboBoxPopupShown
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.popupShown"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:attr:popupShown"
        })
#endif

-- VVV Prop "row-span-column"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@row-span-column@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' comboBox #rowSpanColumn
-- @
getComboBoxRowSpanColumn :: (MonadIO m, IsComboBox o) => o -> m Int32
getComboBoxRowSpanColumn obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "row-span-column"

-- | Set the value of the “@row-span-column@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' comboBox [ #rowSpanColumn 'Data.GI.Base.Attributes.:=' value ]
-- @
setComboBoxRowSpanColumn :: (MonadIO m, IsComboBox o) => o -> Int32 -> m ()
setComboBoxRowSpanColumn obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "row-span-column" val

-- | Construct a `GValueConstruct` with valid value for the “@row-span-column@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructComboBoxRowSpanColumn :: (IsComboBox o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructComboBoxRowSpanColumn val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "row-span-column" val

#if defined(ENABLE_OVERLOADING)
data ComboBoxRowSpanColumnPropertyInfo
instance AttrInfo ComboBoxRowSpanColumnPropertyInfo where
    type AttrAllowedOps ComboBoxRowSpanColumnPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ComboBoxRowSpanColumnPropertyInfo = IsComboBox
    type AttrSetTypeConstraint ComboBoxRowSpanColumnPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint ComboBoxRowSpanColumnPropertyInfo = (~) Int32
    type AttrTransferType ComboBoxRowSpanColumnPropertyInfo = Int32
    type AttrGetType ComboBoxRowSpanColumnPropertyInfo = Int32
    type AttrLabel ComboBoxRowSpanColumnPropertyInfo = "row-span-column"
    type AttrOrigin ComboBoxRowSpanColumnPropertyInfo = ComboBox
    attrGet = getComboBoxRowSpanColumn
    attrSet = setComboBoxRowSpanColumn
    attrTransfer _ v = do
        return v
    attrConstruct = constructComboBoxRowSpanColumn
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.rowSpanColumn"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:attr:rowSpanColumn"
        })
#endif

-- VVV Prop "tearoff-title"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@tearoff-title@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' comboBox #tearoffTitle
-- @
getComboBoxTearoffTitle :: (MonadIO m, IsComboBox o) => o -> m (Maybe T.Text)
getComboBoxTearoffTitle obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "tearoff-title"

-- | Set the value of the “@tearoff-title@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' comboBox [ #tearoffTitle 'Data.GI.Base.Attributes.:=' value ]
-- @
setComboBoxTearoffTitle :: (MonadIO m, IsComboBox o) => o -> T.Text -> m ()
setComboBoxTearoffTitle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "tearoff-title" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@tearoff-title@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructComboBoxTearoffTitle :: (IsComboBox o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructComboBoxTearoffTitle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "tearoff-title" (P.Just val)

-- | Set the value of the “@tearoff-title@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #tearoffTitle
-- @
clearComboBoxTearoffTitle :: (MonadIO m, IsComboBox o) => o -> m ()
clearComboBoxTearoffTitle obj = liftIO $ B.Properties.setObjectPropertyString obj "tearoff-title" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data ComboBoxTearoffTitlePropertyInfo
instance AttrInfo ComboBoxTearoffTitlePropertyInfo where
    type AttrAllowedOps ComboBoxTearoffTitlePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ComboBoxTearoffTitlePropertyInfo = IsComboBox
    type AttrSetTypeConstraint ComboBoxTearoffTitlePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint ComboBoxTearoffTitlePropertyInfo = (~) T.Text
    type AttrTransferType ComboBoxTearoffTitlePropertyInfo = T.Text
    type AttrGetType ComboBoxTearoffTitlePropertyInfo = (Maybe T.Text)
    type AttrLabel ComboBoxTearoffTitlePropertyInfo = "tearoff-title"
    type AttrOrigin ComboBoxTearoffTitlePropertyInfo = ComboBox
    attrGet = getComboBoxTearoffTitle
    attrSet = setComboBoxTearoffTitle
    attrTransfer _ v = do
        return v
    attrConstruct = constructComboBoxTearoffTitle
    attrClear = clearComboBoxTearoffTitle
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.tearoffTitle"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:attr:tearoffTitle"
        })
#endif

-- VVV Prop "wrap-width"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@wrap-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' comboBox #wrapWidth
-- @
getComboBoxWrapWidth :: (MonadIO m, IsComboBox o) => o -> m Int32
getComboBoxWrapWidth obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "wrap-width"

-- | Set the value of the “@wrap-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' comboBox [ #wrapWidth 'Data.GI.Base.Attributes.:=' value ]
-- @
setComboBoxWrapWidth :: (MonadIO m, IsComboBox o) => o -> Int32 -> m ()
setComboBoxWrapWidth obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "wrap-width" val

-- | Construct a `GValueConstruct` with valid value for the “@wrap-width@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructComboBoxWrapWidth :: (IsComboBox o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructComboBoxWrapWidth val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "wrap-width" val

#if defined(ENABLE_OVERLOADING)
data ComboBoxWrapWidthPropertyInfo
instance AttrInfo ComboBoxWrapWidthPropertyInfo where
    type AttrAllowedOps ComboBoxWrapWidthPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ComboBoxWrapWidthPropertyInfo = IsComboBox
    type AttrSetTypeConstraint ComboBoxWrapWidthPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint ComboBoxWrapWidthPropertyInfo = (~) Int32
    type AttrTransferType ComboBoxWrapWidthPropertyInfo = Int32
    type AttrGetType ComboBoxWrapWidthPropertyInfo = Int32
    type AttrLabel ComboBoxWrapWidthPropertyInfo = "wrap-width"
    type AttrOrigin ComboBoxWrapWidthPropertyInfo = ComboBox
    attrGet = getComboBoxWrapWidth
    attrSet = setComboBoxWrapWidth
    attrTransfer _ v = do
        return v
    attrConstruct = constructComboBoxWrapWidth
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.wrapWidth"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#g:attr:wrapWidth"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ComboBox
type instance O.AttributeList ComboBox = ComboBoxAttributeList
type ComboBoxAttributeList = ('[ '("active", ComboBoxActivePropertyInfo), '("activeId", ComboBoxActiveIdPropertyInfo), '("addTearoffs", ComboBoxAddTearoffsPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("buttonSensitivity", ComboBoxButtonSensitivityPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("cellArea", ComboBoxCellAreaPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("columnSpanColumn", ComboBoxColumnSpanColumnPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("editingCanceled", Gtk.CellEditable.CellEditableEditingCanceledPropertyInfo), '("entryTextColumn", ComboBoxEntryTextColumnPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasEntry", ComboBoxHasEntryPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasFrame", ComboBoxHasFramePropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("idColumn", ComboBoxIdColumnPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("model", ComboBoxModelPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("popupFixedWidth", ComboBoxPopupFixedWidthPropertyInfo), '("popupShown", ComboBoxPopupShownPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("rowSpanColumn", ComboBoxRowSpanColumnPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tearoffTitle", ComboBoxTearoffTitlePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("wrapWidth", ComboBoxWrapWidthPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
comboBoxActive :: AttrLabelProxy "active"
comboBoxActive = AttrLabelProxy

comboBoxActiveId :: AttrLabelProxy "activeId"
comboBoxActiveId = AttrLabelProxy

comboBoxAddTearoffs :: AttrLabelProxy "addTearoffs"
comboBoxAddTearoffs = AttrLabelProxy

comboBoxButtonSensitivity :: AttrLabelProxy "buttonSensitivity"
comboBoxButtonSensitivity = AttrLabelProxy

comboBoxCellArea :: AttrLabelProxy "cellArea"
comboBoxCellArea = AttrLabelProxy

comboBoxColumnSpanColumn :: AttrLabelProxy "columnSpanColumn"
comboBoxColumnSpanColumn = AttrLabelProxy

comboBoxEntryTextColumn :: AttrLabelProxy "entryTextColumn"
comboBoxEntryTextColumn = AttrLabelProxy

comboBoxHasEntry :: AttrLabelProxy "hasEntry"
comboBoxHasEntry = AttrLabelProxy

comboBoxHasFrame :: AttrLabelProxy "hasFrame"
comboBoxHasFrame = AttrLabelProxy

comboBoxIdColumn :: AttrLabelProxy "idColumn"
comboBoxIdColumn = AttrLabelProxy

comboBoxModel :: AttrLabelProxy "model"
comboBoxModel = AttrLabelProxy

comboBoxPopupFixedWidth :: AttrLabelProxy "popupFixedWidth"
comboBoxPopupFixedWidth = AttrLabelProxy

comboBoxPopupShown :: AttrLabelProxy "popupShown"
comboBoxPopupShown = AttrLabelProxy

comboBoxRowSpanColumn :: AttrLabelProxy "rowSpanColumn"
comboBoxRowSpanColumn = AttrLabelProxy

comboBoxTearoffTitle :: AttrLabelProxy "tearoffTitle"
comboBoxTearoffTitle = AttrLabelProxy

comboBoxWrapWidth :: AttrLabelProxy "wrapWidth"
comboBoxWrapWidth = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList ComboBox = ComboBoxSignalList
type ComboBoxSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("changed", ComboBoxChangedSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("editingDone", Gtk.CellEditable.CellEditableEditingDoneSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("formatEntryText", ComboBoxFormatEntryTextSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveActive", ComboBoxMoveActiveSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popdown", ComboBoxPopdownSignalInfo), '("popup", ComboBoxPopupSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("removeWidget", Gtk.CellEditable.CellEditableRemoveWidgetSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method ComboBox::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ComboBox" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_combo_box_new" gtk_combo_box_new :: 
    IO (Ptr ComboBox)

-- | Creates a new empty t'GI.Gtk.Objects.ComboBox.ComboBox'.
-- 
-- /Since: 2.4/
comboBoxNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m ComboBox
    -- ^ __Returns:__ A new t'GI.Gtk.Objects.ComboBox.ComboBox'.
comboBoxNew  = liftIO $ do
    result <- gtk_combo_box_new
    checkUnexpectedReturnNULL "comboBoxNew" result
    result' <- (newObject ComboBox) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ComboBox::new_with_area
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkCellArea to use to layout cell renderers"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ComboBox" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_combo_box_new_with_area" gtk_combo_box_new_with_area :: 
    Ptr Gtk.CellArea.CellArea ->            -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    IO (Ptr ComboBox)

-- | Creates a new empty t'GI.Gtk.Objects.ComboBox.ComboBox' using /@area@/ to layout cells.
comboBoxNewWithArea ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.CellArea.IsCellArea a) =>
    a
    -- ^ /@area@/: the t'GI.Gtk.Objects.CellArea.CellArea' to use to layout cell renderers
    -> m ComboBox
    -- ^ __Returns:__ A new t'GI.Gtk.Objects.ComboBox.ComboBox'.
comboBoxNewWithArea area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_combo_box_new_with_area area'
    checkUnexpectedReturnNULL "comboBoxNewWithArea" result
    result' <- (newObject ComboBox) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ComboBox::new_with_area_and_entry
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkCellArea to use to layout cell renderers"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ComboBox" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_combo_box_new_with_area_and_entry" gtk_combo_box_new_with_area_and_entry :: 
    Ptr Gtk.CellArea.CellArea ->            -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    IO (Ptr ComboBox)

-- | Creates a new empty t'GI.Gtk.Objects.ComboBox.ComboBox' with an entry.
-- 
-- The new combo box will use /@area@/ to layout cells.
comboBoxNewWithAreaAndEntry ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.CellArea.IsCellArea a) =>
    a
    -- ^ /@area@/: the t'GI.Gtk.Objects.CellArea.CellArea' to use to layout cell renderers
    -> m ComboBox
    -- ^ __Returns:__ A new t'GI.Gtk.Objects.ComboBox.ComboBox'.
comboBoxNewWithAreaAndEntry area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_combo_box_new_with_area_and_entry area'
    checkUnexpectedReturnNULL "comboBoxNewWithAreaAndEntry" result
    result' <- (newObject ComboBox) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ComboBox::new_with_entry
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ComboBox" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_combo_box_new_with_entry" gtk_combo_box_new_with_entry :: 
    IO (Ptr ComboBox)

-- | Creates a new empty t'GI.Gtk.Objects.ComboBox.ComboBox' with an entry.
-- 
-- /Since: 2.24/
comboBoxNewWithEntry ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m ComboBox
    -- ^ __Returns:__ A new t'GI.Gtk.Objects.ComboBox.ComboBox'.
comboBoxNewWithEntry  = liftIO $ do
    result <- gtk_combo_box_new_with_entry
    checkUnexpectedReturnNULL "comboBoxNewWithEntry" result
    result' <- (newObject ComboBox) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ComboBox::new_with_model
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModel." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ComboBox" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_combo_box_new_with_model" gtk_combo_box_new_with_model :: 
    Ptr Gtk.TreeModel.TreeModel ->          -- model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    IO (Ptr ComboBox)

-- | Creates a new t'GI.Gtk.Objects.ComboBox.ComboBox' with the model initialized to /@model@/.
-- 
-- /Since: 2.4/
comboBoxNewWithModel ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TreeModel.IsTreeModel a) =>
    a
    -- ^ /@model@/: A t'GI.Gtk.Interfaces.TreeModel.TreeModel'.
    -> m ComboBox
    -- ^ __Returns:__ A new t'GI.Gtk.Objects.ComboBox.ComboBox'.
comboBoxNewWithModel model = liftIO $ do
    model' <- unsafeManagedPtrCastPtr model
    result <- gtk_combo_box_new_with_model model'
    checkUnexpectedReturnNULL "comboBoxNewWithModel" result
    result' <- (newObject ComboBox) result
    touchManagedPtr model
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ComboBox::new_with_model_and_entry
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ComboBox" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_combo_box_new_with_model_and_entry" gtk_combo_box_new_with_model_and_entry :: 
    Ptr Gtk.TreeModel.TreeModel ->          -- model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    IO (Ptr ComboBox)

-- | Creates a new empty t'GI.Gtk.Objects.ComboBox.ComboBox' with an entry
-- and with the model initialized to /@model@/.
-- 
-- /Since: 2.24/
comboBoxNewWithModelAndEntry ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TreeModel.IsTreeModel a) =>
    a
    -- ^ /@model@/: A t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> m ComboBox
    -- ^ __Returns:__ A new t'GI.Gtk.Objects.ComboBox.ComboBox'
comboBoxNewWithModelAndEntry model = liftIO $ do
    model' <- unsafeManagedPtrCastPtr model
    result <- gtk_combo_box_new_with_model_and_entry model'
    checkUnexpectedReturnNULL "comboBoxNewWithModelAndEntry" result
    result' <- (newObject ComboBox) result
    touchManagedPtr model
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ComboBox::get_active
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
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
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_combo_box_get_active" gtk_combo_box_get_active :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO Int32

-- | Returns the index of the currently active item, or -1 if there’s no
-- active item. If the model is a non-flat treemodel, and the active item
-- is not an immediate child of the root of the tree, this function returns
-- @gtk_tree_path_get_indices (path)[0]@, where
-- @path@ is the t'GI.Gtk.Structs.TreePath.TreePath' of the active item.
-- 
-- /Since: 2.4/
comboBoxGetActive ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m Int32
    -- ^ __Returns:__ An integer which is the index of the currently active item,
    --     or -1 if there’s no active item.
comboBoxGetActive comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    result <- gtk_combo_box_get_active comboBox'
    touchManagedPtr comboBox
    return result

#if defined(ENABLE_OVERLOADING)
data ComboBoxGetActiveMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxGetActiveMethodInfo a signature where
    overloadedMethod = comboBoxGetActive

instance O.OverloadedMethodInfo ComboBoxGetActiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxGetActive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxGetActive"
        })


#endif

-- method ComboBox::get_active_id
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_get_active_id" gtk_combo_box_get_active_id :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO CString

-- | Returns the ID of the active row of /@comboBox@/.  This value is taken
-- from the active row and the column specified by the [ComboBox:idColumn]("GI.Gtk.Objects.ComboBox#g:attr:idColumn")
-- property of /@comboBox@/ (see 'GI.Gtk.Objects.ComboBox.comboBoxSetIdColumn').
-- 
-- The returned value is an interned string which means that you can
-- compare the pointer by value to other interned strings and that you
-- must not free it.
-- 
-- If the [ComboBox:idColumn]("GI.Gtk.Objects.ComboBox#g:attr:idColumn") property of /@comboBox@/ is not set, or if
-- no row is active, or if the active row has a 'P.Nothing' ID value, then 'P.Nothing'
-- is returned.
-- 
-- /Since: 3.0/
comboBoxGetActiveId ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the ID of the active row, or 'P.Nothing'
comboBoxGetActiveId comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    result <- gtk_combo_box_get_active_id comboBox'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr comboBox
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ComboBoxGetActiveIdMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxGetActiveIdMethodInfo a signature where
    overloadedMethod = comboBoxGetActiveId

instance O.OverloadedMethodInfo ComboBoxGetActiveIdMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxGetActiveId",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxGetActiveId"
        })


#endif

-- method ComboBox::get_active_iter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
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
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeIter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_get_active_iter" gtk_combo_box_get_active_iter :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO CInt

-- | Sets /@iter@/ to point to the currently active item, if any item is active.
-- Otherwise, /@iter@/ is left unchanged.
-- 
-- /Since: 2.4/
comboBoxGetActiveIter ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m ((Bool, Gtk.TreeIter.TreeIter))
    -- ^ __Returns:__ 'P.True' if /@iter@/ was set, 'P.False' otherwise
comboBoxGetActiveIter comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    iter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    result <- gtk_combo_box_get_active_iter comboBox' iter
    let result' = (/= 0) result
    iter' <- (wrapBoxed Gtk.TreeIter.TreeIter) iter
    touchManagedPtr comboBox
    return (result', iter')

#if defined(ENABLE_OVERLOADING)
data ComboBoxGetActiveIterMethodInfo
instance (signature ~ (m ((Bool, Gtk.TreeIter.TreeIter))), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxGetActiveIterMethodInfo a signature where
    overloadedMethod = comboBoxGetActiveIter

instance O.OverloadedMethodInfo ComboBoxGetActiveIterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxGetActiveIter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxGetActiveIter"
        })


#endif

-- method ComboBox::get_add_tearoffs
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_get_add_tearoffs" gtk_combo_box_get_add_tearoffs :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO CInt

{-# DEPRECATED comboBoxGetAddTearoffs ["(Since version 3.10)"] #-}
-- | Gets the current value of the :add-tearoffs property.
comboBoxGetAddTearoffs ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m Bool
    -- ^ __Returns:__ the current value of the :add-tearoffs property.
comboBoxGetAddTearoffs comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    result <- gtk_combo_box_get_add_tearoffs comboBox'
    let result' = (/= 0) result
    touchManagedPtr comboBox
    return result'

#if defined(ENABLE_OVERLOADING)
data ComboBoxGetAddTearoffsMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxGetAddTearoffsMethodInfo a signature where
    overloadedMethod = comboBoxGetAddTearoffs

instance O.OverloadedMethodInfo ComboBoxGetAddTearoffsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxGetAddTearoffs",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxGetAddTearoffs"
        })


#endif

-- method ComboBox::get_button_sensitivity
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "SensitivityType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_combo_box_get_button_sensitivity" gtk_combo_box_get_button_sensitivity :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO CUInt

-- | Returns whether the combo box sets the dropdown button
-- sensitive or not when there are no items in the model.
-- 
-- /Since: 2.14/
comboBoxGetButtonSensitivity ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m Gtk.Enums.SensitivityType
    -- ^ __Returns:__ 'GI.Gtk.Enums.SensitivityTypeOn' if the dropdown button
    --    is sensitive when the model is empty, 'GI.Gtk.Enums.SensitivityTypeOff'
    --    if the button is always insensitive or
    --    'GI.Gtk.Enums.SensitivityTypeAuto' if it is only sensitive as long as
    --    the model has one item to be selected.
comboBoxGetButtonSensitivity comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    result <- gtk_combo_box_get_button_sensitivity comboBox'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr comboBox
    return result'

#if defined(ENABLE_OVERLOADING)
data ComboBoxGetButtonSensitivityMethodInfo
instance (signature ~ (m Gtk.Enums.SensitivityType), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxGetButtonSensitivityMethodInfo a signature where
    overloadedMethod = comboBoxGetButtonSensitivity

instance O.OverloadedMethodInfo ComboBoxGetButtonSensitivityMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxGetButtonSensitivity",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxGetButtonSensitivity"
        })


#endif

-- method ComboBox::get_column_span_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
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
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_combo_box_get_column_span_column" gtk_combo_box_get_column_span_column :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO Int32

-- | Returns the column with column span information for /@comboBox@/.
-- 
-- /Since: 2.6/
comboBoxGetColumnSpanColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m Int32
    -- ^ __Returns:__ the column span column.
comboBoxGetColumnSpanColumn comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    result <- gtk_combo_box_get_column_span_column comboBox'
    touchManagedPtr comboBox
    return result

#if defined(ENABLE_OVERLOADING)
data ComboBoxGetColumnSpanColumnMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxGetColumnSpanColumnMethodInfo a signature where
    overloadedMethod = comboBoxGetColumnSpanColumn

instance O.OverloadedMethodInfo ComboBoxGetColumnSpanColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxGetColumnSpanColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxGetColumnSpanColumn"
        })


#endif

-- method ComboBox::get_entry_text_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkComboBox." , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_get_entry_text_column" gtk_combo_box_get_entry_text_column :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO Int32

-- | Returns the column which /@comboBox@/ is using to get the strings
-- from to display in the internal entry.
-- 
-- /Since: 2.24/
comboBoxGetEntryTextColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'.
    -> m Int32
    -- ^ __Returns:__ A column in the data source model of /@comboBox@/.
comboBoxGetEntryTextColumn comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    result <- gtk_combo_box_get_entry_text_column comboBox'
    touchManagedPtr comboBox
    return result

#if defined(ENABLE_OVERLOADING)
data ComboBoxGetEntryTextColumnMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxGetEntryTextColumnMethodInfo a signature where
    overloadedMethod = comboBoxGetEntryTextColumn

instance O.OverloadedMethodInfo ComboBoxGetEntryTextColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxGetEntryTextColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxGetEntryTextColumn"
        })


#endif

-- method ComboBox::get_focus_on_click
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_get_focus_on_click" gtk_combo_box_get_focus_on_click :: 
    Ptr ComboBox ->                         -- combo : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO CInt

{-# DEPRECATED comboBoxGetFocusOnClick ["(Since version 3.20)","Use 'GI.Gtk.Objects.Widget.widgetGetFocusOnClick' instead"] #-}
-- | Returns whether the combo box grabs focus when it is clicked
-- with the mouse. See 'GI.Gtk.Objects.ComboBox.comboBoxSetFocusOnClick'.
-- 
-- /Since: 2.6/
comboBoxGetFocusOnClick ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@combo@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the combo box grabs focus when it is
    --     clicked with the mouse.
comboBoxGetFocusOnClick combo = liftIO $ do
    combo' <- unsafeManagedPtrCastPtr combo
    result <- gtk_combo_box_get_focus_on_click combo'
    let result' = (/= 0) result
    touchManagedPtr combo
    return result'

#if defined(ENABLE_OVERLOADING)
data ComboBoxGetFocusOnClickMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxGetFocusOnClickMethodInfo a signature where
    overloadedMethod = comboBoxGetFocusOnClick

instance O.OverloadedMethodInfo ComboBoxGetFocusOnClickMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxGetFocusOnClick",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxGetFocusOnClick"
        })


#endif

-- method ComboBox::get_has_entry
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_get_has_entry" gtk_combo_box_get_has_entry :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO CInt

-- | Returns whether the combo box has an entry.
-- 
-- /Since: 2.24/
comboBoxGetHasEntry ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m Bool
    -- ^ __Returns:__ whether there is an entry in /@comboBox@/.
comboBoxGetHasEntry comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    result <- gtk_combo_box_get_has_entry comboBox'
    let result' = (/= 0) result
    touchManagedPtr comboBox
    return result'

#if defined(ENABLE_OVERLOADING)
data ComboBoxGetHasEntryMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxGetHasEntryMethodInfo a signature where
    overloadedMethod = comboBoxGetHasEntry

instance O.OverloadedMethodInfo ComboBoxGetHasEntryMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxGetHasEntry",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxGetHasEntry"
        })


#endif

-- method ComboBox::get_id_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
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
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_combo_box_get_id_column" gtk_combo_box_get_id_column :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO Int32

-- | Returns the column which /@comboBox@/ is using to get string IDs
-- for values from.
-- 
-- /Since: 3.0/
comboBoxGetIdColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m Int32
    -- ^ __Returns:__ A column in the data source model of /@comboBox@/.
comboBoxGetIdColumn comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    result <- gtk_combo_box_get_id_column comboBox'
    touchManagedPtr comboBox
    return result

#if defined(ENABLE_OVERLOADING)
data ComboBoxGetIdColumnMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxGetIdColumnMethodInfo a signature where
    overloadedMethod = comboBoxGetIdColumn

instance O.OverloadedMethodInfo ComboBoxGetIdColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxGetIdColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxGetIdColumn"
        })


#endif

-- method ComboBox::get_model
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
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
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TreeModel" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_combo_box_get_model" gtk_combo_box_get_model :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO (Ptr Gtk.TreeModel.TreeModel)

-- | Returns the t'GI.Gtk.Interfaces.TreeModel.TreeModel' which is acting as data source for /@comboBox@/.
-- 
-- /Since: 2.4/
comboBoxGetModel ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m Gtk.TreeModel.TreeModel
    -- ^ __Returns:__ A t'GI.Gtk.Interfaces.TreeModel.TreeModel' which was passed
    --     during construction.
comboBoxGetModel comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    result <- gtk_combo_box_get_model comboBox'
    checkUnexpectedReturnNULL "comboBoxGetModel" result
    result' <- (newObject Gtk.TreeModel.TreeModel) result
    touchManagedPtr comboBox
    return result'

#if defined(ENABLE_OVERLOADING)
data ComboBoxGetModelMethodInfo
instance (signature ~ (m Gtk.TreeModel.TreeModel), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxGetModelMethodInfo a signature where
    overloadedMethod = comboBoxGetModel

instance O.OverloadedMethodInfo ComboBoxGetModelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxGetModel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxGetModel"
        })


#endif

-- method ComboBox::get_popup_accessible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Atk" , name = "Object" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_combo_box_get_popup_accessible" gtk_combo_box_get_popup_accessible :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO (Ptr Atk.Object.Object)

-- | Gets the accessible object corresponding to the combo box’s popup.
-- 
-- This function is mostly intended for use by accessibility technologies;
-- applications should have little use for it.
-- 
-- /Since: 2.6/
comboBoxGetPopupAccessible ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m Atk.Object.Object
    -- ^ __Returns:__ the accessible object corresponding
    --     to the combo box’s popup.
comboBoxGetPopupAccessible comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    result <- gtk_combo_box_get_popup_accessible comboBox'
    checkUnexpectedReturnNULL "comboBoxGetPopupAccessible" result
    result' <- (newObject Atk.Object.Object) result
    touchManagedPtr comboBox
    return result'

#if defined(ENABLE_OVERLOADING)
data ComboBoxGetPopupAccessibleMethodInfo
instance (signature ~ (m Atk.Object.Object), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxGetPopupAccessibleMethodInfo a signature where
    overloadedMethod = comboBoxGetPopupAccessible

instance O.OverloadedMethodInfo ComboBoxGetPopupAccessibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxGetPopupAccessible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxGetPopupAccessible"
        })


#endif

-- method ComboBox::get_popup_fixed_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_get_popup_fixed_width" gtk_combo_box_get_popup_fixed_width :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO CInt

-- | Gets whether the popup uses a fixed width matching
-- the allocated width of the combo box.
-- 
-- /Since: 3.0/
comboBoxGetPopupFixedWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the popup uses a fixed width
comboBoxGetPopupFixedWidth comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    result <- gtk_combo_box_get_popup_fixed_width comboBox'
    let result' = (/= 0) result
    touchManagedPtr comboBox
    return result'

#if defined(ENABLE_OVERLOADING)
data ComboBoxGetPopupFixedWidthMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxGetPopupFixedWidthMethodInfo a signature where
    overloadedMethod = comboBoxGetPopupFixedWidth

instance O.OverloadedMethodInfo ComboBoxGetPopupFixedWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxGetPopupFixedWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxGetPopupFixedWidth"
        })


#endif

-- method ComboBox::get_row_span_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
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
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_combo_box_get_row_span_column" gtk_combo_box_get_row_span_column :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO Int32

-- | Returns the column with row span information for /@comboBox@/.
-- 
-- /Since: 2.6/
comboBoxGetRowSpanColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m Int32
    -- ^ __Returns:__ the row span column.
comboBoxGetRowSpanColumn comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    result <- gtk_combo_box_get_row_span_column comboBox'
    touchManagedPtr comboBox
    return result

#if defined(ENABLE_OVERLOADING)
data ComboBoxGetRowSpanColumnMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxGetRowSpanColumnMethodInfo a signature where
    overloadedMethod = comboBoxGetRowSpanColumn

instance O.OverloadedMethodInfo ComboBoxGetRowSpanColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxGetRowSpanColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxGetRowSpanColumn"
        })


#endif

-- method ComboBox::get_title
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_get_title" gtk_combo_box_get_title :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO CString

{-# DEPRECATED comboBoxGetTitle ["(Since version 3.10)"] #-}
-- | Gets the current title of the menu in tearoff mode. See
-- 'GI.Gtk.Objects.ComboBox.comboBoxSetAddTearoffs'.
-- 
-- /Since: 2.10/
comboBoxGetTitle ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m T.Text
    -- ^ __Returns:__ the menu’s title in tearoff mode. This is an internal copy of the
    -- string which must not be freed.
comboBoxGetTitle comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    result <- gtk_combo_box_get_title comboBox'
    checkUnexpectedReturnNULL "comboBoxGetTitle" result
    result' <- cstringToText result
    touchManagedPtr comboBox
    return result'

#if defined(ENABLE_OVERLOADING)
data ComboBoxGetTitleMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxGetTitleMethodInfo a signature where
    overloadedMethod = comboBoxGetTitle

instance O.OverloadedMethodInfo ComboBoxGetTitleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxGetTitle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxGetTitle"
        })


#endif

-- method ComboBox::get_wrap_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
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
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_combo_box_get_wrap_width" gtk_combo_box_get_wrap_width :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO Int32

-- | Returns the wrap width which is used to determine the number of columns
-- for the popup menu. If the wrap width is larger than 1, the combo box
-- is in table mode.
-- 
-- /Since: 2.6/
comboBoxGetWrapWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m Int32
    -- ^ __Returns:__ the wrap width.
comboBoxGetWrapWidth comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    result <- gtk_combo_box_get_wrap_width comboBox'
    touchManagedPtr comboBox
    return result

#if defined(ENABLE_OVERLOADING)
data ComboBoxGetWrapWidthMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxGetWrapWidthMethodInfo a signature where
    overloadedMethod = comboBoxGetWrapWidth

instance O.OverloadedMethodInfo ComboBoxGetWrapWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxGetWrapWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxGetWrapWidth"
        })


#endif

-- method ComboBox::popdown
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_popdown" gtk_combo_box_popdown :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO ()

-- | Hides the menu or dropdown list of /@comboBox@/.
-- 
-- This function is mostly intended for use by accessibility technologies;
-- applications should have little use for it.
-- 
-- /Since: 2.4/
comboBoxPopdown ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m ()
comboBoxPopdown comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    gtk_combo_box_popdown comboBox'
    touchManagedPtr comboBox
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxPopdownMethodInfo
instance (signature ~ (m ()), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxPopdownMethodInfo a signature where
    overloadedMethod = comboBoxPopdown

instance O.OverloadedMethodInfo ComboBoxPopdownMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxPopdown",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxPopdown"
        })


#endif

-- method ComboBox::popup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_popup" gtk_combo_box_popup :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    IO ()

-- | Pops up the menu or dropdown list of /@comboBox@/.
-- 
-- This function is mostly intended for use by accessibility technologies;
-- applications should have little use for it.
-- 
-- Before calling this, /@comboBox@/ must be mapped, or nothing will happen.
-- 
-- /Since: 2.4/
comboBoxPopup ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> m ()
comboBoxPopup comboBox = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    gtk_combo_box_popup comboBox'
    touchManagedPtr comboBox
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxPopupMethodInfo
instance (signature ~ (m ()), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxPopupMethodInfo a signature where
    overloadedMethod = comboBoxPopup

instance O.OverloadedMethodInfo ComboBoxPopupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxPopup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxPopup"
        })


#endif

-- method ComboBox::popup_for_device
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "device"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Device" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkDevice" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_popup_for_device" gtk_combo_box_popup_for_device :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    Ptr Gdk.Device.Device ->                -- device : TInterface (Name {namespace = "Gdk", name = "Device"})
    IO ()

-- | Pops up the menu or dropdown list of /@comboBox@/, the popup window
-- will be grabbed so only /@device@/ and its associated pointer\/keyboard
-- are the only @/GdkDevices/@ able to send events to it.
-- 
-- /Since: 3.0/
comboBoxPopupForDevice ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a, Gdk.Device.IsDevice b) =>
    a
    -- ^ /@comboBox@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> b
    -- ^ /@device@/: a t'GI.Gdk.Objects.Device.Device'
    -> m ()
comboBoxPopupForDevice comboBox device = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    device' <- unsafeManagedPtrCastPtr device
    gtk_combo_box_popup_for_device comboBox' device'
    touchManagedPtr comboBox
    touchManagedPtr device
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxPopupForDeviceMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsComboBox a, Gdk.Device.IsDevice b) => O.OverloadedMethod ComboBoxPopupForDeviceMethodInfo a signature where
    overloadedMethod = comboBoxPopupForDevice

instance O.OverloadedMethodInfo ComboBoxPopupForDeviceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxPopupForDevice",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxPopupForDevice"
        })


#endif

-- method ComboBox::set_active
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
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
--           { argCName = "index_"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "An index in the model passed during construction, or -1 to have\nno active item"
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

foreign import ccall "gtk_combo_box_set_active" gtk_combo_box_set_active :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    Int32 ->                                -- index_ : TBasicType TInt
    IO ()

-- | Sets the active item of /@comboBox@/ to be the item at /@index@/.
-- 
-- /Since: 2.4/
comboBoxSetActive ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> Int32
    -- ^ /@index_@/: An index in the model passed during construction, or -1 to have
    -- no active item
    -> m ()
comboBoxSetActive comboBox index_ = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    gtk_combo_box_set_active comboBox' index_
    touchManagedPtr comboBox
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxSetActiveMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxSetActiveMethodInfo a signature where
    overloadedMethod = comboBoxSetActive

instance O.OverloadedMethodInfo ComboBoxSetActiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxSetActive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxSetActive"
        })


#endif

-- method ComboBox::set_active_id
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "active_id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the ID of the row to select, or %NULL"
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

foreign import ccall "gtk_combo_box_set_active_id" gtk_combo_box_set_active_id :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    CString ->                              -- active_id : TBasicType TUTF8
    IO CInt

-- | Changes the active row of /@comboBox@/ to the one that has an ID equal to
-- /@activeId@/, or unsets the active row if /@activeId@/ is 'P.Nothing'.  Rows having
-- a 'P.Nothing' ID string cannot be made active by this function.
-- 
-- If the [ComboBox:idColumn]("GI.Gtk.Objects.ComboBox#g:attr:idColumn") property of /@comboBox@/ is unset or if no
-- row has the given ID then the function does nothing and returns 'P.False'.
-- 
-- /Since: 3.0/
comboBoxSetActiveId ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> Maybe (T.Text)
    -- ^ /@activeId@/: the ID of the row to select, or 'P.Nothing'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if a row with a matching ID was found.  If a 'P.Nothing'
    --          /@activeId@/ was given to unset the active row, the function
    --          always returns 'P.True'.
comboBoxSetActiveId comboBox activeId = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    maybeActiveId <- case activeId of
        Nothing -> return nullPtr
        Just jActiveId -> do
            jActiveId' <- textToCString jActiveId
            return jActiveId'
    result <- gtk_combo_box_set_active_id comboBox' maybeActiveId
    let result' = (/= 0) result
    touchManagedPtr comboBox
    freeMem maybeActiveId
    return result'

#if defined(ENABLE_OVERLOADING)
data ComboBoxSetActiveIdMethodInfo
instance (signature ~ (Maybe (T.Text) -> m Bool), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxSetActiveIdMethodInfo a signature where
    overloadedMethod = comboBoxSetActiveId

instance O.OverloadedMethodInfo ComboBoxSetActiveIdMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxSetActiveId",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxSetActiveId"
        })


#endif

-- method ComboBox::set_active_iter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
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
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkTreeIter, or %NULL"
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

foreign import ccall "gtk_combo_box_set_active_iter" gtk_combo_box_set_active_iter :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Sets the current active item to be the one referenced by /@iter@/, or
-- unsets the active item if /@iter@/ is 'P.Nothing'.
-- 
-- /Since: 2.4/
comboBoxSetActiveIter ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> Maybe (Gtk.TreeIter.TreeIter)
    -- ^ /@iter@/: The t'GI.Gtk.Structs.TreeIter.TreeIter', or 'P.Nothing'
    -> m ()
comboBoxSetActiveIter comboBox iter = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    maybeIter <- case iter of
        Nothing -> return nullPtr
        Just jIter -> do
            jIter' <- unsafeManagedPtrGetPtr jIter
            return jIter'
    gtk_combo_box_set_active_iter comboBox' maybeIter
    touchManagedPtr comboBox
    whenJust iter touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxSetActiveIterMethodInfo
instance (signature ~ (Maybe (Gtk.TreeIter.TreeIter) -> m ()), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxSetActiveIterMethodInfo a signature where
    overloadedMethod = comboBoxSetActiveIter

instance O.OverloadedMethodInfo ComboBoxSetActiveIterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxSetActiveIter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxSetActiveIter"
        })


#endif

-- method ComboBox::set_add_tearoffs
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "add_tearoffs"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to add tearoff menu items"
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

foreign import ccall "gtk_combo_box_set_add_tearoffs" gtk_combo_box_set_add_tearoffs :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    CInt ->                                 -- add_tearoffs : TBasicType TBoolean
    IO ()

{-# DEPRECATED comboBoxSetAddTearoffs ["(Since version 3.10)"] #-}
-- | Sets whether the popup menu should have a tearoff
-- menu item.
-- 
-- /Since: 2.6/
comboBoxSetAddTearoffs ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> Bool
    -- ^ /@addTearoffs@/: 'P.True' to add tearoff menu items
    -> m ()
comboBoxSetAddTearoffs comboBox addTearoffs = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    let addTearoffs' = (fromIntegral . fromEnum) addTearoffs
    gtk_combo_box_set_add_tearoffs comboBox' addTearoffs'
    touchManagedPtr comboBox
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxSetAddTearoffsMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxSetAddTearoffsMethodInfo a signature where
    overloadedMethod = comboBoxSetAddTearoffs

instance O.OverloadedMethodInfo ComboBoxSetAddTearoffsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxSetAddTearoffs",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxSetAddTearoffs"
        })


#endif

-- method ComboBox::set_button_sensitivity
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sensitivity"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SensitivityType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "specify the sensitivity of the dropdown button"
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

foreign import ccall "gtk_combo_box_set_button_sensitivity" gtk_combo_box_set_button_sensitivity :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    CUInt ->                                -- sensitivity : TInterface (Name {namespace = "Gtk", name = "SensitivityType"})
    IO ()

-- | Sets whether the dropdown button of the combo box should be
-- always sensitive ('GI.Gtk.Enums.SensitivityTypeOn'), never sensitive ('GI.Gtk.Enums.SensitivityTypeOff')
-- or only if there is at least one item to display ('GI.Gtk.Enums.SensitivityTypeAuto').
-- 
-- /Since: 2.14/
comboBoxSetButtonSensitivity ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> Gtk.Enums.SensitivityType
    -- ^ /@sensitivity@/: specify the sensitivity of the dropdown button
    -> m ()
comboBoxSetButtonSensitivity comboBox sensitivity = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    let sensitivity' = (fromIntegral . fromEnum) sensitivity
    gtk_combo_box_set_button_sensitivity comboBox' sensitivity'
    touchManagedPtr comboBox
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxSetButtonSensitivityMethodInfo
instance (signature ~ (Gtk.Enums.SensitivityType -> m ()), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxSetButtonSensitivityMethodInfo a signature where
    overloadedMethod = comboBoxSetButtonSensitivity

instance O.OverloadedMethodInfo ComboBoxSetButtonSensitivityMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxSetButtonSensitivity",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxSetButtonSensitivity"
        })


#endif

-- method ComboBox::set_column_span_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
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
--           { argCName = "column_span"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "A column in the model passed during construction"
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

foreign import ccall "gtk_combo_box_set_column_span_column" gtk_combo_box_set_column_span_column :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    Int32 ->                                -- column_span : TBasicType TInt
    IO ()

-- | Sets the column with column span information for /@comboBox@/ to be
-- /@columnSpan@/. The column span column contains integers which indicate
-- how many columns an item should span.
-- 
-- /Since: 2.4/
comboBoxSetColumnSpanColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> Int32
    -- ^ /@columnSpan@/: A column in the model passed during construction
    -> m ()
comboBoxSetColumnSpanColumn comboBox columnSpan = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    gtk_combo_box_set_column_span_column comboBox' columnSpan
    touchManagedPtr comboBox
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxSetColumnSpanColumnMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxSetColumnSpanColumnMethodInfo a signature where
    overloadedMethod = comboBoxSetColumnSpanColumn

instance O.OverloadedMethodInfo ComboBoxSetColumnSpanColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxSetColumnSpanColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxSetColumnSpanColumn"
        })


#endif

-- method ComboBox::set_entry_text_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
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
--           { argCName = "text_column"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "A column in @model to get the strings from for\n    the internal entry"
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

foreign import ccall "gtk_combo_box_set_entry_text_column" gtk_combo_box_set_entry_text_column :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    Int32 ->                                -- text_column : TBasicType TInt
    IO ()

-- | Sets the model column which /@comboBox@/ should use to get strings from
-- to be /@textColumn@/. The column /@textColumn@/ in the model of /@comboBox@/
-- must be of type @/G_TYPE_STRING/@.
-- 
-- This is only relevant if /@comboBox@/ has been created with
-- [ComboBox:hasEntry]("GI.Gtk.Objects.ComboBox#g:attr:hasEntry") as 'P.True'.
-- 
-- /Since: 2.24/
comboBoxSetEntryTextColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> Int32
    -- ^ /@textColumn@/: A column in /@model@/ to get the strings from for
    --     the internal entry
    -> m ()
comboBoxSetEntryTextColumn comboBox textColumn = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    gtk_combo_box_set_entry_text_column comboBox' textColumn
    touchManagedPtr comboBox
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxSetEntryTextColumnMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxSetEntryTextColumnMethodInfo a signature where
    overloadedMethod = comboBoxSetEntryTextColumn

instance O.OverloadedMethodInfo ComboBoxSetEntryTextColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxSetEntryTextColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxSetEntryTextColumn"
        })


#endif

-- method ComboBox::set_focus_on_click
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "focus_on_click"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "whether the combo box grabs focus when clicked\n   with the mouse"
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

foreign import ccall "gtk_combo_box_set_focus_on_click" gtk_combo_box_set_focus_on_click :: 
    Ptr ComboBox ->                         -- combo : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    CInt ->                                 -- focus_on_click : TBasicType TBoolean
    IO ()

{-# DEPRECATED comboBoxSetFocusOnClick ["(Since version 3.20)","Use 'GI.Gtk.Objects.Widget.widgetSetFocusOnClick' instead"] #-}
-- | Sets whether the combo box will grab focus when it is clicked with
-- the mouse. Making mouse clicks not grab focus is useful in places
-- like toolbars where you don’t want the keyboard focus removed from
-- the main area of the application.
-- 
-- /Since: 2.6/
comboBoxSetFocusOnClick ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@combo@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> Bool
    -- ^ /@focusOnClick@/: whether the combo box grabs focus when clicked
    --    with the mouse
    -> m ()
comboBoxSetFocusOnClick combo focusOnClick = liftIO $ do
    combo' <- unsafeManagedPtrCastPtr combo
    let focusOnClick' = (fromIntegral . fromEnum) focusOnClick
    gtk_combo_box_set_focus_on_click combo' focusOnClick'
    touchManagedPtr combo
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxSetFocusOnClickMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxSetFocusOnClickMethodInfo a signature where
    overloadedMethod = comboBoxSetFocusOnClick

instance O.OverloadedMethodInfo ComboBoxSetFocusOnClickMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxSetFocusOnClick",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxSetFocusOnClick"
        })


#endif

-- method ComboBox::set_id_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
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
--           { argCName = "id_column"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "A column in @model to get string IDs for values from"
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

foreign import ccall "gtk_combo_box_set_id_column" gtk_combo_box_set_id_column :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    Int32 ->                                -- id_column : TBasicType TInt
    IO ()

-- | Sets the model column which /@comboBox@/ should use to get string IDs
-- for values from. The column /@idColumn@/ in the model of /@comboBox@/
-- must be of type @/G_TYPE_STRING/@.
-- 
-- /Since: 3.0/
comboBoxSetIdColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> Int32
    -- ^ /@idColumn@/: A column in /@model@/ to get string IDs for values from
    -> m ()
comboBoxSetIdColumn comboBox idColumn = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    gtk_combo_box_set_id_column comboBox' idColumn
    touchManagedPtr comboBox
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxSetIdColumnMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxSetIdColumnMethodInfo a signature where
    overloadedMethod = comboBoxSetIdColumn

instance O.OverloadedMethodInfo ComboBoxSetIdColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxSetIdColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxSetIdColumn"
        })


#endif

-- method ComboBox::set_model
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
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
--           { argCName = "model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_combo_box_set_model" gtk_combo_box_set_model :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    Ptr Gtk.TreeModel.TreeModel ->          -- model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    IO ()

-- | Sets the model used by /@comboBox@/ to be /@model@/. Will unset a previously set
-- model (if applicable). If model is 'P.Nothing', then it will unset the model.
-- 
-- Note that this function does not clear the cell renderers, you have to
-- call 'GI.Gtk.Interfaces.CellLayout.cellLayoutClear' yourself if you need to set up different
-- cell renderers for the new model.
-- 
-- /Since: 2.4/
comboBoxSetModel ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a, Gtk.TreeModel.IsTreeModel b) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> Maybe (b)
    -- ^ /@model@/: A t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> m ()
comboBoxSetModel comboBox model = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    maybeModel <- case model of
        Nothing -> return nullPtr
        Just jModel -> do
            jModel' <- unsafeManagedPtrCastPtr jModel
            return jModel'
    gtk_combo_box_set_model comboBox' maybeModel
    touchManagedPtr comboBox
    whenJust model touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxSetModelMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsComboBox a, Gtk.TreeModel.IsTreeModel b) => O.OverloadedMethod ComboBoxSetModelMethodInfo a signature where
    overloadedMethod = comboBoxSetModel

instance O.OverloadedMethodInfo ComboBoxSetModelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxSetModel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxSetModel"
        })


#endif

-- method ComboBox::set_popup_fixed_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "fixed"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether to use a fixed popup width"
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

foreign import ccall "gtk_combo_box_set_popup_fixed_width" gtk_combo_box_set_popup_fixed_width :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    CInt ->                                 -- fixed : TBasicType TBoolean
    IO ()

-- | Specifies whether the popup’s width should be a fixed width
-- matching the allocated width of the combo box.
-- 
-- /Since: 3.0/
comboBoxSetPopupFixedWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> Bool
    -- ^ /@fixed@/: whether to use a fixed popup width
    -> m ()
comboBoxSetPopupFixedWidth comboBox fixed = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    let fixed' = (fromIntegral . fromEnum) fixed
    gtk_combo_box_set_popup_fixed_width comboBox' fixed'
    touchManagedPtr comboBox
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxSetPopupFixedWidthMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxSetPopupFixedWidthMethodInfo a signature where
    overloadedMethod = comboBoxSetPopupFixedWidth

instance O.OverloadedMethodInfo ComboBoxSetPopupFixedWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxSetPopupFixedWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxSetPopupFixedWidth"
        })


#endif

-- method ComboBox::set_row_separator_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "TreeViewRowSeparatorFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeViewRowSeparatorFunc"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
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
--                 { rawDocText = Just "user data to pass to @func, or %NULL"
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "destroy notifier for @data, or %NULL"
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

foreign import ccall "gtk_combo_box_set_row_separator_func" gtk_combo_box_set_row_separator_func :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    FunPtr Gtk.Callbacks.C_TreeViewRowSeparatorFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "TreeViewRowSeparatorFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Sets the row separator function, which is used to determine
-- whether a row should be drawn as a separator. If the row separator
-- function is 'P.Nothing', no separators are drawn. This is the default value.
-- 
-- /Since: 2.6/
comboBoxSetRowSeparatorFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> Gtk.Callbacks.TreeViewRowSeparatorFunc
    -- ^ /@func@/: a t'GI.Gtk.Callbacks.TreeViewRowSeparatorFunc'
    -> m ()
comboBoxSetRowSeparatorFunc comboBox func = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    func' <- Gtk.Callbacks.mk_TreeViewRowSeparatorFunc (Gtk.Callbacks.wrap_TreeViewRowSeparatorFunc Nothing (Gtk.Callbacks.drop_closures_TreeViewRowSeparatorFunc func))
    let data_ = castFunPtrToPtr func'
    let destroy = SP.safeFreeFunPtrPtr
    gtk_combo_box_set_row_separator_func comboBox' func' data_ destroy
    touchManagedPtr comboBox
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxSetRowSeparatorFuncMethodInfo
instance (signature ~ (Gtk.Callbacks.TreeViewRowSeparatorFunc -> m ()), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxSetRowSeparatorFuncMethodInfo a signature where
    overloadedMethod = comboBoxSetRowSeparatorFunc

instance O.OverloadedMethodInfo ComboBoxSetRowSeparatorFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxSetRowSeparatorFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxSetRowSeparatorFunc"
        })


#endif

-- method ComboBox::set_row_span_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkComboBox." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "row_span"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "A column in the model passed during construction."
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

foreign import ccall "gtk_combo_box_set_row_span_column" gtk_combo_box_set_row_span_column :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    Int32 ->                                -- row_span : TBasicType TInt
    IO ()

-- | Sets the column with row span information for /@comboBox@/ to be /@rowSpan@/.
-- The row span column contains integers which indicate how many rows
-- an item should span.
-- 
-- /Since: 2.4/
comboBoxSetRowSpanColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'.
    -> Int32
    -- ^ /@rowSpan@/: A column in the model passed during construction.
    -> m ()
comboBoxSetRowSpanColumn comboBox rowSpan = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    gtk_combo_box_set_row_span_column comboBox' rowSpan
    touchManagedPtr comboBox
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxSetRowSpanColumnMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxSetRowSpanColumnMethodInfo a signature where
    overloadedMethod = comboBoxSetRowSpanColumn

instance O.OverloadedMethodInfo ComboBoxSetRowSpanColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxSetRowSpanColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxSetRowSpanColumn"
        })


#endif

-- method ComboBox::set_title
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkComboBox" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a title for the menu in tearoff mode"
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

foreign import ccall "gtk_combo_box_set_title" gtk_combo_box_set_title :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    CString ->                              -- title : TBasicType TUTF8
    IO ()

{-# DEPRECATED comboBoxSetTitle ["(Since version 3.10)"] #-}
-- | Sets the menu’s title in tearoff mode.
-- 
-- /Since: 2.10/
comboBoxSetTitle ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: a t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> T.Text
    -- ^ /@title@/: a title for the menu in tearoff mode
    -> m ()
comboBoxSetTitle comboBox title = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    title' <- textToCString title
    gtk_combo_box_set_title comboBox' title'
    touchManagedPtr comboBox
    freeMem title'
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxSetTitleMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxSetTitleMethodInfo a signature where
    overloadedMethod = comboBoxSetTitle

instance O.OverloadedMethodInfo ComboBoxSetTitleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxSetTitle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxSetTitle"
        })


#endif

-- method ComboBox::set_wrap_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "combo_box"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ComboBox" }
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
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Preferred number of columns"
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

foreign import ccall "gtk_combo_box_set_wrap_width" gtk_combo_box_set_wrap_width :: 
    Ptr ComboBox ->                         -- combo_box : TInterface (Name {namespace = "Gtk", name = "ComboBox"})
    Int32 ->                                -- width : TBasicType TInt
    IO ()

-- | Sets the wrap width of /@comboBox@/ to be /@width@/. The wrap width is basically
-- the preferred number of columns when you want the popup to be layed out
-- in a table.
-- 
-- /Since: 2.4/
comboBoxSetWrapWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsComboBox a) =>
    a
    -- ^ /@comboBox@/: A t'GI.Gtk.Objects.ComboBox.ComboBox'
    -> Int32
    -- ^ /@width@/: Preferred number of columns
    -> m ()
comboBoxSetWrapWidth comboBox width = liftIO $ do
    comboBox' <- unsafeManagedPtrCastPtr comboBox
    gtk_combo_box_set_wrap_width comboBox' width
    touchManagedPtr comboBox
    return ()

#if defined(ENABLE_OVERLOADING)
data ComboBoxSetWrapWidthMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsComboBox a) => O.OverloadedMethod ComboBoxSetWrapWidthMethodInfo a signature where
    overloadedMethod = comboBoxSetWrapWidth

instance O.OverloadedMethodInfo ComboBoxSetWrapWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ComboBox.comboBoxSetWrapWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ComboBox.html#v:comboBoxSetWrapWidth"
        })


#endif


