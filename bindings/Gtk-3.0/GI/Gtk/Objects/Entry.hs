{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.Entry.Entry' widget is a single line text entry
-- widget. A fairly large set of key bindings are supported
-- by default. If the entered text is longer than the allocation
-- of the widget, the widget will scroll so that the cursor
-- position is visible.
-- 
-- When using an entry for passwords and other sensitive information,
-- it can be put into “password mode” using 'GI.Gtk.Objects.Entry.entrySetVisibility'.
-- In this mode, entered text is displayed using a “invisible” character.
-- By default, GTK+ picks the best invisible character that is available
-- in the current font, but it can be changed with
-- 'GI.Gtk.Objects.Entry.entrySetInvisibleChar'. Since 2.16, GTK+ displays a warning
-- when Caps Lock or input methods might interfere with entering text in
-- a password entry. The warning can be turned off with the
-- [Entry:capsLockWarning]("GI.Gtk.Objects.Entry#g:attr:capsLockWarning") property.
-- 
-- Since 2.16, GtkEntry has the ability to display progress or activity
-- information behind the text. To make an entry display such information,
-- use 'GI.Gtk.Objects.Entry.entrySetProgressFraction' or 'GI.Gtk.Objects.Entry.entrySetProgressPulseStep'.
-- 
-- Additionally, GtkEntry can show icons at either side of the entry. These
-- icons can be activatable by clicking, can be set up as drag source and
-- can have tooltips. To add an icon, use 'GI.Gtk.Objects.Entry.entrySetIconFromGicon' or
-- one of the various other functions that set an icon from a stock id, an
-- icon name or a pixbuf. To trigger an action when the user clicks an icon,
-- connect to the [Entry::iconPress]("GI.Gtk.Objects.Entry#g:signal:iconPress") signal. To allow DND operations
-- from an icon, use 'GI.Gtk.Objects.Entry.entrySetIconDragSource'. To set a tooltip on
-- an icon, use 'GI.Gtk.Objects.Entry.entrySetIconTooltipText' or the corresponding function
-- for markup.
-- 
-- Note that functionality or information that is only available by clicking
-- on an icon in an entry may not be accessible at all to users which are not
-- able to use a mouse or other pointing device. It is therefore recommended
-- that any such functionality should also be available by other means, e.g.
-- via the context menu of the entry.
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >entry[.read-only][.flat][.warning][.error]
-- >├── image.left
-- >├── image.right
-- >├── undershoot.left
-- >├── undershoot.right
-- >├── [selection]
-- >├── [progress[.pulse]]
-- >╰── [window.popup]
-- 
-- 
-- GtkEntry has a main node with the name entry. Depending on the properties
-- of the entry, the style classes .read-only and .flat may appear. The style
-- classes .warning and .error may also be used with entries.
-- 
-- When the entry shows icons, it adds subnodes with the name image and the
-- style class .left or .right, depending on where the icon appears.
-- 
-- When the entry has a selection, it adds a subnode with the name selection.
-- 
-- When the entry shows progress, it adds a subnode with the name progress.
-- The node has the style class .pulse when the shown progress is pulsing.
-- 
-- The CSS node for a context menu is added as a subnode below entry as well.
-- 
-- The undershoot nodes are used to draw the underflow indication when content
-- is scrolled out of view. These nodes get the .left and .right style classes
-- added depending on where the indication is drawn.
-- 
-- When touch is used and touch selection handles are shown, they are using
-- CSS nodes with name cursor-handle. They get the .top or .bottom style class
-- depending on where they are shown in relation to the selection. If there is
-- just a single handle for the text cursor, it gets the style class
-- .insertion-cursor.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Entry
    ( 

-- * Exported types
    Entry(..)                               ,
    IsEntry                                 ,
    toEntry                                 ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childNotify]("GI.Gtk.Objects.Widget#g:method:childNotify"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [copyClipboard]("GI.Gtk.Interfaces.Editable#g:method:copyClipboard"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [cutClipboard]("GI.Gtk.Interfaces.Editable#g:method:cutClipboard"), [deleteSelection]("GI.Gtk.Interfaces.Editable#g:method:deleteSelection"), [deleteText]("GI.Gtk.Interfaces.Editable#g:method:deleteText"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [editingDone]("GI.Gtk.Interfaces.CellEditable#g:method:editingDone"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabFocusWithoutSelecting]("GI.Gtk.Objects.Entry#g:method:grabFocusWithoutSelecting"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [imContextFilterKeypress]("GI.Gtk.Objects.Entry#g:method:imContextFilterKeypress"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [insertText]("GI.Gtk.Interfaces.Editable#g:method:insertText"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [layoutIndexToTextIndex]("GI.Gtk.Objects.Entry#g:method:layoutIndexToTextIndex"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [pasteClipboard]("GI.Gtk.Interfaces.Editable#g:method:pasteClipboard"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [progressPulse]("GI.Gtk.Objects.Entry#g:method:progressPulse"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [removeWidget]("GI.Gtk.Interfaces.CellEditable#g:method:removeWidget"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetImContext]("GI.Gtk.Objects.Entry#g:method:resetImContext"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [selectRegion]("GI.Gtk.Interfaces.Editable#g:method:selectRegion"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [startEditing]("GI.Gtk.Interfaces.CellEditable#g:method:startEditing"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [textIndexToLayoutIndex]("GI.Gtk.Objects.Entry#g:method:textIndexToLayoutIndex"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetInvisibleChar]("GI.Gtk.Objects.Entry#g:method:unsetInvisibleChar"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActivatesDefault]("GI.Gtk.Objects.Entry#g:method:getActivatesDefault"), [getAlignment]("GI.Gtk.Objects.Entry#g:method:getAlignment"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getAttributes]("GI.Gtk.Objects.Entry#g:method:getAttributes"), [getBuffer]("GI.Gtk.Objects.Entry#g:method:getBuffer"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChars]("GI.Gtk.Interfaces.Editable#g:method:getChars"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompletion]("GI.Gtk.Objects.Entry#g:method:getCompletion"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getCurrentIconDragSource]("GI.Gtk.Objects.Entry#g:method:getCurrentIconDragSource"), [getCursorHadjustment]("GI.Gtk.Objects.Entry#g:method:getCursorHadjustment"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEditable]("GI.Gtk.Interfaces.Editable#g:method:getEditable"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasFrame]("GI.Gtk.Objects.Entry#g:method:getHasFrame"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getIconActivatable]("GI.Gtk.Objects.Entry#g:method:getIconActivatable"), [getIconArea]("GI.Gtk.Objects.Entry#g:method:getIconArea"), [getIconAtPos]("GI.Gtk.Objects.Entry#g:method:getIconAtPos"), [getIconGicon]("GI.Gtk.Objects.Entry#g:method:getIconGicon"), [getIconName]("GI.Gtk.Objects.Entry#g:method:getIconName"), [getIconPixbuf]("GI.Gtk.Objects.Entry#g:method:getIconPixbuf"), [getIconSensitive]("GI.Gtk.Objects.Entry#g:method:getIconSensitive"), [getIconStock]("GI.Gtk.Objects.Entry#g:method:getIconStock"), [getIconStorageType]("GI.Gtk.Objects.Entry#g:method:getIconStorageType"), [getIconTooltipMarkup]("GI.Gtk.Objects.Entry#g:method:getIconTooltipMarkup"), [getIconTooltipText]("GI.Gtk.Objects.Entry#g:method:getIconTooltipText"), [getInnerBorder]("GI.Gtk.Objects.Entry#g:method:getInnerBorder"), [getInputHints]("GI.Gtk.Objects.Entry#g:method:getInputHints"), [getInputPurpose]("GI.Gtk.Objects.Entry#g:method:getInputPurpose"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getInvisibleChar]("GI.Gtk.Objects.Entry#g:method:getInvisibleChar"), [getLayout]("GI.Gtk.Objects.Entry#g:method:getLayout"), [getLayoutOffsets]("GI.Gtk.Objects.Entry#g:method:getLayoutOffsets"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMaxLength]("GI.Gtk.Objects.Entry#g:method:getMaxLength"), [getMaxWidthChars]("GI.Gtk.Objects.Entry#g:method:getMaxWidthChars"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getOverwriteMode]("GI.Gtk.Objects.Entry#g:method:getOverwriteMode"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPlaceholderText]("GI.Gtk.Objects.Entry#g:method:getPlaceholderText"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPosition]("GI.Gtk.Interfaces.Editable#g:method:getPosition"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProgressFraction]("GI.Gtk.Objects.Entry#g:method:getProgressFraction"), [getProgressPulseStep]("GI.Gtk.Objects.Entry#g:method:getProgressPulseStep"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSelectionBounds]("GI.Gtk.Interfaces.Editable#g:method:getSelectionBounds"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTabs]("GI.Gtk.Objects.Entry#g:method:getTabs"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getText]("GI.Gtk.Objects.Entry#g:method:getText"), [getTextArea]("GI.Gtk.Objects.Entry#g:method:getTextArea"), [getTextLength]("GI.Gtk.Objects.Entry#g:method:getTextLength"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisibility]("GI.Gtk.Objects.Entry#g:method:getVisibility"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWidthChars]("GI.Gtk.Objects.Entry#g:method:getWidthChars"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActivatesDefault]("GI.Gtk.Objects.Entry#g:method:setActivatesDefault"), [setAlignment]("GI.Gtk.Objects.Entry#g:method:setAlignment"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setAttributes]("GI.Gtk.Objects.Entry#g:method:setAttributes"), [setBuffer]("GI.Gtk.Objects.Entry#g:method:setBuffer"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompletion]("GI.Gtk.Objects.Entry#g:method:setCompletion"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setCursorHadjustment]("GI.Gtk.Objects.Entry#g:method:setCursorHadjustment"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEditable]("GI.Gtk.Interfaces.Editable#g:method:setEditable"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasFrame]("GI.Gtk.Objects.Entry#g:method:setHasFrame"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setIconActivatable]("GI.Gtk.Objects.Entry#g:method:setIconActivatable"), [setIconDragSource]("GI.Gtk.Objects.Entry#g:method:setIconDragSource"), [setIconFromGicon]("GI.Gtk.Objects.Entry#g:method:setIconFromGicon"), [setIconFromIconName]("GI.Gtk.Objects.Entry#g:method:setIconFromIconName"), [setIconFromPixbuf]("GI.Gtk.Objects.Entry#g:method:setIconFromPixbuf"), [setIconFromStock]("GI.Gtk.Objects.Entry#g:method:setIconFromStock"), [setIconSensitive]("GI.Gtk.Objects.Entry#g:method:setIconSensitive"), [setIconTooltipMarkup]("GI.Gtk.Objects.Entry#g:method:setIconTooltipMarkup"), [setIconTooltipText]("GI.Gtk.Objects.Entry#g:method:setIconTooltipText"), [setInnerBorder]("GI.Gtk.Objects.Entry#g:method:setInnerBorder"), [setInputHints]("GI.Gtk.Objects.Entry#g:method:setInputHints"), [setInputPurpose]("GI.Gtk.Objects.Entry#g:method:setInputPurpose"), [setInvisibleChar]("GI.Gtk.Objects.Entry#g:method:setInvisibleChar"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMaxLength]("GI.Gtk.Objects.Entry#g:method:setMaxLength"), [setMaxWidthChars]("GI.Gtk.Objects.Entry#g:method:setMaxWidthChars"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setOverwriteMode]("GI.Gtk.Objects.Entry#g:method:setOverwriteMode"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPlaceholderText]("GI.Gtk.Objects.Entry#g:method:setPlaceholderText"), [setPosition]("GI.Gtk.Interfaces.Editable#g:method:setPosition"), [setProgressFraction]("GI.Gtk.Objects.Entry#g:method:setProgressFraction"), [setProgressPulseStep]("GI.Gtk.Objects.Entry#g:method:setProgressPulseStep"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTabs]("GI.Gtk.Objects.Entry#g:method:setTabs"), [setText]("GI.Gtk.Objects.Entry#g:method:setText"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisibility]("GI.Gtk.Objects.Entry#g:method:setVisibility"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWidthChars]("GI.Gtk.Objects.Entry#g:method:setWidthChars"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveEntryMethod                      ,
#endif

-- ** getActivatesDefault #method:getActivatesDefault#

#if defined(ENABLE_OVERLOADING)
    EntryGetActivatesDefaultMethodInfo      ,
#endif
    entryGetActivatesDefault                ,


-- ** getAlignment #method:getAlignment#

#if defined(ENABLE_OVERLOADING)
    EntryGetAlignmentMethodInfo             ,
#endif
    entryGetAlignment                       ,


-- ** getAttributes #method:getAttributes#

#if defined(ENABLE_OVERLOADING)
    EntryGetAttributesMethodInfo            ,
#endif
    entryGetAttributes                      ,


-- ** getBuffer #method:getBuffer#

#if defined(ENABLE_OVERLOADING)
    EntryGetBufferMethodInfo                ,
#endif
    entryGetBuffer                          ,


-- ** getCompletion #method:getCompletion#

#if defined(ENABLE_OVERLOADING)
    EntryGetCompletionMethodInfo            ,
#endif
    entryGetCompletion                      ,


-- ** getCurrentIconDragSource #method:getCurrentIconDragSource#

#if defined(ENABLE_OVERLOADING)
    EntryGetCurrentIconDragSourceMethodInfo ,
#endif
    entryGetCurrentIconDragSource           ,


-- ** getCursorHadjustment #method:getCursorHadjustment#

#if defined(ENABLE_OVERLOADING)
    EntryGetCursorHadjustmentMethodInfo     ,
#endif
    entryGetCursorHadjustment               ,


-- ** getHasFrame #method:getHasFrame#

#if defined(ENABLE_OVERLOADING)
    EntryGetHasFrameMethodInfo              ,
#endif
    entryGetHasFrame                        ,


-- ** getIconActivatable #method:getIconActivatable#

#if defined(ENABLE_OVERLOADING)
    EntryGetIconActivatableMethodInfo       ,
#endif
    entryGetIconActivatable                 ,


-- ** getIconArea #method:getIconArea#

#if defined(ENABLE_OVERLOADING)
    EntryGetIconAreaMethodInfo              ,
#endif
    entryGetIconArea                        ,


-- ** getIconAtPos #method:getIconAtPos#

#if defined(ENABLE_OVERLOADING)
    EntryGetIconAtPosMethodInfo             ,
#endif
    entryGetIconAtPos                       ,


-- ** getIconGicon #method:getIconGicon#

#if defined(ENABLE_OVERLOADING)
    EntryGetIconGiconMethodInfo             ,
#endif
    entryGetIconGicon                       ,


-- ** getIconName #method:getIconName#

#if defined(ENABLE_OVERLOADING)
    EntryGetIconNameMethodInfo              ,
#endif
    entryGetIconName                        ,


-- ** getIconPixbuf #method:getIconPixbuf#

#if defined(ENABLE_OVERLOADING)
    EntryGetIconPixbufMethodInfo            ,
#endif
    entryGetIconPixbuf                      ,


-- ** getIconSensitive #method:getIconSensitive#

#if defined(ENABLE_OVERLOADING)
    EntryGetIconSensitiveMethodInfo         ,
#endif
    entryGetIconSensitive                   ,


-- ** getIconStock #method:getIconStock#

#if defined(ENABLE_OVERLOADING)
    EntryGetIconStockMethodInfo             ,
#endif
    entryGetIconStock                       ,


-- ** getIconStorageType #method:getIconStorageType#

#if defined(ENABLE_OVERLOADING)
    EntryGetIconStorageTypeMethodInfo       ,
#endif
    entryGetIconStorageType                 ,


-- ** getIconTooltipMarkup #method:getIconTooltipMarkup#

#if defined(ENABLE_OVERLOADING)
    EntryGetIconTooltipMarkupMethodInfo     ,
#endif
    entryGetIconTooltipMarkup               ,


-- ** getIconTooltipText #method:getIconTooltipText#

#if defined(ENABLE_OVERLOADING)
    EntryGetIconTooltipTextMethodInfo       ,
#endif
    entryGetIconTooltipText                 ,


-- ** getInnerBorder #method:getInnerBorder#

#if defined(ENABLE_OVERLOADING)
    EntryGetInnerBorderMethodInfo           ,
#endif
    entryGetInnerBorder                     ,


-- ** getInputHints #method:getInputHints#

#if defined(ENABLE_OVERLOADING)
    EntryGetInputHintsMethodInfo            ,
#endif
    entryGetInputHints                      ,


-- ** getInputPurpose #method:getInputPurpose#

#if defined(ENABLE_OVERLOADING)
    EntryGetInputPurposeMethodInfo          ,
#endif
    entryGetInputPurpose                    ,


-- ** getInvisibleChar #method:getInvisibleChar#

#if defined(ENABLE_OVERLOADING)
    EntryGetInvisibleCharMethodInfo         ,
#endif
    entryGetInvisibleChar                   ,


-- ** getLayout #method:getLayout#

#if defined(ENABLE_OVERLOADING)
    EntryGetLayoutMethodInfo                ,
#endif
    entryGetLayout                          ,


-- ** getLayoutOffsets #method:getLayoutOffsets#

#if defined(ENABLE_OVERLOADING)
    EntryGetLayoutOffsetsMethodInfo         ,
#endif
    entryGetLayoutOffsets                   ,


-- ** getMaxLength #method:getMaxLength#

#if defined(ENABLE_OVERLOADING)
    EntryGetMaxLengthMethodInfo             ,
#endif
    entryGetMaxLength                       ,


-- ** getMaxWidthChars #method:getMaxWidthChars#

#if defined(ENABLE_OVERLOADING)
    EntryGetMaxWidthCharsMethodInfo         ,
#endif
    entryGetMaxWidthChars                   ,


-- ** getOverwriteMode #method:getOverwriteMode#

#if defined(ENABLE_OVERLOADING)
    EntryGetOverwriteModeMethodInfo         ,
#endif
    entryGetOverwriteMode                   ,


-- ** getPlaceholderText #method:getPlaceholderText#

#if defined(ENABLE_OVERLOADING)
    EntryGetPlaceholderTextMethodInfo       ,
#endif
    entryGetPlaceholderText                 ,


-- ** getProgressFraction #method:getProgressFraction#

#if defined(ENABLE_OVERLOADING)
    EntryGetProgressFractionMethodInfo      ,
#endif
    entryGetProgressFraction                ,


-- ** getProgressPulseStep #method:getProgressPulseStep#

#if defined(ENABLE_OVERLOADING)
    EntryGetProgressPulseStepMethodInfo     ,
#endif
    entryGetProgressPulseStep               ,


-- ** getTabs #method:getTabs#

#if defined(ENABLE_OVERLOADING)
    EntryGetTabsMethodInfo                  ,
#endif
    entryGetTabs                            ,


-- ** getText #method:getText#

#if defined(ENABLE_OVERLOADING)
    EntryGetTextMethodInfo                  ,
#endif
    entryGetText                            ,


-- ** getTextArea #method:getTextArea#

#if defined(ENABLE_OVERLOADING)
    EntryGetTextAreaMethodInfo              ,
#endif
    entryGetTextArea                        ,


-- ** getTextLength #method:getTextLength#

#if defined(ENABLE_OVERLOADING)
    EntryGetTextLengthMethodInfo            ,
#endif
    entryGetTextLength                      ,


-- ** getVisibility #method:getVisibility#

#if defined(ENABLE_OVERLOADING)
    EntryGetVisibilityMethodInfo            ,
#endif
    entryGetVisibility                      ,


-- ** getWidthChars #method:getWidthChars#

#if defined(ENABLE_OVERLOADING)
    EntryGetWidthCharsMethodInfo            ,
#endif
    entryGetWidthChars                      ,


-- ** grabFocusWithoutSelecting #method:grabFocusWithoutSelecting#

#if defined(ENABLE_OVERLOADING)
    EntryGrabFocusWithoutSelectingMethodInfo,
#endif
    entryGrabFocusWithoutSelecting          ,


-- ** imContextFilterKeypress #method:imContextFilterKeypress#

#if defined(ENABLE_OVERLOADING)
    EntryImContextFilterKeypressMethodInfo  ,
#endif
    entryImContextFilterKeypress            ,


-- ** layoutIndexToTextIndex #method:layoutIndexToTextIndex#

#if defined(ENABLE_OVERLOADING)
    EntryLayoutIndexToTextIndexMethodInfo   ,
#endif
    entryLayoutIndexToTextIndex             ,


-- ** new #method:new#

    entryNew                                ,


-- ** newWithBuffer #method:newWithBuffer#

    entryNewWithBuffer                      ,


-- ** progressPulse #method:progressPulse#

#if defined(ENABLE_OVERLOADING)
    EntryProgressPulseMethodInfo            ,
#endif
    entryProgressPulse                      ,


-- ** resetImContext #method:resetImContext#

#if defined(ENABLE_OVERLOADING)
    EntryResetImContextMethodInfo           ,
#endif
    entryResetImContext                     ,


-- ** setActivatesDefault #method:setActivatesDefault#

#if defined(ENABLE_OVERLOADING)
    EntrySetActivatesDefaultMethodInfo      ,
#endif
    entrySetActivatesDefault                ,


-- ** setAlignment #method:setAlignment#

#if defined(ENABLE_OVERLOADING)
    EntrySetAlignmentMethodInfo             ,
#endif
    entrySetAlignment                       ,


-- ** setAttributes #method:setAttributes#

#if defined(ENABLE_OVERLOADING)
    EntrySetAttributesMethodInfo            ,
#endif
    entrySetAttributes                      ,


-- ** setBuffer #method:setBuffer#

#if defined(ENABLE_OVERLOADING)
    EntrySetBufferMethodInfo                ,
#endif
    entrySetBuffer                          ,


-- ** setCompletion #method:setCompletion#

#if defined(ENABLE_OVERLOADING)
    EntrySetCompletionMethodInfo            ,
#endif
    entrySetCompletion                      ,


-- ** setCursorHadjustment #method:setCursorHadjustment#

#if defined(ENABLE_OVERLOADING)
    EntrySetCursorHadjustmentMethodInfo     ,
#endif
    entrySetCursorHadjustment               ,


-- ** setHasFrame #method:setHasFrame#

#if defined(ENABLE_OVERLOADING)
    EntrySetHasFrameMethodInfo              ,
#endif
    entrySetHasFrame                        ,


-- ** setIconActivatable #method:setIconActivatable#

#if defined(ENABLE_OVERLOADING)
    EntrySetIconActivatableMethodInfo       ,
#endif
    entrySetIconActivatable                 ,


-- ** setIconDragSource #method:setIconDragSource#

#if defined(ENABLE_OVERLOADING)
    EntrySetIconDragSourceMethodInfo        ,
#endif
    entrySetIconDragSource                  ,


-- ** setIconFromGicon #method:setIconFromGicon#

#if defined(ENABLE_OVERLOADING)
    EntrySetIconFromGiconMethodInfo         ,
#endif
    entrySetIconFromGicon                   ,


-- ** setIconFromIconName #method:setIconFromIconName#

#if defined(ENABLE_OVERLOADING)
    EntrySetIconFromIconNameMethodInfo      ,
#endif
    entrySetIconFromIconName                ,


-- ** setIconFromPixbuf #method:setIconFromPixbuf#

#if defined(ENABLE_OVERLOADING)
    EntrySetIconFromPixbufMethodInfo        ,
#endif
    entrySetIconFromPixbuf                  ,


-- ** setIconFromStock #method:setIconFromStock#

#if defined(ENABLE_OVERLOADING)
    EntrySetIconFromStockMethodInfo         ,
#endif
    entrySetIconFromStock                   ,


-- ** setIconSensitive #method:setIconSensitive#

#if defined(ENABLE_OVERLOADING)
    EntrySetIconSensitiveMethodInfo         ,
#endif
    entrySetIconSensitive                   ,


-- ** setIconTooltipMarkup #method:setIconTooltipMarkup#

#if defined(ENABLE_OVERLOADING)
    EntrySetIconTooltipMarkupMethodInfo     ,
#endif
    entrySetIconTooltipMarkup               ,


-- ** setIconTooltipText #method:setIconTooltipText#

#if defined(ENABLE_OVERLOADING)
    EntrySetIconTooltipTextMethodInfo       ,
#endif
    entrySetIconTooltipText                 ,


-- ** setInnerBorder #method:setInnerBorder#

#if defined(ENABLE_OVERLOADING)
    EntrySetInnerBorderMethodInfo           ,
#endif
    entrySetInnerBorder                     ,


-- ** setInputHints #method:setInputHints#

#if defined(ENABLE_OVERLOADING)
    EntrySetInputHintsMethodInfo            ,
#endif
    entrySetInputHints                      ,


-- ** setInputPurpose #method:setInputPurpose#

#if defined(ENABLE_OVERLOADING)
    EntrySetInputPurposeMethodInfo          ,
#endif
    entrySetInputPurpose                    ,


-- ** setInvisibleChar #method:setInvisibleChar#

#if defined(ENABLE_OVERLOADING)
    EntrySetInvisibleCharMethodInfo         ,
#endif
    entrySetInvisibleChar                   ,


-- ** setMaxLength #method:setMaxLength#

#if defined(ENABLE_OVERLOADING)
    EntrySetMaxLengthMethodInfo             ,
#endif
    entrySetMaxLength                       ,


-- ** setMaxWidthChars #method:setMaxWidthChars#

#if defined(ENABLE_OVERLOADING)
    EntrySetMaxWidthCharsMethodInfo         ,
#endif
    entrySetMaxWidthChars                   ,


-- ** setOverwriteMode #method:setOverwriteMode#

#if defined(ENABLE_OVERLOADING)
    EntrySetOverwriteModeMethodInfo         ,
#endif
    entrySetOverwriteMode                   ,


-- ** setPlaceholderText #method:setPlaceholderText#

#if defined(ENABLE_OVERLOADING)
    EntrySetPlaceholderTextMethodInfo       ,
#endif
    entrySetPlaceholderText                 ,


-- ** setProgressFraction #method:setProgressFraction#

#if defined(ENABLE_OVERLOADING)
    EntrySetProgressFractionMethodInfo      ,
#endif
    entrySetProgressFraction                ,


-- ** setProgressPulseStep #method:setProgressPulseStep#

#if defined(ENABLE_OVERLOADING)
    EntrySetProgressPulseStepMethodInfo     ,
#endif
    entrySetProgressPulseStep               ,


-- ** setTabs #method:setTabs#

#if defined(ENABLE_OVERLOADING)
    EntrySetTabsMethodInfo                  ,
#endif
    entrySetTabs                            ,


-- ** setText #method:setText#

#if defined(ENABLE_OVERLOADING)
    EntrySetTextMethodInfo                  ,
#endif
    entrySetText                            ,


-- ** setVisibility #method:setVisibility#

#if defined(ENABLE_OVERLOADING)
    EntrySetVisibilityMethodInfo            ,
#endif
    entrySetVisibility                      ,


-- ** setWidthChars #method:setWidthChars#

#if defined(ENABLE_OVERLOADING)
    EntrySetWidthCharsMethodInfo            ,
#endif
    entrySetWidthChars                      ,


-- ** textIndexToLayoutIndex #method:textIndexToLayoutIndex#

#if defined(ENABLE_OVERLOADING)
    EntryTextIndexToLayoutIndexMethodInfo   ,
#endif
    entryTextIndexToLayoutIndex             ,


-- ** unsetInvisibleChar #method:unsetInvisibleChar#

#if defined(ENABLE_OVERLOADING)
    EntryUnsetInvisibleCharMethodInfo       ,
#endif
    entryUnsetInvisibleChar                 ,




 -- * Properties


-- ** activatesDefault #attr:activatesDefault#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    EntryActivatesDefaultPropertyInfo       ,
#endif
    constructEntryActivatesDefault          ,
#if defined(ENABLE_OVERLOADING)
    entryActivatesDefault                   ,
#endif
    getEntryActivatesDefault                ,
    setEntryActivatesDefault                ,


-- ** attributes #attr:attributes#
-- | A list of Pango attributes to apply to the text of the entry.
-- 
-- This is mainly useful to change the size or weight of the text.
-- 
-- The t'GI.Pango.Structs.Attribute.Attribute'\'s /@startIndex@/ and /@endIndex@/ must refer to the
-- t'GI.Gtk.Objects.EntryBuffer.EntryBuffer' text, i.e. without the preedit string.
-- 
-- /Since: 3.6/

#if defined(ENABLE_OVERLOADING)
    EntryAttributesPropertyInfo             ,
#endif
    constructEntryAttributes                ,
#if defined(ENABLE_OVERLOADING)
    entryAttributes                         ,
#endif
    getEntryAttributes                      ,
    setEntryAttributes                      ,


-- ** buffer #attr:buffer#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    EntryBufferPropertyInfo                 ,
#endif
    constructEntryBuffer                    ,
#if defined(ENABLE_OVERLOADING)
    entryBuffer                             ,
#endif
    getEntryBuffer                          ,
    setEntryBuffer                          ,


-- ** capsLockWarning #attr:capsLockWarning#
-- | Whether password entries will show a warning when Caps Lock is on.
-- 
-- Note that the warning is shown using a secondary icon, and thus
-- does not work if you are using the secondary icon position for some
-- other purpose.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntryCapsLockWarningPropertyInfo        ,
#endif
    constructEntryCapsLockWarning           ,
#if defined(ENABLE_OVERLOADING)
    entryCapsLockWarning                    ,
#endif
    getEntryCapsLockWarning                 ,
    setEntryCapsLockWarning                 ,


-- ** completion #attr:completion#
-- | The auxiliary completion object to use with the entry.
-- 
-- /Since: 3.2/

#if defined(ENABLE_OVERLOADING)
    EntryCompletionPropertyInfo             ,
#endif
    clearEntryCompletion                    ,
    constructEntryCompletion                ,
#if defined(ENABLE_OVERLOADING)
    entryCompletion                         ,
#endif
    getEntryCompletion                      ,
    setEntryCompletion                      ,


-- ** cursorPosition #attr:cursorPosition#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    EntryCursorPositionPropertyInfo         ,
#endif
#if defined(ENABLE_OVERLOADING)
    entryCursorPosition                     ,
#endif
    getEntryCursorPosition                  ,


-- ** editable #attr:editable#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    EntryEditablePropertyInfo               ,
#endif
    constructEntryEditable                  ,
#if defined(ENABLE_OVERLOADING)
    entryEditable                           ,
#endif
    getEntryEditable                        ,
    setEntryEditable                        ,


-- ** enableEmojiCompletion #attr:enableEmojiCompletion#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    EntryEnableEmojiCompletionPropertyInfo  ,
#endif
    constructEntryEnableEmojiCompletion     ,
#if defined(ENABLE_OVERLOADING)
    entryEnableEmojiCompletion              ,
#endif
    getEntryEnableEmojiCompletion           ,
    setEntryEnableEmojiCompletion           ,


-- ** hasFrame #attr:hasFrame#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    EntryHasFramePropertyInfo               ,
#endif
    constructEntryHasFrame                  ,
#if defined(ENABLE_OVERLOADING)
    entryHasFrame                           ,
#endif
    getEntryHasFrame                        ,
    setEntryHasFrame                        ,


-- ** imModule #attr:imModule#
-- | Which IM (input method) module should be used for this entry.
-- See t'GI.Gtk.Objects.IMContext.IMContext'.
-- 
-- Setting this to a non-'P.Nothing' value overrides the
-- system-wide IM module setting. See the GtkSettings
-- [Settings:gtkImModule]("GI.Gtk.Objects.Settings#g:attr:gtkImModule") property.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntryImModulePropertyInfo               ,
#endif
    clearEntryImModule                      ,
    constructEntryImModule                  ,
#if defined(ENABLE_OVERLOADING)
    entryImModule                           ,
#endif
    getEntryImModule                        ,
    setEntryImModule                        ,


-- ** innerBorder #attr:innerBorder#
-- | Sets the text area\'s border between the text and the frame.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    EntryInnerBorderPropertyInfo            ,
#endif
    clearEntryInnerBorder                   ,
    constructEntryInnerBorder               ,
#if defined(ENABLE_OVERLOADING)
    entryInnerBorder                        ,
#endif
    getEntryInnerBorder                     ,
    setEntryInnerBorder                     ,


-- ** inputHints #attr:inputHints#
-- | Additional hints (beyond [Entry:inputPurpose]("GI.Gtk.Objects.Entry#g:attr:inputPurpose")) that
-- allow input methods to fine-tune their behaviour.
-- 
-- /Since: 3.6/

#if defined(ENABLE_OVERLOADING)
    EntryInputHintsPropertyInfo             ,
#endif
    constructEntryInputHints                ,
#if defined(ENABLE_OVERLOADING)
    entryInputHints                         ,
#endif
    getEntryInputHints                      ,
    setEntryInputHints                      ,


-- ** inputPurpose #attr:inputPurpose#
-- | The purpose of this text field.
-- 
-- This property can be used by on-screen keyboards and other input
-- methods to adjust their behaviour.
-- 
-- Note that setting the purpose to 'GI.Gtk.Enums.InputPurposePassword' or
-- 'GI.Gtk.Enums.InputPurposePin' is independent from setting
-- [Entry:visibility]("GI.Gtk.Objects.Entry#g:attr:visibility").
-- 
-- /Since: 3.6/

#if defined(ENABLE_OVERLOADING)
    EntryInputPurposePropertyInfo           ,
#endif
    constructEntryInputPurpose              ,
#if defined(ENABLE_OVERLOADING)
    entryInputPurpose                       ,
#endif
    getEntryInputPurpose                    ,
    setEntryInputPurpose                    ,


-- ** invisibleChar #attr:invisibleChar#
-- | The invisible character is used when masking entry contents (in
-- \\\"password mode\\\")\"). When it is not explicitly set with the
-- [Entry:invisibleChar]("GI.Gtk.Objects.Entry#g:attr:invisibleChar") property, GTK+ determines the character
-- to use from a list of possible candidates, depending on availability
-- in the current font.
-- 
-- This style property allows the theme to prepend a character
-- to the list of candidates.
-- 
-- /Since: 2.18/

#if defined(ENABLE_OVERLOADING)
    EntryInvisibleCharPropertyInfo          ,
#endif
    constructEntryInvisibleChar             ,
#if defined(ENABLE_OVERLOADING)
    entryInvisibleChar                      ,
#endif
    getEntryInvisibleChar                   ,
    setEntryInvisibleChar                   ,


-- ** invisibleCharSet #attr:invisibleCharSet#
-- | Whether the invisible char has been set for the t'GI.Gtk.Objects.Entry.Entry'.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntryInvisibleCharSetPropertyInfo       ,
#endif
    constructEntryInvisibleCharSet          ,
#if defined(ENABLE_OVERLOADING)
    entryInvisibleCharSet                   ,
#endif
    getEntryInvisibleCharSet                ,
    setEntryInvisibleCharSet                ,


-- ** maxLength #attr:maxLength#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    EntryMaxLengthPropertyInfo              ,
#endif
    constructEntryMaxLength                 ,
#if defined(ENABLE_OVERLOADING)
    entryMaxLength                          ,
#endif
    getEntryMaxLength                       ,
    setEntryMaxLength                       ,


-- ** maxWidthChars #attr:maxWidthChars#
-- | The desired maximum width of the entry, in characters.
-- If this property is set to -1, the width will be calculated
-- automatically.
-- 
-- /Since: 3.12/

#if defined(ENABLE_OVERLOADING)
    EntryMaxWidthCharsPropertyInfo          ,
#endif
    constructEntryMaxWidthChars             ,
#if defined(ENABLE_OVERLOADING)
    entryMaxWidthChars                      ,
#endif
    getEntryMaxWidthChars                   ,
    setEntryMaxWidthChars                   ,


-- ** overwriteMode #attr:overwriteMode#
-- | If text is overwritten when typing in the t'GI.Gtk.Objects.Entry.Entry'.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    EntryOverwriteModePropertyInfo          ,
#endif
    constructEntryOverwriteMode             ,
#if defined(ENABLE_OVERLOADING)
    entryOverwriteMode                      ,
#endif
    getEntryOverwriteMode                   ,
    setEntryOverwriteMode                   ,


-- ** placeholderText #attr:placeholderText#
-- | The text that will be displayed in the t'GI.Gtk.Objects.Entry.Entry' when it is empty
-- and unfocused.
-- 
-- /Since: 3.2/

#if defined(ENABLE_OVERLOADING)
    EntryPlaceholderTextPropertyInfo        ,
#endif
    clearEntryPlaceholderText               ,
    constructEntryPlaceholderText           ,
#if defined(ENABLE_OVERLOADING)
    entryPlaceholderText                    ,
#endif
    getEntryPlaceholderText                 ,
    setEntryPlaceholderText                 ,


-- ** populateAll #attr:populateAll#
-- | If :populate-all is 'P.True', the [Entry::populatePopup]("GI.Gtk.Objects.Entry#g:signal:populatePopup")
-- signal is also emitted for touch popups.
-- 
-- /Since: 3.8/

#if defined(ENABLE_OVERLOADING)
    EntryPopulateAllPropertyInfo            ,
#endif
    constructEntryPopulateAll               ,
#if defined(ENABLE_OVERLOADING)
    entryPopulateAll                        ,
#endif
    getEntryPopulateAll                     ,
    setEntryPopulateAll                     ,


-- ** primaryIconActivatable #attr:primaryIconActivatable#
-- | Whether the primary icon is activatable.
-- 
-- GTK+ emits the [Entry::iconPress]("GI.Gtk.Objects.Entry#g:signal:iconPress") and [Entry::iconRelease]("GI.Gtk.Objects.Entry#g:signal:iconRelease")
-- signals only on sensitive, activatable icons.
-- 
-- Sensitive, but non-activatable icons can be used for purely
-- informational purposes.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntryPrimaryIconActivatablePropertyInfo ,
#endif
    constructEntryPrimaryIconActivatable    ,
#if defined(ENABLE_OVERLOADING)
    entryPrimaryIconActivatable             ,
#endif
    getEntryPrimaryIconActivatable          ,
    setEntryPrimaryIconActivatable          ,


-- ** primaryIconGicon #attr:primaryIconGicon#
-- | The t'GI.Gio.Interfaces.Icon.Icon' to use for the primary icon for the entry.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntryPrimaryIconGiconPropertyInfo       ,
#endif
    clearEntryPrimaryIconGicon              ,
    constructEntryPrimaryIconGicon          ,
#if defined(ENABLE_OVERLOADING)
    entryPrimaryIconGicon                   ,
#endif
    getEntryPrimaryIconGicon                ,
    setEntryPrimaryIconGicon                ,


-- ** primaryIconName #attr:primaryIconName#
-- | The icon name to use for the primary icon for the entry.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntryPrimaryIconNamePropertyInfo        ,
#endif
    clearEntryPrimaryIconName               ,
    constructEntryPrimaryIconName           ,
#if defined(ENABLE_OVERLOADING)
    entryPrimaryIconName                    ,
#endif
    getEntryPrimaryIconName                 ,
    setEntryPrimaryIconName                 ,


-- ** primaryIconPixbuf #attr:primaryIconPixbuf#
-- | A pixbuf to use as the primary icon for the entry.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntryPrimaryIconPixbufPropertyInfo      ,
#endif
    clearEntryPrimaryIconPixbuf             ,
    constructEntryPrimaryIconPixbuf         ,
#if defined(ENABLE_OVERLOADING)
    entryPrimaryIconPixbuf                  ,
#endif
    getEntryPrimaryIconPixbuf               ,
    setEntryPrimaryIconPixbuf               ,


-- ** primaryIconSensitive #attr:primaryIconSensitive#
-- | Whether the primary icon is sensitive.
-- 
-- An insensitive icon appears grayed out. GTK+ does not emit the
-- [Entry::iconPress]("GI.Gtk.Objects.Entry#g:signal:iconPress") and [Entry::iconRelease]("GI.Gtk.Objects.Entry#g:signal:iconRelease") signals and
-- does not allow DND from insensitive icons.
-- 
-- An icon should be set insensitive if the action that would trigger
-- when clicked is currently not available.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntryPrimaryIconSensitivePropertyInfo   ,
#endif
    constructEntryPrimaryIconSensitive      ,
#if defined(ENABLE_OVERLOADING)
    entryPrimaryIconSensitive               ,
#endif
    getEntryPrimaryIconSensitive            ,
    setEntryPrimaryIconSensitive            ,


-- ** primaryIconStock #attr:primaryIconStock#
-- | The stock id to use for the primary icon for the entry.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntryPrimaryIconStockPropertyInfo       ,
#endif
    clearEntryPrimaryIconStock              ,
    constructEntryPrimaryIconStock          ,
#if defined(ENABLE_OVERLOADING)
    entryPrimaryIconStock                   ,
#endif
    getEntryPrimaryIconStock                ,
    setEntryPrimaryIconStock                ,


-- ** primaryIconStorageType #attr:primaryIconStorageType#
-- | The representation which is used for the primary icon of the entry.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntryPrimaryIconStorageTypePropertyInfo ,
#endif
#if defined(ENABLE_OVERLOADING)
    entryPrimaryIconStorageType             ,
#endif
    getEntryPrimaryIconStorageType          ,


-- ** primaryIconTooltipMarkup #attr:primaryIconTooltipMarkup#
-- | The contents of the tooltip on the primary icon, which is marked up
-- with the [Pango text markup language][PangoMarkupFormat].
-- 
-- Also see 'GI.Gtk.Objects.Entry.entrySetIconTooltipMarkup'.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntryPrimaryIconTooltipMarkupPropertyInfo,
#endif
    clearEntryPrimaryIconTooltipMarkup      ,
    constructEntryPrimaryIconTooltipMarkup  ,
#if defined(ENABLE_OVERLOADING)
    entryPrimaryIconTooltipMarkup           ,
#endif
    getEntryPrimaryIconTooltipMarkup        ,
    setEntryPrimaryIconTooltipMarkup        ,


-- ** primaryIconTooltipText #attr:primaryIconTooltipText#
-- | The contents of the tooltip on the primary icon.
-- 
-- Also see 'GI.Gtk.Objects.Entry.entrySetIconTooltipText'.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntryPrimaryIconTooltipTextPropertyInfo ,
#endif
    clearEntryPrimaryIconTooltipText        ,
    constructEntryPrimaryIconTooltipText    ,
#if defined(ENABLE_OVERLOADING)
    entryPrimaryIconTooltipText             ,
#endif
    getEntryPrimaryIconTooltipText          ,
    setEntryPrimaryIconTooltipText          ,


-- ** progressFraction #attr:progressFraction#
-- | The current fraction of the task that\'s been completed.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntryProgressFractionPropertyInfo       ,
#endif
    constructEntryProgressFraction          ,
#if defined(ENABLE_OVERLOADING)
    entryProgressFraction                   ,
#endif
    getEntryProgressFraction                ,
    setEntryProgressFraction                ,


-- ** progressPulseStep #attr:progressPulseStep#
-- | The fraction of total entry width to move the progress
-- bouncing block for each call to 'GI.Gtk.Objects.Entry.entryProgressPulse'.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntryProgressPulseStepPropertyInfo      ,
#endif
    constructEntryProgressPulseStep         ,
#if defined(ENABLE_OVERLOADING)
    entryProgressPulseStep                  ,
#endif
    getEntryProgressPulseStep               ,
    setEntryProgressPulseStep               ,


-- ** scrollOffset #attr:scrollOffset#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    EntryScrollOffsetPropertyInfo           ,
#endif
#if defined(ENABLE_OVERLOADING)
    entryScrollOffset                       ,
#endif
    getEntryScrollOffset                    ,


-- ** secondaryIconActivatable #attr:secondaryIconActivatable#
-- | Whether the secondary icon is activatable.
-- 
-- GTK+ emits the [Entry::iconPress]("GI.Gtk.Objects.Entry#g:signal:iconPress") and [Entry::iconRelease]("GI.Gtk.Objects.Entry#g:signal:iconRelease")
-- signals only on sensitive, activatable icons.
-- 
-- Sensitive, but non-activatable icons can be used for purely
-- informational purposes.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntrySecondaryIconActivatablePropertyInfo,
#endif
    constructEntrySecondaryIconActivatable  ,
#if defined(ENABLE_OVERLOADING)
    entrySecondaryIconActivatable           ,
#endif
    getEntrySecondaryIconActivatable        ,
    setEntrySecondaryIconActivatable        ,


-- ** secondaryIconGicon #attr:secondaryIconGicon#
-- | The t'GI.Gio.Interfaces.Icon.Icon' to use for the secondary icon for the entry.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntrySecondaryIconGiconPropertyInfo     ,
#endif
    clearEntrySecondaryIconGicon            ,
    constructEntrySecondaryIconGicon        ,
#if defined(ENABLE_OVERLOADING)
    entrySecondaryIconGicon                 ,
#endif
    getEntrySecondaryIconGicon              ,
    setEntrySecondaryIconGicon              ,


-- ** secondaryIconName #attr:secondaryIconName#
-- | The icon name to use for the secondary icon for the entry.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntrySecondaryIconNamePropertyInfo      ,
#endif
    clearEntrySecondaryIconName             ,
    constructEntrySecondaryIconName         ,
#if defined(ENABLE_OVERLOADING)
    entrySecondaryIconName                  ,
#endif
    getEntrySecondaryIconName               ,
    setEntrySecondaryIconName               ,


-- ** secondaryIconPixbuf #attr:secondaryIconPixbuf#
-- | An pixbuf to use as the secondary icon for the entry.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntrySecondaryIconPixbufPropertyInfo    ,
#endif
    clearEntrySecondaryIconPixbuf           ,
    constructEntrySecondaryIconPixbuf       ,
#if defined(ENABLE_OVERLOADING)
    entrySecondaryIconPixbuf                ,
#endif
    getEntrySecondaryIconPixbuf             ,
    setEntrySecondaryIconPixbuf             ,


-- ** secondaryIconSensitive #attr:secondaryIconSensitive#
-- | Whether the secondary icon is sensitive.
-- 
-- An insensitive icon appears grayed out. GTK+ does not emit the
-- [Entry::iconPress]("GI.Gtk.Objects.Entry#g:signal:iconPress") and [Entry::iconRelease]("GI.Gtk.Objects.Entry#g:signal:iconRelease") signals and
-- does not allow DND from insensitive icons.
-- 
-- An icon should be set insensitive if the action that would trigger
-- when clicked is currently not available.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntrySecondaryIconSensitivePropertyInfo ,
#endif
    constructEntrySecondaryIconSensitive    ,
#if defined(ENABLE_OVERLOADING)
    entrySecondaryIconSensitive             ,
#endif
    getEntrySecondaryIconSensitive          ,
    setEntrySecondaryIconSensitive          ,


-- ** secondaryIconStock #attr:secondaryIconStock#
-- | The stock id to use for the secondary icon for the entry.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntrySecondaryIconStockPropertyInfo     ,
#endif
    clearEntrySecondaryIconStock            ,
    constructEntrySecondaryIconStock        ,
#if defined(ENABLE_OVERLOADING)
    entrySecondaryIconStock                 ,
#endif
    getEntrySecondaryIconStock              ,
    setEntrySecondaryIconStock              ,


-- ** secondaryIconStorageType #attr:secondaryIconStorageType#
-- | The representation which is used for the secondary icon of the entry.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntrySecondaryIconStorageTypePropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    entrySecondaryIconStorageType           ,
#endif
    getEntrySecondaryIconStorageType        ,


-- ** secondaryIconTooltipMarkup #attr:secondaryIconTooltipMarkup#
-- | The contents of the tooltip on the secondary icon, which is marked up
-- with the [Pango text markup language][PangoMarkupFormat].
-- 
-- Also see 'GI.Gtk.Objects.Entry.entrySetIconTooltipMarkup'.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntrySecondaryIconTooltipMarkupPropertyInfo,
#endif
    clearEntrySecondaryIconTooltipMarkup    ,
    constructEntrySecondaryIconTooltipMarkup,
#if defined(ENABLE_OVERLOADING)
    entrySecondaryIconTooltipMarkup         ,
#endif
    getEntrySecondaryIconTooltipMarkup      ,
    setEntrySecondaryIconTooltipMarkup      ,


-- ** secondaryIconTooltipText #attr:secondaryIconTooltipText#
-- | The contents of the tooltip on the secondary icon.
-- 
-- Also see 'GI.Gtk.Objects.Entry.entrySetIconTooltipText'.
-- 
-- /Since: 2.16/

#if defined(ENABLE_OVERLOADING)
    EntrySecondaryIconTooltipTextPropertyInfo,
#endif
    clearEntrySecondaryIconTooltipText      ,
    constructEntrySecondaryIconTooltipText  ,
#if defined(ENABLE_OVERLOADING)
    entrySecondaryIconTooltipText           ,
#endif
    getEntrySecondaryIconTooltipText        ,
    setEntrySecondaryIconTooltipText        ,


-- ** selectionBound #attr:selectionBound#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    EntrySelectionBoundPropertyInfo         ,
#endif
#if defined(ENABLE_OVERLOADING)
    entrySelectionBound                     ,
#endif
    getEntrySelectionBound                  ,


-- ** shadowType #attr:shadowType#
-- | Which kind of shadow to draw around the entry when
-- [Entry:hasFrame]("GI.Gtk.Objects.Entry#g:attr:hasFrame") is set to 'P.True'.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    EntryShadowTypePropertyInfo             ,
#endif
    constructEntryShadowType                ,
#if defined(ENABLE_OVERLOADING)
    entryShadowType                         ,
#endif
    getEntryShadowType                      ,
    setEntryShadowType                      ,


-- ** showEmojiIcon #attr:showEmojiIcon#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    EntryShowEmojiIconPropertyInfo          ,
#endif
    constructEntryShowEmojiIcon             ,
#if defined(ENABLE_OVERLOADING)
    entryShowEmojiIcon                      ,
#endif
    getEntryShowEmojiIcon                   ,
    setEntryShowEmojiIcon                   ,


-- ** tabs #attr:tabs#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    EntryTabsPropertyInfo                   ,
#endif
    constructEntryTabs                      ,
#if defined(ENABLE_OVERLOADING)
    entryTabs                               ,
#endif
    getEntryTabs                            ,
    setEntryTabs                            ,


-- ** text #attr:text#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    EntryTextPropertyInfo                   ,
#endif
    constructEntryText                      ,
#if defined(ENABLE_OVERLOADING)
    entryText                               ,
#endif
    getEntryText                            ,
    setEntryText                            ,


-- ** textLength #attr:textLength#
-- | The length of the text in the t'GI.Gtk.Objects.Entry.Entry'.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    EntryTextLengthPropertyInfo             ,
#endif
#if defined(ENABLE_OVERLOADING)
    entryTextLength                         ,
#endif
    getEntryTextLength                      ,


-- ** truncateMultiline #attr:truncateMultiline#
-- | When 'P.True', pasted multi-line text is truncated to the first line.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    EntryTruncateMultilinePropertyInfo      ,
#endif
    constructEntryTruncateMultiline         ,
#if defined(ENABLE_OVERLOADING)
    entryTruncateMultiline                  ,
#endif
    getEntryTruncateMultiline               ,
    setEntryTruncateMultiline               ,


-- ** visibility #attr:visibility#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    EntryVisibilityPropertyInfo             ,
#endif
    constructEntryVisibility                ,
#if defined(ENABLE_OVERLOADING)
    entryVisibility                         ,
#endif
    getEntryVisibility                      ,
    setEntryVisibility                      ,


-- ** widthChars #attr:widthChars#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    EntryWidthCharsPropertyInfo             ,
#endif
    constructEntryWidthChars                ,
#if defined(ENABLE_OVERLOADING)
    entryWidthChars                         ,
#endif
    getEntryWidthChars                      ,
    setEntryWidthChars                      ,


-- ** xalign #attr:xalign#
-- | The horizontal alignment, from 0 (left) to 1 (right).
-- Reversed for RTL layouts.
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    EntryXalignPropertyInfo                 ,
#endif
    constructEntryXalign                    ,
#if defined(ENABLE_OVERLOADING)
    entryXalign                             ,
#endif
    getEntryXalign                          ,
    setEntryXalign                          ,




 -- * Signals


-- ** activate #signal:activate#

    EntryActivateCallback                   ,
#if defined(ENABLE_OVERLOADING)
    EntryActivateSignalInfo                 ,
#endif
    afterEntryActivate                      ,
    onEntryActivate                         ,


-- ** backspace #signal:backspace#

    EntryBackspaceCallback                  ,
#if defined(ENABLE_OVERLOADING)
    EntryBackspaceSignalInfo                ,
#endif
    afterEntryBackspace                     ,
    onEntryBackspace                        ,


-- ** copyClipboard #signal:copyClipboard#

    EntryCopyClipboardCallback              ,
#if defined(ENABLE_OVERLOADING)
    EntryCopyClipboardSignalInfo            ,
#endif
    afterEntryCopyClipboard                 ,
    onEntryCopyClipboard                    ,


-- ** cutClipboard #signal:cutClipboard#

    EntryCutClipboardCallback               ,
#if defined(ENABLE_OVERLOADING)
    EntryCutClipboardSignalInfo             ,
#endif
    afterEntryCutClipboard                  ,
    onEntryCutClipboard                     ,


-- ** deleteFromCursor #signal:deleteFromCursor#

    EntryDeleteFromCursorCallback           ,
#if defined(ENABLE_OVERLOADING)
    EntryDeleteFromCursorSignalInfo         ,
#endif
    afterEntryDeleteFromCursor              ,
    onEntryDeleteFromCursor                 ,


-- ** iconPress #signal:iconPress#

    EntryIconPressCallback                  ,
#if defined(ENABLE_OVERLOADING)
    EntryIconPressSignalInfo                ,
#endif
    afterEntryIconPress                     ,
    onEntryIconPress                        ,


-- ** iconRelease #signal:iconRelease#

    EntryIconReleaseCallback                ,
#if defined(ENABLE_OVERLOADING)
    EntryIconReleaseSignalInfo              ,
#endif
    afterEntryIconRelease                   ,
    onEntryIconRelease                      ,


-- ** insertAtCursor #signal:insertAtCursor#

    EntryInsertAtCursorCallback             ,
#if defined(ENABLE_OVERLOADING)
    EntryInsertAtCursorSignalInfo           ,
#endif
    afterEntryInsertAtCursor                ,
    onEntryInsertAtCursor                   ,


-- ** insertEmoji #signal:insertEmoji#

    EntryInsertEmojiCallback                ,
#if defined(ENABLE_OVERLOADING)
    EntryInsertEmojiSignalInfo              ,
#endif
    afterEntryInsertEmoji                   ,
    onEntryInsertEmoji                      ,


-- ** moveCursor #signal:moveCursor#

    EntryMoveCursorCallback                 ,
#if defined(ENABLE_OVERLOADING)
    EntryMoveCursorSignalInfo               ,
#endif
    afterEntryMoveCursor                    ,
    onEntryMoveCursor                       ,


-- ** pasteClipboard #signal:pasteClipboard#

    EntryPasteClipboardCallback             ,
#if defined(ENABLE_OVERLOADING)
    EntryPasteClipboardSignalInfo           ,
#endif
    afterEntryPasteClipboard                ,
    onEntryPasteClipboard                   ,


-- ** populatePopup #signal:populatePopup#

    EntryPopulatePopupCallback              ,
#if defined(ENABLE_OVERLOADING)
    EntryPopulatePopupSignalInfo            ,
#endif
    afterEntryPopulatePopup                 ,
    onEntryPopulatePopup                    ,


-- ** preeditChanged #signal:preeditChanged#

    EntryPreeditChangedCallback             ,
#if defined(ENABLE_OVERLOADING)
    EntryPreeditChangedSignalInfo           ,
#endif
    afterEntryPreeditChanged                ,
    onEntryPreeditChanged                   ,


-- ** toggleOverwrite #signal:toggleOverwrite#

    EntryToggleOverwriteCallback            ,
#if defined(ENABLE_OVERLOADING)
    EntryToggleOverwriteSignalInfo          ,
#endif
    afterEntryToggleOverwrite               ,
    onEntryToggleOverwrite                  ,




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
import qualified GI.Gdk.Flags as Gdk.Flags
import qualified GI.Gdk.Structs.EventKey as Gdk.EventKey
import qualified GI.Gdk.Structs.Rectangle as Gdk.Rectangle
import qualified GI.Gdk.Unions.Event as Gdk.Event
import qualified GI.GdkPixbuf.Objects.Pixbuf as GdkPixbuf.Pixbuf
import qualified GI.Gio.Interfaces.Icon as Gio.Icon
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.CellEditable as Gtk.CellEditable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Editable as Gtk.Editable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Adjustment as Gtk.Adjustment
import {-# SOURCE #-} qualified GI.Gtk.Objects.EntryBuffer as Gtk.EntryBuffer
import {-# SOURCE #-} qualified GI.Gtk.Objects.EntryCompletion as Gtk.EntryCompletion
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Structs.Border as Gtk.Border
import {-# SOURCE #-} qualified GI.Gtk.Structs.TargetList as Gtk.TargetList
import qualified GI.Pango.Objects.Layout as Pango.Layout
import qualified GI.Pango.Structs.AttrList as Pango.AttrList
import qualified GI.Pango.Structs.TabArray as Pango.TabArray

-- | Memory-managed wrapper type.
newtype Entry = Entry (SP.ManagedPtr Entry)
    deriving (Eq)

instance SP.ManagedPtrNewtype Entry where
    toManagedPtr (Entry p) = p

foreign import ccall "gtk_entry_get_type"
    c_gtk_entry_get_type :: IO B.Types.GType

instance B.Types.TypedObject Entry where
    glibType = c_gtk_entry_get_type

instance B.Types.GObject Entry

-- | Type class for types which can be safely cast to `Entry`, for instance with `toEntry`.
class (SP.GObject o, O.IsDescendantOf Entry o) => IsEntry o
instance (SP.GObject o, O.IsDescendantOf Entry o) => IsEntry o

instance O.HasParentTypes Entry
type instance O.ParentTypes Entry = '[Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable, Gtk.CellEditable.CellEditable, Gtk.Editable.Editable]

-- | Cast to `Entry`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toEntry :: (MIO.MonadIO m, IsEntry o) => o -> m Entry
toEntry = MIO.liftIO . B.ManagedPtr.unsafeCastTo Entry

-- | Convert 'Entry' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Entry) where
    gvalueGType_ = c_gtk_entry_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Entry)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Entry)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Entry ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveEntryMethod (t :: Symbol) (o :: *) :: * where
    ResolveEntryMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveEntryMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveEntryMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveEntryMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveEntryMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveEntryMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveEntryMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveEntryMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveEntryMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveEntryMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveEntryMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveEntryMethod "childNotify" o = Gtk.Widget.WidgetChildNotifyMethodInfo
    ResolveEntryMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveEntryMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveEntryMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveEntryMethod "copyClipboard" o = Gtk.Editable.EditableCopyClipboardMethodInfo
    ResolveEntryMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveEntryMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveEntryMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveEntryMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveEntryMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveEntryMethod "cutClipboard" o = Gtk.Editable.EditableCutClipboardMethodInfo
    ResolveEntryMethod "deleteSelection" o = Gtk.Editable.EditableDeleteSelectionMethodInfo
    ResolveEntryMethod "deleteText" o = Gtk.Editable.EditableDeleteTextMethodInfo
    ResolveEntryMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveEntryMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveEntryMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveEntryMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveEntryMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveEntryMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveEntryMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveEntryMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveEntryMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveEntryMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveEntryMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveEntryMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveEntryMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveEntryMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveEntryMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveEntryMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveEntryMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveEntryMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveEntryMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveEntryMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveEntryMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveEntryMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveEntryMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveEntryMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveEntryMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveEntryMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveEntryMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveEntryMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveEntryMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveEntryMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveEntryMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveEntryMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveEntryMethod "editingDone" o = Gtk.CellEditable.CellEditableEditingDoneMethodInfo
    ResolveEntryMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveEntryMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveEntryMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveEntryMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveEntryMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveEntryMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveEntryMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveEntryMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveEntryMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveEntryMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveEntryMethod "grabFocusWithoutSelecting" o = EntryGrabFocusWithoutSelectingMethodInfo
    ResolveEntryMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveEntryMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveEntryMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveEntryMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveEntryMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveEntryMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveEntryMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveEntryMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveEntryMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveEntryMethod "imContextFilterKeypress" o = EntryImContextFilterKeypressMethodInfo
    ResolveEntryMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveEntryMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveEntryMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveEntryMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveEntryMethod "insertText" o = Gtk.Editable.EditableInsertTextMethodInfo
    ResolveEntryMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveEntryMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveEntryMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveEntryMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveEntryMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveEntryMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveEntryMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveEntryMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveEntryMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveEntryMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveEntryMethod "layoutIndexToTextIndex" o = EntryLayoutIndexToTextIndexMethodInfo
    ResolveEntryMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveEntryMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveEntryMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveEntryMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveEntryMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveEntryMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveEntryMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveEntryMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveEntryMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveEntryMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveEntryMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveEntryMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveEntryMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveEntryMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveEntryMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveEntryMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveEntryMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveEntryMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveEntryMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveEntryMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveEntryMethod "pasteClipboard" o = Gtk.Editable.EditablePasteClipboardMethodInfo
    ResolveEntryMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveEntryMethod "progressPulse" o = EntryProgressPulseMethodInfo
    ResolveEntryMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveEntryMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveEntryMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveEntryMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveEntryMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveEntryMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveEntryMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveEntryMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveEntryMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveEntryMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveEntryMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveEntryMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveEntryMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveEntryMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveEntryMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveEntryMethod "removeWidget" o = Gtk.CellEditable.CellEditableRemoveWidgetMethodInfo
    ResolveEntryMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveEntryMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveEntryMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveEntryMethod "resetImContext" o = EntryResetImContextMethodInfo
    ResolveEntryMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveEntryMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveEntryMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveEntryMethod "selectRegion" o = Gtk.Editable.EditableSelectRegionMethodInfo
    ResolveEntryMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveEntryMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveEntryMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveEntryMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveEntryMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveEntryMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveEntryMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveEntryMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveEntryMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveEntryMethod "startEditing" o = Gtk.CellEditable.CellEditableStartEditingMethodInfo
    ResolveEntryMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveEntryMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveEntryMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveEntryMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveEntryMethod "textIndexToLayoutIndex" o = EntryTextIndexToLayoutIndexMethodInfo
    ResolveEntryMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveEntryMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveEntryMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveEntryMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveEntryMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveEntryMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveEntryMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveEntryMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveEntryMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveEntryMethod "unsetInvisibleChar" o = EntryUnsetInvisibleCharMethodInfo
    ResolveEntryMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveEntryMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveEntryMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveEntryMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveEntryMethod "getActivatesDefault" o = EntryGetActivatesDefaultMethodInfo
    ResolveEntryMethod "getAlignment" o = EntryGetAlignmentMethodInfo
    ResolveEntryMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveEntryMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveEntryMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveEntryMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveEntryMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveEntryMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveEntryMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveEntryMethod "getAttributes" o = EntryGetAttributesMethodInfo
    ResolveEntryMethod "getBuffer" o = EntryGetBufferMethodInfo
    ResolveEntryMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveEntryMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveEntryMethod "getChars" o = Gtk.Editable.EditableGetCharsMethodInfo
    ResolveEntryMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveEntryMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveEntryMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveEntryMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveEntryMethod "getCompletion" o = EntryGetCompletionMethodInfo
    ResolveEntryMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveEntryMethod "getCurrentIconDragSource" o = EntryGetCurrentIconDragSourceMethodInfo
    ResolveEntryMethod "getCursorHadjustment" o = EntryGetCursorHadjustmentMethodInfo
    ResolveEntryMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveEntryMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveEntryMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveEntryMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveEntryMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveEntryMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveEntryMethod "getEditable" o = Gtk.Editable.EditableGetEditableMethodInfo
    ResolveEntryMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveEntryMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveEntryMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveEntryMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveEntryMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveEntryMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveEntryMethod "getHasFrame" o = EntryGetHasFrameMethodInfo
    ResolveEntryMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveEntryMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveEntryMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveEntryMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveEntryMethod "getIconActivatable" o = EntryGetIconActivatableMethodInfo
    ResolveEntryMethod "getIconArea" o = EntryGetIconAreaMethodInfo
    ResolveEntryMethod "getIconAtPos" o = EntryGetIconAtPosMethodInfo
    ResolveEntryMethod "getIconGicon" o = EntryGetIconGiconMethodInfo
    ResolveEntryMethod "getIconName" o = EntryGetIconNameMethodInfo
    ResolveEntryMethod "getIconPixbuf" o = EntryGetIconPixbufMethodInfo
    ResolveEntryMethod "getIconSensitive" o = EntryGetIconSensitiveMethodInfo
    ResolveEntryMethod "getIconStock" o = EntryGetIconStockMethodInfo
    ResolveEntryMethod "getIconStorageType" o = EntryGetIconStorageTypeMethodInfo
    ResolveEntryMethod "getIconTooltipMarkup" o = EntryGetIconTooltipMarkupMethodInfo
    ResolveEntryMethod "getIconTooltipText" o = EntryGetIconTooltipTextMethodInfo
    ResolveEntryMethod "getInnerBorder" o = EntryGetInnerBorderMethodInfo
    ResolveEntryMethod "getInputHints" o = EntryGetInputHintsMethodInfo
    ResolveEntryMethod "getInputPurpose" o = EntryGetInputPurposeMethodInfo
    ResolveEntryMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveEntryMethod "getInvisibleChar" o = EntryGetInvisibleCharMethodInfo
    ResolveEntryMethod "getLayout" o = EntryGetLayoutMethodInfo
    ResolveEntryMethod "getLayoutOffsets" o = EntryGetLayoutOffsetsMethodInfo
    ResolveEntryMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveEntryMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveEntryMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveEntryMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveEntryMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveEntryMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveEntryMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveEntryMethod "getMaxLength" o = EntryGetMaxLengthMethodInfo
    ResolveEntryMethod "getMaxWidthChars" o = EntryGetMaxWidthCharsMethodInfo
    ResolveEntryMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveEntryMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveEntryMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveEntryMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveEntryMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveEntryMethod "getOverwriteMode" o = EntryGetOverwriteModeMethodInfo
    ResolveEntryMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveEntryMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveEntryMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveEntryMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveEntryMethod "getPlaceholderText" o = EntryGetPlaceholderTextMethodInfo
    ResolveEntryMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveEntryMethod "getPosition" o = Gtk.Editable.EditableGetPositionMethodInfo
    ResolveEntryMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveEntryMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveEntryMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveEntryMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveEntryMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveEntryMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveEntryMethod "getProgressFraction" o = EntryGetProgressFractionMethodInfo
    ResolveEntryMethod "getProgressPulseStep" o = EntryGetProgressPulseStepMethodInfo
    ResolveEntryMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveEntryMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveEntryMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveEntryMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveEntryMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveEntryMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveEntryMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveEntryMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveEntryMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveEntryMethod "getSelectionBounds" o = Gtk.Editable.EditableGetSelectionBoundsMethodInfo
    ResolveEntryMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveEntryMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveEntryMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveEntryMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveEntryMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveEntryMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveEntryMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveEntryMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveEntryMethod "getTabs" o = EntryGetTabsMethodInfo
    ResolveEntryMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveEntryMethod "getText" o = EntryGetTextMethodInfo
    ResolveEntryMethod "getTextArea" o = EntryGetTextAreaMethodInfo
    ResolveEntryMethod "getTextLength" o = EntryGetTextLengthMethodInfo
    ResolveEntryMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveEntryMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveEntryMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveEntryMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveEntryMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveEntryMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveEntryMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveEntryMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveEntryMethod "getVisibility" o = EntryGetVisibilityMethodInfo
    ResolveEntryMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveEntryMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveEntryMethod "getWidthChars" o = EntryGetWidthCharsMethodInfo
    ResolveEntryMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveEntryMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveEntryMethod "setActivatesDefault" o = EntrySetActivatesDefaultMethodInfo
    ResolveEntryMethod "setAlignment" o = EntrySetAlignmentMethodInfo
    ResolveEntryMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveEntryMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveEntryMethod "setAttributes" o = EntrySetAttributesMethodInfo
    ResolveEntryMethod "setBuffer" o = EntrySetBufferMethodInfo
    ResolveEntryMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveEntryMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveEntryMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveEntryMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveEntryMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveEntryMethod "setCompletion" o = EntrySetCompletionMethodInfo
    ResolveEntryMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveEntryMethod "setCursorHadjustment" o = EntrySetCursorHadjustmentMethodInfo
    ResolveEntryMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveEntryMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveEntryMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveEntryMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveEntryMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveEntryMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveEntryMethod "setEditable" o = Gtk.Editable.EditableSetEditableMethodInfo
    ResolveEntryMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveEntryMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveEntryMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveEntryMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveEntryMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveEntryMethod "setHasFrame" o = EntrySetHasFrameMethodInfo
    ResolveEntryMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveEntryMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveEntryMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveEntryMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveEntryMethod "setIconActivatable" o = EntrySetIconActivatableMethodInfo
    ResolveEntryMethod "setIconDragSource" o = EntrySetIconDragSourceMethodInfo
    ResolveEntryMethod "setIconFromGicon" o = EntrySetIconFromGiconMethodInfo
    ResolveEntryMethod "setIconFromIconName" o = EntrySetIconFromIconNameMethodInfo
    ResolveEntryMethod "setIconFromPixbuf" o = EntrySetIconFromPixbufMethodInfo
    ResolveEntryMethod "setIconFromStock" o = EntrySetIconFromStockMethodInfo
    ResolveEntryMethod "setIconSensitive" o = EntrySetIconSensitiveMethodInfo
    ResolveEntryMethod "setIconTooltipMarkup" o = EntrySetIconTooltipMarkupMethodInfo
    ResolveEntryMethod "setIconTooltipText" o = EntrySetIconTooltipTextMethodInfo
    ResolveEntryMethod "setInnerBorder" o = EntrySetInnerBorderMethodInfo
    ResolveEntryMethod "setInputHints" o = EntrySetInputHintsMethodInfo
    ResolveEntryMethod "setInputPurpose" o = EntrySetInputPurposeMethodInfo
    ResolveEntryMethod "setInvisibleChar" o = EntrySetInvisibleCharMethodInfo
    ResolveEntryMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveEntryMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveEntryMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveEntryMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveEntryMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveEntryMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveEntryMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveEntryMethod "setMaxLength" o = EntrySetMaxLengthMethodInfo
    ResolveEntryMethod "setMaxWidthChars" o = EntrySetMaxWidthCharsMethodInfo
    ResolveEntryMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveEntryMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveEntryMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveEntryMethod "setOverwriteMode" o = EntrySetOverwriteModeMethodInfo
    ResolveEntryMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveEntryMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveEntryMethod "setPlaceholderText" o = EntrySetPlaceholderTextMethodInfo
    ResolveEntryMethod "setPosition" o = Gtk.Editable.EditableSetPositionMethodInfo
    ResolveEntryMethod "setProgressFraction" o = EntrySetProgressFractionMethodInfo
    ResolveEntryMethod "setProgressPulseStep" o = EntrySetProgressPulseStepMethodInfo
    ResolveEntryMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveEntryMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveEntryMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveEntryMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveEntryMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveEntryMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveEntryMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveEntryMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveEntryMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveEntryMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveEntryMethod "setTabs" o = EntrySetTabsMethodInfo
    ResolveEntryMethod "setText" o = EntrySetTextMethodInfo
    ResolveEntryMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveEntryMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveEntryMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveEntryMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveEntryMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveEntryMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveEntryMethod "setVisibility" o = EntrySetVisibilityMethodInfo
    ResolveEntryMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveEntryMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveEntryMethod "setWidthChars" o = EntrySetWidthCharsMethodInfo
    ResolveEntryMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveEntryMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveEntryMethod t Entry, O.OverloadedMethod info Entry p) => OL.IsLabel t (Entry -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveEntryMethod t Entry, O.OverloadedMethod info Entry p, R.HasField t Entry p) => R.HasField t Entry p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveEntryMethod t Entry, O.OverloadedMethodInfo info Entry) => OL.IsLabel t (O.MethodProxy info Entry) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Entry::activate
-- | The [activate](#g:signal:activate) signal is emitted when the user hits
-- the Enter key.
-- 
-- While this signal is used as a
-- [keybinding signal][GtkBindingSignal],
-- it is also commonly used by applications to intercept
-- activation of entries.
-- 
-- The default bindings for this signal are all forms of the Enter key.
type EntryActivateCallback =
    IO ()

type C_EntryActivateCallback =
    Ptr Entry ->                            -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryActivateCallback`.
foreign import ccall "wrapper"
    mk_EntryActivateCallback :: C_EntryActivateCallback -> IO (FunPtr C_EntryActivateCallback)

wrap_EntryActivateCallback :: 
    GObject a => (a -> EntryActivateCallback) ->
    C_EntryActivateCallback
wrap_EntryActivateCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [activate](#signal:activate) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entry #activate callback
-- @
-- 
-- 
onEntryActivate :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryActivateCallback) -> m SignalHandlerId
onEntryActivate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryActivateCallback wrapped
    wrapped'' <- mk_EntryActivateCallback wrapped'
    connectSignalFunPtr obj "activate" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [activate](#signal:activate) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entry #activate callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryActivate :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryActivateCallback) -> m SignalHandlerId
afterEntryActivate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryActivateCallback wrapped
    wrapped'' <- mk_EntryActivateCallback wrapped'
    connectSignalFunPtr obj "activate" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryActivateSignalInfo
instance SignalInfo EntryActivateSignalInfo where
    type HaskellCallbackType EntryActivateSignalInfo = EntryActivateCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryActivateCallback cb
        cb'' <- mk_EntryActivateCallback cb'
        connectSignalFunPtr obj "activate" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry::activate"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:signal:activate"})

#endif

-- signal Entry::backspace
-- | The [backspace](#g:signal:backspace) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user asks for it.
-- 
-- The default bindings for this signal are
-- Backspace and Shift-Backspace.
type EntryBackspaceCallback =
    IO ()

type C_EntryBackspaceCallback =
    Ptr Entry ->                            -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryBackspaceCallback`.
foreign import ccall "wrapper"
    mk_EntryBackspaceCallback :: C_EntryBackspaceCallback -> IO (FunPtr C_EntryBackspaceCallback)

wrap_EntryBackspaceCallback :: 
    GObject a => (a -> EntryBackspaceCallback) ->
    C_EntryBackspaceCallback
wrap_EntryBackspaceCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [backspace](#signal:backspace) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entry #backspace callback
-- @
-- 
-- 
onEntryBackspace :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryBackspaceCallback) -> m SignalHandlerId
onEntryBackspace obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryBackspaceCallback wrapped
    wrapped'' <- mk_EntryBackspaceCallback wrapped'
    connectSignalFunPtr obj "backspace" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [backspace](#signal:backspace) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entry #backspace callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryBackspace :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryBackspaceCallback) -> m SignalHandlerId
afterEntryBackspace obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryBackspaceCallback wrapped
    wrapped'' <- mk_EntryBackspaceCallback wrapped'
    connectSignalFunPtr obj "backspace" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryBackspaceSignalInfo
instance SignalInfo EntryBackspaceSignalInfo where
    type HaskellCallbackType EntryBackspaceSignalInfo = EntryBackspaceCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryBackspaceCallback cb
        cb'' <- mk_EntryBackspaceCallback cb'
        connectSignalFunPtr obj "backspace" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry::backspace"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:signal:backspace"})

#endif

-- signal Entry::copy-clipboard
-- | The [copyClipboard](#g:signal:copyClipboard) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to copy the selection to the clipboard.
-- 
-- The default bindings for this signal are
-- Ctrl-c and Ctrl-Insert.
type EntryCopyClipboardCallback =
    IO ()

type C_EntryCopyClipboardCallback =
    Ptr Entry ->                            -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryCopyClipboardCallback`.
foreign import ccall "wrapper"
    mk_EntryCopyClipboardCallback :: C_EntryCopyClipboardCallback -> IO (FunPtr C_EntryCopyClipboardCallback)

wrap_EntryCopyClipboardCallback :: 
    GObject a => (a -> EntryCopyClipboardCallback) ->
    C_EntryCopyClipboardCallback
wrap_EntryCopyClipboardCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [copyClipboard](#signal:copyClipboard) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entry #copyClipboard callback
-- @
-- 
-- 
onEntryCopyClipboard :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryCopyClipboardCallback) -> m SignalHandlerId
onEntryCopyClipboard obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryCopyClipboardCallback wrapped
    wrapped'' <- mk_EntryCopyClipboardCallback wrapped'
    connectSignalFunPtr obj "copy-clipboard" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [copyClipboard](#signal:copyClipboard) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entry #copyClipboard callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryCopyClipboard :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryCopyClipboardCallback) -> m SignalHandlerId
afterEntryCopyClipboard obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryCopyClipboardCallback wrapped
    wrapped'' <- mk_EntryCopyClipboardCallback wrapped'
    connectSignalFunPtr obj "copy-clipboard" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryCopyClipboardSignalInfo
instance SignalInfo EntryCopyClipboardSignalInfo where
    type HaskellCallbackType EntryCopyClipboardSignalInfo = EntryCopyClipboardCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryCopyClipboardCallback cb
        cb'' <- mk_EntryCopyClipboardCallback cb'
        connectSignalFunPtr obj "copy-clipboard" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry::copy-clipboard"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:signal:copyClipboard"})

#endif

-- signal Entry::cut-clipboard
-- | The [cutClipboard](#g:signal:cutClipboard) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to cut the selection to the clipboard.
-- 
-- The default bindings for this signal are
-- Ctrl-x and Shift-Delete.
type EntryCutClipboardCallback =
    IO ()

type C_EntryCutClipboardCallback =
    Ptr Entry ->                            -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryCutClipboardCallback`.
foreign import ccall "wrapper"
    mk_EntryCutClipboardCallback :: C_EntryCutClipboardCallback -> IO (FunPtr C_EntryCutClipboardCallback)

wrap_EntryCutClipboardCallback :: 
    GObject a => (a -> EntryCutClipboardCallback) ->
    C_EntryCutClipboardCallback
wrap_EntryCutClipboardCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [cutClipboard](#signal:cutClipboard) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entry #cutClipboard callback
-- @
-- 
-- 
onEntryCutClipboard :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryCutClipboardCallback) -> m SignalHandlerId
onEntryCutClipboard obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryCutClipboardCallback wrapped
    wrapped'' <- mk_EntryCutClipboardCallback wrapped'
    connectSignalFunPtr obj "cut-clipboard" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [cutClipboard](#signal:cutClipboard) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entry #cutClipboard callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryCutClipboard :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryCutClipboardCallback) -> m SignalHandlerId
afterEntryCutClipboard obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryCutClipboardCallback wrapped
    wrapped'' <- mk_EntryCutClipboardCallback wrapped'
    connectSignalFunPtr obj "cut-clipboard" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryCutClipboardSignalInfo
instance SignalInfo EntryCutClipboardSignalInfo where
    type HaskellCallbackType EntryCutClipboardSignalInfo = EntryCutClipboardCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryCutClipboardCallback cb
        cb'' <- mk_EntryCutClipboardCallback cb'
        connectSignalFunPtr obj "cut-clipboard" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry::cut-clipboard"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:signal:cutClipboard"})

#endif

-- signal Entry::delete-from-cursor
-- | The [deleteFromCursor](#g:signal:deleteFromCursor) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user initiates a text deletion.
-- 
-- If the /@type@/ is 'GI.Gtk.Enums.DeleteTypeChars', GTK+ deletes the selection
-- if there is one, otherwise it deletes the requested number
-- of characters.
-- 
-- The default bindings for this signal are
-- Delete for deleting a character and Ctrl-Delete for
-- deleting a word.
type EntryDeleteFromCursorCallback =
    Gtk.Enums.DeleteType
    -- ^ /@type@/: the granularity of the deletion, as a t'GI.Gtk.Enums.DeleteType'
    -> Int32
    -- ^ /@count@/: the number of /@type@/ units to delete
    -> IO ()

type C_EntryDeleteFromCursorCallback =
    Ptr Entry ->                            -- object
    CUInt ->
    Int32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryDeleteFromCursorCallback`.
foreign import ccall "wrapper"
    mk_EntryDeleteFromCursorCallback :: C_EntryDeleteFromCursorCallback -> IO (FunPtr C_EntryDeleteFromCursorCallback)

wrap_EntryDeleteFromCursorCallback :: 
    GObject a => (a -> EntryDeleteFromCursorCallback) ->
    C_EntryDeleteFromCursorCallback
wrap_EntryDeleteFromCursorCallback gi'cb gi'selfPtr type_ count _ = do
    let type_' = (toEnum . fromIntegral) type_
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  type_' count


-- | Connect a signal handler for the [deleteFromCursor](#signal:deleteFromCursor) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entry #deleteFromCursor callback
-- @
-- 
-- 
onEntryDeleteFromCursor :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryDeleteFromCursorCallback) -> m SignalHandlerId
onEntryDeleteFromCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryDeleteFromCursorCallback wrapped
    wrapped'' <- mk_EntryDeleteFromCursorCallback wrapped'
    connectSignalFunPtr obj "delete-from-cursor" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [deleteFromCursor](#signal:deleteFromCursor) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entry #deleteFromCursor callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryDeleteFromCursor :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryDeleteFromCursorCallback) -> m SignalHandlerId
afterEntryDeleteFromCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryDeleteFromCursorCallback wrapped
    wrapped'' <- mk_EntryDeleteFromCursorCallback wrapped'
    connectSignalFunPtr obj "delete-from-cursor" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryDeleteFromCursorSignalInfo
instance SignalInfo EntryDeleteFromCursorSignalInfo where
    type HaskellCallbackType EntryDeleteFromCursorSignalInfo = EntryDeleteFromCursorCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryDeleteFromCursorCallback cb
        cb'' <- mk_EntryDeleteFromCursorCallback cb'
        connectSignalFunPtr obj "delete-from-cursor" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry::delete-from-cursor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:signal:deleteFromCursor"})

#endif

-- signal Entry::icon-press
-- | The [iconPress](#g:signal:iconPress) signal is emitted when an activatable icon
-- is clicked.
-- 
-- /Since: 2.16/
type EntryIconPressCallback =
    Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: The position of the clicked icon
    -> Gdk.Event.Event
    -- ^ /@event@/: the button press event
    -> IO ()

type C_EntryIconPressCallback =
    Ptr Entry ->                            -- object
    CUInt ->
    Ptr Gdk.Event.Event ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryIconPressCallback`.
foreign import ccall "wrapper"
    mk_EntryIconPressCallback :: C_EntryIconPressCallback -> IO (FunPtr C_EntryIconPressCallback)

wrap_EntryIconPressCallback :: 
    GObject a => (a -> EntryIconPressCallback) ->
    C_EntryIconPressCallback
wrap_EntryIconPressCallback gi'cb gi'selfPtr iconPos event _ = do
    let iconPos' = (toEnum . fromIntegral) iconPos
    B.ManagedPtr.withTransient  event $ \event' -> do
        B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  iconPos' event'


-- | Connect a signal handler for the [iconPress](#signal:iconPress) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entry #iconPress callback
-- @
-- 
-- 
onEntryIconPress :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryIconPressCallback) -> m SignalHandlerId
onEntryIconPress obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryIconPressCallback wrapped
    wrapped'' <- mk_EntryIconPressCallback wrapped'
    connectSignalFunPtr obj "icon-press" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [iconPress](#signal:iconPress) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entry #iconPress callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryIconPress :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryIconPressCallback) -> m SignalHandlerId
afterEntryIconPress obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryIconPressCallback wrapped
    wrapped'' <- mk_EntryIconPressCallback wrapped'
    connectSignalFunPtr obj "icon-press" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryIconPressSignalInfo
instance SignalInfo EntryIconPressSignalInfo where
    type HaskellCallbackType EntryIconPressSignalInfo = EntryIconPressCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryIconPressCallback cb
        cb'' <- mk_EntryIconPressCallback cb'
        connectSignalFunPtr obj "icon-press" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry::icon-press"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:signal:iconPress"})

#endif

-- signal Entry::icon-release
-- | The [iconRelease](#g:signal:iconRelease) signal is emitted on the button release from a
-- mouse click over an activatable icon.
-- 
-- /Since: 2.16/
type EntryIconReleaseCallback =
    Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: The position of the clicked icon
    -> Gdk.Event.Event
    -- ^ /@event@/: the button release event
    -> IO ()

type C_EntryIconReleaseCallback =
    Ptr Entry ->                            -- object
    CUInt ->
    Ptr Gdk.Event.Event ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryIconReleaseCallback`.
foreign import ccall "wrapper"
    mk_EntryIconReleaseCallback :: C_EntryIconReleaseCallback -> IO (FunPtr C_EntryIconReleaseCallback)

wrap_EntryIconReleaseCallback :: 
    GObject a => (a -> EntryIconReleaseCallback) ->
    C_EntryIconReleaseCallback
wrap_EntryIconReleaseCallback gi'cb gi'selfPtr iconPos event _ = do
    let iconPos' = (toEnum . fromIntegral) iconPos
    B.ManagedPtr.withTransient  event $ \event' -> do
        B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  iconPos' event'


-- | Connect a signal handler for the [iconRelease](#signal:iconRelease) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entry #iconRelease callback
-- @
-- 
-- 
onEntryIconRelease :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryIconReleaseCallback) -> m SignalHandlerId
onEntryIconRelease obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryIconReleaseCallback wrapped
    wrapped'' <- mk_EntryIconReleaseCallback wrapped'
    connectSignalFunPtr obj "icon-release" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [iconRelease](#signal:iconRelease) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entry #iconRelease callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryIconRelease :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryIconReleaseCallback) -> m SignalHandlerId
afterEntryIconRelease obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryIconReleaseCallback wrapped
    wrapped'' <- mk_EntryIconReleaseCallback wrapped'
    connectSignalFunPtr obj "icon-release" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryIconReleaseSignalInfo
instance SignalInfo EntryIconReleaseSignalInfo where
    type HaskellCallbackType EntryIconReleaseSignalInfo = EntryIconReleaseCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryIconReleaseCallback cb
        cb'' <- mk_EntryIconReleaseCallback cb'
        connectSignalFunPtr obj "icon-release" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry::icon-release"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:signal:iconRelease"})

#endif

-- signal Entry::insert-at-cursor
-- | The [insertAtCursor](#g:signal:insertAtCursor) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user initiates the insertion of a
-- fixed string at the cursor.
-- 
-- This signal has no default bindings.
type EntryInsertAtCursorCallback =
    T.Text
    -- ^ /@string@/: the string to insert
    -> IO ()

type C_EntryInsertAtCursorCallback =
    Ptr Entry ->                            -- object
    CString ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryInsertAtCursorCallback`.
foreign import ccall "wrapper"
    mk_EntryInsertAtCursorCallback :: C_EntryInsertAtCursorCallback -> IO (FunPtr C_EntryInsertAtCursorCallback)

wrap_EntryInsertAtCursorCallback :: 
    GObject a => (a -> EntryInsertAtCursorCallback) ->
    C_EntryInsertAtCursorCallback
wrap_EntryInsertAtCursorCallback gi'cb gi'selfPtr string _ = do
    string' <- cstringToText string
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  string'


-- | Connect a signal handler for the [insertAtCursor](#signal:insertAtCursor) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entry #insertAtCursor callback
-- @
-- 
-- 
onEntryInsertAtCursor :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryInsertAtCursorCallback) -> m SignalHandlerId
onEntryInsertAtCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryInsertAtCursorCallback wrapped
    wrapped'' <- mk_EntryInsertAtCursorCallback wrapped'
    connectSignalFunPtr obj "insert-at-cursor" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [insertAtCursor](#signal:insertAtCursor) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entry #insertAtCursor callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryInsertAtCursor :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryInsertAtCursorCallback) -> m SignalHandlerId
afterEntryInsertAtCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryInsertAtCursorCallback wrapped
    wrapped'' <- mk_EntryInsertAtCursorCallback wrapped'
    connectSignalFunPtr obj "insert-at-cursor" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryInsertAtCursorSignalInfo
instance SignalInfo EntryInsertAtCursorSignalInfo where
    type HaskellCallbackType EntryInsertAtCursorSignalInfo = EntryInsertAtCursorCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryInsertAtCursorCallback cb
        cb'' <- mk_EntryInsertAtCursorCallback cb'
        connectSignalFunPtr obj "insert-at-cursor" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry::insert-at-cursor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:signal:insertAtCursor"})

#endif

-- signal Entry::insert-emoji
-- | The [insertEmoji](#g:signal:insertEmoji) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to present the Emoji chooser for the /@entry@/.
-- 
-- The default bindings for this signal are Ctrl-. and Ctrl-;
-- 
-- /Since: 3.22.27/
type EntryInsertEmojiCallback =
    IO ()

type C_EntryInsertEmojiCallback =
    Ptr Entry ->                            -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryInsertEmojiCallback`.
foreign import ccall "wrapper"
    mk_EntryInsertEmojiCallback :: C_EntryInsertEmojiCallback -> IO (FunPtr C_EntryInsertEmojiCallback)

wrap_EntryInsertEmojiCallback :: 
    GObject a => (a -> EntryInsertEmojiCallback) ->
    C_EntryInsertEmojiCallback
wrap_EntryInsertEmojiCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [insertEmoji](#signal:insertEmoji) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entry #insertEmoji callback
-- @
-- 
-- 
onEntryInsertEmoji :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryInsertEmojiCallback) -> m SignalHandlerId
onEntryInsertEmoji obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryInsertEmojiCallback wrapped
    wrapped'' <- mk_EntryInsertEmojiCallback wrapped'
    connectSignalFunPtr obj "insert-emoji" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [insertEmoji](#signal:insertEmoji) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entry #insertEmoji callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryInsertEmoji :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryInsertEmojiCallback) -> m SignalHandlerId
afterEntryInsertEmoji obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryInsertEmojiCallback wrapped
    wrapped'' <- mk_EntryInsertEmojiCallback wrapped'
    connectSignalFunPtr obj "insert-emoji" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryInsertEmojiSignalInfo
instance SignalInfo EntryInsertEmojiSignalInfo where
    type HaskellCallbackType EntryInsertEmojiSignalInfo = EntryInsertEmojiCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryInsertEmojiCallback cb
        cb'' <- mk_EntryInsertEmojiCallback cb'
        connectSignalFunPtr obj "insert-emoji" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry::insert-emoji"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:signal:insertEmoji"})

#endif

-- signal Entry::move-cursor
-- | The [moveCursor](#g:signal:moveCursor) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user initiates a cursor movement.
-- If the cursor is not visible in /@entry@/, this signal causes
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
type EntryMoveCursorCallback =
    Gtk.Enums.MovementStep
    -- ^ /@step@/: the granularity of the move, as a t'GI.Gtk.Enums.MovementStep'
    -> Int32
    -- ^ /@count@/: the number of /@step@/ units to move
    -> Bool
    -- ^ /@extendSelection@/: 'P.True' if the move should extend the selection
    -> IO ()

type C_EntryMoveCursorCallback =
    Ptr Entry ->                            -- object
    CUInt ->
    Int32 ->
    CInt ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryMoveCursorCallback`.
foreign import ccall "wrapper"
    mk_EntryMoveCursorCallback :: C_EntryMoveCursorCallback -> IO (FunPtr C_EntryMoveCursorCallback)

wrap_EntryMoveCursorCallback :: 
    GObject a => (a -> EntryMoveCursorCallback) ->
    C_EntryMoveCursorCallback
wrap_EntryMoveCursorCallback gi'cb gi'selfPtr step count extendSelection _ = do
    let step' = (toEnum . fromIntegral) step
    let extendSelection' = (/= 0) extendSelection
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  step' count extendSelection'


-- | Connect a signal handler for the [moveCursor](#signal:moveCursor) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entry #moveCursor callback
-- @
-- 
-- 
onEntryMoveCursor :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryMoveCursorCallback) -> m SignalHandlerId
onEntryMoveCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryMoveCursorCallback wrapped
    wrapped'' <- mk_EntryMoveCursorCallback wrapped'
    connectSignalFunPtr obj "move-cursor" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [moveCursor](#signal:moveCursor) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entry #moveCursor callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryMoveCursor :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryMoveCursorCallback) -> m SignalHandlerId
afterEntryMoveCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryMoveCursorCallback wrapped
    wrapped'' <- mk_EntryMoveCursorCallback wrapped'
    connectSignalFunPtr obj "move-cursor" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryMoveCursorSignalInfo
instance SignalInfo EntryMoveCursorSignalInfo where
    type HaskellCallbackType EntryMoveCursorSignalInfo = EntryMoveCursorCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryMoveCursorCallback cb
        cb'' <- mk_EntryMoveCursorCallback cb'
        connectSignalFunPtr obj "move-cursor" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry::move-cursor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:signal:moveCursor"})

#endif

-- signal Entry::paste-clipboard
-- | The [pasteClipboard](#g:signal:pasteClipboard) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to paste the contents of the clipboard
-- into the text view.
-- 
-- The default bindings for this signal are
-- Ctrl-v and Shift-Insert.
type EntryPasteClipboardCallback =
    IO ()

type C_EntryPasteClipboardCallback =
    Ptr Entry ->                            -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryPasteClipboardCallback`.
foreign import ccall "wrapper"
    mk_EntryPasteClipboardCallback :: C_EntryPasteClipboardCallback -> IO (FunPtr C_EntryPasteClipboardCallback)

wrap_EntryPasteClipboardCallback :: 
    GObject a => (a -> EntryPasteClipboardCallback) ->
    C_EntryPasteClipboardCallback
wrap_EntryPasteClipboardCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [pasteClipboard](#signal:pasteClipboard) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entry #pasteClipboard callback
-- @
-- 
-- 
onEntryPasteClipboard :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryPasteClipboardCallback) -> m SignalHandlerId
onEntryPasteClipboard obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryPasteClipboardCallback wrapped
    wrapped'' <- mk_EntryPasteClipboardCallback wrapped'
    connectSignalFunPtr obj "paste-clipboard" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [pasteClipboard](#signal:pasteClipboard) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entry #pasteClipboard callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryPasteClipboard :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryPasteClipboardCallback) -> m SignalHandlerId
afterEntryPasteClipboard obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryPasteClipboardCallback wrapped
    wrapped'' <- mk_EntryPasteClipboardCallback wrapped'
    connectSignalFunPtr obj "paste-clipboard" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryPasteClipboardSignalInfo
instance SignalInfo EntryPasteClipboardSignalInfo where
    type HaskellCallbackType EntryPasteClipboardSignalInfo = EntryPasteClipboardCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryPasteClipboardCallback cb
        cb'' <- mk_EntryPasteClipboardCallback cb'
        connectSignalFunPtr obj "paste-clipboard" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry::paste-clipboard"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:signal:pasteClipboard"})

#endif

-- signal Entry::populate-popup
-- | The [populatePopup](#g:signal:populatePopup) signal gets emitted before showing the
-- context menu of the entry.
-- 
-- If you need to add items to the context menu, connect
-- to this signal and append your items to the /@widget@/, which
-- will be a t'GI.Gtk.Objects.Menu.Menu' in this case.
-- 
-- If [Entry:populateAll]("GI.Gtk.Objects.Entry#g:attr:populateAll") is 'P.True', this signal will
-- also be emitted to populate touch popups. In this case,
-- /@widget@/ will be a different container, e.g. a t'GI.Gtk.Objects.Toolbar.Toolbar'.
-- The signal handler should not make assumptions about the
-- type of /@widget@/.
type EntryPopulatePopupCallback =
    Gtk.Widget.Widget
    -- ^ /@widget@/: the container that is being populated
    -> IO ()

type C_EntryPopulatePopupCallback =
    Ptr Entry ->                            -- object
    Ptr Gtk.Widget.Widget ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryPopulatePopupCallback`.
foreign import ccall "wrapper"
    mk_EntryPopulatePopupCallback :: C_EntryPopulatePopupCallback -> IO (FunPtr C_EntryPopulatePopupCallback)

wrap_EntryPopulatePopupCallback :: 
    GObject a => (a -> EntryPopulatePopupCallback) ->
    C_EntryPopulatePopupCallback
wrap_EntryPopulatePopupCallback gi'cb gi'selfPtr widget _ = do
    widget' <- (newObject Gtk.Widget.Widget) widget
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  widget'


-- | Connect a signal handler for the [populatePopup](#signal:populatePopup) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entry #populatePopup callback
-- @
-- 
-- 
onEntryPopulatePopup :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryPopulatePopupCallback) -> m SignalHandlerId
onEntryPopulatePopup obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryPopulatePopupCallback wrapped
    wrapped'' <- mk_EntryPopulatePopupCallback wrapped'
    connectSignalFunPtr obj "populate-popup" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [populatePopup](#signal:populatePopup) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entry #populatePopup callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryPopulatePopup :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryPopulatePopupCallback) -> m SignalHandlerId
afterEntryPopulatePopup obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryPopulatePopupCallback wrapped
    wrapped'' <- mk_EntryPopulatePopupCallback wrapped'
    connectSignalFunPtr obj "populate-popup" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryPopulatePopupSignalInfo
instance SignalInfo EntryPopulatePopupSignalInfo where
    type HaskellCallbackType EntryPopulatePopupSignalInfo = EntryPopulatePopupCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryPopulatePopupCallback cb
        cb'' <- mk_EntryPopulatePopupCallback cb'
        connectSignalFunPtr obj "populate-popup" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry::populate-popup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:signal:populatePopup"})

#endif

-- signal Entry::preedit-changed
-- | If an input method is used, the typed text will not immediately
-- be committed to the buffer. So if you are interested in the text,
-- connect to this signal.
-- 
-- /Since: 2.20/
type EntryPreeditChangedCallback =
    T.Text
    -- ^ /@preedit@/: the current preedit string
    -> IO ()

type C_EntryPreeditChangedCallback =
    Ptr Entry ->                            -- object
    CString ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryPreeditChangedCallback`.
foreign import ccall "wrapper"
    mk_EntryPreeditChangedCallback :: C_EntryPreeditChangedCallback -> IO (FunPtr C_EntryPreeditChangedCallback)

wrap_EntryPreeditChangedCallback :: 
    GObject a => (a -> EntryPreeditChangedCallback) ->
    C_EntryPreeditChangedCallback
wrap_EntryPreeditChangedCallback gi'cb gi'selfPtr preedit _ = do
    preedit' <- cstringToText preedit
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  preedit'


-- | Connect a signal handler for the [preeditChanged](#signal:preeditChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entry #preeditChanged callback
-- @
-- 
-- 
onEntryPreeditChanged :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryPreeditChangedCallback) -> m SignalHandlerId
onEntryPreeditChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryPreeditChangedCallback wrapped
    wrapped'' <- mk_EntryPreeditChangedCallback wrapped'
    connectSignalFunPtr obj "preedit-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [preeditChanged](#signal:preeditChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entry #preeditChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryPreeditChanged :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryPreeditChangedCallback) -> m SignalHandlerId
afterEntryPreeditChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryPreeditChangedCallback wrapped
    wrapped'' <- mk_EntryPreeditChangedCallback wrapped'
    connectSignalFunPtr obj "preedit-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryPreeditChangedSignalInfo
instance SignalInfo EntryPreeditChangedSignalInfo where
    type HaskellCallbackType EntryPreeditChangedSignalInfo = EntryPreeditChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryPreeditChangedCallback cb
        cb'' <- mk_EntryPreeditChangedCallback cb'
        connectSignalFunPtr obj "preedit-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry::preedit-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:signal:preeditChanged"})

#endif

-- signal Entry::toggle-overwrite
-- | The [toggleOverwrite](#g:signal:toggleOverwrite) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to toggle the overwrite mode of the entry.
-- 
-- The default bindings for this signal is Insert.
type EntryToggleOverwriteCallback =
    IO ()

type C_EntryToggleOverwriteCallback =
    Ptr Entry ->                            -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryToggleOverwriteCallback`.
foreign import ccall "wrapper"
    mk_EntryToggleOverwriteCallback :: C_EntryToggleOverwriteCallback -> IO (FunPtr C_EntryToggleOverwriteCallback)

wrap_EntryToggleOverwriteCallback :: 
    GObject a => (a -> EntryToggleOverwriteCallback) ->
    C_EntryToggleOverwriteCallback
wrap_EntryToggleOverwriteCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [toggleOverwrite](#signal:toggleOverwrite) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entry #toggleOverwrite callback
-- @
-- 
-- 
onEntryToggleOverwrite :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryToggleOverwriteCallback) -> m SignalHandlerId
onEntryToggleOverwrite obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryToggleOverwriteCallback wrapped
    wrapped'' <- mk_EntryToggleOverwriteCallback wrapped'
    connectSignalFunPtr obj "toggle-overwrite" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [toggleOverwrite](#signal:toggleOverwrite) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entry #toggleOverwrite callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryToggleOverwrite :: (IsEntry a, MonadIO m) => a -> ((?self :: a) => EntryToggleOverwriteCallback) -> m SignalHandlerId
afterEntryToggleOverwrite obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryToggleOverwriteCallback wrapped
    wrapped'' <- mk_EntryToggleOverwriteCallback wrapped'
    connectSignalFunPtr obj "toggle-overwrite" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryToggleOverwriteSignalInfo
instance SignalInfo EntryToggleOverwriteSignalInfo where
    type HaskellCallbackType EntryToggleOverwriteSignalInfo = EntryToggleOverwriteCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryToggleOverwriteCallback cb
        cb'' <- mk_EntryToggleOverwriteCallback cb'
        connectSignalFunPtr obj "toggle-overwrite" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry::toggle-overwrite"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:signal:toggleOverwrite"})

#endif

-- VVV Prop "activates-default"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@activates-default@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #activatesDefault
-- @
getEntryActivatesDefault :: (MonadIO m, IsEntry o) => o -> m Bool
getEntryActivatesDefault obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "activates-default"

-- | Set the value of the “@activates-default@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #activatesDefault 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryActivatesDefault :: (MonadIO m, IsEntry o) => o -> Bool -> m ()
setEntryActivatesDefault obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "activates-default" val

-- | Construct a `GValueConstruct` with valid value for the “@activates-default@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryActivatesDefault :: (IsEntry o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryActivatesDefault val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "activates-default" val

#if defined(ENABLE_OVERLOADING)
data EntryActivatesDefaultPropertyInfo
instance AttrInfo EntryActivatesDefaultPropertyInfo where
    type AttrAllowedOps EntryActivatesDefaultPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryActivatesDefaultPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryActivatesDefaultPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryActivatesDefaultPropertyInfo = (~) Bool
    type AttrTransferType EntryActivatesDefaultPropertyInfo = Bool
    type AttrGetType EntryActivatesDefaultPropertyInfo = Bool
    type AttrLabel EntryActivatesDefaultPropertyInfo = "activates-default"
    type AttrOrigin EntryActivatesDefaultPropertyInfo = Entry
    attrGet = getEntryActivatesDefault
    attrSet = setEntryActivatesDefault
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryActivatesDefault
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.activatesDefault"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:activatesDefault"
        })
#endif

-- VVV Prop "attributes"
   -- Type: TInterface (Name {namespace = "Pango", name = "AttrList"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just False)

-- | Get the value of the “@attributes@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #attributes
-- @
getEntryAttributes :: (MonadIO m, IsEntry o) => o -> m (Maybe Pango.AttrList.AttrList)
getEntryAttributes obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "attributes" Pango.AttrList.AttrList

-- | Set the value of the “@attributes@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #attributes 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryAttributes :: (MonadIO m, IsEntry o) => o -> Pango.AttrList.AttrList -> m ()
setEntryAttributes obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "attributes" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@attributes@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryAttributes :: (IsEntry o, MIO.MonadIO m) => Pango.AttrList.AttrList -> m (GValueConstruct o)
constructEntryAttributes val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "attributes" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data EntryAttributesPropertyInfo
instance AttrInfo EntryAttributesPropertyInfo where
    type AttrAllowedOps EntryAttributesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryAttributesPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryAttributesPropertyInfo = (~) Pango.AttrList.AttrList
    type AttrTransferTypeConstraint EntryAttributesPropertyInfo = (~) Pango.AttrList.AttrList
    type AttrTransferType EntryAttributesPropertyInfo = Pango.AttrList.AttrList
    type AttrGetType EntryAttributesPropertyInfo = (Maybe Pango.AttrList.AttrList)
    type AttrLabel EntryAttributesPropertyInfo = "attributes"
    type AttrOrigin EntryAttributesPropertyInfo = Entry
    attrGet = getEntryAttributes
    attrSet = setEntryAttributes
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryAttributes
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.attributes"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:attributes"
        })
#endif

-- VVV Prop "buffer"
   -- Type: TInterface (Name {namespace = "Gtk", name = "EntryBuffer"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@buffer@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #buffer
-- @
getEntryBuffer :: (MonadIO m, IsEntry o) => o -> m Gtk.EntryBuffer.EntryBuffer
getEntryBuffer obj = MIO.liftIO $ checkUnexpectedNothing "getEntryBuffer" $ B.Properties.getObjectPropertyObject obj "buffer" Gtk.EntryBuffer.EntryBuffer

-- | Set the value of the “@buffer@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #buffer 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryBuffer :: (MonadIO m, IsEntry o, Gtk.EntryBuffer.IsEntryBuffer a) => o -> a -> m ()
setEntryBuffer obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "buffer" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@buffer@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryBuffer :: (IsEntry o, MIO.MonadIO m, Gtk.EntryBuffer.IsEntryBuffer a) => a -> m (GValueConstruct o)
constructEntryBuffer val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "buffer" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data EntryBufferPropertyInfo
instance AttrInfo EntryBufferPropertyInfo where
    type AttrAllowedOps EntryBufferPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryBufferPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryBufferPropertyInfo = Gtk.EntryBuffer.IsEntryBuffer
    type AttrTransferTypeConstraint EntryBufferPropertyInfo = Gtk.EntryBuffer.IsEntryBuffer
    type AttrTransferType EntryBufferPropertyInfo = Gtk.EntryBuffer.EntryBuffer
    type AttrGetType EntryBufferPropertyInfo = Gtk.EntryBuffer.EntryBuffer
    type AttrLabel EntryBufferPropertyInfo = "buffer"
    type AttrOrigin EntryBufferPropertyInfo = Entry
    attrGet = getEntryBuffer
    attrSet = setEntryBuffer
    attrTransfer _ v = do
        unsafeCastTo Gtk.EntryBuffer.EntryBuffer v
    attrConstruct = constructEntryBuffer
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.buffer"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:buffer"
        })
#endif

-- VVV Prop "caps-lock-warning"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@caps-lock-warning@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #capsLockWarning
-- @
getEntryCapsLockWarning :: (MonadIO m, IsEntry o) => o -> m Bool
getEntryCapsLockWarning obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "caps-lock-warning"

-- | Set the value of the “@caps-lock-warning@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #capsLockWarning 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryCapsLockWarning :: (MonadIO m, IsEntry o) => o -> Bool -> m ()
setEntryCapsLockWarning obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "caps-lock-warning" val

-- | Construct a `GValueConstruct` with valid value for the “@caps-lock-warning@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryCapsLockWarning :: (IsEntry o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryCapsLockWarning val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "caps-lock-warning" val

#if defined(ENABLE_OVERLOADING)
data EntryCapsLockWarningPropertyInfo
instance AttrInfo EntryCapsLockWarningPropertyInfo where
    type AttrAllowedOps EntryCapsLockWarningPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryCapsLockWarningPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryCapsLockWarningPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryCapsLockWarningPropertyInfo = (~) Bool
    type AttrTransferType EntryCapsLockWarningPropertyInfo = Bool
    type AttrGetType EntryCapsLockWarningPropertyInfo = Bool
    type AttrLabel EntryCapsLockWarningPropertyInfo = "caps-lock-warning"
    type AttrOrigin EntryCapsLockWarningPropertyInfo = Entry
    attrGet = getEntryCapsLockWarning
    attrSet = setEntryCapsLockWarning
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryCapsLockWarning
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.capsLockWarning"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:capsLockWarning"
        })
#endif

-- VVV Prop "completion"
   -- Type: TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just True)

-- | Get the value of the “@completion@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #completion
-- @
getEntryCompletion :: (MonadIO m, IsEntry o) => o -> m Gtk.EntryCompletion.EntryCompletion
getEntryCompletion obj = MIO.liftIO $ checkUnexpectedNothing "getEntryCompletion" $ B.Properties.getObjectPropertyObject obj "completion" Gtk.EntryCompletion.EntryCompletion

-- | Set the value of the “@completion@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #completion 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryCompletion :: (MonadIO m, IsEntry o, Gtk.EntryCompletion.IsEntryCompletion a) => o -> a -> m ()
setEntryCompletion obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "completion" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@completion@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryCompletion :: (IsEntry o, MIO.MonadIO m, Gtk.EntryCompletion.IsEntryCompletion a) => a -> m (GValueConstruct o)
constructEntryCompletion val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "completion" (P.Just val)

-- | Set the value of the “@completion@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #completion
-- @
clearEntryCompletion :: (MonadIO m, IsEntry o) => o -> m ()
clearEntryCompletion obj = liftIO $ B.Properties.setObjectPropertyObject obj "completion" (Nothing :: Maybe Gtk.EntryCompletion.EntryCompletion)

#if defined(ENABLE_OVERLOADING)
data EntryCompletionPropertyInfo
instance AttrInfo EntryCompletionPropertyInfo where
    type AttrAllowedOps EntryCompletionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntryCompletionPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryCompletionPropertyInfo = Gtk.EntryCompletion.IsEntryCompletion
    type AttrTransferTypeConstraint EntryCompletionPropertyInfo = Gtk.EntryCompletion.IsEntryCompletion
    type AttrTransferType EntryCompletionPropertyInfo = Gtk.EntryCompletion.EntryCompletion
    type AttrGetType EntryCompletionPropertyInfo = Gtk.EntryCompletion.EntryCompletion
    type AttrLabel EntryCompletionPropertyInfo = "completion"
    type AttrOrigin EntryCompletionPropertyInfo = Entry
    attrGet = getEntryCompletion
    attrSet = setEntryCompletion
    attrTransfer _ v = do
        unsafeCastTo Gtk.EntryCompletion.EntryCompletion v
    attrConstruct = constructEntryCompletion
    attrClear = clearEntryCompletion
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.completion"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:completion"
        })
#endif

-- VVV Prop "cursor-position"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@cursor-position@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #cursorPosition
-- @
getEntryCursorPosition :: (MonadIO m, IsEntry o) => o -> m Int32
getEntryCursorPosition obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "cursor-position"

#if defined(ENABLE_OVERLOADING)
data EntryCursorPositionPropertyInfo
instance AttrInfo EntryCursorPositionPropertyInfo where
    type AttrAllowedOps EntryCursorPositionPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint EntryCursorPositionPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryCursorPositionPropertyInfo = (~) ()
    type AttrTransferTypeConstraint EntryCursorPositionPropertyInfo = (~) ()
    type AttrTransferType EntryCursorPositionPropertyInfo = ()
    type AttrGetType EntryCursorPositionPropertyInfo = Int32
    type AttrLabel EntryCursorPositionPropertyInfo = "cursor-position"
    type AttrOrigin EntryCursorPositionPropertyInfo = Entry
    attrGet = getEntryCursorPosition
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.cursorPosition"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:cursorPosition"
        })
#endif

-- VVV Prop "editable"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@editable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #editable
-- @
getEntryEditable :: (MonadIO m, IsEntry o) => o -> m Bool
getEntryEditable obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "editable"

-- | Set the value of the “@editable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #editable 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryEditable :: (MonadIO m, IsEntry o) => o -> Bool -> m ()
setEntryEditable obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "editable" val

-- | Construct a `GValueConstruct` with valid value for the “@editable@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryEditable :: (IsEntry o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryEditable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "editable" val

#if defined(ENABLE_OVERLOADING)
data EntryEditablePropertyInfo
instance AttrInfo EntryEditablePropertyInfo where
    type AttrAllowedOps EntryEditablePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryEditablePropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryEditablePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryEditablePropertyInfo = (~) Bool
    type AttrTransferType EntryEditablePropertyInfo = Bool
    type AttrGetType EntryEditablePropertyInfo = Bool
    type AttrLabel EntryEditablePropertyInfo = "editable"
    type AttrOrigin EntryEditablePropertyInfo = Entry
    attrGet = getEntryEditable
    attrSet = setEntryEditable
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryEditable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.editable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:editable"
        })
#endif

-- VVV Prop "enable-emoji-completion"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@enable-emoji-completion@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #enableEmojiCompletion
-- @
getEntryEnableEmojiCompletion :: (MonadIO m, IsEntry o) => o -> m Bool
getEntryEnableEmojiCompletion obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "enable-emoji-completion"

-- | Set the value of the “@enable-emoji-completion@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #enableEmojiCompletion 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryEnableEmojiCompletion :: (MonadIO m, IsEntry o) => o -> Bool -> m ()
setEntryEnableEmojiCompletion obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "enable-emoji-completion" val

-- | Construct a `GValueConstruct` with valid value for the “@enable-emoji-completion@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryEnableEmojiCompletion :: (IsEntry o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryEnableEmojiCompletion val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "enable-emoji-completion" val

#if defined(ENABLE_OVERLOADING)
data EntryEnableEmojiCompletionPropertyInfo
instance AttrInfo EntryEnableEmojiCompletionPropertyInfo where
    type AttrAllowedOps EntryEnableEmojiCompletionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryEnableEmojiCompletionPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryEnableEmojiCompletionPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryEnableEmojiCompletionPropertyInfo = (~) Bool
    type AttrTransferType EntryEnableEmojiCompletionPropertyInfo = Bool
    type AttrGetType EntryEnableEmojiCompletionPropertyInfo = Bool
    type AttrLabel EntryEnableEmojiCompletionPropertyInfo = "enable-emoji-completion"
    type AttrOrigin EntryEnableEmojiCompletionPropertyInfo = Entry
    attrGet = getEntryEnableEmojiCompletion
    attrSet = setEntryEnableEmojiCompletion
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryEnableEmojiCompletion
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.enableEmojiCompletion"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:enableEmojiCompletion"
        })
#endif

-- VVV Prop "has-frame"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@has-frame@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #hasFrame
-- @
getEntryHasFrame :: (MonadIO m, IsEntry o) => o -> m Bool
getEntryHasFrame obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "has-frame"

-- | Set the value of the “@has-frame@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #hasFrame 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryHasFrame :: (MonadIO m, IsEntry o) => o -> Bool -> m ()
setEntryHasFrame obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "has-frame" val

-- | Construct a `GValueConstruct` with valid value for the “@has-frame@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryHasFrame :: (IsEntry o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryHasFrame val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "has-frame" val

#if defined(ENABLE_OVERLOADING)
data EntryHasFramePropertyInfo
instance AttrInfo EntryHasFramePropertyInfo where
    type AttrAllowedOps EntryHasFramePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryHasFramePropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryHasFramePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryHasFramePropertyInfo = (~) Bool
    type AttrTransferType EntryHasFramePropertyInfo = Bool
    type AttrGetType EntryHasFramePropertyInfo = Bool
    type AttrLabel EntryHasFramePropertyInfo = "has-frame"
    type AttrOrigin EntryHasFramePropertyInfo = Entry
    attrGet = getEntryHasFrame
    attrSet = setEntryHasFrame
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryHasFrame
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.hasFrame"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:hasFrame"
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
-- 'Data.GI.Base.Attributes.get' entry #imModule
-- @
getEntryImModule :: (MonadIO m, IsEntry o) => o -> m (Maybe T.Text)
getEntryImModule obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "im-module"

-- | Set the value of the “@im-module@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #imModule 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryImModule :: (MonadIO m, IsEntry o) => o -> T.Text -> m ()
setEntryImModule obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "im-module" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@im-module@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryImModule :: (IsEntry o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructEntryImModule val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "im-module" (P.Just val)

-- | Set the value of the “@im-module@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #imModule
-- @
clearEntryImModule :: (MonadIO m, IsEntry o) => o -> m ()
clearEntryImModule obj = liftIO $ B.Properties.setObjectPropertyString obj "im-module" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data EntryImModulePropertyInfo
instance AttrInfo EntryImModulePropertyInfo where
    type AttrAllowedOps EntryImModulePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntryImModulePropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryImModulePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint EntryImModulePropertyInfo = (~) T.Text
    type AttrTransferType EntryImModulePropertyInfo = T.Text
    type AttrGetType EntryImModulePropertyInfo = (Maybe T.Text)
    type AttrLabel EntryImModulePropertyInfo = "im-module"
    type AttrOrigin EntryImModulePropertyInfo = Entry
    attrGet = getEntryImModule
    attrSet = setEntryImModule
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryImModule
    attrClear = clearEntryImModule
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.imModule"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:imModule"
        })
#endif

-- VVV Prop "inner-border"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Border"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@inner-border@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #innerBorder
-- @
getEntryInnerBorder :: (MonadIO m, IsEntry o) => o -> m (Maybe Gtk.Border.Border)
getEntryInnerBorder obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "inner-border" Gtk.Border.Border

-- | Set the value of the “@inner-border@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #innerBorder 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryInnerBorder :: (MonadIO m, IsEntry o) => o -> Gtk.Border.Border -> m ()
setEntryInnerBorder obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "inner-border" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@inner-border@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryInnerBorder :: (IsEntry o, MIO.MonadIO m) => Gtk.Border.Border -> m (GValueConstruct o)
constructEntryInnerBorder val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "inner-border" (P.Just val)

-- | Set the value of the “@inner-border@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #innerBorder
-- @
clearEntryInnerBorder :: (MonadIO m, IsEntry o) => o -> m ()
clearEntryInnerBorder obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "inner-border" (Nothing :: Maybe Gtk.Border.Border)

#if defined(ENABLE_OVERLOADING)
data EntryInnerBorderPropertyInfo
instance AttrInfo EntryInnerBorderPropertyInfo where
    type AttrAllowedOps EntryInnerBorderPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntryInnerBorderPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryInnerBorderPropertyInfo = (~) Gtk.Border.Border
    type AttrTransferTypeConstraint EntryInnerBorderPropertyInfo = (~) Gtk.Border.Border
    type AttrTransferType EntryInnerBorderPropertyInfo = Gtk.Border.Border
    type AttrGetType EntryInnerBorderPropertyInfo = (Maybe Gtk.Border.Border)
    type AttrLabel EntryInnerBorderPropertyInfo = "inner-border"
    type AttrOrigin EntryInnerBorderPropertyInfo = Entry
    attrGet = getEntryInnerBorder
    attrSet = setEntryInnerBorder
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryInnerBorder
    attrClear = clearEntryInnerBorder
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.innerBorder"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:innerBorder"
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
-- 'Data.GI.Base.Attributes.get' entry #inputHints
-- @
getEntryInputHints :: (MonadIO m, IsEntry o) => o -> m [Gtk.Flags.InputHints]
getEntryInputHints obj = MIO.liftIO $ B.Properties.getObjectPropertyFlags obj "input-hints"

-- | Set the value of the “@input-hints@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #inputHints 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryInputHints :: (MonadIO m, IsEntry o) => o -> [Gtk.Flags.InputHints] -> m ()
setEntryInputHints obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFlags obj "input-hints" val

-- | Construct a `GValueConstruct` with valid value for the “@input-hints@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryInputHints :: (IsEntry o, MIO.MonadIO m) => [Gtk.Flags.InputHints] -> m (GValueConstruct o)
constructEntryInputHints val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFlags "input-hints" val

#if defined(ENABLE_OVERLOADING)
data EntryInputHintsPropertyInfo
instance AttrInfo EntryInputHintsPropertyInfo where
    type AttrAllowedOps EntryInputHintsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryInputHintsPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryInputHintsPropertyInfo = (~) [Gtk.Flags.InputHints]
    type AttrTransferTypeConstraint EntryInputHintsPropertyInfo = (~) [Gtk.Flags.InputHints]
    type AttrTransferType EntryInputHintsPropertyInfo = [Gtk.Flags.InputHints]
    type AttrGetType EntryInputHintsPropertyInfo = [Gtk.Flags.InputHints]
    type AttrLabel EntryInputHintsPropertyInfo = "input-hints"
    type AttrOrigin EntryInputHintsPropertyInfo = Entry
    attrGet = getEntryInputHints
    attrSet = setEntryInputHints
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryInputHints
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.inputHints"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:inputHints"
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
-- 'Data.GI.Base.Attributes.get' entry #inputPurpose
-- @
getEntryInputPurpose :: (MonadIO m, IsEntry o) => o -> m Gtk.Enums.InputPurpose
getEntryInputPurpose obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "input-purpose"

-- | Set the value of the “@input-purpose@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #inputPurpose 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryInputPurpose :: (MonadIO m, IsEntry o) => o -> Gtk.Enums.InputPurpose -> m ()
setEntryInputPurpose obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "input-purpose" val

-- | Construct a `GValueConstruct` with valid value for the “@input-purpose@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryInputPurpose :: (IsEntry o, MIO.MonadIO m) => Gtk.Enums.InputPurpose -> m (GValueConstruct o)
constructEntryInputPurpose val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "input-purpose" val

#if defined(ENABLE_OVERLOADING)
data EntryInputPurposePropertyInfo
instance AttrInfo EntryInputPurposePropertyInfo where
    type AttrAllowedOps EntryInputPurposePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryInputPurposePropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryInputPurposePropertyInfo = (~) Gtk.Enums.InputPurpose
    type AttrTransferTypeConstraint EntryInputPurposePropertyInfo = (~) Gtk.Enums.InputPurpose
    type AttrTransferType EntryInputPurposePropertyInfo = Gtk.Enums.InputPurpose
    type AttrGetType EntryInputPurposePropertyInfo = Gtk.Enums.InputPurpose
    type AttrLabel EntryInputPurposePropertyInfo = "input-purpose"
    type AttrOrigin EntryInputPurposePropertyInfo = Entry
    attrGet = getEntryInputPurpose
    attrSet = setEntryInputPurpose
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryInputPurpose
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.inputPurpose"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:inputPurpose"
        })
#endif

-- VVV Prop "invisible-char"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@invisible-char@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #invisibleChar
-- @
getEntryInvisibleChar :: (MonadIO m, IsEntry o) => o -> m Word32
getEntryInvisibleChar obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "invisible-char"

-- | Set the value of the “@invisible-char@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #invisibleChar 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryInvisibleChar :: (MonadIO m, IsEntry o) => o -> Word32 -> m ()
setEntryInvisibleChar obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "invisible-char" val

-- | Construct a `GValueConstruct` with valid value for the “@invisible-char@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryInvisibleChar :: (IsEntry o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructEntryInvisibleChar val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "invisible-char" val

#if defined(ENABLE_OVERLOADING)
data EntryInvisibleCharPropertyInfo
instance AttrInfo EntryInvisibleCharPropertyInfo where
    type AttrAllowedOps EntryInvisibleCharPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryInvisibleCharPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryInvisibleCharPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint EntryInvisibleCharPropertyInfo = (~) Word32
    type AttrTransferType EntryInvisibleCharPropertyInfo = Word32
    type AttrGetType EntryInvisibleCharPropertyInfo = Word32
    type AttrLabel EntryInvisibleCharPropertyInfo = "invisible-char"
    type AttrOrigin EntryInvisibleCharPropertyInfo = Entry
    attrGet = getEntryInvisibleChar
    attrSet = setEntryInvisibleChar
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryInvisibleChar
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.invisibleChar"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:invisibleChar"
        })
#endif

-- VVV Prop "invisible-char-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@invisible-char-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #invisibleCharSet
-- @
getEntryInvisibleCharSet :: (MonadIO m, IsEntry o) => o -> m Bool
getEntryInvisibleCharSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "invisible-char-set"

-- | Set the value of the “@invisible-char-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #invisibleCharSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryInvisibleCharSet :: (MonadIO m, IsEntry o) => o -> Bool -> m ()
setEntryInvisibleCharSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "invisible-char-set" val

-- | Construct a `GValueConstruct` with valid value for the “@invisible-char-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryInvisibleCharSet :: (IsEntry o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryInvisibleCharSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "invisible-char-set" val

#if defined(ENABLE_OVERLOADING)
data EntryInvisibleCharSetPropertyInfo
instance AttrInfo EntryInvisibleCharSetPropertyInfo where
    type AttrAllowedOps EntryInvisibleCharSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryInvisibleCharSetPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryInvisibleCharSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryInvisibleCharSetPropertyInfo = (~) Bool
    type AttrTransferType EntryInvisibleCharSetPropertyInfo = Bool
    type AttrGetType EntryInvisibleCharSetPropertyInfo = Bool
    type AttrLabel EntryInvisibleCharSetPropertyInfo = "invisible-char-set"
    type AttrOrigin EntryInvisibleCharSetPropertyInfo = Entry
    attrGet = getEntryInvisibleCharSet
    attrSet = setEntryInvisibleCharSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryInvisibleCharSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.invisibleCharSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:invisibleCharSet"
        })
#endif

-- VVV Prop "max-length"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@max-length@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #maxLength
-- @
getEntryMaxLength :: (MonadIO m, IsEntry o) => o -> m Int32
getEntryMaxLength obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "max-length"

-- | Set the value of the “@max-length@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #maxLength 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryMaxLength :: (MonadIO m, IsEntry o) => o -> Int32 -> m ()
setEntryMaxLength obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "max-length" val

-- | Construct a `GValueConstruct` with valid value for the “@max-length@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryMaxLength :: (IsEntry o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructEntryMaxLength val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "max-length" val

#if defined(ENABLE_OVERLOADING)
data EntryMaxLengthPropertyInfo
instance AttrInfo EntryMaxLengthPropertyInfo where
    type AttrAllowedOps EntryMaxLengthPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryMaxLengthPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryMaxLengthPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint EntryMaxLengthPropertyInfo = (~) Int32
    type AttrTransferType EntryMaxLengthPropertyInfo = Int32
    type AttrGetType EntryMaxLengthPropertyInfo = Int32
    type AttrLabel EntryMaxLengthPropertyInfo = "max-length"
    type AttrOrigin EntryMaxLengthPropertyInfo = Entry
    attrGet = getEntryMaxLength
    attrSet = setEntryMaxLength
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryMaxLength
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.maxLength"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:maxLength"
        })
#endif

-- VVV Prop "max-width-chars"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@max-width-chars@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #maxWidthChars
-- @
getEntryMaxWidthChars :: (MonadIO m, IsEntry o) => o -> m Int32
getEntryMaxWidthChars obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "max-width-chars"

-- | Set the value of the “@max-width-chars@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #maxWidthChars 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryMaxWidthChars :: (MonadIO m, IsEntry o) => o -> Int32 -> m ()
setEntryMaxWidthChars obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "max-width-chars" val

-- | Construct a `GValueConstruct` with valid value for the “@max-width-chars@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryMaxWidthChars :: (IsEntry o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructEntryMaxWidthChars val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "max-width-chars" val

#if defined(ENABLE_OVERLOADING)
data EntryMaxWidthCharsPropertyInfo
instance AttrInfo EntryMaxWidthCharsPropertyInfo where
    type AttrAllowedOps EntryMaxWidthCharsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryMaxWidthCharsPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryMaxWidthCharsPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint EntryMaxWidthCharsPropertyInfo = (~) Int32
    type AttrTransferType EntryMaxWidthCharsPropertyInfo = Int32
    type AttrGetType EntryMaxWidthCharsPropertyInfo = Int32
    type AttrLabel EntryMaxWidthCharsPropertyInfo = "max-width-chars"
    type AttrOrigin EntryMaxWidthCharsPropertyInfo = Entry
    attrGet = getEntryMaxWidthChars
    attrSet = setEntryMaxWidthChars
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryMaxWidthChars
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.maxWidthChars"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:maxWidthChars"
        })
#endif

-- VVV Prop "overwrite-mode"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@overwrite-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #overwriteMode
-- @
getEntryOverwriteMode :: (MonadIO m, IsEntry o) => o -> m Bool
getEntryOverwriteMode obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "overwrite-mode"

-- | Set the value of the “@overwrite-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #overwriteMode 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryOverwriteMode :: (MonadIO m, IsEntry o) => o -> Bool -> m ()
setEntryOverwriteMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "overwrite-mode" val

-- | Construct a `GValueConstruct` with valid value for the “@overwrite-mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryOverwriteMode :: (IsEntry o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryOverwriteMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "overwrite-mode" val

#if defined(ENABLE_OVERLOADING)
data EntryOverwriteModePropertyInfo
instance AttrInfo EntryOverwriteModePropertyInfo where
    type AttrAllowedOps EntryOverwriteModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryOverwriteModePropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryOverwriteModePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryOverwriteModePropertyInfo = (~) Bool
    type AttrTransferType EntryOverwriteModePropertyInfo = Bool
    type AttrGetType EntryOverwriteModePropertyInfo = Bool
    type AttrLabel EntryOverwriteModePropertyInfo = "overwrite-mode"
    type AttrOrigin EntryOverwriteModePropertyInfo = Entry
    attrGet = getEntryOverwriteMode
    attrSet = setEntryOverwriteMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryOverwriteMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.overwriteMode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:overwriteMode"
        })
#endif

-- VVV Prop "placeholder-text"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just True)

-- | Get the value of the “@placeholder-text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #placeholderText
-- @
getEntryPlaceholderText :: (MonadIO m, IsEntry o) => o -> m T.Text
getEntryPlaceholderText obj = MIO.liftIO $ checkUnexpectedNothing "getEntryPlaceholderText" $ B.Properties.getObjectPropertyString obj "placeholder-text"

-- | Set the value of the “@placeholder-text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #placeholderText 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryPlaceholderText :: (MonadIO m, IsEntry o) => o -> T.Text -> m ()
setEntryPlaceholderText obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "placeholder-text" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@placeholder-text@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryPlaceholderText :: (IsEntry o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructEntryPlaceholderText val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "placeholder-text" (P.Just val)

-- | Set the value of the “@placeholder-text@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #placeholderText
-- @
clearEntryPlaceholderText :: (MonadIO m, IsEntry o) => o -> m ()
clearEntryPlaceholderText obj = liftIO $ B.Properties.setObjectPropertyString obj "placeholder-text" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data EntryPlaceholderTextPropertyInfo
instance AttrInfo EntryPlaceholderTextPropertyInfo where
    type AttrAllowedOps EntryPlaceholderTextPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntryPlaceholderTextPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryPlaceholderTextPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint EntryPlaceholderTextPropertyInfo = (~) T.Text
    type AttrTransferType EntryPlaceholderTextPropertyInfo = T.Text
    type AttrGetType EntryPlaceholderTextPropertyInfo = T.Text
    type AttrLabel EntryPlaceholderTextPropertyInfo = "placeholder-text"
    type AttrOrigin EntryPlaceholderTextPropertyInfo = Entry
    attrGet = getEntryPlaceholderText
    attrSet = setEntryPlaceholderText
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryPlaceholderText
    attrClear = clearEntryPlaceholderText
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.placeholderText"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:placeholderText"
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
-- 'Data.GI.Base.Attributes.get' entry #populateAll
-- @
getEntryPopulateAll :: (MonadIO m, IsEntry o) => o -> m Bool
getEntryPopulateAll obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "populate-all"

-- | Set the value of the “@populate-all@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #populateAll 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryPopulateAll :: (MonadIO m, IsEntry o) => o -> Bool -> m ()
setEntryPopulateAll obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "populate-all" val

-- | Construct a `GValueConstruct` with valid value for the “@populate-all@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryPopulateAll :: (IsEntry o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryPopulateAll val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "populate-all" val

#if defined(ENABLE_OVERLOADING)
data EntryPopulateAllPropertyInfo
instance AttrInfo EntryPopulateAllPropertyInfo where
    type AttrAllowedOps EntryPopulateAllPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryPopulateAllPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryPopulateAllPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryPopulateAllPropertyInfo = (~) Bool
    type AttrTransferType EntryPopulateAllPropertyInfo = Bool
    type AttrGetType EntryPopulateAllPropertyInfo = Bool
    type AttrLabel EntryPopulateAllPropertyInfo = "populate-all"
    type AttrOrigin EntryPopulateAllPropertyInfo = Entry
    attrGet = getEntryPopulateAll
    attrSet = setEntryPopulateAll
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryPopulateAll
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.populateAll"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:populateAll"
        })
#endif

-- VVV Prop "primary-icon-activatable"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@primary-icon-activatable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #primaryIconActivatable
-- @
getEntryPrimaryIconActivatable :: (MonadIO m, IsEntry o) => o -> m Bool
getEntryPrimaryIconActivatable obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "primary-icon-activatable"

-- | Set the value of the “@primary-icon-activatable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #primaryIconActivatable 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryPrimaryIconActivatable :: (MonadIO m, IsEntry o) => o -> Bool -> m ()
setEntryPrimaryIconActivatable obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "primary-icon-activatable" val

-- | Construct a `GValueConstruct` with valid value for the “@primary-icon-activatable@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryPrimaryIconActivatable :: (IsEntry o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryPrimaryIconActivatable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "primary-icon-activatable" val

#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconActivatablePropertyInfo
instance AttrInfo EntryPrimaryIconActivatablePropertyInfo where
    type AttrAllowedOps EntryPrimaryIconActivatablePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryPrimaryIconActivatablePropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryPrimaryIconActivatablePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryPrimaryIconActivatablePropertyInfo = (~) Bool
    type AttrTransferType EntryPrimaryIconActivatablePropertyInfo = Bool
    type AttrGetType EntryPrimaryIconActivatablePropertyInfo = Bool
    type AttrLabel EntryPrimaryIconActivatablePropertyInfo = "primary-icon-activatable"
    type AttrOrigin EntryPrimaryIconActivatablePropertyInfo = Entry
    attrGet = getEntryPrimaryIconActivatable
    attrSet = setEntryPrimaryIconActivatable
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryPrimaryIconActivatable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.primaryIconActivatable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:primaryIconActivatable"
        })
#endif

-- VVV Prop "primary-icon-gicon"
   -- Type: TInterface (Name {namespace = "Gio", name = "Icon"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@primary-icon-gicon@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #primaryIconGicon
-- @
getEntryPrimaryIconGicon :: (MonadIO m, IsEntry o) => o -> m (Maybe Gio.Icon.Icon)
getEntryPrimaryIconGicon obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "primary-icon-gicon" Gio.Icon.Icon

-- | Set the value of the “@primary-icon-gicon@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #primaryIconGicon 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryPrimaryIconGicon :: (MonadIO m, IsEntry o, Gio.Icon.IsIcon a) => o -> a -> m ()
setEntryPrimaryIconGicon obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "primary-icon-gicon" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@primary-icon-gicon@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryPrimaryIconGicon :: (IsEntry o, MIO.MonadIO m, Gio.Icon.IsIcon a) => a -> m (GValueConstruct o)
constructEntryPrimaryIconGicon val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "primary-icon-gicon" (P.Just val)

-- | Set the value of the “@primary-icon-gicon@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #primaryIconGicon
-- @
clearEntryPrimaryIconGicon :: (MonadIO m, IsEntry o) => o -> m ()
clearEntryPrimaryIconGicon obj = liftIO $ B.Properties.setObjectPropertyObject obj "primary-icon-gicon" (Nothing :: Maybe Gio.Icon.Icon)

#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconGiconPropertyInfo
instance AttrInfo EntryPrimaryIconGiconPropertyInfo where
    type AttrAllowedOps EntryPrimaryIconGiconPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntryPrimaryIconGiconPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryPrimaryIconGiconPropertyInfo = Gio.Icon.IsIcon
    type AttrTransferTypeConstraint EntryPrimaryIconGiconPropertyInfo = Gio.Icon.IsIcon
    type AttrTransferType EntryPrimaryIconGiconPropertyInfo = Gio.Icon.Icon
    type AttrGetType EntryPrimaryIconGiconPropertyInfo = (Maybe Gio.Icon.Icon)
    type AttrLabel EntryPrimaryIconGiconPropertyInfo = "primary-icon-gicon"
    type AttrOrigin EntryPrimaryIconGiconPropertyInfo = Entry
    attrGet = getEntryPrimaryIconGicon
    attrSet = setEntryPrimaryIconGicon
    attrTransfer _ v = do
        unsafeCastTo Gio.Icon.Icon v
    attrConstruct = constructEntryPrimaryIconGicon
    attrClear = clearEntryPrimaryIconGicon
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.primaryIconGicon"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:primaryIconGicon"
        })
#endif

-- VVV Prop "primary-icon-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@primary-icon-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #primaryIconName
-- @
getEntryPrimaryIconName :: (MonadIO m, IsEntry o) => o -> m (Maybe T.Text)
getEntryPrimaryIconName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "primary-icon-name"

-- | Set the value of the “@primary-icon-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #primaryIconName 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryPrimaryIconName :: (MonadIO m, IsEntry o) => o -> T.Text -> m ()
setEntryPrimaryIconName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "primary-icon-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@primary-icon-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryPrimaryIconName :: (IsEntry o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructEntryPrimaryIconName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "primary-icon-name" (P.Just val)

-- | Set the value of the “@primary-icon-name@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #primaryIconName
-- @
clearEntryPrimaryIconName :: (MonadIO m, IsEntry o) => o -> m ()
clearEntryPrimaryIconName obj = liftIO $ B.Properties.setObjectPropertyString obj "primary-icon-name" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconNamePropertyInfo
instance AttrInfo EntryPrimaryIconNamePropertyInfo where
    type AttrAllowedOps EntryPrimaryIconNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntryPrimaryIconNamePropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryPrimaryIconNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint EntryPrimaryIconNamePropertyInfo = (~) T.Text
    type AttrTransferType EntryPrimaryIconNamePropertyInfo = T.Text
    type AttrGetType EntryPrimaryIconNamePropertyInfo = (Maybe T.Text)
    type AttrLabel EntryPrimaryIconNamePropertyInfo = "primary-icon-name"
    type AttrOrigin EntryPrimaryIconNamePropertyInfo = Entry
    attrGet = getEntryPrimaryIconName
    attrSet = setEntryPrimaryIconName
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryPrimaryIconName
    attrClear = clearEntryPrimaryIconName
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.primaryIconName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:primaryIconName"
        })
#endif

-- VVV Prop "primary-icon-pixbuf"
   -- Type: TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@primary-icon-pixbuf@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #primaryIconPixbuf
-- @
getEntryPrimaryIconPixbuf :: (MonadIO m, IsEntry o) => o -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)
getEntryPrimaryIconPixbuf obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "primary-icon-pixbuf" GdkPixbuf.Pixbuf.Pixbuf

-- | Set the value of the “@primary-icon-pixbuf@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #primaryIconPixbuf 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryPrimaryIconPixbuf :: (MonadIO m, IsEntry o, GdkPixbuf.Pixbuf.IsPixbuf a) => o -> a -> m ()
setEntryPrimaryIconPixbuf obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "primary-icon-pixbuf" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@primary-icon-pixbuf@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryPrimaryIconPixbuf :: (IsEntry o, MIO.MonadIO m, GdkPixbuf.Pixbuf.IsPixbuf a) => a -> m (GValueConstruct o)
constructEntryPrimaryIconPixbuf val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "primary-icon-pixbuf" (P.Just val)

-- | Set the value of the “@primary-icon-pixbuf@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #primaryIconPixbuf
-- @
clearEntryPrimaryIconPixbuf :: (MonadIO m, IsEntry o) => o -> m ()
clearEntryPrimaryIconPixbuf obj = liftIO $ B.Properties.setObjectPropertyObject obj "primary-icon-pixbuf" (Nothing :: Maybe GdkPixbuf.Pixbuf.Pixbuf)

#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconPixbufPropertyInfo
instance AttrInfo EntryPrimaryIconPixbufPropertyInfo where
    type AttrAllowedOps EntryPrimaryIconPixbufPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntryPrimaryIconPixbufPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryPrimaryIconPixbufPropertyInfo = GdkPixbuf.Pixbuf.IsPixbuf
    type AttrTransferTypeConstraint EntryPrimaryIconPixbufPropertyInfo = GdkPixbuf.Pixbuf.IsPixbuf
    type AttrTransferType EntryPrimaryIconPixbufPropertyInfo = GdkPixbuf.Pixbuf.Pixbuf
    type AttrGetType EntryPrimaryIconPixbufPropertyInfo = (Maybe GdkPixbuf.Pixbuf.Pixbuf)
    type AttrLabel EntryPrimaryIconPixbufPropertyInfo = "primary-icon-pixbuf"
    type AttrOrigin EntryPrimaryIconPixbufPropertyInfo = Entry
    attrGet = getEntryPrimaryIconPixbuf
    attrSet = setEntryPrimaryIconPixbuf
    attrTransfer _ v = do
        unsafeCastTo GdkPixbuf.Pixbuf.Pixbuf v
    attrConstruct = constructEntryPrimaryIconPixbuf
    attrClear = clearEntryPrimaryIconPixbuf
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.primaryIconPixbuf"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:primaryIconPixbuf"
        })
#endif

-- VVV Prop "primary-icon-sensitive"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@primary-icon-sensitive@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #primaryIconSensitive
-- @
getEntryPrimaryIconSensitive :: (MonadIO m, IsEntry o) => o -> m Bool
getEntryPrimaryIconSensitive obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "primary-icon-sensitive"

-- | Set the value of the “@primary-icon-sensitive@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #primaryIconSensitive 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryPrimaryIconSensitive :: (MonadIO m, IsEntry o) => o -> Bool -> m ()
setEntryPrimaryIconSensitive obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "primary-icon-sensitive" val

-- | Construct a `GValueConstruct` with valid value for the “@primary-icon-sensitive@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryPrimaryIconSensitive :: (IsEntry o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryPrimaryIconSensitive val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "primary-icon-sensitive" val

#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconSensitivePropertyInfo
instance AttrInfo EntryPrimaryIconSensitivePropertyInfo where
    type AttrAllowedOps EntryPrimaryIconSensitivePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryPrimaryIconSensitivePropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryPrimaryIconSensitivePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryPrimaryIconSensitivePropertyInfo = (~) Bool
    type AttrTransferType EntryPrimaryIconSensitivePropertyInfo = Bool
    type AttrGetType EntryPrimaryIconSensitivePropertyInfo = Bool
    type AttrLabel EntryPrimaryIconSensitivePropertyInfo = "primary-icon-sensitive"
    type AttrOrigin EntryPrimaryIconSensitivePropertyInfo = Entry
    attrGet = getEntryPrimaryIconSensitive
    attrSet = setEntryPrimaryIconSensitive
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryPrimaryIconSensitive
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.primaryIconSensitive"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:primaryIconSensitive"
        })
#endif

-- VVV Prop "primary-icon-stock"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@primary-icon-stock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #primaryIconStock
-- @
getEntryPrimaryIconStock :: (MonadIO m, IsEntry o) => o -> m (Maybe T.Text)
getEntryPrimaryIconStock obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "primary-icon-stock"

-- | Set the value of the “@primary-icon-stock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #primaryIconStock 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryPrimaryIconStock :: (MonadIO m, IsEntry o) => o -> T.Text -> m ()
setEntryPrimaryIconStock obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "primary-icon-stock" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@primary-icon-stock@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryPrimaryIconStock :: (IsEntry o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructEntryPrimaryIconStock val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "primary-icon-stock" (P.Just val)

-- | Set the value of the “@primary-icon-stock@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #primaryIconStock
-- @
clearEntryPrimaryIconStock :: (MonadIO m, IsEntry o) => o -> m ()
clearEntryPrimaryIconStock obj = liftIO $ B.Properties.setObjectPropertyString obj "primary-icon-stock" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconStockPropertyInfo
instance AttrInfo EntryPrimaryIconStockPropertyInfo where
    type AttrAllowedOps EntryPrimaryIconStockPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntryPrimaryIconStockPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryPrimaryIconStockPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint EntryPrimaryIconStockPropertyInfo = (~) T.Text
    type AttrTransferType EntryPrimaryIconStockPropertyInfo = T.Text
    type AttrGetType EntryPrimaryIconStockPropertyInfo = (Maybe T.Text)
    type AttrLabel EntryPrimaryIconStockPropertyInfo = "primary-icon-stock"
    type AttrOrigin EntryPrimaryIconStockPropertyInfo = Entry
    attrGet = getEntryPrimaryIconStock
    attrSet = setEntryPrimaryIconStock
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryPrimaryIconStock
    attrClear = clearEntryPrimaryIconStock
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.primaryIconStock"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:primaryIconStock"
        })
#endif

-- VVV Prop "primary-icon-storage-type"
   -- Type: TInterface (Name {namespace = "Gtk", name = "ImageType"})
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@primary-icon-storage-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #primaryIconStorageType
-- @
getEntryPrimaryIconStorageType :: (MonadIO m, IsEntry o) => o -> m Gtk.Enums.ImageType
getEntryPrimaryIconStorageType obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "primary-icon-storage-type"

#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconStorageTypePropertyInfo
instance AttrInfo EntryPrimaryIconStorageTypePropertyInfo where
    type AttrAllowedOps EntryPrimaryIconStorageTypePropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint EntryPrimaryIconStorageTypePropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryPrimaryIconStorageTypePropertyInfo = (~) ()
    type AttrTransferTypeConstraint EntryPrimaryIconStorageTypePropertyInfo = (~) ()
    type AttrTransferType EntryPrimaryIconStorageTypePropertyInfo = ()
    type AttrGetType EntryPrimaryIconStorageTypePropertyInfo = Gtk.Enums.ImageType
    type AttrLabel EntryPrimaryIconStorageTypePropertyInfo = "primary-icon-storage-type"
    type AttrOrigin EntryPrimaryIconStorageTypePropertyInfo = Entry
    attrGet = getEntryPrimaryIconStorageType
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.primaryIconStorageType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:primaryIconStorageType"
        })
#endif

-- VVV Prop "primary-icon-tooltip-markup"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@primary-icon-tooltip-markup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #primaryIconTooltipMarkup
-- @
getEntryPrimaryIconTooltipMarkup :: (MonadIO m, IsEntry o) => o -> m (Maybe T.Text)
getEntryPrimaryIconTooltipMarkup obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "primary-icon-tooltip-markup"

-- | Set the value of the “@primary-icon-tooltip-markup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #primaryIconTooltipMarkup 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryPrimaryIconTooltipMarkup :: (MonadIO m, IsEntry o) => o -> T.Text -> m ()
setEntryPrimaryIconTooltipMarkup obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "primary-icon-tooltip-markup" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@primary-icon-tooltip-markup@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryPrimaryIconTooltipMarkup :: (IsEntry o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructEntryPrimaryIconTooltipMarkup val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "primary-icon-tooltip-markup" (P.Just val)

-- | Set the value of the “@primary-icon-tooltip-markup@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #primaryIconTooltipMarkup
-- @
clearEntryPrimaryIconTooltipMarkup :: (MonadIO m, IsEntry o) => o -> m ()
clearEntryPrimaryIconTooltipMarkup obj = liftIO $ B.Properties.setObjectPropertyString obj "primary-icon-tooltip-markup" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconTooltipMarkupPropertyInfo
instance AttrInfo EntryPrimaryIconTooltipMarkupPropertyInfo where
    type AttrAllowedOps EntryPrimaryIconTooltipMarkupPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntryPrimaryIconTooltipMarkupPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryPrimaryIconTooltipMarkupPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint EntryPrimaryIconTooltipMarkupPropertyInfo = (~) T.Text
    type AttrTransferType EntryPrimaryIconTooltipMarkupPropertyInfo = T.Text
    type AttrGetType EntryPrimaryIconTooltipMarkupPropertyInfo = (Maybe T.Text)
    type AttrLabel EntryPrimaryIconTooltipMarkupPropertyInfo = "primary-icon-tooltip-markup"
    type AttrOrigin EntryPrimaryIconTooltipMarkupPropertyInfo = Entry
    attrGet = getEntryPrimaryIconTooltipMarkup
    attrSet = setEntryPrimaryIconTooltipMarkup
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryPrimaryIconTooltipMarkup
    attrClear = clearEntryPrimaryIconTooltipMarkup
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.primaryIconTooltipMarkup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:primaryIconTooltipMarkup"
        })
#endif

-- VVV Prop "primary-icon-tooltip-text"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@primary-icon-tooltip-text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #primaryIconTooltipText
-- @
getEntryPrimaryIconTooltipText :: (MonadIO m, IsEntry o) => o -> m (Maybe T.Text)
getEntryPrimaryIconTooltipText obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "primary-icon-tooltip-text"

-- | Set the value of the “@primary-icon-tooltip-text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #primaryIconTooltipText 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryPrimaryIconTooltipText :: (MonadIO m, IsEntry o) => o -> T.Text -> m ()
setEntryPrimaryIconTooltipText obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "primary-icon-tooltip-text" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@primary-icon-tooltip-text@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryPrimaryIconTooltipText :: (IsEntry o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructEntryPrimaryIconTooltipText val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "primary-icon-tooltip-text" (P.Just val)

-- | Set the value of the “@primary-icon-tooltip-text@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #primaryIconTooltipText
-- @
clearEntryPrimaryIconTooltipText :: (MonadIO m, IsEntry o) => o -> m ()
clearEntryPrimaryIconTooltipText obj = liftIO $ B.Properties.setObjectPropertyString obj "primary-icon-tooltip-text" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data EntryPrimaryIconTooltipTextPropertyInfo
instance AttrInfo EntryPrimaryIconTooltipTextPropertyInfo where
    type AttrAllowedOps EntryPrimaryIconTooltipTextPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntryPrimaryIconTooltipTextPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryPrimaryIconTooltipTextPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint EntryPrimaryIconTooltipTextPropertyInfo = (~) T.Text
    type AttrTransferType EntryPrimaryIconTooltipTextPropertyInfo = T.Text
    type AttrGetType EntryPrimaryIconTooltipTextPropertyInfo = (Maybe T.Text)
    type AttrLabel EntryPrimaryIconTooltipTextPropertyInfo = "primary-icon-tooltip-text"
    type AttrOrigin EntryPrimaryIconTooltipTextPropertyInfo = Entry
    attrGet = getEntryPrimaryIconTooltipText
    attrSet = setEntryPrimaryIconTooltipText
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryPrimaryIconTooltipText
    attrClear = clearEntryPrimaryIconTooltipText
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.primaryIconTooltipText"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:primaryIconTooltipText"
        })
#endif

-- VVV Prop "progress-fraction"
   -- Type: TBasicType TDouble
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@progress-fraction@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #progressFraction
-- @
getEntryProgressFraction :: (MonadIO m, IsEntry o) => o -> m Double
getEntryProgressFraction obj = MIO.liftIO $ B.Properties.getObjectPropertyDouble obj "progress-fraction"

-- | Set the value of the “@progress-fraction@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #progressFraction 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryProgressFraction :: (MonadIO m, IsEntry o) => o -> Double -> m ()
setEntryProgressFraction obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyDouble obj "progress-fraction" val

-- | Construct a `GValueConstruct` with valid value for the “@progress-fraction@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryProgressFraction :: (IsEntry o, MIO.MonadIO m) => Double -> m (GValueConstruct o)
constructEntryProgressFraction val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyDouble "progress-fraction" val

#if defined(ENABLE_OVERLOADING)
data EntryProgressFractionPropertyInfo
instance AttrInfo EntryProgressFractionPropertyInfo where
    type AttrAllowedOps EntryProgressFractionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryProgressFractionPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryProgressFractionPropertyInfo = (~) Double
    type AttrTransferTypeConstraint EntryProgressFractionPropertyInfo = (~) Double
    type AttrTransferType EntryProgressFractionPropertyInfo = Double
    type AttrGetType EntryProgressFractionPropertyInfo = Double
    type AttrLabel EntryProgressFractionPropertyInfo = "progress-fraction"
    type AttrOrigin EntryProgressFractionPropertyInfo = Entry
    attrGet = getEntryProgressFraction
    attrSet = setEntryProgressFraction
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryProgressFraction
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.progressFraction"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:progressFraction"
        })
#endif

-- VVV Prop "progress-pulse-step"
   -- Type: TBasicType TDouble
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@progress-pulse-step@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #progressPulseStep
-- @
getEntryProgressPulseStep :: (MonadIO m, IsEntry o) => o -> m Double
getEntryProgressPulseStep obj = MIO.liftIO $ B.Properties.getObjectPropertyDouble obj "progress-pulse-step"

-- | Set the value of the “@progress-pulse-step@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #progressPulseStep 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryProgressPulseStep :: (MonadIO m, IsEntry o) => o -> Double -> m ()
setEntryProgressPulseStep obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyDouble obj "progress-pulse-step" val

-- | Construct a `GValueConstruct` with valid value for the “@progress-pulse-step@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryProgressPulseStep :: (IsEntry o, MIO.MonadIO m) => Double -> m (GValueConstruct o)
constructEntryProgressPulseStep val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyDouble "progress-pulse-step" val

#if defined(ENABLE_OVERLOADING)
data EntryProgressPulseStepPropertyInfo
instance AttrInfo EntryProgressPulseStepPropertyInfo where
    type AttrAllowedOps EntryProgressPulseStepPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryProgressPulseStepPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryProgressPulseStepPropertyInfo = (~) Double
    type AttrTransferTypeConstraint EntryProgressPulseStepPropertyInfo = (~) Double
    type AttrTransferType EntryProgressPulseStepPropertyInfo = Double
    type AttrGetType EntryProgressPulseStepPropertyInfo = Double
    type AttrLabel EntryProgressPulseStepPropertyInfo = "progress-pulse-step"
    type AttrOrigin EntryProgressPulseStepPropertyInfo = Entry
    attrGet = getEntryProgressPulseStep
    attrSet = setEntryProgressPulseStep
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryProgressPulseStep
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.progressPulseStep"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:progressPulseStep"
        })
#endif

-- VVV Prop "scroll-offset"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@scroll-offset@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #scrollOffset
-- @
getEntryScrollOffset :: (MonadIO m, IsEntry o) => o -> m Int32
getEntryScrollOffset obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "scroll-offset"

#if defined(ENABLE_OVERLOADING)
data EntryScrollOffsetPropertyInfo
instance AttrInfo EntryScrollOffsetPropertyInfo where
    type AttrAllowedOps EntryScrollOffsetPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint EntryScrollOffsetPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryScrollOffsetPropertyInfo = (~) ()
    type AttrTransferTypeConstraint EntryScrollOffsetPropertyInfo = (~) ()
    type AttrTransferType EntryScrollOffsetPropertyInfo = ()
    type AttrGetType EntryScrollOffsetPropertyInfo = Int32
    type AttrLabel EntryScrollOffsetPropertyInfo = "scroll-offset"
    type AttrOrigin EntryScrollOffsetPropertyInfo = Entry
    attrGet = getEntryScrollOffset
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.scrollOffset"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:scrollOffset"
        })
#endif

-- VVV Prop "secondary-icon-activatable"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@secondary-icon-activatable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #secondaryIconActivatable
-- @
getEntrySecondaryIconActivatable :: (MonadIO m, IsEntry o) => o -> m Bool
getEntrySecondaryIconActivatable obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "secondary-icon-activatable"

-- | Set the value of the “@secondary-icon-activatable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #secondaryIconActivatable 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntrySecondaryIconActivatable :: (MonadIO m, IsEntry o) => o -> Bool -> m ()
setEntrySecondaryIconActivatable obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "secondary-icon-activatable" val

-- | Construct a `GValueConstruct` with valid value for the “@secondary-icon-activatable@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntrySecondaryIconActivatable :: (IsEntry o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntrySecondaryIconActivatable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "secondary-icon-activatable" val

#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconActivatablePropertyInfo
instance AttrInfo EntrySecondaryIconActivatablePropertyInfo where
    type AttrAllowedOps EntrySecondaryIconActivatablePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntrySecondaryIconActivatablePropertyInfo = IsEntry
    type AttrSetTypeConstraint EntrySecondaryIconActivatablePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntrySecondaryIconActivatablePropertyInfo = (~) Bool
    type AttrTransferType EntrySecondaryIconActivatablePropertyInfo = Bool
    type AttrGetType EntrySecondaryIconActivatablePropertyInfo = Bool
    type AttrLabel EntrySecondaryIconActivatablePropertyInfo = "secondary-icon-activatable"
    type AttrOrigin EntrySecondaryIconActivatablePropertyInfo = Entry
    attrGet = getEntrySecondaryIconActivatable
    attrSet = setEntrySecondaryIconActivatable
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntrySecondaryIconActivatable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.secondaryIconActivatable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:secondaryIconActivatable"
        })
#endif

-- VVV Prop "secondary-icon-gicon"
   -- Type: TInterface (Name {namespace = "Gio", name = "Icon"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@secondary-icon-gicon@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #secondaryIconGicon
-- @
getEntrySecondaryIconGicon :: (MonadIO m, IsEntry o) => o -> m (Maybe Gio.Icon.Icon)
getEntrySecondaryIconGicon obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "secondary-icon-gicon" Gio.Icon.Icon

-- | Set the value of the “@secondary-icon-gicon@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #secondaryIconGicon 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntrySecondaryIconGicon :: (MonadIO m, IsEntry o, Gio.Icon.IsIcon a) => o -> a -> m ()
setEntrySecondaryIconGicon obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "secondary-icon-gicon" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@secondary-icon-gicon@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntrySecondaryIconGicon :: (IsEntry o, MIO.MonadIO m, Gio.Icon.IsIcon a) => a -> m (GValueConstruct o)
constructEntrySecondaryIconGicon val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "secondary-icon-gicon" (P.Just val)

-- | Set the value of the “@secondary-icon-gicon@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #secondaryIconGicon
-- @
clearEntrySecondaryIconGicon :: (MonadIO m, IsEntry o) => o -> m ()
clearEntrySecondaryIconGicon obj = liftIO $ B.Properties.setObjectPropertyObject obj "secondary-icon-gicon" (Nothing :: Maybe Gio.Icon.Icon)

#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconGiconPropertyInfo
instance AttrInfo EntrySecondaryIconGiconPropertyInfo where
    type AttrAllowedOps EntrySecondaryIconGiconPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntrySecondaryIconGiconPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntrySecondaryIconGiconPropertyInfo = Gio.Icon.IsIcon
    type AttrTransferTypeConstraint EntrySecondaryIconGiconPropertyInfo = Gio.Icon.IsIcon
    type AttrTransferType EntrySecondaryIconGiconPropertyInfo = Gio.Icon.Icon
    type AttrGetType EntrySecondaryIconGiconPropertyInfo = (Maybe Gio.Icon.Icon)
    type AttrLabel EntrySecondaryIconGiconPropertyInfo = "secondary-icon-gicon"
    type AttrOrigin EntrySecondaryIconGiconPropertyInfo = Entry
    attrGet = getEntrySecondaryIconGicon
    attrSet = setEntrySecondaryIconGicon
    attrTransfer _ v = do
        unsafeCastTo Gio.Icon.Icon v
    attrConstruct = constructEntrySecondaryIconGicon
    attrClear = clearEntrySecondaryIconGicon
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.secondaryIconGicon"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:secondaryIconGicon"
        })
#endif

-- VVV Prop "secondary-icon-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@secondary-icon-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #secondaryIconName
-- @
getEntrySecondaryIconName :: (MonadIO m, IsEntry o) => o -> m (Maybe T.Text)
getEntrySecondaryIconName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "secondary-icon-name"

-- | Set the value of the “@secondary-icon-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #secondaryIconName 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntrySecondaryIconName :: (MonadIO m, IsEntry o) => o -> T.Text -> m ()
setEntrySecondaryIconName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "secondary-icon-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@secondary-icon-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntrySecondaryIconName :: (IsEntry o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructEntrySecondaryIconName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "secondary-icon-name" (P.Just val)

-- | Set the value of the “@secondary-icon-name@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #secondaryIconName
-- @
clearEntrySecondaryIconName :: (MonadIO m, IsEntry o) => o -> m ()
clearEntrySecondaryIconName obj = liftIO $ B.Properties.setObjectPropertyString obj "secondary-icon-name" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconNamePropertyInfo
instance AttrInfo EntrySecondaryIconNamePropertyInfo where
    type AttrAllowedOps EntrySecondaryIconNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntrySecondaryIconNamePropertyInfo = IsEntry
    type AttrSetTypeConstraint EntrySecondaryIconNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint EntrySecondaryIconNamePropertyInfo = (~) T.Text
    type AttrTransferType EntrySecondaryIconNamePropertyInfo = T.Text
    type AttrGetType EntrySecondaryIconNamePropertyInfo = (Maybe T.Text)
    type AttrLabel EntrySecondaryIconNamePropertyInfo = "secondary-icon-name"
    type AttrOrigin EntrySecondaryIconNamePropertyInfo = Entry
    attrGet = getEntrySecondaryIconName
    attrSet = setEntrySecondaryIconName
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntrySecondaryIconName
    attrClear = clearEntrySecondaryIconName
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.secondaryIconName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:secondaryIconName"
        })
#endif

-- VVV Prop "secondary-icon-pixbuf"
   -- Type: TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@secondary-icon-pixbuf@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #secondaryIconPixbuf
-- @
getEntrySecondaryIconPixbuf :: (MonadIO m, IsEntry o) => o -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)
getEntrySecondaryIconPixbuf obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "secondary-icon-pixbuf" GdkPixbuf.Pixbuf.Pixbuf

-- | Set the value of the “@secondary-icon-pixbuf@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #secondaryIconPixbuf 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntrySecondaryIconPixbuf :: (MonadIO m, IsEntry o, GdkPixbuf.Pixbuf.IsPixbuf a) => o -> a -> m ()
setEntrySecondaryIconPixbuf obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "secondary-icon-pixbuf" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@secondary-icon-pixbuf@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntrySecondaryIconPixbuf :: (IsEntry o, MIO.MonadIO m, GdkPixbuf.Pixbuf.IsPixbuf a) => a -> m (GValueConstruct o)
constructEntrySecondaryIconPixbuf val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "secondary-icon-pixbuf" (P.Just val)

-- | Set the value of the “@secondary-icon-pixbuf@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #secondaryIconPixbuf
-- @
clearEntrySecondaryIconPixbuf :: (MonadIO m, IsEntry o) => o -> m ()
clearEntrySecondaryIconPixbuf obj = liftIO $ B.Properties.setObjectPropertyObject obj "secondary-icon-pixbuf" (Nothing :: Maybe GdkPixbuf.Pixbuf.Pixbuf)

#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconPixbufPropertyInfo
instance AttrInfo EntrySecondaryIconPixbufPropertyInfo where
    type AttrAllowedOps EntrySecondaryIconPixbufPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntrySecondaryIconPixbufPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntrySecondaryIconPixbufPropertyInfo = GdkPixbuf.Pixbuf.IsPixbuf
    type AttrTransferTypeConstraint EntrySecondaryIconPixbufPropertyInfo = GdkPixbuf.Pixbuf.IsPixbuf
    type AttrTransferType EntrySecondaryIconPixbufPropertyInfo = GdkPixbuf.Pixbuf.Pixbuf
    type AttrGetType EntrySecondaryIconPixbufPropertyInfo = (Maybe GdkPixbuf.Pixbuf.Pixbuf)
    type AttrLabel EntrySecondaryIconPixbufPropertyInfo = "secondary-icon-pixbuf"
    type AttrOrigin EntrySecondaryIconPixbufPropertyInfo = Entry
    attrGet = getEntrySecondaryIconPixbuf
    attrSet = setEntrySecondaryIconPixbuf
    attrTransfer _ v = do
        unsafeCastTo GdkPixbuf.Pixbuf.Pixbuf v
    attrConstruct = constructEntrySecondaryIconPixbuf
    attrClear = clearEntrySecondaryIconPixbuf
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.secondaryIconPixbuf"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:secondaryIconPixbuf"
        })
#endif

-- VVV Prop "secondary-icon-sensitive"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@secondary-icon-sensitive@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #secondaryIconSensitive
-- @
getEntrySecondaryIconSensitive :: (MonadIO m, IsEntry o) => o -> m Bool
getEntrySecondaryIconSensitive obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "secondary-icon-sensitive"

-- | Set the value of the “@secondary-icon-sensitive@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #secondaryIconSensitive 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntrySecondaryIconSensitive :: (MonadIO m, IsEntry o) => o -> Bool -> m ()
setEntrySecondaryIconSensitive obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "secondary-icon-sensitive" val

-- | Construct a `GValueConstruct` with valid value for the “@secondary-icon-sensitive@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntrySecondaryIconSensitive :: (IsEntry o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntrySecondaryIconSensitive val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "secondary-icon-sensitive" val

#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconSensitivePropertyInfo
instance AttrInfo EntrySecondaryIconSensitivePropertyInfo where
    type AttrAllowedOps EntrySecondaryIconSensitivePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntrySecondaryIconSensitivePropertyInfo = IsEntry
    type AttrSetTypeConstraint EntrySecondaryIconSensitivePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntrySecondaryIconSensitivePropertyInfo = (~) Bool
    type AttrTransferType EntrySecondaryIconSensitivePropertyInfo = Bool
    type AttrGetType EntrySecondaryIconSensitivePropertyInfo = Bool
    type AttrLabel EntrySecondaryIconSensitivePropertyInfo = "secondary-icon-sensitive"
    type AttrOrigin EntrySecondaryIconSensitivePropertyInfo = Entry
    attrGet = getEntrySecondaryIconSensitive
    attrSet = setEntrySecondaryIconSensitive
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntrySecondaryIconSensitive
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.secondaryIconSensitive"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:secondaryIconSensitive"
        })
#endif

-- VVV Prop "secondary-icon-stock"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@secondary-icon-stock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #secondaryIconStock
-- @
getEntrySecondaryIconStock :: (MonadIO m, IsEntry o) => o -> m (Maybe T.Text)
getEntrySecondaryIconStock obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "secondary-icon-stock"

-- | Set the value of the “@secondary-icon-stock@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #secondaryIconStock 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntrySecondaryIconStock :: (MonadIO m, IsEntry o) => o -> T.Text -> m ()
setEntrySecondaryIconStock obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "secondary-icon-stock" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@secondary-icon-stock@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntrySecondaryIconStock :: (IsEntry o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructEntrySecondaryIconStock val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "secondary-icon-stock" (P.Just val)

-- | Set the value of the “@secondary-icon-stock@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #secondaryIconStock
-- @
clearEntrySecondaryIconStock :: (MonadIO m, IsEntry o) => o -> m ()
clearEntrySecondaryIconStock obj = liftIO $ B.Properties.setObjectPropertyString obj "secondary-icon-stock" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconStockPropertyInfo
instance AttrInfo EntrySecondaryIconStockPropertyInfo where
    type AttrAllowedOps EntrySecondaryIconStockPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntrySecondaryIconStockPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntrySecondaryIconStockPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint EntrySecondaryIconStockPropertyInfo = (~) T.Text
    type AttrTransferType EntrySecondaryIconStockPropertyInfo = T.Text
    type AttrGetType EntrySecondaryIconStockPropertyInfo = (Maybe T.Text)
    type AttrLabel EntrySecondaryIconStockPropertyInfo = "secondary-icon-stock"
    type AttrOrigin EntrySecondaryIconStockPropertyInfo = Entry
    attrGet = getEntrySecondaryIconStock
    attrSet = setEntrySecondaryIconStock
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntrySecondaryIconStock
    attrClear = clearEntrySecondaryIconStock
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.secondaryIconStock"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:secondaryIconStock"
        })
#endif

-- VVV Prop "secondary-icon-storage-type"
   -- Type: TInterface (Name {namespace = "Gtk", name = "ImageType"})
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@secondary-icon-storage-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #secondaryIconStorageType
-- @
getEntrySecondaryIconStorageType :: (MonadIO m, IsEntry o) => o -> m Gtk.Enums.ImageType
getEntrySecondaryIconStorageType obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "secondary-icon-storage-type"

#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconStorageTypePropertyInfo
instance AttrInfo EntrySecondaryIconStorageTypePropertyInfo where
    type AttrAllowedOps EntrySecondaryIconStorageTypePropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint EntrySecondaryIconStorageTypePropertyInfo = IsEntry
    type AttrSetTypeConstraint EntrySecondaryIconStorageTypePropertyInfo = (~) ()
    type AttrTransferTypeConstraint EntrySecondaryIconStorageTypePropertyInfo = (~) ()
    type AttrTransferType EntrySecondaryIconStorageTypePropertyInfo = ()
    type AttrGetType EntrySecondaryIconStorageTypePropertyInfo = Gtk.Enums.ImageType
    type AttrLabel EntrySecondaryIconStorageTypePropertyInfo = "secondary-icon-storage-type"
    type AttrOrigin EntrySecondaryIconStorageTypePropertyInfo = Entry
    attrGet = getEntrySecondaryIconStorageType
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.secondaryIconStorageType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:secondaryIconStorageType"
        })
#endif

-- VVV Prop "secondary-icon-tooltip-markup"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@secondary-icon-tooltip-markup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #secondaryIconTooltipMarkup
-- @
getEntrySecondaryIconTooltipMarkup :: (MonadIO m, IsEntry o) => o -> m (Maybe T.Text)
getEntrySecondaryIconTooltipMarkup obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "secondary-icon-tooltip-markup"

-- | Set the value of the “@secondary-icon-tooltip-markup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #secondaryIconTooltipMarkup 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntrySecondaryIconTooltipMarkup :: (MonadIO m, IsEntry o) => o -> T.Text -> m ()
setEntrySecondaryIconTooltipMarkup obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "secondary-icon-tooltip-markup" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@secondary-icon-tooltip-markup@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntrySecondaryIconTooltipMarkup :: (IsEntry o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructEntrySecondaryIconTooltipMarkup val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "secondary-icon-tooltip-markup" (P.Just val)

-- | Set the value of the “@secondary-icon-tooltip-markup@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #secondaryIconTooltipMarkup
-- @
clearEntrySecondaryIconTooltipMarkup :: (MonadIO m, IsEntry o) => o -> m ()
clearEntrySecondaryIconTooltipMarkup obj = liftIO $ B.Properties.setObjectPropertyString obj "secondary-icon-tooltip-markup" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconTooltipMarkupPropertyInfo
instance AttrInfo EntrySecondaryIconTooltipMarkupPropertyInfo where
    type AttrAllowedOps EntrySecondaryIconTooltipMarkupPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntrySecondaryIconTooltipMarkupPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntrySecondaryIconTooltipMarkupPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint EntrySecondaryIconTooltipMarkupPropertyInfo = (~) T.Text
    type AttrTransferType EntrySecondaryIconTooltipMarkupPropertyInfo = T.Text
    type AttrGetType EntrySecondaryIconTooltipMarkupPropertyInfo = (Maybe T.Text)
    type AttrLabel EntrySecondaryIconTooltipMarkupPropertyInfo = "secondary-icon-tooltip-markup"
    type AttrOrigin EntrySecondaryIconTooltipMarkupPropertyInfo = Entry
    attrGet = getEntrySecondaryIconTooltipMarkup
    attrSet = setEntrySecondaryIconTooltipMarkup
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntrySecondaryIconTooltipMarkup
    attrClear = clearEntrySecondaryIconTooltipMarkup
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.secondaryIconTooltipMarkup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:secondaryIconTooltipMarkup"
        })
#endif

-- VVV Prop "secondary-icon-tooltip-text"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@secondary-icon-tooltip-text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #secondaryIconTooltipText
-- @
getEntrySecondaryIconTooltipText :: (MonadIO m, IsEntry o) => o -> m (Maybe T.Text)
getEntrySecondaryIconTooltipText obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "secondary-icon-tooltip-text"

-- | Set the value of the “@secondary-icon-tooltip-text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #secondaryIconTooltipText 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntrySecondaryIconTooltipText :: (MonadIO m, IsEntry o) => o -> T.Text -> m ()
setEntrySecondaryIconTooltipText obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "secondary-icon-tooltip-text" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@secondary-icon-tooltip-text@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntrySecondaryIconTooltipText :: (IsEntry o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructEntrySecondaryIconTooltipText val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "secondary-icon-tooltip-text" (P.Just val)

-- | Set the value of the “@secondary-icon-tooltip-text@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #secondaryIconTooltipText
-- @
clearEntrySecondaryIconTooltipText :: (MonadIO m, IsEntry o) => o -> m ()
clearEntrySecondaryIconTooltipText obj = liftIO $ B.Properties.setObjectPropertyString obj "secondary-icon-tooltip-text" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data EntrySecondaryIconTooltipTextPropertyInfo
instance AttrInfo EntrySecondaryIconTooltipTextPropertyInfo where
    type AttrAllowedOps EntrySecondaryIconTooltipTextPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntrySecondaryIconTooltipTextPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntrySecondaryIconTooltipTextPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint EntrySecondaryIconTooltipTextPropertyInfo = (~) T.Text
    type AttrTransferType EntrySecondaryIconTooltipTextPropertyInfo = T.Text
    type AttrGetType EntrySecondaryIconTooltipTextPropertyInfo = (Maybe T.Text)
    type AttrLabel EntrySecondaryIconTooltipTextPropertyInfo = "secondary-icon-tooltip-text"
    type AttrOrigin EntrySecondaryIconTooltipTextPropertyInfo = Entry
    attrGet = getEntrySecondaryIconTooltipText
    attrSet = setEntrySecondaryIconTooltipText
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntrySecondaryIconTooltipText
    attrClear = clearEntrySecondaryIconTooltipText
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.secondaryIconTooltipText"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:secondaryIconTooltipText"
        })
#endif

-- VVV Prop "selection-bound"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@selection-bound@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #selectionBound
-- @
getEntrySelectionBound :: (MonadIO m, IsEntry o) => o -> m Int32
getEntrySelectionBound obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "selection-bound"

#if defined(ENABLE_OVERLOADING)
data EntrySelectionBoundPropertyInfo
instance AttrInfo EntrySelectionBoundPropertyInfo where
    type AttrAllowedOps EntrySelectionBoundPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint EntrySelectionBoundPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntrySelectionBoundPropertyInfo = (~) ()
    type AttrTransferTypeConstraint EntrySelectionBoundPropertyInfo = (~) ()
    type AttrTransferType EntrySelectionBoundPropertyInfo = ()
    type AttrGetType EntrySelectionBoundPropertyInfo = Int32
    type AttrLabel EntrySelectionBoundPropertyInfo = "selection-bound"
    type AttrOrigin EntrySelectionBoundPropertyInfo = Entry
    attrGet = getEntrySelectionBound
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.selectionBound"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:selectionBound"
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
-- 'Data.GI.Base.Attributes.get' entry #shadowType
-- @
getEntryShadowType :: (MonadIO m, IsEntry o) => o -> m Gtk.Enums.ShadowType
getEntryShadowType obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "shadow-type"

-- | Set the value of the “@shadow-type@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #shadowType 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryShadowType :: (MonadIO m, IsEntry o) => o -> Gtk.Enums.ShadowType -> m ()
setEntryShadowType obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "shadow-type" val

-- | Construct a `GValueConstruct` with valid value for the “@shadow-type@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryShadowType :: (IsEntry o, MIO.MonadIO m) => Gtk.Enums.ShadowType -> m (GValueConstruct o)
constructEntryShadowType val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "shadow-type" val

#if defined(ENABLE_OVERLOADING)
data EntryShadowTypePropertyInfo
instance AttrInfo EntryShadowTypePropertyInfo where
    type AttrAllowedOps EntryShadowTypePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryShadowTypePropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryShadowTypePropertyInfo = (~) Gtk.Enums.ShadowType
    type AttrTransferTypeConstraint EntryShadowTypePropertyInfo = (~) Gtk.Enums.ShadowType
    type AttrTransferType EntryShadowTypePropertyInfo = Gtk.Enums.ShadowType
    type AttrGetType EntryShadowTypePropertyInfo = Gtk.Enums.ShadowType
    type AttrLabel EntryShadowTypePropertyInfo = "shadow-type"
    type AttrOrigin EntryShadowTypePropertyInfo = Entry
    attrGet = getEntryShadowType
    attrSet = setEntryShadowType
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryShadowType
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.shadowType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:shadowType"
        })
#endif

-- VVV Prop "show-emoji-icon"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@show-emoji-icon@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #showEmojiIcon
-- @
getEntryShowEmojiIcon :: (MonadIO m, IsEntry o) => o -> m Bool
getEntryShowEmojiIcon obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-emoji-icon"

-- | Set the value of the “@show-emoji-icon@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #showEmojiIcon 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryShowEmojiIcon :: (MonadIO m, IsEntry o) => o -> Bool -> m ()
setEntryShowEmojiIcon obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-emoji-icon" val

-- | Construct a `GValueConstruct` with valid value for the “@show-emoji-icon@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryShowEmojiIcon :: (IsEntry o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryShowEmojiIcon val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-emoji-icon" val

#if defined(ENABLE_OVERLOADING)
data EntryShowEmojiIconPropertyInfo
instance AttrInfo EntryShowEmojiIconPropertyInfo where
    type AttrAllowedOps EntryShowEmojiIconPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryShowEmojiIconPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryShowEmojiIconPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryShowEmojiIconPropertyInfo = (~) Bool
    type AttrTransferType EntryShowEmojiIconPropertyInfo = Bool
    type AttrGetType EntryShowEmojiIconPropertyInfo = Bool
    type AttrLabel EntryShowEmojiIconPropertyInfo = "show-emoji-icon"
    type AttrOrigin EntryShowEmojiIconPropertyInfo = Entry
    attrGet = getEntryShowEmojiIcon
    attrSet = setEntryShowEmojiIcon
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryShowEmojiIcon
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.showEmojiIcon"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:showEmojiIcon"
        })
#endif

-- VVV Prop "tabs"
   -- Type: TInterface (Name {namespace = "Pango", name = "TabArray"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just False)

-- | Get the value of the “@tabs@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #tabs
-- @
getEntryTabs :: (MonadIO m, IsEntry o) => o -> m (Maybe Pango.TabArray.TabArray)
getEntryTabs obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "tabs" Pango.TabArray.TabArray

-- | Set the value of the “@tabs@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #tabs 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryTabs :: (MonadIO m, IsEntry o) => o -> Pango.TabArray.TabArray -> m ()
setEntryTabs obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "tabs" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@tabs@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryTabs :: (IsEntry o, MIO.MonadIO m) => Pango.TabArray.TabArray -> m (GValueConstruct o)
constructEntryTabs val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "tabs" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data EntryTabsPropertyInfo
instance AttrInfo EntryTabsPropertyInfo where
    type AttrAllowedOps EntryTabsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryTabsPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryTabsPropertyInfo = (~) Pango.TabArray.TabArray
    type AttrTransferTypeConstraint EntryTabsPropertyInfo = (~) Pango.TabArray.TabArray
    type AttrTransferType EntryTabsPropertyInfo = Pango.TabArray.TabArray
    type AttrGetType EntryTabsPropertyInfo = (Maybe Pango.TabArray.TabArray)
    type AttrLabel EntryTabsPropertyInfo = "tabs"
    type AttrOrigin EntryTabsPropertyInfo = Entry
    attrGet = getEntryTabs
    attrSet = setEntryTabs
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryTabs
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.tabs"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:tabs"
        })
#endif

-- VVV Prop "text"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #text
-- @
getEntryText :: (MonadIO m, IsEntry o) => o -> m T.Text
getEntryText obj = MIO.liftIO $ checkUnexpectedNothing "getEntryText" $ B.Properties.getObjectPropertyString obj "text"

-- | Set the value of the “@text@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #text 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryText :: (MonadIO m, IsEntry o) => o -> T.Text -> m ()
setEntryText obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "text" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@text@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryText :: (IsEntry o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructEntryText val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "text" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data EntryTextPropertyInfo
instance AttrInfo EntryTextPropertyInfo where
    type AttrAllowedOps EntryTextPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryTextPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryTextPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint EntryTextPropertyInfo = (~) T.Text
    type AttrTransferType EntryTextPropertyInfo = T.Text
    type AttrGetType EntryTextPropertyInfo = T.Text
    type AttrLabel EntryTextPropertyInfo = "text"
    type AttrOrigin EntryTextPropertyInfo = Entry
    attrGet = getEntryText
    attrSet = setEntryText
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryText
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.text"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:text"
        })
#endif

-- VVV Prop "text-length"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@text-length@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #textLength
-- @
getEntryTextLength :: (MonadIO m, IsEntry o) => o -> m Word32
getEntryTextLength obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "text-length"

#if defined(ENABLE_OVERLOADING)
data EntryTextLengthPropertyInfo
instance AttrInfo EntryTextLengthPropertyInfo where
    type AttrAllowedOps EntryTextLengthPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint EntryTextLengthPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryTextLengthPropertyInfo = (~) ()
    type AttrTransferTypeConstraint EntryTextLengthPropertyInfo = (~) ()
    type AttrTransferType EntryTextLengthPropertyInfo = ()
    type AttrGetType EntryTextLengthPropertyInfo = Word32
    type AttrLabel EntryTextLengthPropertyInfo = "text-length"
    type AttrOrigin EntryTextLengthPropertyInfo = Entry
    attrGet = getEntryTextLength
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.textLength"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:textLength"
        })
#endif

-- VVV Prop "truncate-multiline"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@truncate-multiline@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #truncateMultiline
-- @
getEntryTruncateMultiline :: (MonadIO m, IsEntry o) => o -> m Bool
getEntryTruncateMultiline obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "truncate-multiline"

-- | Set the value of the “@truncate-multiline@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #truncateMultiline 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryTruncateMultiline :: (MonadIO m, IsEntry o) => o -> Bool -> m ()
setEntryTruncateMultiline obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "truncate-multiline" val

-- | Construct a `GValueConstruct` with valid value for the “@truncate-multiline@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryTruncateMultiline :: (IsEntry o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryTruncateMultiline val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "truncate-multiline" val

#if defined(ENABLE_OVERLOADING)
data EntryTruncateMultilinePropertyInfo
instance AttrInfo EntryTruncateMultilinePropertyInfo where
    type AttrAllowedOps EntryTruncateMultilinePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryTruncateMultilinePropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryTruncateMultilinePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryTruncateMultilinePropertyInfo = (~) Bool
    type AttrTransferType EntryTruncateMultilinePropertyInfo = Bool
    type AttrGetType EntryTruncateMultilinePropertyInfo = Bool
    type AttrLabel EntryTruncateMultilinePropertyInfo = "truncate-multiline"
    type AttrOrigin EntryTruncateMultilinePropertyInfo = Entry
    attrGet = getEntryTruncateMultiline
    attrSet = setEntryTruncateMultiline
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryTruncateMultiline
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.truncateMultiline"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:truncateMultiline"
        })
#endif

-- VVV Prop "visibility"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@visibility@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #visibility
-- @
getEntryVisibility :: (MonadIO m, IsEntry o) => o -> m Bool
getEntryVisibility obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "visibility"

-- | Set the value of the “@visibility@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #visibility 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryVisibility :: (MonadIO m, IsEntry o) => o -> Bool -> m ()
setEntryVisibility obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "visibility" val

-- | Construct a `GValueConstruct` with valid value for the “@visibility@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryVisibility :: (IsEntry o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryVisibility val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "visibility" val

#if defined(ENABLE_OVERLOADING)
data EntryVisibilityPropertyInfo
instance AttrInfo EntryVisibilityPropertyInfo where
    type AttrAllowedOps EntryVisibilityPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryVisibilityPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryVisibilityPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryVisibilityPropertyInfo = (~) Bool
    type AttrTransferType EntryVisibilityPropertyInfo = Bool
    type AttrGetType EntryVisibilityPropertyInfo = Bool
    type AttrLabel EntryVisibilityPropertyInfo = "visibility"
    type AttrOrigin EntryVisibilityPropertyInfo = Entry
    attrGet = getEntryVisibility
    attrSet = setEntryVisibility
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryVisibility
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.visibility"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:visibility"
        })
#endif

-- VVV Prop "width-chars"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@width-chars@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #widthChars
-- @
getEntryWidthChars :: (MonadIO m, IsEntry o) => o -> m Int32
getEntryWidthChars obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "width-chars"

-- | Set the value of the “@width-chars@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #widthChars 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryWidthChars :: (MonadIO m, IsEntry o) => o -> Int32 -> m ()
setEntryWidthChars obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "width-chars" val

-- | Construct a `GValueConstruct` with valid value for the “@width-chars@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryWidthChars :: (IsEntry o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructEntryWidthChars val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "width-chars" val

#if defined(ENABLE_OVERLOADING)
data EntryWidthCharsPropertyInfo
instance AttrInfo EntryWidthCharsPropertyInfo where
    type AttrAllowedOps EntryWidthCharsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryWidthCharsPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryWidthCharsPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint EntryWidthCharsPropertyInfo = (~) Int32
    type AttrTransferType EntryWidthCharsPropertyInfo = Int32
    type AttrGetType EntryWidthCharsPropertyInfo = Int32
    type AttrLabel EntryWidthCharsPropertyInfo = "width-chars"
    type AttrOrigin EntryWidthCharsPropertyInfo = Entry
    attrGet = getEntryWidthChars
    attrSet = setEntryWidthChars
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryWidthChars
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.widthChars"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:widthChars"
        })
#endif

-- VVV Prop "xalign"
   -- Type: TBasicType TFloat
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@xalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entry #xalign
-- @
getEntryXalign :: (MonadIO m, IsEntry o) => o -> m Float
getEntryXalign obj = MIO.liftIO $ B.Properties.getObjectPropertyFloat obj "xalign"

-- | Set the value of the “@xalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entry [ #xalign 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryXalign :: (MonadIO m, IsEntry o) => o -> Float -> m ()
setEntryXalign obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFloat obj "xalign" val

-- | Construct a `GValueConstruct` with valid value for the “@xalign@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryXalign :: (IsEntry o, MIO.MonadIO m) => Float -> m (GValueConstruct o)
constructEntryXalign val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFloat "xalign" val

#if defined(ENABLE_OVERLOADING)
data EntryXalignPropertyInfo
instance AttrInfo EntryXalignPropertyInfo where
    type AttrAllowedOps EntryXalignPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryXalignPropertyInfo = IsEntry
    type AttrSetTypeConstraint EntryXalignPropertyInfo = (~) Float
    type AttrTransferTypeConstraint EntryXalignPropertyInfo = (~) Float
    type AttrTransferType EntryXalignPropertyInfo = Float
    type AttrGetType EntryXalignPropertyInfo = Float
    type AttrLabel EntryXalignPropertyInfo = "xalign"
    type AttrOrigin EntryXalignPropertyInfo = Entry
    attrGet = getEntryXalign
    attrSet = setEntryXalign
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryXalign
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.xalign"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#g:attr:xalign"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Entry
type instance O.AttributeList Entry = EntryAttributeList
type EntryAttributeList = ('[ '("activatesDefault", EntryActivatesDefaultPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("attributes", EntryAttributesPropertyInfo), '("buffer", EntryBufferPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("capsLockWarning", EntryCapsLockWarningPropertyInfo), '("completion", EntryCompletionPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("cursorPosition", EntryCursorPositionPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("editable", EntryEditablePropertyInfo), '("editingCanceled", Gtk.CellEditable.CellEditableEditingCanceledPropertyInfo), '("enableEmojiCompletion", EntryEnableEmojiCompletionPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasFrame", EntryHasFramePropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("imModule", EntryImModulePropertyInfo), '("innerBorder", EntryInnerBorderPropertyInfo), '("inputHints", EntryInputHintsPropertyInfo), '("inputPurpose", EntryInputPurposePropertyInfo), '("invisibleChar", EntryInvisibleCharPropertyInfo), '("invisibleCharSet", EntryInvisibleCharSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("maxLength", EntryMaxLengthPropertyInfo), '("maxWidthChars", EntryMaxWidthCharsPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("overwriteMode", EntryOverwriteModePropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("placeholderText", EntryPlaceholderTextPropertyInfo), '("populateAll", EntryPopulateAllPropertyInfo), '("primaryIconActivatable", EntryPrimaryIconActivatablePropertyInfo), '("primaryIconGicon", EntryPrimaryIconGiconPropertyInfo), '("primaryIconName", EntryPrimaryIconNamePropertyInfo), '("primaryIconPixbuf", EntryPrimaryIconPixbufPropertyInfo), '("primaryIconSensitive", EntryPrimaryIconSensitivePropertyInfo), '("primaryIconStock", EntryPrimaryIconStockPropertyInfo), '("primaryIconStorageType", EntryPrimaryIconStorageTypePropertyInfo), '("primaryIconTooltipMarkup", EntryPrimaryIconTooltipMarkupPropertyInfo), '("primaryIconTooltipText", EntryPrimaryIconTooltipTextPropertyInfo), '("progressFraction", EntryProgressFractionPropertyInfo), '("progressPulseStep", EntryProgressPulseStepPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("scrollOffset", EntryScrollOffsetPropertyInfo), '("secondaryIconActivatable", EntrySecondaryIconActivatablePropertyInfo), '("secondaryIconGicon", EntrySecondaryIconGiconPropertyInfo), '("secondaryIconName", EntrySecondaryIconNamePropertyInfo), '("secondaryIconPixbuf", EntrySecondaryIconPixbufPropertyInfo), '("secondaryIconSensitive", EntrySecondaryIconSensitivePropertyInfo), '("secondaryIconStock", EntrySecondaryIconStockPropertyInfo), '("secondaryIconStorageType", EntrySecondaryIconStorageTypePropertyInfo), '("secondaryIconTooltipMarkup", EntrySecondaryIconTooltipMarkupPropertyInfo), '("secondaryIconTooltipText", EntrySecondaryIconTooltipTextPropertyInfo), '("selectionBound", EntrySelectionBoundPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("shadowType", EntryShadowTypePropertyInfo), '("showEmojiIcon", EntryShowEmojiIconPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tabs", EntryTabsPropertyInfo), '("text", EntryTextPropertyInfo), '("textLength", EntryTextLengthPropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("truncateMultiline", EntryTruncateMultilinePropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visibility", EntryVisibilityPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthChars", EntryWidthCharsPropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("xalign", EntryXalignPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
entryActivatesDefault :: AttrLabelProxy "activatesDefault"
entryActivatesDefault = AttrLabelProxy

entryAttributes :: AttrLabelProxy "attributes"
entryAttributes = AttrLabelProxy

entryBuffer :: AttrLabelProxy "buffer"
entryBuffer = AttrLabelProxy

entryCapsLockWarning :: AttrLabelProxy "capsLockWarning"
entryCapsLockWarning = AttrLabelProxy

entryCompletion :: AttrLabelProxy "completion"
entryCompletion = AttrLabelProxy

entryCursorPosition :: AttrLabelProxy "cursorPosition"
entryCursorPosition = AttrLabelProxy

entryEditable :: AttrLabelProxy "editable"
entryEditable = AttrLabelProxy

entryEnableEmojiCompletion :: AttrLabelProxy "enableEmojiCompletion"
entryEnableEmojiCompletion = AttrLabelProxy

entryHasFrame :: AttrLabelProxy "hasFrame"
entryHasFrame = AttrLabelProxy

entryImModule :: AttrLabelProxy "imModule"
entryImModule = AttrLabelProxy

entryInnerBorder :: AttrLabelProxy "innerBorder"
entryInnerBorder = AttrLabelProxy

entryInputHints :: AttrLabelProxy "inputHints"
entryInputHints = AttrLabelProxy

entryInputPurpose :: AttrLabelProxy "inputPurpose"
entryInputPurpose = AttrLabelProxy

entryInvisibleChar :: AttrLabelProxy "invisibleChar"
entryInvisibleChar = AttrLabelProxy

entryInvisibleCharSet :: AttrLabelProxy "invisibleCharSet"
entryInvisibleCharSet = AttrLabelProxy

entryMaxLength :: AttrLabelProxy "maxLength"
entryMaxLength = AttrLabelProxy

entryMaxWidthChars :: AttrLabelProxy "maxWidthChars"
entryMaxWidthChars = AttrLabelProxy

entryOverwriteMode :: AttrLabelProxy "overwriteMode"
entryOverwriteMode = AttrLabelProxy

entryPlaceholderText :: AttrLabelProxy "placeholderText"
entryPlaceholderText = AttrLabelProxy

entryPopulateAll :: AttrLabelProxy "populateAll"
entryPopulateAll = AttrLabelProxy

entryPrimaryIconActivatable :: AttrLabelProxy "primaryIconActivatable"
entryPrimaryIconActivatable = AttrLabelProxy

entryPrimaryIconGicon :: AttrLabelProxy "primaryIconGicon"
entryPrimaryIconGicon = AttrLabelProxy

entryPrimaryIconName :: AttrLabelProxy "primaryIconName"
entryPrimaryIconName = AttrLabelProxy

entryPrimaryIconPixbuf :: AttrLabelProxy "primaryIconPixbuf"
entryPrimaryIconPixbuf = AttrLabelProxy

entryPrimaryIconSensitive :: AttrLabelProxy "primaryIconSensitive"
entryPrimaryIconSensitive = AttrLabelProxy

entryPrimaryIconStock :: AttrLabelProxy "primaryIconStock"
entryPrimaryIconStock = AttrLabelProxy

entryPrimaryIconStorageType :: AttrLabelProxy "primaryIconStorageType"
entryPrimaryIconStorageType = AttrLabelProxy

entryPrimaryIconTooltipMarkup :: AttrLabelProxy "primaryIconTooltipMarkup"
entryPrimaryIconTooltipMarkup = AttrLabelProxy

entryPrimaryIconTooltipText :: AttrLabelProxy "primaryIconTooltipText"
entryPrimaryIconTooltipText = AttrLabelProxy

entryProgressFraction :: AttrLabelProxy "progressFraction"
entryProgressFraction = AttrLabelProxy

entryProgressPulseStep :: AttrLabelProxy "progressPulseStep"
entryProgressPulseStep = AttrLabelProxy

entryScrollOffset :: AttrLabelProxy "scrollOffset"
entryScrollOffset = AttrLabelProxy

entrySecondaryIconActivatable :: AttrLabelProxy "secondaryIconActivatable"
entrySecondaryIconActivatable = AttrLabelProxy

entrySecondaryIconGicon :: AttrLabelProxy "secondaryIconGicon"
entrySecondaryIconGicon = AttrLabelProxy

entrySecondaryIconName :: AttrLabelProxy "secondaryIconName"
entrySecondaryIconName = AttrLabelProxy

entrySecondaryIconPixbuf :: AttrLabelProxy "secondaryIconPixbuf"
entrySecondaryIconPixbuf = AttrLabelProxy

entrySecondaryIconSensitive :: AttrLabelProxy "secondaryIconSensitive"
entrySecondaryIconSensitive = AttrLabelProxy

entrySecondaryIconStock :: AttrLabelProxy "secondaryIconStock"
entrySecondaryIconStock = AttrLabelProxy

entrySecondaryIconStorageType :: AttrLabelProxy "secondaryIconStorageType"
entrySecondaryIconStorageType = AttrLabelProxy

entrySecondaryIconTooltipMarkup :: AttrLabelProxy "secondaryIconTooltipMarkup"
entrySecondaryIconTooltipMarkup = AttrLabelProxy

entrySecondaryIconTooltipText :: AttrLabelProxy "secondaryIconTooltipText"
entrySecondaryIconTooltipText = AttrLabelProxy

entrySelectionBound :: AttrLabelProxy "selectionBound"
entrySelectionBound = AttrLabelProxy

entryShadowType :: AttrLabelProxy "shadowType"
entryShadowType = AttrLabelProxy

entryShowEmojiIcon :: AttrLabelProxy "showEmojiIcon"
entryShowEmojiIcon = AttrLabelProxy

entryTabs :: AttrLabelProxy "tabs"
entryTabs = AttrLabelProxy

entryText :: AttrLabelProxy "text"
entryText = AttrLabelProxy

entryTextLength :: AttrLabelProxy "textLength"
entryTextLength = AttrLabelProxy

entryTruncateMultiline :: AttrLabelProxy "truncateMultiline"
entryTruncateMultiline = AttrLabelProxy

entryVisibility :: AttrLabelProxy "visibility"
entryVisibility = AttrLabelProxy

entryWidthChars :: AttrLabelProxy "widthChars"
entryWidthChars = AttrLabelProxy

entryXalign :: AttrLabelProxy "xalign"
entryXalign = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Entry = EntrySignalList
type EntrySignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activate", EntryActivateSignalInfo), '("backspace", EntryBackspaceSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("changed", Gtk.Editable.EditableChangedSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("copyClipboard", EntryCopyClipboardSignalInfo), '("cutClipboard", EntryCutClipboardSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("deleteFromCursor", EntryDeleteFromCursorSignalInfo), '("deleteText", Gtk.Editable.EditableDeleteTextSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("editingDone", Gtk.CellEditable.CellEditableEditingDoneSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("iconPress", EntryIconPressSignalInfo), '("iconRelease", EntryIconReleaseSignalInfo), '("insertAtCursor", EntryInsertAtCursorSignalInfo), '("insertEmoji", EntryInsertEmojiSignalInfo), '("insertText", Gtk.Editable.EditableInsertTextSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveCursor", EntryMoveCursorSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("pasteClipboard", EntryPasteClipboardSignalInfo), '("populatePopup", EntryPopulatePopupSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("preeditChanged", EntryPreeditChangedSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("removeWidget", Gtk.CellEditable.CellEditableRemoveWidgetSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("toggleOverwrite", EntryToggleOverwriteSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Entry::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Entry" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_new" gtk_entry_new :: 
    IO (Ptr Entry)

-- | Creates a new entry.
entryNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Entry
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Entry.Entry'.
entryNew  = liftIO $ do
    result <- gtk_entry_new
    checkUnexpectedReturnNULL "entryNew" result
    result' <- (newObject Entry) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Entry::new_with_buffer
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The buffer to use for the new #GtkEntry."
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Entry" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_new_with_buffer" gtk_entry_new_with_buffer :: 
    Ptr Gtk.EntryBuffer.EntryBuffer ->      -- buffer : TInterface (Name {namespace = "Gtk", name = "EntryBuffer"})
    IO (Ptr Entry)

-- | Creates a new entry with the specified text buffer.
-- 
-- /Since: 2.18/
entryNewWithBuffer ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.EntryBuffer.IsEntryBuffer a) =>
    a
    -- ^ /@buffer@/: The buffer to use for the new t'GI.Gtk.Objects.Entry.Entry'.
    -> m Entry
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Entry.Entry'
entryNewWithBuffer buffer = liftIO $ do
    buffer' <- unsafeManagedPtrCastPtr buffer
    result <- gtk_entry_new_with_buffer buffer'
    checkUnexpectedReturnNULL "entryNewWithBuffer" result
    result' <- (newObject Entry) result
    touchManagedPtr buffer
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Entry::get_activates_default
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_activates_default" gtk_entry_get_activates_default :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO CInt

-- | Retrieves the value set by 'GI.Gtk.Objects.Entry.entrySetActivatesDefault'.
entryGetActivatesDefault ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the entry will activate the default widget
entryGetActivatesDefault entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_activates_default entry'
    let result' = (/= 0) result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetActivatesDefaultMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetActivatesDefaultMethodInfo a signature where
    overloadedMethod = entryGetActivatesDefault

instance O.OverloadedMethodInfo EntryGetActivatesDefaultMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetActivatesDefault",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetActivatesDefault"
        })


#endif

-- method Entry::get_alignment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TFloat)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_get_alignment" gtk_entry_get_alignment :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO CFloat

-- | Gets the value set by 'GI.Gtk.Objects.Entry.entrySetAlignment'.
-- 
-- /Since: 2.4/
entryGetAlignment ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m Float
    -- ^ __Returns:__ the alignment
entryGetAlignment entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_alignment entry'
    let result' = realToFrac result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetAlignmentMethodInfo
instance (signature ~ (m Float), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetAlignmentMethodInfo a signature where
    overloadedMethod = entryGetAlignment

instance O.OverloadedMethodInfo EntryGetAlignmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetAlignment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetAlignment"
        })


#endif

-- method Entry::get_attributes
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Pango" , name = "AttrList" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_get_attributes" gtk_entry_get_attributes :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO (Ptr Pango.AttrList.AttrList)

-- | Gets the attribute list that was set on the entry using
-- 'GI.Gtk.Objects.Entry.entrySetAttributes', if any.
-- 
-- /Since: 3.6/
entryGetAttributes ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m (Maybe Pango.AttrList.AttrList)
    -- ^ __Returns:__ the attribute list, or 'P.Nothing'
    --     if none was set.
entryGetAttributes entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_attributes entry'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newBoxed Pango.AttrList.AttrList) result'
        return result''
    touchManagedPtr entry
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data EntryGetAttributesMethodInfo
instance (signature ~ (m (Maybe Pango.AttrList.AttrList)), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetAttributesMethodInfo a signature where
    overloadedMethod = entryGetAttributes

instance O.OverloadedMethodInfo EntryGetAttributesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetAttributes",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetAttributes"
        })


#endif

-- method Entry::get_buffer
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "EntryBuffer" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_get_buffer" gtk_entry_get_buffer :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO (Ptr Gtk.EntryBuffer.EntryBuffer)

-- | Get the t'GI.Gtk.Objects.EntryBuffer.EntryBuffer' object which holds the text for
-- this widget.
-- 
-- /Since: 2.18/
entryGetBuffer ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m Gtk.EntryBuffer.EntryBuffer
    -- ^ __Returns:__ A t'GI.Gtk.Objects.EntryBuffer.EntryBuffer' object.
entryGetBuffer entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_buffer entry'
    checkUnexpectedReturnNULL "entryGetBuffer" result
    result' <- (newObject Gtk.EntryBuffer.EntryBuffer) result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetBufferMethodInfo
instance (signature ~ (m Gtk.EntryBuffer.EntryBuffer), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetBufferMethodInfo a signature where
    overloadedMethod = entryGetBuffer

instance O.OverloadedMethodInfo EntryGetBufferMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetBuffer",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetBuffer"
        })


#endif

-- method Entry::get_completion
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "EntryCompletion" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_get_completion" gtk_entry_get_completion :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO (Ptr Gtk.EntryCompletion.EntryCompletion)

-- | Returns the auxiliary completion object currently in use by /@entry@/.
-- 
-- /Since: 2.4/
entryGetCompletion ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: A t'GI.Gtk.Objects.Entry.Entry'
    -> m Gtk.EntryCompletion.EntryCompletion
    -- ^ __Returns:__ The auxiliary completion object currently
    --     in use by /@entry@/.
entryGetCompletion entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_completion entry'
    checkUnexpectedReturnNULL "entryGetCompletion" result
    result' <- (newObject Gtk.EntryCompletion.EntryCompletion) result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetCompletionMethodInfo
instance (signature ~ (m Gtk.EntryCompletion.EntryCompletion), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetCompletionMethodInfo a signature where
    overloadedMethod = entryGetCompletion

instance O.OverloadedMethodInfo EntryGetCompletionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetCompletion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetCompletion"
        })


#endif

-- method Entry::get_current_icon_drag_source
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_current_icon_drag_source" gtk_entry_get_current_icon_drag_source :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO Int32

-- | Returns the index of the icon which is the source of the current
-- DND operation, or -1.
-- 
-- This function is meant to be used in a [Widget::dragDataGet]("GI.Gtk.Objects.Widget#g:signal:dragDataGet")
-- callback.
-- 
-- /Since: 2.16/
entryGetCurrentIconDragSource ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m Int32
    -- ^ __Returns:__ index of the icon which is the source of the current
    --          DND operation, or -1.
entryGetCurrentIconDragSource entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_current_icon_drag_source entry'
    touchManagedPtr entry
    return result

#if defined(ENABLE_OVERLOADING)
data EntryGetCurrentIconDragSourceMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetCurrentIconDragSourceMethodInfo a signature where
    overloadedMethod = entryGetCurrentIconDragSource

instance O.OverloadedMethodInfo EntryGetCurrentIconDragSourceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetCurrentIconDragSource",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetCurrentIconDragSource"
        })


#endif

-- method Entry::get_cursor_hadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_cursor_hadjustment" gtk_entry_get_cursor_hadjustment :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO (Ptr Gtk.Adjustment.Adjustment)

-- | Retrieves the horizontal cursor adjustment for the entry.
-- See 'GI.Gtk.Objects.Entry.entrySetCursorHadjustment'.
-- 
-- /Since: 2.12/
entryGetCursorHadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m (Maybe Gtk.Adjustment.Adjustment)
    -- ^ __Returns:__ the horizontal cursor adjustment, or 'P.Nothing'
    --   if none has been set.
entryGetCursorHadjustment entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_cursor_hadjustment entry'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Adjustment.Adjustment) result'
        return result''
    touchManagedPtr entry
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data EntryGetCursorHadjustmentMethodInfo
instance (signature ~ (m (Maybe Gtk.Adjustment.Adjustment)), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetCursorHadjustmentMethodInfo a signature where
    overloadedMethod = entryGetCursorHadjustment

instance O.OverloadedMethodInfo EntryGetCursorHadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetCursorHadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetCursorHadjustment"
        })


#endif

-- method Entry::get_has_frame
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_has_frame" gtk_entry_get_has_frame :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO CInt

-- | Gets the value set by 'GI.Gtk.Objects.Entry.entrySetHasFrame'.
entryGetHasFrame ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m Bool
    -- ^ __Returns:__ whether the entry has a beveled frame
entryGetHasFrame entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_has_frame entry'
    let result' = (/= 0) result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetHasFrameMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetHasFrameMethodInfo a signature where
    overloadedMethod = entryGetHasFrame

instance O.OverloadedMethodInfo EntryGetHasFrameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetHasFrame",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetHasFrame"
        })


#endif

-- method Entry::get_icon_activatable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Icon position" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_icon_activatable" gtk_entry_get_icon_activatable :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    IO CInt

-- | Returns whether the icon is activatable.
-- 
-- /Since: 2.16/
entryGetIconActivatable ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: Icon position
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the icon is activatable.
entryGetIconActivatable entry iconPos = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    result <- gtk_entry_get_icon_activatable entry' iconPos'
    let result' = (/= 0) result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetIconActivatableMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> m Bool), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetIconActivatableMethodInfo a signature where
    overloadedMethod = entryGetIconActivatable

instance O.OverloadedMethodInfo EntryGetIconActivatableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetIconActivatable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetIconActivatable"
        })


#endif

-- method Entry::get_icon_area
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Icon position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return location for the icon\8217s area"
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

foreign import ccall "gtk_entry_get_icon_area" gtk_entry_get_icon_area :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    Ptr Gdk.Rectangle.Rectangle ->          -- icon_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO ()

-- | Gets the area where entry’s icon at /@iconPos@/ is drawn.
-- This function is useful when drawing something to the
-- entry in a draw callback.
-- 
-- If the entry is not realized or has no icon at the given position,
-- /@iconArea@/ is filled with zeros. Otherwise, /@iconArea@/ will be filled
-- with the icon’s allocation, relative to /@entry@/’s allocation.
-- 
-- See also 'GI.Gtk.Objects.Entry.entryGetTextArea'
-- 
-- /Since: 3.0/
entryGetIconArea ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: A t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: Icon position
    -> m (Gdk.Rectangle.Rectangle)
entryGetIconArea entry iconPos = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    iconArea <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    gtk_entry_get_icon_area entry' iconPos' iconArea
    iconArea' <- (wrapBoxed Gdk.Rectangle.Rectangle) iconArea
    touchManagedPtr entry
    return iconArea'

#if defined(ENABLE_OVERLOADING)
data EntryGetIconAreaMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> m (Gdk.Rectangle.Rectangle)), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetIconAreaMethodInfo a signature where
    overloadedMethod = entryGetIconArea

instance O.OverloadedMethodInfo EntryGetIconAreaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetIconArea",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetIconArea"
        })


#endif

-- method Entry::get_icon_at_pos
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the x coordinate of the position to find"
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
--                 { rawDocText = Just "the y coordinate of the position to find"
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

foreign import ccall "gtk_entry_get_icon_at_pos" gtk_entry_get_icon_at_pos :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    IO Int32

-- | Finds the icon at the given position and return its index. The
-- position’s coordinates are relative to the /@entry@/’s top left corner.
-- If /@x@/, /@y@/ doesn’t lie inside an icon, -1 is returned.
-- This function is intended for use in a [Widget::queryTooltip]("GI.Gtk.Objects.Widget#g:signal:queryTooltip")
-- signal handler.
-- 
-- /Since: 2.16/
entryGetIconAtPos ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Int32
    -- ^ /@x@/: the x coordinate of the position to find
    -> Int32
    -- ^ /@y@/: the y coordinate of the position to find
    -> m Int32
    -- ^ __Returns:__ the index of the icon at the given position, or -1
entryGetIconAtPos entry x y = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_icon_at_pos entry' x y
    touchManagedPtr entry
    return result

#if defined(ENABLE_OVERLOADING)
data EntryGetIconAtPosMethodInfo
instance (signature ~ (Int32 -> Int32 -> m Int32), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetIconAtPosMethodInfo a signature where
    overloadedMethod = entryGetIconAtPos

instance O.OverloadedMethodInfo EntryGetIconAtPosMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetIconAtPos",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetIconAtPos"
        })


#endif

-- method Entry::get_icon_gicon
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Icon position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gio" , name = "Icon" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_get_icon_gicon" gtk_entry_get_icon_gicon :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    IO (Ptr Gio.Icon.Icon)

-- | Retrieves the t'GI.Gio.Interfaces.Icon.Icon' used for the icon, or 'P.Nothing' if there is
-- no icon or if the icon was set by some other method (e.g., by
-- stock, pixbuf, or icon name).
-- 
-- /Since: 2.16/
entryGetIconGicon ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: A t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: Icon position
    -> m (Maybe Gio.Icon.Icon)
    -- ^ __Returns:__ A t'GI.Gio.Interfaces.Icon.Icon', or 'P.Nothing' if no icon is set
    --     or if the icon is not a t'GI.Gio.Interfaces.Icon.Icon'
entryGetIconGicon entry iconPos = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    result <- gtk_entry_get_icon_gicon entry' iconPos'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gio.Icon.Icon) result'
        return result''
    touchManagedPtr entry
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data EntryGetIconGiconMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> m (Maybe Gio.Icon.Icon)), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetIconGiconMethodInfo a signature where
    overloadedMethod = entryGetIconGicon

instance O.OverloadedMethodInfo EntryGetIconGiconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetIconGicon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetIconGicon"
        })


#endif

-- method Entry::get_icon_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Icon position" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_icon_name" gtk_entry_get_icon_name :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    IO CString

-- | Retrieves the icon name used for the icon, or 'P.Nothing' if there is
-- no icon or if the icon was set by some other method (e.g., by
-- pixbuf, stock or gicon).
-- 
-- /Since: 2.16/
entryGetIconName ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: A t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: Icon position
    -> m (Maybe T.Text)
    -- ^ __Returns:__ An icon name, or 'P.Nothing' if no icon is set or if the icon
    --          wasn’t set from an icon name
entryGetIconName entry iconPos = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    result <- gtk_entry_get_icon_name entry' iconPos'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr entry
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data EntryGetIconNameMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> m (Maybe T.Text)), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetIconNameMethodInfo a signature where
    overloadedMethod = entryGetIconName

instance O.OverloadedMethodInfo EntryGetIconNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetIconName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetIconName"
        })


#endif

-- method Entry::get_icon_pixbuf
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Icon position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_get_icon_pixbuf" gtk_entry_get_icon_pixbuf :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

-- | Retrieves the image used for the icon.
-- 
-- Unlike the other methods of setting and getting icon data, this
-- method will work regardless of whether the icon was set using a
-- t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf', a t'GI.Gio.Interfaces.Icon.Icon', a stock item, or an icon name.
-- 
-- /Since: 2.16/
entryGetIconPixbuf ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: A t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: Icon position
    -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)
    -- ^ __Returns:__ A t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf', or 'P.Nothing' if no icon is
    --     set for this position.
entryGetIconPixbuf entry iconPos = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    result <- gtk_entry_get_icon_pixbuf entry' iconPos'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject GdkPixbuf.Pixbuf.Pixbuf) result'
        return result''
    touchManagedPtr entry
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data EntryGetIconPixbufMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetIconPixbufMethodInfo a signature where
    overloadedMethod = entryGetIconPixbuf

instance O.OverloadedMethodInfo EntryGetIconPixbufMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetIconPixbuf",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetIconPixbuf"
        })


#endif

-- method Entry::get_icon_sensitive
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Icon position" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_icon_sensitive" gtk_entry_get_icon_sensitive :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    IO CInt

-- | Returns whether the icon appears sensitive or insensitive.
-- 
-- /Since: 2.16/
entryGetIconSensitive ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: Icon position
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the icon is sensitive.
entryGetIconSensitive entry iconPos = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    result <- gtk_entry_get_icon_sensitive entry' iconPos'
    let result' = (/= 0) result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetIconSensitiveMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> m Bool), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetIconSensitiveMethodInfo a signature where
    overloadedMethod = entryGetIconSensitive

instance O.OverloadedMethodInfo EntryGetIconSensitiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetIconSensitive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetIconSensitive"
        })


#endif

-- method Entry::get_icon_stock
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Icon position" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_icon_stock" gtk_entry_get_icon_stock :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    IO CString

{-# DEPRECATED entryGetIconStock ["(Since version 3.10)","Use 'GI.Gtk.Objects.Entry.entryGetIconName' instead."] #-}
-- | Retrieves the stock id used for the icon, or 'P.Nothing' if there is
-- no icon or if the icon was set by some other method (e.g., by
-- pixbuf, icon name or gicon).
-- 
-- /Since: 2.16/
entryGetIconStock ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: A t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: Icon position
    -> m T.Text
    -- ^ __Returns:__ A stock id, or 'P.Nothing' if no icon is set or if the icon
    --          wasn’t set from a stock id
entryGetIconStock entry iconPos = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    result <- gtk_entry_get_icon_stock entry' iconPos'
    checkUnexpectedReturnNULL "entryGetIconStock" result
    result' <- cstringToText result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetIconStockMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> m T.Text), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetIconStockMethodInfo a signature where
    overloadedMethod = entryGetIconStock

instance O.OverloadedMethodInfo EntryGetIconStockMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetIconStock",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetIconStock"
        })


#endif

-- method Entry::get_icon_storage_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Icon position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ImageType" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_get_icon_storage_type" gtk_entry_get_icon_storage_type :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    IO CUInt

-- | Gets the type of representation being used by the icon
-- to store image data. If the icon has no image data,
-- the return value will be 'GI.Gtk.Enums.ImageTypeEmpty'.
-- 
-- /Since: 2.16/
entryGetIconStorageType ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: Icon position
    -> m Gtk.Enums.ImageType
    -- ^ __Returns:__ image representation being used
entryGetIconStorageType entry iconPos = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    result <- gtk_entry_get_icon_storage_type entry' iconPos'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetIconStorageTypeMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> m Gtk.Enums.ImageType), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetIconStorageTypeMethodInfo a signature where
    overloadedMethod = entryGetIconStorageType

instance O.OverloadedMethodInfo EntryGetIconStorageTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetIconStorageType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetIconStorageType"
        })


#endif

-- method Entry::get_icon_tooltip_markup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the icon position" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_icon_tooltip_markup" gtk_entry_get_icon_tooltip_markup :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    IO CString

-- | Gets the contents of the tooltip on the icon at the specified
-- position in /@entry@/.
-- 
-- /Since: 2.16/
entryGetIconTooltipMarkup ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: the icon position
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the tooltip text, or 'P.Nothing'. Free the returned
    --     string with 'GI.GLib.Functions.free' when done.
entryGetIconTooltipMarkup entry iconPos = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    result <- gtk_entry_get_icon_tooltip_markup entry' iconPos'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        freeMem result'
        return result''
    touchManagedPtr entry
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data EntryGetIconTooltipMarkupMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> m (Maybe T.Text)), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetIconTooltipMarkupMethodInfo a signature where
    overloadedMethod = entryGetIconTooltipMarkup

instance O.OverloadedMethodInfo EntryGetIconTooltipMarkupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetIconTooltipMarkup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetIconTooltipMarkup"
        })


#endif

-- method Entry::get_icon_tooltip_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the icon position" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_icon_tooltip_text" gtk_entry_get_icon_tooltip_text :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    IO CString

-- | Gets the contents of the tooltip on the icon at the specified
-- position in /@entry@/.
-- 
-- /Since: 2.16/
entryGetIconTooltipText ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: the icon position
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the tooltip text, or 'P.Nothing'. Free the returned
    --     string with 'GI.GLib.Functions.free' when done.
entryGetIconTooltipText entry iconPos = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    result <- gtk_entry_get_icon_tooltip_text entry' iconPos'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        freeMem result'
        return result''
    touchManagedPtr entry
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data EntryGetIconTooltipTextMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> m (Maybe T.Text)), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetIconTooltipTextMethodInfo a signature where
    overloadedMethod = entryGetIconTooltipText

instance O.OverloadedMethodInfo EntryGetIconTooltipTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetIconTooltipText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetIconTooltipText"
        })


#endif

-- method Entry::get_inner_border
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Border" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_get_inner_border" gtk_entry_get_inner_border :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO (Ptr Gtk.Border.Border)

{-# DEPRECATED entryGetInnerBorder ["(Since version 3.4)","Use the standard border and padding CSS properties (through","  objects like t'GI.Gtk.Objects.StyleContext.StyleContext' and t'GI.Gtk.Objects.CssProvider.CssProvider'); the value returned by","  this function is ignored by t'GI.Gtk.Objects.Entry.Entry'."] #-}
-- | This function returns the entry’s [Entry:innerBorder]("GI.Gtk.Objects.Entry#g:attr:innerBorder") property. See
-- 'GI.Gtk.Objects.Entry.entrySetInnerBorder' for more information.
-- 
-- /Since: 2.10/
entryGetInnerBorder ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m (Maybe Gtk.Border.Border)
    -- ^ __Returns:__ the entry’s t'GI.Gtk.Structs.Border.Border', or
    --   'P.Nothing' if none was set.
entryGetInnerBorder entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_inner_border entry'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newBoxed Gtk.Border.Border) result'
        return result''
    touchManagedPtr entry
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data EntryGetInnerBorderMethodInfo
instance (signature ~ (m (Maybe Gtk.Border.Border)), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetInnerBorderMethodInfo a signature where
    overloadedMethod = entryGetInnerBorder

instance O.OverloadedMethodInfo EntryGetInnerBorderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetInnerBorder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetInnerBorder"
        })


#endif

-- method Entry::get_input_hints
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_input_hints" gtk_entry_get_input_hints :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO CUInt

-- | Gets the value of the [Entry:inputHints]("GI.Gtk.Objects.Entry#g:attr:inputHints") property.
-- 
-- /Since: 3.6/
entryGetInputHints ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m [Gtk.Flags.InputHints]
entryGetInputHints entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_input_hints entry'
    let result' = wordToGFlags result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetInputHintsMethodInfo
instance (signature ~ (m [Gtk.Flags.InputHints]), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetInputHintsMethodInfo a signature where
    overloadedMethod = entryGetInputHints

instance O.OverloadedMethodInfo EntryGetInputHintsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetInputHints",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetInputHints"
        })


#endif

-- method Entry::get_input_purpose
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_input_purpose" gtk_entry_get_input_purpose :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO CUInt

-- | Gets the value of the [Entry:inputPurpose]("GI.Gtk.Objects.Entry#g:attr:inputPurpose") property.
-- 
-- /Since: 3.6/
entryGetInputPurpose ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m Gtk.Enums.InputPurpose
entryGetInputPurpose entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_input_purpose entry'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetInputPurposeMethodInfo
instance (signature ~ (m Gtk.Enums.InputPurpose), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetInputPurposeMethodInfo a signature where
    overloadedMethod = entryGetInputPurpose

instance O.OverloadedMethodInfo EntryGetInputPurposeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetInputPurpose",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetInputPurpose"
        })


#endif

-- method Entry::get_invisible_char
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUniChar)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_get_invisible_char" gtk_entry_get_invisible_char :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO CInt

-- | Retrieves the character displayed in place of the real characters
-- for entries with visibility set to false. See 'GI.Gtk.Objects.Entry.entrySetInvisibleChar'.
entryGetInvisibleChar ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m Char
    -- ^ __Returns:__ the current invisible char, or 0, if the entry does not
    --               show invisible text at all.
entryGetInvisibleChar entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_invisible_char entry'
    let result' = (chr . fromIntegral) result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetInvisibleCharMethodInfo
instance (signature ~ (m Char), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetInvisibleCharMethodInfo a signature where
    overloadedMethod = entryGetInvisibleChar

instance O.OverloadedMethodInfo EntryGetInvisibleCharMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetInvisibleChar",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetInvisibleChar"
        })


#endif

-- method Entry::get_layout
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_layout" gtk_entry_get_layout :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO (Ptr Pango.Layout.Layout)

-- | Gets the t'GI.Pango.Objects.Layout.Layout' used to display the entry.
-- The layout is useful to e.g. convert text positions to
-- pixel positions, in combination with 'GI.Gtk.Objects.Entry.entryGetLayoutOffsets'.
-- The returned layout is owned by the entry and must not be
-- modified or freed by the caller.
-- 
-- Keep in mind that the layout text may contain a preedit string, so
-- 'GI.Gtk.Objects.Entry.entryLayoutIndexToTextIndex' and
-- 'GI.Gtk.Objects.Entry.entryTextIndexToLayoutIndex' are needed to convert byte
-- indices in the layout to byte indices in the entry contents.
entryGetLayout ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m Pango.Layout.Layout
    -- ^ __Returns:__ the t'GI.Pango.Objects.Layout.Layout' for this entry
entryGetLayout entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_layout entry'
    checkUnexpectedReturnNULL "entryGetLayout" result
    result' <- (newObject Pango.Layout.Layout) result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetLayoutMethodInfo
instance (signature ~ (m Pango.Layout.Layout), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetLayoutMethodInfo a signature where
    overloadedMethod = entryGetLayout

instance O.OverloadedMethodInfo EntryGetLayoutMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetLayout",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetLayout"
        })


#endif

-- method Entry::get_layout_offsets
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_layout_offsets" gtk_entry_get_layout_offsets :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    Ptr Int32 ->                            -- x : TBasicType TInt
    Ptr Int32 ->                            -- y : TBasicType TInt
    IO ()

-- | Obtains the position of the t'GI.Pango.Objects.Layout.Layout' used to render text
-- in the entry, in widget coordinates. Useful if you want to line
-- up the text in an entry with some other text, e.g. when using the
-- entry to implement editable cells in a sheet widget.
-- 
-- Also useful to convert mouse events into coordinates inside the
-- t'GI.Pango.Objects.Layout.Layout', e.g. to take some action if some part of the entry text
-- is clicked.
-- 
-- Note that as the user scrolls around in the entry the offsets will
-- change; you’ll need to connect to the “notify[scrollOffset](#g:signal:scrollOffset)”
-- signal to track this. Remember when using the t'GI.Pango.Objects.Layout.Layout'
-- functions you need to convert to and from pixels using
-- @/PANGO_PIXELS()/@ or 'GI.Pango.Constants.SCALE'.
-- 
-- Keep in mind that the layout text may contain a preedit string, so
-- 'GI.Gtk.Objects.Entry.entryLayoutIndexToTextIndex' and
-- 'GI.Gtk.Objects.Entry.entryTextIndexToLayoutIndex' are needed to convert byte
-- indices in the layout to byte indices in the entry contents.
entryGetLayoutOffsets ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m ((Int32, Int32))
entryGetLayoutOffsets entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    x <- allocMem :: IO (Ptr Int32)
    y <- allocMem :: IO (Ptr Int32)
    gtk_entry_get_layout_offsets entry' x y
    x' <- peek x
    y' <- peek y
    touchManagedPtr entry
    freeMem x
    freeMem y
    return (x', y')

#if defined(ENABLE_OVERLOADING)
data EntryGetLayoutOffsetsMethodInfo
instance (signature ~ (m ((Int32, Int32))), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetLayoutOffsetsMethodInfo a signature where
    overloadedMethod = entryGetLayoutOffsets

instance O.OverloadedMethodInfo EntryGetLayoutOffsetsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetLayoutOffsets",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetLayoutOffsets"
        })


#endif

-- method Entry::get_max_length
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_max_length" gtk_entry_get_max_length :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO Int32

-- | Retrieves the maximum allowed length of the text in
-- /@entry@/. See 'GI.Gtk.Objects.Entry.entrySetMaxLength'.
-- 
-- This is equivalent to getting /@entry@/\'s t'GI.Gtk.Objects.EntryBuffer.EntryBuffer' and
-- calling 'GI.Gtk.Objects.EntryBuffer.entryBufferGetMaxLength' on it.
entryGetMaxLength ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m Int32
    -- ^ __Returns:__ the maximum allowed number of characters
    --               in t'GI.Gtk.Objects.Entry.Entry', or 0 if there is no maximum.
entryGetMaxLength entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_max_length entry'
    touchManagedPtr entry
    return result

#if defined(ENABLE_OVERLOADING)
data EntryGetMaxLengthMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetMaxLengthMethodInfo a signature where
    overloadedMethod = entryGetMaxLength

instance O.OverloadedMethodInfo EntryGetMaxLengthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetMaxLength",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetMaxLength"
        })


#endif

-- method Entry::get_max_width_chars
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_max_width_chars" gtk_entry_get_max_width_chars :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO Int32

-- | Retrieves the desired maximum width of /@entry@/, in characters.
-- See 'GI.Gtk.Objects.Entry.entrySetMaxWidthChars'.
-- 
-- /Since: 3.12/
entryGetMaxWidthChars ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m Int32
    -- ^ __Returns:__ the maximum width of the entry, in characters
entryGetMaxWidthChars entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_max_width_chars entry'
    touchManagedPtr entry
    return result

#if defined(ENABLE_OVERLOADING)
data EntryGetMaxWidthCharsMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetMaxWidthCharsMethodInfo a signature where
    overloadedMethod = entryGetMaxWidthChars

instance O.OverloadedMethodInfo EntryGetMaxWidthCharsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetMaxWidthChars",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetMaxWidthChars"
        })


#endif

-- method Entry::get_overwrite_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_overwrite_mode" gtk_entry_get_overwrite_mode :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO CInt

-- | Gets the value set by 'GI.Gtk.Objects.Entry.entrySetOverwriteMode'.
-- 
-- /Since: 2.14/
entryGetOverwriteMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m Bool
    -- ^ __Returns:__ whether the text is overwritten when typing.
entryGetOverwriteMode entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_overwrite_mode entry'
    let result' = (/= 0) result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetOverwriteModeMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetOverwriteModeMethodInfo a signature where
    overloadedMethod = entryGetOverwriteMode

instance O.OverloadedMethodInfo EntryGetOverwriteModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetOverwriteMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetOverwriteMode"
        })


#endif

-- method Entry::get_placeholder_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_placeholder_text" gtk_entry_get_placeholder_text :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO CString

-- | Retrieves the text that will be displayed when /@entry@/ is empty and unfocused
-- 
-- /Since: 3.2/
entryGetPlaceholderText ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m T.Text
    -- ^ __Returns:__ a pointer to the placeholder text as a string. This string points to internally allocated
    -- storage in the widget and must not be freed, modified or stored.
entryGetPlaceholderText entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_placeholder_text entry'
    checkUnexpectedReturnNULL "entryGetPlaceholderText" result
    result' <- cstringToText result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetPlaceholderTextMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetPlaceholderTextMethodInfo a signature where
    overloadedMethod = entryGetPlaceholderText

instance O.OverloadedMethodInfo EntryGetPlaceholderTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetPlaceholderText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetPlaceholderText"
        })


#endif

-- method Entry::get_progress_fraction
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_progress_fraction" gtk_entry_get_progress_fraction :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO CDouble

-- | Returns the current fraction of the task that’s been completed.
-- See 'GI.Gtk.Objects.Entry.entrySetProgressFraction'.
-- 
-- /Since: 2.16/
entryGetProgressFraction ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m Double
    -- ^ __Returns:__ a fraction from 0.0 to 1.0
entryGetProgressFraction entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_progress_fraction entry'
    let result' = realToFrac result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetProgressFractionMethodInfo
instance (signature ~ (m Double), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetProgressFractionMethodInfo a signature where
    overloadedMethod = entryGetProgressFraction

instance O.OverloadedMethodInfo EntryGetProgressFractionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetProgressFraction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetProgressFraction"
        })


#endif

-- method Entry::get_progress_pulse_step
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_progress_pulse_step" gtk_entry_get_progress_pulse_step :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO CDouble

-- | Retrieves the pulse step set with 'GI.Gtk.Objects.Entry.entrySetProgressPulseStep'.
-- 
-- /Since: 2.16/
entryGetProgressPulseStep ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m Double
    -- ^ __Returns:__ a fraction from 0.0 to 1.0
entryGetProgressPulseStep entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_progress_pulse_step entry'
    let result' = realToFrac result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetProgressPulseStepMethodInfo
instance (signature ~ (m Double), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetProgressPulseStepMethodInfo a signature where
    overloadedMethod = entryGetProgressPulseStep

instance O.OverloadedMethodInfo EntryGetProgressPulseStepMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetProgressPulseStep",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetProgressPulseStep"
        })


#endif

-- method Entry::get_tabs
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_tabs" gtk_entry_get_tabs :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO (Ptr Pango.TabArray.TabArray)

-- | Gets the tabstops that were set on the entry using 'GI.Gtk.Objects.Entry.entrySetTabs', if
-- any.
-- 
-- /Since: 3.10/
entryGetTabs ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m (Maybe Pango.TabArray.TabArray)
    -- ^ __Returns:__ the tabstops, or 'P.Nothing' if none was set.
entryGetTabs entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_tabs entry'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newBoxed Pango.TabArray.TabArray) result'
        return result''
    touchManagedPtr entry
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data EntryGetTabsMethodInfo
instance (signature ~ (m (Maybe Pango.TabArray.TabArray)), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetTabsMethodInfo a signature where
    overloadedMethod = entryGetTabs

instance O.OverloadedMethodInfo EntryGetTabsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetTabs",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetTabs"
        })


#endif

-- method Entry::get_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_text" gtk_entry_get_text :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO CString

-- | Retrieves the contents of the entry widget.
-- See also 'GI.Gtk.Interfaces.Editable.editableGetChars'.
-- 
-- This is equivalent to getting /@entry@/\'s t'GI.Gtk.Objects.EntryBuffer.EntryBuffer' and calling
-- 'GI.Gtk.Objects.EntryBuffer.entryBufferGetText' on it.
entryGetText ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m T.Text
    -- ^ __Returns:__ a pointer to the contents of the widget as a
    --      string. This string points to internally allocated
    --      storage in the widget and must not be freed, modified or
    --      stored.
entryGetText entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_text entry'
    checkUnexpectedReturnNULL "entryGetText" result
    result' <- cstringToText result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetTextMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetTextMethodInfo a signature where
    overloadedMethod = entryGetText

instance O.OverloadedMethodInfo EntryGetTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetText"
        })


#endif

-- method Entry::get_text_area
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "text_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return location for the text area."
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

foreign import ccall "gtk_entry_get_text_area" gtk_entry_get_text_area :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    Ptr Gdk.Rectangle.Rectangle ->          -- text_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO ()

-- | Gets the area where the entry’s text is drawn. This function is
-- useful when drawing something to the entry in a draw callback.
-- 
-- If the entry is not realized, /@textArea@/ is filled with zeros.
-- 
-- See also 'GI.Gtk.Objects.Entry.entryGetIconArea'.
-- 
-- /Since: 3.0/
entryGetTextArea ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m (Gdk.Rectangle.Rectangle)
entryGetTextArea entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    textArea <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    gtk_entry_get_text_area entry' textArea
    textArea' <- (wrapBoxed Gdk.Rectangle.Rectangle) textArea
    touchManagedPtr entry
    return textArea'

#if defined(ENABLE_OVERLOADING)
data EntryGetTextAreaMethodInfo
instance (signature ~ (m (Gdk.Rectangle.Rectangle)), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetTextAreaMethodInfo a signature where
    overloadedMethod = entryGetTextArea

instance O.OverloadedMethodInfo EntryGetTextAreaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetTextArea",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetTextArea"
        })


#endif

-- method Entry::get_text_length
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUInt16)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_get_text_length" gtk_entry_get_text_length :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO Word16

-- | Retrieves the current length of the text in
-- /@entry@/.
-- 
-- This is equivalent to getting /@entry@/\'s t'GI.Gtk.Objects.EntryBuffer.EntryBuffer' and
-- calling 'GI.Gtk.Objects.EntryBuffer.entryBufferGetLength' on it.
-- 
-- /Since: 2.14/
entryGetTextLength ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m Word16
    -- ^ __Returns:__ the current number of characters
    --               in t'GI.Gtk.Objects.Entry.Entry', or 0 if there are none.
entryGetTextLength entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_text_length entry'
    touchManagedPtr entry
    return result

#if defined(ENABLE_OVERLOADING)
data EntryGetTextLengthMethodInfo
instance (signature ~ (m Word16), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetTextLengthMethodInfo a signature where
    overloadedMethod = entryGetTextLength

instance O.OverloadedMethodInfo EntryGetTextLengthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetTextLength",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetTextLength"
        })


#endif

-- method Entry::get_visibility
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_visibility" gtk_entry_get_visibility :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO CInt

-- | Retrieves whether the text in /@entry@/ is visible. See
-- 'GI.Gtk.Objects.Entry.entrySetVisibility'.
entryGetVisibility ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the text is currently visible
entryGetVisibility entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_visibility entry'
    let result' = (/= 0) result
    touchManagedPtr entry
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryGetVisibilityMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetVisibilityMethodInfo a signature where
    overloadedMethod = entryGetVisibility

instance O.OverloadedMethodInfo EntryGetVisibilityMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetVisibility",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetVisibility"
        })


#endif

-- method Entry::get_width_chars
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_get_width_chars" gtk_entry_get_width_chars :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO Int32

-- | Gets the value set by 'GI.Gtk.Objects.Entry.entrySetWidthChars'.
entryGetWidthChars ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m Int32
    -- ^ __Returns:__ number of chars to request space for, or negative if unset
entryGetWidthChars entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_get_width_chars entry'
    touchManagedPtr entry
    return result

#if defined(ENABLE_OVERLOADING)
data EntryGetWidthCharsMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGetWidthCharsMethodInfo a signature where
    overloadedMethod = entryGetWidthChars

instance O.OverloadedMethodInfo EntryGetWidthCharsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGetWidthChars",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGetWidthChars"
        })


#endif

-- method Entry::grab_focus_without_selecting
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_grab_focus_without_selecting" gtk_entry_grab_focus_without_selecting :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO ()

-- | Causes /@entry@/ to have keyboard focus.
-- 
-- It behaves like 'GI.Gtk.Objects.Widget.widgetGrabFocus',
-- except that it doesn\'t select the contents of the entry.
-- You only want to call this on some special entries
-- which the user usually doesn\'t want to replace all text in,
-- such as search-as-you-type entries.
-- 
-- /Since: 3.16/
entryGrabFocusWithoutSelecting ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m ()
entryGrabFocusWithoutSelecting entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    gtk_entry_grab_focus_without_selecting entry'
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryGrabFocusWithoutSelectingMethodInfo
instance (signature ~ (m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntryGrabFocusWithoutSelectingMethodInfo a signature where
    overloadedMethod = entryGrabFocusWithoutSelecting

instance O.OverloadedMethodInfo EntryGrabFocusWithoutSelectingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryGrabFocusWithoutSelecting",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryGrabFocusWithoutSelecting"
        })


#endif

-- method Entry::im_context_filter_keypress
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_im_context_filter_keypress" gtk_entry_im_context_filter_keypress :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    Ptr Gdk.EventKey.EventKey ->            -- event : TInterface (Name {namespace = "Gdk", name = "EventKey"})
    IO CInt

-- | Allow the t'GI.Gtk.Objects.Entry.Entry' input method to internally handle key press
-- and release events. If this function returns 'P.True', then no further
-- processing should be done for this key event. See
-- 'GI.Gtk.Objects.IMContext.iMContextFilterKeypress'.
-- 
-- Note that you are expected to call this function from your handler
-- when overriding key event handling. This is needed in the case when
-- you need to insert your own key handling between the input method
-- and the default key event handling of the t'GI.Gtk.Objects.Entry.Entry'.
-- See 'GI.Gtk.Objects.TextView.textViewResetImContext' for an example of use.
-- 
-- /Since: 2.22/
entryImContextFilterKeypress ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Gdk.EventKey.EventKey
    -- ^ /@event@/: the key event
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the input method handled the key event.
entryImContextFilterKeypress entry event = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    event' <- unsafeManagedPtrGetPtr event
    result <- gtk_entry_im_context_filter_keypress entry' event'
    let result' = (/= 0) result
    touchManagedPtr entry
    touchManagedPtr event
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryImContextFilterKeypressMethodInfo
instance (signature ~ (Gdk.EventKey.EventKey -> m Bool), MonadIO m, IsEntry a) => O.OverloadedMethod EntryImContextFilterKeypressMethodInfo a signature where
    overloadedMethod = entryImContextFilterKeypress

instance O.OverloadedMethodInfo EntryImContextFilterKeypressMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryImContextFilterKeypress",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryImContextFilterKeypress"
        })


#endif

-- method Entry::layout_index_to_text_index
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "layout_index"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "byte index into the entry layout text"
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

foreign import ccall "gtk_entry_layout_index_to_text_index" gtk_entry_layout_index_to_text_index :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    Int32 ->                                -- layout_index : TBasicType TInt
    IO Int32

-- | Converts from a position in the entry’s t'GI.Pango.Objects.Layout.Layout' (returned by
-- 'GI.Gtk.Objects.Entry.entryGetLayout') to a position in the entry contents
-- (returned by 'GI.Gtk.Objects.Entry.entryGetText').
entryLayoutIndexToTextIndex ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Int32
    -- ^ /@layoutIndex@/: byte index into the entry layout text
    -> m Int32
    -- ^ __Returns:__ byte index into the entry contents
entryLayoutIndexToTextIndex entry layoutIndex = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_layout_index_to_text_index entry' layoutIndex
    touchManagedPtr entry
    return result

#if defined(ENABLE_OVERLOADING)
data EntryLayoutIndexToTextIndexMethodInfo
instance (signature ~ (Int32 -> m Int32), MonadIO m, IsEntry a) => O.OverloadedMethod EntryLayoutIndexToTextIndexMethodInfo a signature where
    overloadedMethod = entryLayoutIndexToTextIndex

instance O.OverloadedMethodInfo EntryLayoutIndexToTextIndexMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryLayoutIndexToTextIndex",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryLayoutIndexToTextIndex"
        })


#endif

-- method Entry::progress_pulse
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_progress_pulse" gtk_entry_progress_pulse :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO ()

-- | Indicates that some progress is made, but you don’t know how much.
-- Causes the entry’s progress indicator to enter “activity mode,”
-- where a block bounces back and forth. Each call to
-- 'GI.Gtk.Objects.Entry.entryProgressPulse' causes the block to move by a little bit
-- (the amount of movement per pulse is determined by
-- 'GI.Gtk.Objects.Entry.entrySetProgressPulseStep').
-- 
-- /Since: 2.16/
entryProgressPulse ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m ()
entryProgressPulse entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    gtk_entry_progress_pulse entry'
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryProgressPulseMethodInfo
instance (signature ~ (m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntryProgressPulseMethodInfo a signature where
    overloadedMethod = entryProgressPulse

instance O.OverloadedMethodInfo EntryProgressPulseMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryProgressPulse",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryProgressPulse"
        })


#endif

-- method Entry::reset_im_context
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_reset_im_context" gtk_entry_reset_im_context :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO ()

-- | Reset the input method context of the entry if needed.
-- 
-- This can be necessary in the case where modifying the buffer
-- would confuse on-going input method behavior.
-- 
-- /Since: 2.22/
entryResetImContext ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m ()
entryResetImContext entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    gtk_entry_reset_im_context entry'
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryResetImContextMethodInfo
instance (signature ~ (m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntryResetImContextMethodInfo a signature where
    overloadedMethod = entryResetImContext

instance O.OverloadedMethodInfo EntryResetImContextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryResetImContext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryResetImContext"
        })


#endif

-- method Entry::set_activates_default
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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
--                 { rawDocText =
--                     Just
--                       "%TRUE to activate window\8217s default widget on Enter keypress"
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

foreign import ccall "gtk_entry_set_activates_default" gtk_entry_set_activates_default :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | If /@setting@/ is 'P.True', pressing Enter in the /@entry@/ will activate the default
-- widget for the window containing the entry. This usually means that
-- the dialog box containing the entry will be closed, since the default
-- widget is usually one of the dialog buttons.
-- 
-- (For experts: if /@setting@/ is 'P.True', the entry calls
-- 'GI.Gtk.Objects.Window.windowActivateDefault' on the window containing the entry, in
-- the default handler for the [Entry::activate]("GI.Gtk.Objects.Entry#g:signal:activate") signal.)
entrySetActivatesDefault ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Bool
    -- ^ /@setting@/: 'P.True' to activate window’s default widget on Enter keypress
    -> m ()
entrySetActivatesDefault entry setting = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let setting' = (fromIntegral . fromEnum) setting
    gtk_entry_set_activates_default entry' setting'
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetActivatesDefaultMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetActivatesDefaultMethodInfo a signature where
    overloadedMethod = entrySetActivatesDefault

instance O.OverloadedMethodInfo EntrySetActivatesDefaultMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetActivatesDefault",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetActivatesDefault"
        })


#endif

-- method Entry::set_alignment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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
--                       "The horizontal alignment, from 0 (left) to 1 (right).\n         Reversed for RTL layouts"
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

foreign import ccall "gtk_entry_set_alignment" gtk_entry_set_alignment :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CFloat ->                               -- xalign : TBasicType TFloat
    IO ()

-- | Sets the alignment for the contents of the entry. This controls
-- the horizontal positioning of the contents when the displayed
-- text is shorter than the width of the entry.
-- 
-- /Since: 2.4/
entrySetAlignment ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Float
    -- ^ /@xalign@/: The horizontal alignment, from 0 (left) to 1 (right).
    --          Reversed for RTL layouts
    -> m ()
entrySetAlignment entry xalign = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let xalign' = realToFrac xalign
    gtk_entry_set_alignment entry' xalign'
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetAlignmentMethodInfo
instance (signature ~ (Float -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetAlignmentMethodInfo a signature where
    overloadedMethod = entrySetAlignment

instance O.OverloadedMethodInfo EntrySetAlignmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetAlignment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetAlignment"
        })


#endif

-- method Entry::set_attributes
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "attrs"
--           , argType =
--               TInterface Name { namespace = "Pango" , name = "AttrList" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #PangoAttrList" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_set_attributes" gtk_entry_set_attributes :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    Ptr Pango.AttrList.AttrList ->          -- attrs : TInterface (Name {namespace = "Pango", name = "AttrList"})
    IO ()

-- | Sets a t'GI.Pango.Structs.AttrList.AttrList'; the attributes in the list are applied to the
-- entry text.
-- 
-- /Since: 3.6/
entrySetAttributes ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Pango.AttrList.AttrList
    -- ^ /@attrs@/: a t'GI.Pango.Structs.AttrList.AttrList'
    -> m ()
entrySetAttributes entry attrs = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    attrs' <- unsafeManagedPtrGetPtr attrs
    gtk_entry_set_attributes entry' attrs'
    touchManagedPtr entry
    touchManagedPtr attrs
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetAttributesMethodInfo
instance (signature ~ (Pango.AttrList.AttrList -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetAttributesMethodInfo a signature where
    overloadedMethod = entrySetAttributes

instance O.OverloadedMethodInfo EntrySetAttributesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetAttributes",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetAttributes"
        })


#endif

-- method Entry::set_buffer
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "buffer"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryBuffer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryBuffer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_set_buffer" gtk_entry_set_buffer :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    Ptr Gtk.EntryBuffer.EntryBuffer ->      -- buffer : TInterface (Name {namespace = "Gtk", name = "EntryBuffer"})
    IO ()

-- | Set the t'GI.Gtk.Objects.EntryBuffer.EntryBuffer' object which holds the text for
-- this widget.
-- 
-- /Since: 2.18/
entrySetBuffer ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a, Gtk.EntryBuffer.IsEntryBuffer b) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> b
    -- ^ /@buffer@/: a t'GI.Gtk.Objects.EntryBuffer.EntryBuffer'
    -> m ()
entrySetBuffer entry buffer = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    buffer' <- unsafeManagedPtrCastPtr buffer
    gtk_entry_set_buffer entry' buffer'
    touchManagedPtr entry
    touchManagedPtr buffer
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetBufferMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsEntry a, Gtk.EntryBuffer.IsEntryBuffer b) => O.OverloadedMethod EntrySetBufferMethodInfo a signature where
    overloadedMethod = entrySetBuffer

instance O.OverloadedMethodInfo EntrySetBufferMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetBuffer",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetBuffer"
        })


#endif

-- method Entry::set_completion
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkEntryCompletion or %NULL"
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

foreign import ccall "gtk_entry_set_completion" gtk_entry_set_completion :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    Ptr Gtk.EntryCompletion.EntryCompletion -> -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    IO ()

-- | Sets /@completion@/ to be the auxiliary completion object to use with /@entry@/.
-- All further configuration of the completion mechanism is done on
-- /@completion@/ using the t'GI.Gtk.Objects.EntryCompletion.EntryCompletion' API. Completion is disabled if
-- /@completion@/ is set to 'P.Nothing'.
-- 
-- /Since: 2.4/
entrySetCompletion ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a, Gtk.EntryCompletion.IsEntryCompletion b) =>
    a
    -- ^ /@entry@/: A t'GI.Gtk.Objects.Entry.Entry'
    -> Maybe (b)
    -- ^ /@completion@/: The t'GI.Gtk.Objects.EntryCompletion.EntryCompletion' or 'P.Nothing'
    -> m ()
entrySetCompletion entry completion = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    maybeCompletion <- case completion of
        Nothing -> return nullPtr
        Just jCompletion -> do
            jCompletion' <- unsafeManagedPtrCastPtr jCompletion
            return jCompletion'
    gtk_entry_set_completion entry' maybeCompletion
    touchManagedPtr entry
    whenJust completion touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetCompletionMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsEntry a, Gtk.EntryCompletion.IsEntryCompletion b) => O.OverloadedMethod EntrySetCompletionMethodInfo a signature where
    overloadedMethod = entrySetCompletion

instance O.OverloadedMethodInfo EntrySetCompletionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetCompletion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetCompletion"
        })


#endif

-- method Entry::set_cursor_hadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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
--                       "an adjustment which should be adjusted when the cursor\n             is moved, or %NULL"
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

foreign import ccall "gtk_entry_set_cursor_hadjustment" gtk_entry_set_cursor_hadjustment :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    Ptr Gtk.Adjustment.Adjustment ->        -- adjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    IO ()

-- | Hooks up an adjustment to the cursor position in an entry, so that when
-- the cursor is moved, the adjustment is scrolled to show that position.
-- See 'GI.Gtk.Objects.ScrolledWindow.scrolledWindowGetHadjustment' for a typical way of obtaining
-- the adjustment.
-- 
-- The adjustment has to be in pixel units and in the same coordinate system
-- as the entry.
-- 
-- /Since: 2.12/
entrySetCursorHadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a, Gtk.Adjustment.IsAdjustment b) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Maybe (b)
    -- ^ /@adjustment@/: an adjustment which should be adjusted when the cursor
    --              is moved, or 'P.Nothing'
    -> m ()
entrySetCursorHadjustment entry adjustment = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    maybeAdjustment <- case adjustment of
        Nothing -> return nullPtr
        Just jAdjustment -> do
            jAdjustment' <- unsafeManagedPtrCastPtr jAdjustment
            return jAdjustment'
    gtk_entry_set_cursor_hadjustment entry' maybeAdjustment
    touchManagedPtr entry
    whenJust adjustment touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetCursorHadjustmentMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsEntry a, Gtk.Adjustment.IsAdjustment b) => O.OverloadedMethod EntrySetCursorHadjustmentMethodInfo a signature where
    overloadedMethod = entrySetCursorHadjustment

instance O.OverloadedMethodInfo EntrySetCursorHadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetCursorHadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetCursorHadjustment"
        })


#endif

-- method Entry::set_has_frame
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "new value" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_set_has_frame" gtk_entry_set_has_frame :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Sets whether the entry has a beveled frame around it.
entrySetHasFrame ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Bool
    -- ^ /@setting@/: new value
    -> m ()
entrySetHasFrame entry setting = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let setting' = (fromIntegral . fromEnum) setting
    gtk_entry_set_has_frame entry' setting'
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetHasFrameMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetHasFrameMethodInfo a signature where
    overloadedMethod = entrySetHasFrame

instance O.OverloadedMethodInfo EntrySetHasFrameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetHasFrame",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetHasFrame"
        })


#endif

-- method Entry::set_icon_activatable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Icon position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "activatable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if the icon should be activatable"
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

foreign import ccall "gtk_entry_set_icon_activatable" gtk_entry_set_icon_activatable :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    CInt ->                                 -- activatable : TBasicType TBoolean
    IO ()

-- | Sets whether the icon is activatable.
-- 
-- /Since: 2.16/
entrySetIconActivatable ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: A t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: Icon position
    -> Bool
    -- ^ /@activatable@/: 'P.True' if the icon should be activatable
    -> m ()
entrySetIconActivatable entry iconPos activatable = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    let activatable' = (fromIntegral . fromEnum) activatable
    gtk_entry_set_icon_activatable entry' iconPos' activatable'
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetIconActivatableMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> Bool -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetIconActivatableMethodInfo a signature where
    overloadedMethod = entrySetIconActivatable

instance O.OverloadedMethodInfo EntrySetIconActivatableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetIconActivatable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetIconActivatable"
        })


#endif

-- method Entry::set_icon_drag_source
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "icon position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "target_list"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TargetList" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the targets (data formats) in which the data can be provided"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "actions"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "DragAction" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a bitmask of the allowed drag actions"
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

foreign import ccall "gtk_entry_set_icon_drag_source" gtk_entry_set_icon_drag_source :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    Ptr Gtk.TargetList.TargetList ->        -- target_list : TInterface (Name {namespace = "Gtk", name = "TargetList"})
    CUInt ->                                -- actions : TInterface (Name {namespace = "Gdk", name = "DragAction"})
    IO ()

-- | Sets up the icon at the given position so that GTK+ will start a drag
-- operation when the user clicks and drags the icon.
-- 
-- To handle the drag operation, you need to connect to the usual
-- [Widget::dragDataGet]("GI.Gtk.Objects.Widget#g:signal:dragDataGet") (or possibly [Widget::dragDataDelete]("GI.Gtk.Objects.Widget#g:signal:dragDataDelete"))
-- signal, and use 'GI.Gtk.Objects.Entry.entryGetCurrentIconDragSource' in
-- your signal handler to find out if the drag was started from
-- an icon.
-- 
-- By default, GTK+ uses the icon as the drag icon. You can use the
-- [Widget::dragBegin]("GI.Gtk.Objects.Widget#g:signal:dragBegin") signal to set a different icon. Note that you
-- have to use @/g_signal_connect_after()/@ to ensure that your signal handler
-- gets executed after the default handler.
-- 
-- /Since: 2.16/
entrySetIconDragSource ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: icon position
    -> Gtk.TargetList.TargetList
    -- ^ /@targetList@/: the targets (data formats) in which the data can be provided
    -> [Gdk.Flags.DragAction]
    -- ^ /@actions@/: a bitmask of the allowed drag actions
    -> m ()
entrySetIconDragSource entry iconPos targetList actions = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    targetList' <- unsafeManagedPtrGetPtr targetList
    let actions' = gflagsToWord actions
    gtk_entry_set_icon_drag_source entry' iconPos' targetList' actions'
    touchManagedPtr entry
    touchManagedPtr targetList
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetIconDragSourceMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> Gtk.TargetList.TargetList -> [Gdk.Flags.DragAction] -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetIconDragSourceMethodInfo a signature where
    overloadedMethod = entrySetIconDragSource

instance O.OverloadedMethodInfo EntrySetIconDragSourceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetIconDragSource",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetIconDragSource"
        })


#endif

-- method Entry::set_icon_from_gicon
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The position at which to set the icon"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon"
--           , argType = TInterface Name { namespace = "Gio" , name = "Icon" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The icon to set, or %NULL"
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

foreign import ccall "gtk_entry_set_icon_from_gicon" gtk_entry_set_icon_from_gicon :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    Ptr Gio.Icon.Icon ->                    -- icon : TInterface (Name {namespace = "Gio", name = "Icon"})
    IO ()

-- | Sets the icon shown in the entry at the specified position
-- from the current icon theme.
-- If the icon isn’t known, a “broken image” icon will be displayed
-- instead.
-- 
-- If /@icon@/ is 'P.Nothing', no icon will be shown in the specified position.
-- 
-- /Since: 2.16/
entrySetIconFromGicon ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a, Gio.Icon.IsIcon b) =>
    a
    -- ^ /@entry@/: A t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: The position at which to set the icon
    -> Maybe (b)
    -- ^ /@icon@/: The icon to set, or 'P.Nothing'
    -> m ()
entrySetIconFromGicon entry iconPos icon = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    maybeIcon <- case icon of
        Nothing -> return nullPtr
        Just jIcon -> do
            jIcon' <- unsafeManagedPtrCastPtr jIcon
            return jIcon'
    gtk_entry_set_icon_from_gicon entry' iconPos' maybeIcon
    touchManagedPtr entry
    whenJust icon touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetIconFromGiconMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> Maybe (b) -> m ()), MonadIO m, IsEntry a, Gio.Icon.IsIcon b) => O.OverloadedMethod EntrySetIconFromGiconMethodInfo a signature where
    overloadedMethod = entrySetIconFromGicon

instance O.OverloadedMethodInfo EntrySetIconFromGiconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetIconFromGicon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetIconFromGicon"
        })


#endif

-- method Entry::set_icon_from_icon_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The position at which to set the icon"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "An icon name, or %NULL"
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

foreign import ccall "gtk_entry_set_icon_from_icon_name" gtk_entry_set_icon_from_icon_name :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    CString ->                              -- icon_name : TBasicType TUTF8
    IO ()

-- | Sets the icon shown in the entry at the specified position
-- from the current icon theme.
-- 
-- If the icon name isn’t known, a “broken image” icon will be displayed
-- instead.
-- 
-- If /@iconName@/ is 'P.Nothing', no icon will be shown in the specified position.
-- 
-- /Since: 2.16/
entrySetIconFromIconName ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: A t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: The position at which to set the icon
    -> Maybe (T.Text)
    -- ^ /@iconName@/: An icon name, or 'P.Nothing'
    -> m ()
entrySetIconFromIconName entry iconPos iconName = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    maybeIconName <- case iconName of
        Nothing -> return nullPtr
        Just jIconName -> do
            jIconName' <- textToCString jIconName
            return jIconName'
    gtk_entry_set_icon_from_icon_name entry' iconPos' maybeIconName
    touchManagedPtr entry
    freeMem maybeIconName
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetIconFromIconNameMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> Maybe (T.Text) -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetIconFromIconNameMethodInfo a signature where
    overloadedMethod = entrySetIconFromIconName

instance O.OverloadedMethodInfo EntrySetIconFromIconNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetIconFromIconName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetIconFromIconName"
        })


#endif

-- method Entry::set_icon_from_pixbuf
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Icon position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pixbuf"
--           , argType =
--               TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GdkPixbuf, or %NULL"
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

foreign import ccall "gtk_entry_set_icon_from_pixbuf" gtk_entry_set_icon_from_pixbuf :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    Ptr GdkPixbuf.Pixbuf.Pixbuf ->          -- pixbuf : TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
    IO ()

-- | Sets the icon shown in the specified position using a pixbuf.
-- 
-- If /@pixbuf@/ is 'P.Nothing', no icon will be shown in the specified position.
-- 
-- /Since: 2.16/
entrySetIconFromPixbuf ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a, GdkPixbuf.Pixbuf.IsPixbuf b) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: Icon position
    -> Maybe (b)
    -- ^ /@pixbuf@/: A t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf', or 'P.Nothing'
    -> m ()
entrySetIconFromPixbuf entry iconPos pixbuf = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    maybePixbuf <- case pixbuf of
        Nothing -> return nullPtr
        Just jPixbuf -> do
            jPixbuf' <- unsafeManagedPtrCastPtr jPixbuf
            return jPixbuf'
    gtk_entry_set_icon_from_pixbuf entry' iconPos' maybePixbuf
    touchManagedPtr entry
    whenJust pixbuf touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetIconFromPixbufMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> Maybe (b) -> m ()), MonadIO m, IsEntry a, GdkPixbuf.Pixbuf.IsPixbuf b) => O.OverloadedMethod EntrySetIconFromPixbufMethodInfo a signature where
    overloadedMethod = entrySetIconFromPixbuf

instance O.OverloadedMethodInfo EntrySetIconFromPixbufMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetIconFromPixbuf",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetIconFromPixbuf"
        })


#endif

-- method Entry::set_icon_from_stock
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Icon position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "stock_id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The name of the stock item, or %NULL"
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

foreign import ccall "gtk_entry_set_icon_from_stock" gtk_entry_set_icon_from_stock :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    CString ->                              -- stock_id : TBasicType TUTF8
    IO ()

{-# DEPRECATED entrySetIconFromStock ["(Since version 3.10)","Use 'GI.Gtk.Objects.Entry.entrySetIconFromIconName' instead."] #-}
-- | Sets the icon shown in the entry at the specified position from
-- a stock image.
-- 
-- If /@stockId@/ is 'P.Nothing', no icon will be shown in the specified position.
-- 
-- /Since: 2.16/
entrySetIconFromStock ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: A t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: Icon position
    -> Maybe (T.Text)
    -- ^ /@stockId@/: The name of the stock item, or 'P.Nothing'
    -> m ()
entrySetIconFromStock entry iconPos stockId = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    maybeStockId <- case stockId of
        Nothing -> return nullPtr
        Just jStockId -> do
            jStockId' <- textToCString jStockId
            return jStockId'
    gtk_entry_set_icon_from_stock entry' iconPos' maybeStockId
    touchManagedPtr entry
    freeMem maybeStockId
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetIconFromStockMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> Maybe (T.Text) -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetIconFromStockMethodInfo a signature where
    overloadedMethod = entrySetIconFromStock

instance O.OverloadedMethodInfo EntrySetIconFromStockMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetIconFromStock",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetIconFromStock"
        })


#endif

-- method Entry::set_icon_sensitive
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Icon position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sensitive"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Specifies whether the icon should appear\n            sensitive or insensitive"
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

foreign import ccall "gtk_entry_set_icon_sensitive" gtk_entry_set_icon_sensitive :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    CInt ->                                 -- sensitive : TBasicType TBoolean
    IO ()

-- | Sets the sensitivity for the specified icon.
-- 
-- /Since: 2.16/
entrySetIconSensitive ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: A t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: Icon position
    -> Bool
    -- ^ /@sensitive@/: Specifies whether the icon should appear
    --             sensitive or insensitive
    -> m ()
entrySetIconSensitive entry iconPos sensitive = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    let sensitive' = (fromIntegral . fromEnum) sensitive
    gtk_entry_set_icon_sensitive entry' iconPos' sensitive'
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetIconSensitiveMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> Bool -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetIconSensitiveMethodInfo a signature where
    overloadedMethod = entrySetIconSensitive

instance O.OverloadedMethodInfo EntrySetIconSensitiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetIconSensitive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetIconSensitive"
        })


#endif

-- method Entry::set_icon_tooltip_markup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the icon position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tooltip"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the contents of the tooltip for the icon, or %NULL"
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

foreign import ccall "gtk_entry_set_icon_tooltip_markup" gtk_entry_set_icon_tooltip_markup :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    CString ->                              -- tooltip : TBasicType TUTF8
    IO ()

-- | Sets /@tooltip@/ as the contents of the tooltip for the icon at
-- the specified position. /@tooltip@/ is assumed to be marked up with
-- the [Pango text markup language][PangoMarkupFormat].
-- 
-- Use 'P.Nothing' for /@tooltip@/ to remove an existing tooltip.
-- 
-- See also 'GI.Gtk.Objects.Widget.widgetSetTooltipMarkup' and
-- 'GI.Gtk.Objects.Entry.entrySetIconTooltipText'.
-- 
-- /Since: 2.16/
entrySetIconTooltipMarkup ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: the icon position
    -> Maybe (T.Text)
    -- ^ /@tooltip@/: the contents of the tooltip for the icon, or 'P.Nothing'
    -> m ()
entrySetIconTooltipMarkup entry iconPos tooltip = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    maybeTooltip <- case tooltip of
        Nothing -> return nullPtr
        Just jTooltip -> do
            jTooltip' <- textToCString jTooltip
            return jTooltip'
    gtk_entry_set_icon_tooltip_markup entry' iconPos' maybeTooltip
    touchManagedPtr entry
    freeMem maybeTooltip
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetIconTooltipMarkupMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> Maybe (T.Text) -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetIconTooltipMarkupMethodInfo a signature where
    overloadedMethod = entrySetIconTooltipMarkup

instance O.OverloadedMethodInfo EntrySetIconTooltipMarkupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetIconTooltipMarkup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetIconTooltipMarkup"
        })


#endif

-- method Entry::set_icon_tooltip_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_pos"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryIconPosition" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the icon position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tooltip"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the contents of the tooltip for the icon, or %NULL"
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

foreign import ccall "gtk_entry_set_icon_tooltip_text" gtk_entry_set_icon_tooltip_text :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- icon_pos : TInterface (Name {namespace = "Gtk", name = "EntryIconPosition"})
    CString ->                              -- tooltip : TBasicType TUTF8
    IO ()

-- | Sets /@tooltip@/ as the contents of the tooltip for the icon
-- at the specified position.
-- 
-- Use 'P.Nothing' for /@tooltip@/ to remove an existing tooltip.
-- 
-- See also 'GI.Gtk.Objects.Widget.widgetSetTooltipText' and
-- 'GI.Gtk.Objects.Entry.entrySetIconTooltipMarkup'.
-- 
-- If you unset the widget tooltip via 'GI.Gtk.Objects.Widget.widgetSetTooltipText' or
-- 'GI.Gtk.Objects.Widget.widgetSetTooltipMarkup', this sets GtkWidget:has-tooltip to 'P.False',
-- which suppresses icon tooltips too. You can resolve this by then calling
-- 'GI.Gtk.Objects.Widget.widgetSetHasTooltip' to set GtkWidget:has-tooltip back to 'P.True', or
-- setting at least one non-empty tooltip on any icon achieves the same result.
-- 
-- /Since: 2.16/
entrySetIconTooltipText ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.EntryIconPosition
    -- ^ /@iconPos@/: the icon position
    -> Maybe (T.Text)
    -- ^ /@tooltip@/: the contents of the tooltip for the icon, or 'P.Nothing'
    -> m ()
entrySetIconTooltipText entry iconPos tooltip = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let iconPos' = (fromIntegral . fromEnum) iconPos
    maybeTooltip <- case tooltip of
        Nothing -> return nullPtr
        Just jTooltip -> do
            jTooltip' <- textToCString jTooltip
            return jTooltip'
    gtk_entry_set_icon_tooltip_text entry' iconPos' maybeTooltip
    touchManagedPtr entry
    freeMem maybeTooltip
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetIconTooltipTextMethodInfo
instance (signature ~ (Gtk.Enums.EntryIconPosition -> Maybe (T.Text) -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetIconTooltipTextMethodInfo a signature where
    overloadedMethod = entrySetIconTooltipText

instance O.OverloadedMethodInfo EntrySetIconTooltipTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetIconTooltipText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetIconTooltipText"
        })


#endif

-- method Entry::set_inner_border
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "border"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Border" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBorder, or %NULL"
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

foreign import ccall "gtk_entry_set_inner_border" gtk_entry_set_inner_border :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    Ptr Gtk.Border.Border ->                -- border : TInterface (Name {namespace = "Gtk", name = "Border"})
    IO ()

{-# DEPRECATED entrySetInnerBorder ["(Since version 3.4)","Use the standard border and padding CSS properties (through","  objects like t'GI.Gtk.Objects.StyleContext.StyleContext' and t'GI.Gtk.Objects.CssProvider.CssProvider'); the value set with","  this function is ignored by t'GI.Gtk.Objects.Entry.Entry'."] #-}
-- | Sets @/entry/@’s inner-border property to /@border@/, or clears it if 'P.Nothing'
-- is passed. The inner-border is the area around the entry’s text, but
-- inside its frame.
-- 
-- If set, this property overrides the inner-border style property.
-- Overriding the style-provided border is useful when you want to do
-- in-place editing of some text in a canvas or list widget, where
-- pixel-exact positioning of the entry is important.
-- 
-- /Since: 2.10/
entrySetInnerBorder ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Maybe (Gtk.Border.Border)
    -- ^ /@border@/: a t'GI.Gtk.Structs.Border.Border', or 'P.Nothing'
    -> m ()
entrySetInnerBorder entry border = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    maybeBorder <- case border of
        Nothing -> return nullPtr
        Just jBorder -> do
            jBorder' <- unsafeManagedPtrGetPtr jBorder
            return jBorder'
    gtk_entry_set_inner_border entry' maybeBorder
    touchManagedPtr entry
    whenJust border touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetInnerBorderMethodInfo
instance (signature ~ (Maybe (Gtk.Border.Border) -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetInnerBorderMethodInfo a signature where
    overloadedMethod = entrySetInnerBorder

instance O.OverloadedMethodInfo EntrySetInnerBorderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetInnerBorder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetInnerBorder"
        })


#endif

-- method Entry::set_input_hints
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_set_input_hints" gtk_entry_set_input_hints :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- hints : TInterface (Name {namespace = "Gtk", name = "InputHints"})
    IO ()

-- | Sets the [Entry:inputHints]("GI.Gtk.Objects.Entry#g:attr:inputHints") property, which
-- allows input methods to fine-tune their behaviour.
-- 
-- /Since: 3.6/
entrySetInputHints ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> [Gtk.Flags.InputHints]
    -- ^ /@hints@/: the hints
    -> m ()
entrySetInputHints entry hints = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let hints' = gflagsToWord hints
    gtk_entry_set_input_hints entry' hints'
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetInputHintsMethodInfo
instance (signature ~ ([Gtk.Flags.InputHints] -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetInputHintsMethodInfo a signature where
    overloadedMethod = entrySetInputHints

instance O.OverloadedMethodInfo EntrySetInputHintsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetInputHints",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetInputHints"
        })


#endif

-- method Entry::set_input_purpose
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_set_input_purpose" gtk_entry_set_input_purpose :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CUInt ->                                -- purpose : TInterface (Name {namespace = "Gtk", name = "InputPurpose"})
    IO ()

-- | Sets the [Entry:inputPurpose]("GI.Gtk.Objects.Entry#g:attr:inputPurpose") property which
-- can be used by on-screen keyboards and other input
-- methods to adjust their behaviour.
-- 
-- /Since: 3.6/
entrySetInputPurpose ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Gtk.Enums.InputPurpose
    -- ^ /@purpose@/: the purpose
    -> m ()
entrySetInputPurpose entry purpose = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let purpose' = (fromIntegral . fromEnum) purpose
    gtk_entry_set_input_purpose entry' purpose'
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetInputPurposeMethodInfo
instance (signature ~ (Gtk.Enums.InputPurpose -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetInputPurposeMethodInfo a signature where
    overloadedMethod = entrySetInputPurpose

instance O.OverloadedMethodInfo EntrySetInputPurposeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetInputPurpose",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetInputPurpose"
        })


#endif

-- method Entry::set_invisible_char
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "ch"
--           , argType = TBasicType TUniChar
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a Unicode character"
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

foreign import ccall "gtk_entry_set_invisible_char" gtk_entry_set_invisible_char :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CInt ->                                 -- ch : TBasicType TUniChar
    IO ()

-- | Sets the character to use in place of the actual text when
-- 'GI.Gtk.Objects.Entry.entrySetVisibility' has been called to set text visibility
-- to 'P.False'. i.e. this is the character used in “password mode” to
-- show the user how many characters have been typed. By default, GTK+
-- picks the best invisible char available in the current font. If you
-- set the invisible char to 0, then the user will get no feedback
-- at all; there will be no text on the screen as they type.
entrySetInvisibleChar ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Char
    -- ^ /@ch@/: a Unicode character
    -> m ()
entrySetInvisibleChar entry ch = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let ch' = (fromIntegral . ord) ch
    gtk_entry_set_invisible_char entry' ch'
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetInvisibleCharMethodInfo
instance (signature ~ (Char -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetInvisibleCharMethodInfo a signature where
    overloadedMethod = entrySetInvisibleChar

instance O.OverloadedMethodInfo EntrySetInvisibleCharMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetInvisibleChar",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetInvisibleChar"
        })


#endif

-- method Entry::set_max_length
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "max"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the maximum length of the entry, or 0 for no maximum.\n  (other than the maximum length of entries.) The value passed in will\n  be clamped to the range 0-65536."
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

foreign import ccall "gtk_entry_set_max_length" gtk_entry_set_max_length :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    Int32 ->                                -- max : TBasicType TInt
    IO ()

-- | Sets the maximum allowed length of the contents of the widget. If
-- the current contents are longer than the given length, then they
-- will be truncated to fit.
-- 
-- This is equivalent to getting /@entry@/\'s t'GI.Gtk.Objects.EntryBuffer.EntryBuffer' and
-- calling 'GI.Gtk.Objects.EntryBuffer.entryBufferSetMaxLength' on it.
-- ]|
entrySetMaxLength ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Int32
    -- ^ /@max@/: the maximum length of the entry, or 0 for no maximum.
    --   (other than the maximum length of entries.) The value passed in will
    --   be clamped to the range 0-65536.
    -> m ()
entrySetMaxLength entry max = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    gtk_entry_set_max_length entry' max
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetMaxLengthMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetMaxLengthMethodInfo a signature where
    overloadedMethod = entrySetMaxLength

instance O.OverloadedMethodInfo EntrySetMaxLengthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetMaxLength",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetMaxLength"
        })


#endif

-- method Entry::set_max_width_chars
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_chars"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new desired maximum width, in characters"
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

foreign import ccall "gtk_entry_set_max_width_chars" gtk_entry_set_max_width_chars :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    Int32 ->                                -- n_chars : TBasicType TInt
    IO ()

-- | Sets the desired maximum width in characters of /@entry@/.
-- 
-- /Since: 3.12/
entrySetMaxWidthChars ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Int32
    -- ^ /@nChars@/: the new desired maximum width, in characters
    -> m ()
entrySetMaxWidthChars entry nChars = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    gtk_entry_set_max_width_chars entry' nChars
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetMaxWidthCharsMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetMaxWidthCharsMethodInfo a signature where
    overloadedMethod = entrySetMaxWidthChars

instance O.OverloadedMethodInfo EntrySetMaxWidthCharsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetMaxWidthChars",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetMaxWidthChars"
        })


#endif

-- method Entry::set_overwrite_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "new value" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_set_overwrite_mode" gtk_entry_set_overwrite_mode :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CInt ->                                 -- overwrite : TBasicType TBoolean
    IO ()

-- | Sets whether the text is overwritten when typing in the t'GI.Gtk.Objects.Entry.Entry'.
-- 
-- /Since: 2.14/
entrySetOverwriteMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Bool
    -- ^ /@overwrite@/: new value
    -> m ()
entrySetOverwriteMode entry overwrite = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let overwrite' = (fromIntegral . fromEnum) overwrite
    gtk_entry_set_overwrite_mode entry' overwrite'
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetOverwriteModeMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetOverwriteModeMethodInfo a signature where
    overloadedMethod = entrySetOverwriteMode

instance O.OverloadedMethodInfo EntrySetOverwriteModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetOverwriteMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetOverwriteMode"
        })


#endif

-- method Entry::set_placeholder_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a string to be displayed when @entry is empty and unfocused, or %NULL"
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

foreign import ccall "gtk_entry_set_placeholder_text" gtk_entry_set_placeholder_text :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CString ->                              -- text : TBasicType TUTF8
    IO ()

-- | Sets text to be displayed in /@entry@/ when it is empty and unfocused.
-- This can be used to give a visual hint of the expected contents of
-- the t'GI.Gtk.Objects.Entry.Entry'.
-- 
-- Note that since the placeholder text gets removed when the entry
-- received focus, using this feature is a bit problematic if the entry
-- is given the initial focus in a window. Sometimes this can be
-- worked around by delaying the initial focus setting until the
-- first key event arrives.
-- 
-- /Since: 3.2/
entrySetPlaceholderText ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Maybe (T.Text)
    -- ^ /@text@/: a string to be displayed when /@entry@/ is empty and unfocused, or 'P.Nothing'
    -> m ()
entrySetPlaceholderText entry text = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    maybeText <- case text of
        Nothing -> return nullPtr
        Just jText -> do
            jText' <- textToCString jText
            return jText'
    gtk_entry_set_placeholder_text entry' maybeText
    touchManagedPtr entry
    freeMem maybeText
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetPlaceholderTextMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetPlaceholderTextMethodInfo a signature where
    overloadedMethod = entrySetPlaceholderText

instance O.OverloadedMethodInfo EntrySetPlaceholderTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetPlaceholderText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetPlaceholderText"
        })


#endif

-- method Entry::set_progress_fraction
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "fraction"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "fraction of the task that\8217s been completed"
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

foreign import ccall "gtk_entry_set_progress_fraction" gtk_entry_set_progress_fraction :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CDouble ->                              -- fraction : TBasicType TDouble
    IO ()

-- | Causes the entry’s progress indicator to “fill in” the given
-- fraction of the bar. The fraction should be between 0.0 and 1.0,
-- inclusive.
-- 
-- /Since: 2.16/
entrySetProgressFraction ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Double
    -- ^ /@fraction@/: fraction of the task that’s been completed
    -> m ()
entrySetProgressFraction entry fraction = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let fraction' = realToFrac fraction
    gtk_entry_set_progress_fraction entry' fraction'
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetProgressFractionMethodInfo
instance (signature ~ (Double -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetProgressFractionMethodInfo a signature where
    overloadedMethod = entrySetProgressFraction

instance O.OverloadedMethodInfo EntrySetProgressFractionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetProgressFraction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetProgressFraction"
        })


#endif

-- method Entry::set_progress_pulse_step
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "fraction"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "fraction between 0.0 and 1.0"
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

foreign import ccall "gtk_entry_set_progress_pulse_step" gtk_entry_set_progress_pulse_step :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CDouble ->                              -- fraction : TBasicType TDouble
    IO ()

-- | Sets the fraction of total entry width to move the progress
-- bouncing block for each call to 'GI.Gtk.Objects.Entry.entryProgressPulse'.
-- 
-- /Since: 2.16/
entrySetProgressPulseStep ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Double
    -- ^ /@fraction@/: fraction between 0.0 and 1.0
    -> m ()
entrySetProgressPulseStep entry fraction = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let fraction' = realToFrac fraction
    gtk_entry_set_progress_pulse_step entry' fraction'
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetProgressPulseStepMethodInfo
instance (signature ~ (Double -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetProgressPulseStepMethodInfo a signature where
    overloadedMethod = entrySetProgressPulseStep

instance O.OverloadedMethodInfo EntrySetProgressPulseStepMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetProgressPulseStep",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetProgressPulseStep"
        })


#endif

-- method Entry::set_tabs
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a #PangoTabArray" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_set_tabs" gtk_entry_set_tabs :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    Ptr Pango.TabArray.TabArray ->          -- tabs : TInterface (Name {namespace = "Pango", name = "TabArray"})
    IO ()

-- | Sets a t'GI.Pango.Structs.TabArray.TabArray'; the tabstops in the array are applied to the entry
-- text.
-- 
-- /Since: 3.10/
entrySetTabs ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Pango.TabArray.TabArray
    -- ^ /@tabs@/: a t'GI.Pango.Structs.TabArray.TabArray'
    -> m ()
entrySetTabs entry tabs = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    tabs' <- unsafeManagedPtrGetPtr tabs
    gtk_entry_set_tabs entry' tabs'
    touchManagedPtr entry
    touchManagedPtr tabs
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetTabsMethodInfo
instance (signature ~ (Pango.TabArray.TabArray -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetTabsMethodInfo a signature where
    overloadedMethod = entrySetTabs

instance O.OverloadedMethodInfo EntrySetTabsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetTabs",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetTabs"
        })


#endif

-- method Entry::set_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the new text" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_set_text" gtk_entry_set_text :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CString ->                              -- text : TBasicType TUTF8
    IO ()

-- | Sets the text in the widget to the given
-- value, replacing the current contents.
-- 
-- See 'GI.Gtk.Objects.EntryBuffer.entryBufferSetText'.
entrySetText ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> T.Text
    -- ^ /@text@/: the new text
    -> m ()
entrySetText entry text = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    text' <- textToCString text
    gtk_entry_set_text entry' text'
    touchManagedPtr entry
    freeMem text'
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetTextMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetTextMethodInfo a signature where
    overloadedMethod = entrySetText

instance O.OverloadedMethodInfo EntrySetTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetText"
        })


#endif

-- method Entry::set_visibility
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "visible"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "%TRUE if the contents of the entry are displayed\n          as plaintext"
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

foreign import ccall "gtk_entry_set_visibility" gtk_entry_set_visibility :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    CInt ->                                 -- visible : TBasicType TBoolean
    IO ()

-- | Sets whether the contents of the entry are visible or not.
-- When visibility is set to 'P.False', characters are displayed
-- as the invisible char, and will also appear that way when
-- the text in the entry widget is copied elsewhere.
-- 
-- By default, GTK+ picks the best invisible character available
-- in the current font, but it can be changed with
-- 'GI.Gtk.Objects.Entry.entrySetInvisibleChar'.
-- 
-- Note that you probably want to set [Entry:inputPurpose]("GI.Gtk.Objects.Entry#g:attr:inputPurpose")
-- to 'GI.Gtk.Enums.InputPurposePassword' or 'GI.Gtk.Enums.InputPurposePin' to
-- inform input methods about the purpose of this entry,
-- in addition to setting visibility to 'P.False'.
entrySetVisibility ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Bool
    -- ^ /@visible@/: 'P.True' if the contents of the entry are displayed
    --           as plaintext
    -> m ()
entrySetVisibility entry visible = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    let visible' = (fromIntegral . fromEnum) visible
    gtk_entry_set_visibility entry' visible'
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetVisibilityMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetVisibilityMethodInfo a signature where
    overloadedMethod = entrySetVisibility

instance O.OverloadedMethodInfo EntrySetVisibilityMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetVisibility",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetVisibility"
        })


#endif

-- method Entry::set_width_chars
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_chars"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "width in chars" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_set_width_chars" gtk_entry_set_width_chars :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    Int32 ->                                -- n_chars : TBasicType TInt
    IO ()

-- | Changes the size request of the entry to be about the right size
-- for /@nChars@/ characters. Note that it changes the size
-- request, the size can still be affected by
-- how you pack the widget into containers. If /@nChars@/ is -1, the
-- size reverts to the default entry size.
entrySetWidthChars ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Int32
    -- ^ /@nChars@/: width in chars
    -> m ()
entrySetWidthChars entry nChars = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    gtk_entry_set_width_chars entry' nChars
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntrySetWidthCharsMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntrySetWidthCharsMethodInfo a signature where
    overloadedMethod = entrySetWidthChars

instance O.OverloadedMethodInfo EntrySetWidthCharsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entrySetWidthChars",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entrySetWidthChars"
        })


#endif

-- method Entry::text_index_to_layout_index
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "text_index"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "byte index into the entry contents"
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

foreign import ccall "gtk_entry_text_index_to_layout_index" gtk_entry_text_index_to_layout_index :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    Int32 ->                                -- text_index : TBasicType TInt
    IO Int32

-- | Converts from a position in the entry contents (returned
-- by 'GI.Gtk.Objects.Entry.entryGetText') to a position in the
-- entry’s t'GI.Pango.Objects.Layout.Layout' (returned by 'GI.Gtk.Objects.Entry.entryGetLayout',
-- with text retrieved via 'GI.Pango.Objects.Layout.layoutGetText').
entryTextIndexToLayoutIndex ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> Int32
    -- ^ /@textIndex@/: byte index into the entry contents
    -> m Int32
    -- ^ __Returns:__ byte index into the entry layout text
entryTextIndexToLayoutIndex entry textIndex = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    result <- gtk_entry_text_index_to_layout_index entry' textIndex
    touchManagedPtr entry
    return result

#if defined(ENABLE_OVERLOADING)
data EntryTextIndexToLayoutIndexMethodInfo
instance (signature ~ (Int32 -> m Int32), MonadIO m, IsEntry a) => O.OverloadedMethod EntryTextIndexToLayoutIndexMethodInfo a signature where
    overloadedMethod = entryTextIndexToLayoutIndex

instance O.OverloadedMethodInfo EntryTextIndexToLayoutIndexMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryTextIndexToLayoutIndex",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryTextIndexToLayoutIndex"
        })


#endif

-- method Entry::unset_invisible_char
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "entry"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Entry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntry" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_unset_invisible_char" gtk_entry_unset_invisible_char :: 
    Ptr Entry ->                            -- entry : TInterface (Name {namespace = "Gtk", name = "Entry"})
    IO ()

-- | Unsets the invisible char previously set with
-- 'GI.Gtk.Objects.Entry.entrySetInvisibleChar'. So that the
-- default invisible char is used again.
-- 
-- /Since: 2.16/
entryUnsetInvisibleChar ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntry a) =>
    a
    -- ^ /@entry@/: a t'GI.Gtk.Objects.Entry.Entry'
    -> m ()
entryUnsetInvisibleChar entry = liftIO $ do
    entry' <- unsafeManagedPtrCastPtr entry
    gtk_entry_unset_invisible_char entry'
    touchManagedPtr entry
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryUnsetInvisibleCharMethodInfo
instance (signature ~ (m ()), MonadIO m, IsEntry a) => O.OverloadedMethod EntryUnsetInvisibleCharMethodInfo a signature where
    overloadedMethod = entryUnsetInvisibleChar

instance O.OverloadedMethodInfo EntryUnsetInvisibleCharMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Entry.entryUnsetInvisibleChar",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Entry.html#v:entryUnsetInvisibleChar"
        })


#endif


