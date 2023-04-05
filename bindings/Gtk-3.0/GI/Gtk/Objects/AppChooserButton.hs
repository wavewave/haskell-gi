{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.AppChooserButton.AppChooserButton' is a widget that lets the user select
-- an application. It implements the t'GI.Gtk.Interfaces.AppChooser.AppChooser' interface.
-- 
-- Initially, a t'GI.Gtk.Objects.AppChooserButton.AppChooserButton' selects the first application
-- in its list, which will either be the most-recently used application
-- or, if [AppChooserButton:showDefaultItem]("GI.Gtk.Objects.AppChooserButton#g:attr:showDefaultItem") is 'P.True', the
-- default application.
-- 
-- The list of applications shown in a t'GI.Gtk.Objects.AppChooserButton.AppChooserButton' includes
-- the recommended applications for the given content type. When
-- [AppChooserButton:showDefaultItem]("GI.Gtk.Objects.AppChooserButton#g:attr:showDefaultItem") is set, the default application
-- is also included. To let the user chooser other applications,
-- you can set the [AppChooserButton:showDialogItem]("GI.Gtk.Objects.AppChooserButton#g:attr:showDialogItem") property,
-- which allows to open a full t'GI.Gtk.Objects.AppChooserDialog.AppChooserDialog'.
-- 
-- It is possible to add custom items to the list, using
-- 'GI.Gtk.Objects.AppChooserButton.appChooserButtonAppendCustomItem'. These items cause
-- the [AppChooserButton::customItemActivated]("GI.Gtk.Objects.AppChooserButton#g:signal:customItemActivated") signal to be
-- emitted when they are selected.
-- 
-- To track changes in the selected application, use the
-- [ComboBox::changed]("GI.Gtk.Objects.ComboBox#g:signal:changed") signal.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.AppChooserButton
    ( 

-- * Exported types
    AppChooserButton(..)                    ,
    IsAppChooserButton                      ,
    toAppChooserButton                      ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addAttribute]("GI.Gtk.Interfaces.CellLayout#g:method:addAttribute"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [appendCustomItem]("GI.Gtk.Objects.AppChooserButton#g:method:appendCustomItem"), [appendSeparator]("GI.Gtk.Objects.AppChooserButton#g:method:appendSeparator"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [clear]("GI.Gtk.Interfaces.CellLayout#g:method:clear"), [clearAttributes]("GI.Gtk.Interfaces.CellLayout#g:method:clearAttributes"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [editingDone]("GI.Gtk.Interfaces.CellEditable#g:method:editingDone"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [packEnd]("GI.Gtk.Interfaces.CellLayout#g:method:packEnd"), [packStart]("GI.Gtk.Interfaces.CellLayout#g:method:packStart"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [popdown]("GI.Gtk.Objects.ComboBox#g:method:popdown"), [popup]("GI.Gtk.Objects.ComboBox#g:method:popup"), [popupForDevice]("GI.Gtk.Objects.ComboBox#g:method:popupForDevice"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refresh]("GI.Gtk.Interfaces.AppChooser#g:method:refresh"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [removeWidget]("GI.Gtk.Interfaces.CellEditable#g:method:removeWidget"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reorder]("GI.Gtk.Interfaces.CellLayout#g:method:reorder"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [startEditing]("GI.Gtk.Interfaces.CellEditable#g:method:startEditing"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActive]("GI.Gtk.Objects.ComboBox#g:method:getActive"), [getActiveId]("GI.Gtk.Objects.ComboBox#g:method:getActiveId"), [getActiveIter]("GI.Gtk.Objects.ComboBox#g:method:getActiveIter"), [getAddTearoffs]("GI.Gtk.Objects.ComboBox#g:method:getAddTearoffs"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppInfo]("GI.Gtk.Interfaces.AppChooser#g:method:getAppInfo"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getArea]("GI.Gtk.Interfaces.CellLayout#g:method:getArea"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getButtonSensitivity]("GI.Gtk.Objects.ComboBox#g:method:getButtonSensitivity"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getCells]("GI.Gtk.Interfaces.CellLayout#g:method:getCells"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getColumnSpanColumn]("GI.Gtk.Objects.ComboBox#g:method:getColumnSpanColumn"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getContentType]("GI.Gtk.Interfaces.AppChooser#g:method:getContentType"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEntryTextColumn]("GI.Gtk.Objects.ComboBox#g:method:getEntryTextColumn"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.ComboBox#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasEntry]("GI.Gtk.Objects.ComboBox#g:method:getHasEntry"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHeading]("GI.Gtk.Objects.AppChooserButton#g:method:getHeading"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getIdColumn]("GI.Gtk.Objects.ComboBox#g:method:getIdColumn"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModel]("GI.Gtk.Objects.ComboBox#g:method:getModel"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPopupAccessible]("GI.Gtk.Objects.ComboBox#g:method:getPopupAccessible"), [getPopupFixedWidth]("GI.Gtk.Objects.ComboBox#g:method:getPopupFixedWidth"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getRowSpanColumn]("GI.Gtk.Objects.ComboBox#g:method:getRowSpanColumn"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getShowDefaultItem]("GI.Gtk.Objects.AppChooserButton#g:method:getShowDefaultItem"), [getShowDialogItem]("GI.Gtk.Objects.AppChooserButton#g:method:getShowDialogItem"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTitle]("GI.Gtk.Objects.ComboBox#g:method:getTitle"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow"), [getWrapWidth]("GI.Gtk.Objects.ComboBox#g:method:getWrapWidth").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActive]("GI.Gtk.Objects.ComboBox#g:method:setActive"), [setActiveCustomItem]("GI.Gtk.Objects.AppChooserButton#g:method:setActiveCustomItem"), [setActiveId]("GI.Gtk.Objects.ComboBox#g:method:setActiveId"), [setActiveIter]("GI.Gtk.Objects.ComboBox#g:method:setActiveIter"), [setAddTearoffs]("GI.Gtk.Objects.ComboBox#g:method:setAddTearoffs"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setButtonSensitivity]("GI.Gtk.Objects.ComboBox#g:method:setButtonSensitivity"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setCellDataFunc]("GI.Gtk.Interfaces.CellLayout#g:method:setCellDataFunc"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setColumnSpanColumn]("GI.Gtk.Objects.ComboBox#g:method:setColumnSpanColumn"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEntryTextColumn]("GI.Gtk.Objects.ComboBox#g:method:setEntryTextColumn"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.ComboBox#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHeading]("GI.Gtk.Objects.AppChooserButton#g:method:setHeading"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setIdColumn]("GI.Gtk.Objects.ComboBox#g:method:setIdColumn"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setModel]("GI.Gtk.Objects.ComboBox#g:method:setModel"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPopupFixedWidth]("GI.Gtk.Objects.ComboBox#g:method:setPopupFixedWidth"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setRowSeparatorFunc]("GI.Gtk.Objects.ComboBox#g:method:setRowSeparatorFunc"), [setRowSpanColumn]("GI.Gtk.Objects.ComboBox#g:method:setRowSpanColumn"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setShowDefaultItem]("GI.Gtk.Objects.AppChooserButton#g:method:setShowDefaultItem"), [setShowDialogItem]("GI.Gtk.Objects.AppChooserButton#g:method:setShowDialogItem"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTitle]("GI.Gtk.Objects.ComboBox#g:method:setTitle"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow"), [setWrapWidth]("GI.Gtk.Objects.ComboBox#g:method:setWrapWidth").

#if defined(ENABLE_OVERLOADING)
    ResolveAppChooserButtonMethod           ,
#endif

-- ** appendCustomItem #method:appendCustomItem#

#if defined(ENABLE_OVERLOADING)
    AppChooserButtonAppendCustomItemMethodInfo,
#endif
    appChooserButtonAppendCustomItem        ,


-- ** appendSeparator #method:appendSeparator#

#if defined(ENABLE_OVERLOADING)
    AppChooserButtonAppendSeparatorMethodInfo,
#endif
    appChooserButtonAppendSeparator         ,


-- ** getHeading #method:getHeading#

#if defined(ENABLE_OVERLOADING)
    AppChooserButtonGetHeadingMethodInfo    ,
#endif
    appChooserButtonGetHeading              ,


-- ** getShowDefaultItem #method:getShowDefaultItem#

#if defined(ENABLE_OVERLOADING)
    AppChooserButtonGetShowDefaultItemMethodInfo,
#endif
    appChooserButtonGetShowDefaultItem      ,


-- ** getShowDialogItem #method:getShowDialogItem#

#if defined(ENABLE_OVERLOADING)
    AppChooserButtonGetShowDialogItemMethodInfo,
#endif
    appChooserButtonGetShowDialogItem       ,


-- ** new #method:new#

    appChooserButtonNew                     ,


-- ** setActiveCustomItem #method:setActiveCustomItem#

#if defined(ENABLE_OVERLOADING)
    AppChooserButtonSetActiveCustomItemMethodInfo,
#endif
    appChooserButtonSetActiveCustomItem     ,


-- ** setHeading #method:setHeading#

#if defined(ENABLE_OVERLOADING)
    AppChooserButtonSetHeadingMethodInfo    ,
#endif
    appChooserButtonSetHeading              ,


-- ** setShowDefaultItem #method:setShowDefaultItem#

#if defined(ENABLE_OVERLOADING)
    AppChooserButtonSetShowDefaultItemMethodInfo,
#endif
    appChooserButtonSetShowDefaultItem      ,


-- ** setShowDialogItem #method:setShowDialogItem#

#if defined(ENABLE_OVERLOADING)
    AppChooserButtonSetShowDialogItemMethodInfo,
#endif
    appChooserButtonSetShowDialogItem       ,




 -- * Properties


-- ** heading #attr:heading#
-- | The text to show at the top of the dialog that can be
-- opened from the button. The string may contain Pango markup.

#if defined(ENABLE_OVERLOADING)
    AppChooserButtonHeadingPropertyInfo     ,
#endif
#if defined(ENABLE_OVERLOADING)
    appChooserButtonHeading                 ,
#endif
    constructAppChooserButtonHeading        ,
    getAppChooserButtonHeading              ,
    setAppChooserButtonHeading              ,


-- ** showDefaultItem #attr:showDefaultItem#
-- | The [AppChooserButton:showDefaultItem]("GI.Gtk.Objects.AppChooserButton#g:attr:showDefaultItem") property determines
-- whether the dropdown menu should show the default application
-- on top for the provided content type.
-- 
-- /Since: 3.2/

#if defined(ENABLE_OVERLOADING)
    AppChooserButtonShowDefaultItemPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    appChooserButtonShowDefaultItem         ,
#endif
    constructAppChooserButtonShowDefaultItem,
    getAppChooserButtonShowDefaultItem      ,
    setAppChooserButtonShowDefaultItem      ,


-- ** showDialogItem #attr:showDialogItem#
-- | The [AppChooserButton:showDialogItem]("GI.Gtk.Objects.AppChooserButton#g:attr:showDialogItem") property determines
-- whether the dropdown menu should show an item that triggers
-- a t'GI.Gtk.Objects.AppChooserDialog.AppChooserDialog' when clicked.

#if defined(ENABLE_OVERLOADING)
    AppChooserButtonShowDialogItemPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    appChooserButtonShowDialogItem          ,
#endif
    constructAppChooserButtonShowDialogItem ,
    getAppChooserButtonShowDialogItem       ,
    setAppChooserButtonShowDialogItem       ,




 -- * Signals


-- ** customItemActivated #signal:customItemActivated#

    AppChooserButtonCustomItemActivatedCallback,
#if defined(ENABLE_OVERLOADING)
    AppChooserButtonCustomItemActivatedSignalInfo,
#endif
    afterAppChooserButtonCustomItemActivated,
    onAppChooserButtonCustomItemActivated   ,




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
import qualified GI.Gio.Interfaces.Icon as Gio.Icon
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.AppChooser as Gtk.AppChooser
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.CellEditable as Gtk.CellEditable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.CellLayout as Gtk.CellLayout
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.ComboBox as Gtk.ComboBox
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype AppChooserButton = AppChooserButton (SP.ManagedPtr AppChooserButton)
    deriving (Eq)

instance SP.ManagedPtrNewtype AppChooserButton where
    toManagedPtr (AppChooserButton p) = p

foreign import ccall "gtk_app_chooser_button_get_type"
    c_gtk_app_chooser_button_get_type :: IO B.Types.GType

instance B.Types.TypedObject AppChooserButton where
    glibType = c_gtk_app_chooser_button_get_type

instance B.Types.GObject AppChooserButton

-- | Type class for types which can be safely cast to `AppChooserButton`, for instance with `toAppChooserButton`.
class (SP.GObject o, O.IsDescendantOf AppChooserButton o) => IsAppChooserButton o
instance (SP.GObject o, O.IsDescendantOf AppChooserButton o) => IsAppChooserButton o

instance O.HasParentTypes AppChooserButton
type instance O.ParentTypes AppChooserButton = '[Gtk.ComboBox.ComboBox, Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.AppChooser.AppChooser, Gtk.Buildable.Buildable, Gtk.CellEditable.CellEditable, Gtk.CellLayout.CellLayout]

-- | Cast to `AppChooserButton`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toAppChooserButton :: (MIO.MonadIO m, IsAppChooserButton o) => o -> m AppChooserButton
toAppChooserButton = MIO.liftIO . B.ManagedPtr.unsafeCastTo AppChooserButton

-- | Convert 'AppChooserButton' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe AppChooserButton) where
    gvalueGType_ = c_gtk_app_chooser_button_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr AppChooserButton)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr AppChooserButton)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject AppChooserButton ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveAppChooserButtonMethod (t :: Symbol) (o :: *) :: * where
    ResolveAppChooserButtonMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveAppChooserButtonMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveAppChooserButtonMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveAppChooserButtonMethod "addAttribute" o = Gtk.CellLayout.CellLayoutAddAttributeMethodInfo
    ResolveAppChooserButtonMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveAppChooserButtonMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveAppChooserButtonMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveAppChooserButtonMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveAppChooserButtonMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveAppChooserButtonMethod "appendCustomItem" o = AppChooserButtonAppendCustomItemMethodInfo
    ResolveAppChooserButtonMethod "appendSeparator" o = AppChooserButtonAppendSeparatorMethodInfo
    ResolveAppChooserButtonMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveAppChooserButtonMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveAppChooserButtonMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveAppChooserButtonMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveAppChooserButtonMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveAppChooserButtonMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveAppChooserButtonMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveAppChooserButtonMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveAppChooserButtonMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveAppChooserButtonMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveAppChooserButtonMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveAppChooserButtonMethod "clear" o = Gtk.CellLayout.CellLayoutClearMethodInfo
    ResolveAppChooserButtonMethod "clearAttributes" o = Gtk.CellLayout.CellLayoutClearAttributesMethodInfo
    ResolveAppChooserButtonMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveAppChooserButtonMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveAppChooserButtonMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveAppChooserButtonMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveAppChooserButtonMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveAppChooserButtonMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveAppChooserButtonMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveAppChooserButtonMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveAppChooserButtonMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveAppChooserButtonMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveAppChooserButtonMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveAppChooserButtonMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveAppChooserButtonMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveAppChooserButtonMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveAppChooserButtonMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveAppChooserButtonMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveAppChooserButtonMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveAppChooserButtonMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveAppChooserButtonMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveAppChooserButtonMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveAppChooserButtonMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveAppChooserButtonMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveAppChooserButtonMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveAppChooserButtonMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveAppChooserButtonMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveAppChooserButtonMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveAppChooserButtonMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveAppChooserButtonMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveAppChooserButtonMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveAppChooserButtonMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveAppChooserButtonMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveAppChooserButtonMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveAppChooserButtonMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveAppChooserButtonMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveAppChooserButtonMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveAppChooserButtonMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveAppChooserButtonMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveAppChooserButtonMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveAppChooserButtonMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveAppChooserButtonMethod "editingDone" o = Gtk.CellEditable.CellEditableEditingDoneMethodInfo
    ResolveAppChooserButtonMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveAppChooserButtonMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveAppChooserButtonMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveAppChooserButtonMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveAppChooserButtonMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveAppChooserButtonMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveAppChooserButtonMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveAppChooserButtonMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveAppChooserButtonMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveAppChooserButtonMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveAppChooserButtonMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveAppChooserButtonMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveAppChooserButtonMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveAppChooserButtonMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveAppChooserButtonMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveAppChooserButtonMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveAppChooserButtonMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveAppChooserButtonMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveAppChooserButtonMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveAppChooserButtonMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveAppChooserButtonMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveAppChooserButtonMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveAppChooserButtonMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveAppChooserButtonMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveAppChooserButtonMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveAppChooserButtonMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveAppChooserButtonMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveAppChooserButtonMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveAppChooserButtonMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveAppChooserButtonMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveAppChooserButtonMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveAppChooserButtonMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveAppChooserButtonMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveAppChooserButtonMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveAppChooserButtonMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveAppChooserButtonMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveAppChooserButtonMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveAppChooserButtonMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveAppChooserButtonMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveAppChooserButtonMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveAppChooserButtonMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveAppChooserButtonMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveAppChooserButtonMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveAppChooserButtonMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveAppChooserButtonMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveAppChooserButtonMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveAppChooserButtonMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveAppChooserButtonMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveAppChooserButtonMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveAppChooserButtonMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveAppChooserButtonMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveAppChooserButtonMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveAppChooserButtonMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveAppChooserButtonMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveAppChooserButtonMethod "packEnd" o = Gtk.CellLayout.CellLayoutPackEndMethodInfo
    ResolveAppChooserButtonMethod "packStart" o = Gtk.CellLayout.CellLayoutPackStartMethodInfo
    ResolveAppChooserButtonMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveAppChooserButtonMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveAppChooserButtonMethod "popdown" o = Gtk.ComboBox.ComboBoxPopdownMethodInfo
    ResolveAppChooserButtonMethod "popup" o = Gtk.ComboBox.ComboBoxPopupMethodInfo
    ResolveAppChooserButtonMethod "popupForDevice" o = Gtk.ComboBox.ComboBoxPopupForDeviceMethodInfo
    ResolveAppChooserButtonMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveAppChooserButtonMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveAppChooserButtonMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveAppChooserButtonMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveAppChooserButtonMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveAppChooserButtonMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveAppChooserButtonMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveAppChooserButtonMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveAppChooserButtonMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveAppChooserButtonMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveAppChooserButtonMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveAppChooserButtonMethod "refresh" o = Gtk.AppChooser.AppChooserRefreshMethodInfo
    ResolveAppChooserButtonMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveAppChooserButtonMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveAppChooserButtonMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveAppChooserButtonMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveAppChooserButtonMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveAppChooserButtonMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveAppChooserButtonMethod "removeWidget" o = Gtk.CellEditable.CellEditableRemoveWidgetMethodInfo
    ResolveAppChooserButtonMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveAppChooserButtonMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveAppChooserButtonMethod "reorder" o = Gtk.CellLayout.CellLayoutReorderMethodInfo
    ResolveAppChooserButtonMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveAppChooserButtonMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveAppChooserButtonMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveAppChooserButtonMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveAppChooserButtonMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveAppChooserButtonMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveAppChooserButtonMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveAppChooserButtonMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveAppChooserButtonMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveAppChooserButtonMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveAppChooserButtonMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveAppChooserButtonMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveAppChooserButtonMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveAppChooserButtonMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveAppChooserButtonMethod "startEditing" o = Gtk.CellEditable.CellEditableStartEditingMethodInfo
    ResolveAppChooserButtonMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveAppChooserButtonMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveAppChooserButtonMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveAppChooserButtonMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveAppChooserButtonMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveAppChooserButtonMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveAppChooserButtonMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveAppChooserButtonMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveAppChooserButtonMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveAppChooserButtonMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveAppChooserButtonMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveAppChooserButtonMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveAppChooserButtonMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveAppChooserButtonMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveAppChooserButtonMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveAppChooserButtonMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveAppChooserButtonMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveAppChooserButtonMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveAppChooserButtonMethod "getActive" o = Gtk.ComboBox.ComboBoxGetActiveMethodInfo
    ResolveAppChooserButtonMethod "getActiveId" o = Gtk.ComboBox.ComboBoxGetActiveIdMethodInfo
    ResolveAppChooserButtonMethod "getActiveIter" o = Gtk.ComboBox.ComboBoxGetActiveIterMethodInfo
    ResolveAppChooserButtonMethod "getAddTearoffs" o = Gtk.ComboBox.ComboBoxGetAddTearoffsMethodInfo
    ResolveAppChooserButtonMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveAppChooserButtonMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveAppChooserButtonMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveAppChooserButtonMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveAppChooserButtonMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveAppChooserButtonMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveAppChooserButtonMethod "getAppInfo" o = Gtk.AppChooser.AppChooserGetAppInfoMethodInfo
    ResolveAppChooserButtonMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveAppChooserButtonMethod "getArea" o = Gtk.CellLayout.CellLayoutGetAreaMethodInfo
    ResolveAppChooserButtonMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveAppChooserButtonMethod "getButtonSensitivity" o = Gtk.ComboBox.ComboBoxGetButtonSensitivityMethodInfo
    ResolveAppChooserButtonMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveAppChooserButtonMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveAppChooserButtonMethod "getCells" o = Gtk.CellLayout.CellLayoutGetCellsMethodInfo
    ResolveAppChooserButtonMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveAppChooserButtonMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveAppChooserButtonMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveAppChooserButtonMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveAppChooserButtonMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveAppChooserButtonMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveAppChooserButtonMethod "getColumnSpanColumn" o = Gtk.ComboBox.ComboBoxGetColumnSpanColumnMethodInfo
    ResolveAppChooserButtonMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveAppChooserButtonMethod "getContentType" o = Gtk.AppChooser.AppChooserGetContentTypeMethodInfo
    ResolveAppChooserButtonMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveAppChooserButtonMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveAppChooserButtonMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveAppChooserButtonMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveAppChooserButtonMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveAppChooserButtonMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveAppChooserButtonMethod "getEntryTextColumn" o = Gtk.ComboBox.ComboBoxGetEntryTextColumnMethodInfo
    ResolveAppChooserButtonMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveAppChooserButtonMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveAppChooserButtonMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveAppChooserButtonMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveAppChooserButtonMethod "getFocusOnClick" o = Gtk.ComboBox.ComboBoxGetFocusOnClickMethodInfo
    ResolveAppChooserButtonMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveAppChooserButtonMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveAppChooserButtonMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveAppChooserButtonMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveAppChooserButtonMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveAppChooserButtonMethod "getHasEntry" o = Gtk.ComboBox.ComboBoxGetHasEntryMethodInfo
    ResolveAppChooserButtonMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveAppChooserButtonMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveAppChooserButtonMethod "getHeading" o = AppChooserButtonGetHeadingMethodInfo
    ResolveAppChooserButtonMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveAppChooserButtonMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveAppChooserButtonMethod "getIdColumn" o = Gtk.ComboBox.ComboBoxGetIdColumnMethodInfo
    ResolveAppChooserButtonMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveAppChooserButtonMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveAppChooserButtonMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveAppChooserButtonMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveAppChooserButtonMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveAppChooserButtonMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveAppChooserButtonMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveAppChooserButtonMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveAppChooserButtonMethod "getModel" o = Gtk.ComboBox.ComboBoxGetModelMethodInfo
    ResolveAppChooserButtonMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveAppChooserButtonMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveAppChooserButtonMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveAppChooserButtonMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveAppChooserButtonMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveAppChooserButtonMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveAppChooserButtonMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveAppChooserButtonMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveAppChooserButtonMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveAppChooserButtonMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveAppChooserButtonMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveAppChooserButtonMethod "getPopupAccessible" o = Gtk.ComboBox.ComboBoxGetPopupAccessibleMethodInfo
    ResolveAppChooserButtonMethod "getPopupFixedWidth" o = Gtk.ComboBox.ComboBoxGetPopupFixedWidthMethodInfo
    ResolveAppChooserButtonMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveAppChooserButtonMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveAppChooserButtonMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveAppChooserButtonMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveAppChooserButtonMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveAppChooserButtonMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveAppChooserButtonMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveAppChooserButtonMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveAppChooserButtonMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveAppChooserButtonMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveAppChooserButtonMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveAppChooserButtonMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveAppChooserButtonMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveAppChooserButtonMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveAppChooserButtonMethod "getRowSpanColumn" o = Gtk.ComboBox.ComboBoxGetRowSpanColumnMethodInfo
    ResolveAppChooserButtonMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveAppChooserButtonMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveAppChooserButtonMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveAppChooserButtonMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveAppChooserButtonMethod "getShowDefaultItem" o = AppChooserButtonGetShowDefaultItemMethodInfo
    ResolveAppChooserButtonMethod "getShowDialogItem" o = AppChooserButtonGetShowDialogItemMethodInfo
    ResolveAppChooserButtonMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveAppChooserButtonMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveAppChooserButtonMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveAppChooserButtonMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveAppChooserButtonMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveAppChooserButtonMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveAppChooserButtonMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveAppChooserButtonMethod "getTitle" o = Gtk.ComboBox.ComboBoxGetTitleMethodInfo
    ResolveAppChooserButtonMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveAppChooserButtonMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveAppChooserButtonMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveAppChooserButtonMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveAppChooserButtonMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveAppChooserButtonMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveAppChooserButtonMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveAppChooserButtonMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveAppChooserButtonMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveAppChooserButtonMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveAppChooserButtonMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveAppChooserButtonMethod "getWrapWidth" o = Gtk.ComboBox.ComboBoxGetWrapWidthMethodInfo
    ResolveAppChooserButtonMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveAppChooserButtonMethod "setActive" o = Gtk.ComboBox.ComboBoxSetActiveMethodInfo
    ResolveAppChooserButtonMethod "setActiveCustomItem" o = AppChooserButtonSetActiveCustomItemMethodInfo
    ResolveAppChooserButtonMethod "setActiveId" o = Gtk.ComboBox.ComboBoxSetActiveIdMethodInfo
    ResolveAppChooserButtonMethod "setActiveIter" o = Gtk.ComboBox.ComboBoxSetActiveIterMethodInfo
    ResolveAppChooserButtonMethod "setAddTearoffs" o = Gtk.ComboBox.ComboBoxSetAddTearoffsMethodInfo
    ResolveAppChooserButtonMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveAppChooserButtonMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveAppChooserButtonMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveAppChooserButtonMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveAppChooserButtonMethod "setButtonSensitivity" o = Gtk.ComboBox.ComboBoxSetButtonSensitivityMethodInfo
    ResolveAppChooserButtonMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveAppChooserButtonMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveAppChooserButtonMethod "setCellDataFunc" o = Gtk.CellLayout.CellLayoutSetCellDataFuncMethodInfo
    ResolveAppChooserButtonMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveAppChooserButtonMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveAppChooserButtonMethod "setColumnSpanColumn" o = Gtk.ComboBox.ComboBoxSetColumnSpanColumnMethodInfo
    ResolveAppChooserButtonMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveAppChooserButtonMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveAppChooserButtonMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveAppChooserButtonMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveAppChooserButtonMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveAppChooserButtonMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveAppChooserButtonMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveAppChooserButtonMethod "setEntryTextColumn" o = Gtk.ComboBox.ComboBoxSetEntryTextColumnMethodInfo
    ResolveAppChooserButtonMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveAppChooserButtonMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveAppChooserButtonMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveAppChooserButtonMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveAppChooserButtonMethod "setFocusOnClick" o = Gtk.ComboBox.ComboBoxSetFocusOnClickMethodInfo
    ResolveAppChooserButtonMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveAppChooserButtonMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveAppChooserButtonMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveAppChooserButtonMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveAppChooserButtonMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveAppChooserButtonMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveAppChooserButtonMethod "setHeading" o = AppChooserButtonSetHeadingMethodInfo
    ResolveAppChooserButtonMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveAppChooserButtonMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveAppChooserButtonMethod "setIdColumn" o = Gtk.ComboBox.ComboBoxSetIdColumnMethodInfo
    ResolveAppChooserButtonMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveAppChooserButtonMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveAppChooserButtonMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveAppChooserButtonMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveAppChooserButtonMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveAppChooserButtonMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveAppChooserButtonMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveAppChooserButtonMethod "setModel" o = Gtk.ComboBox.ComboBoxSetModelMethodInfo
    ResolveAppChooserButtonMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveAppChooserButtonMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveAppChooserButtonMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveAppChooserButtonMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveAppChooserButtonMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveAppChooserButtonMethod "setPopupFixedWidth" o = Gtk.ComboBox.ComboBoxSetPopupFixedWidthMethodInfo
    ResolveAppChooserButtonMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveAppChooserButtonMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveAppChooserButtonMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveAppChooserButtonMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveAppChooserButtonMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveAppChooserButtonMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveAppChooserButtonMethod "setRowSeparatorFunc" o = Gtk.ComboBox.ComboBoxSetRowSeparatorFuncMethodInfo
    ResolveAppChooserButtonMethod "setRowSpanColumn" o = Gtk.ComboBox.ComboBoxSetRowSpanColumnMethodInfo
    ResolveAppChooserButtonMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveAppChooserButtonMethod "setShowDefaultItem" o = AppChooserButtonSetShowDefaultItemMethodInfo
    ResolveAppChooserButtonMethod "setShowDialogItem" o = AppChooserButtonSetShowDialogItemMethodInfo
    ResolveAppChooserButtonMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveAppChooserButtonMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveAppChooserButtonMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveAppChooserButtonMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveAppChooserButtonMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveAppChooserButtonMethod "setTitle" o = Gtk.ComboBox.ComboBoxSetTitleMethodInfo
    ResolveAppChooserButtonMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveAppChooserButtonMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveAppChooserButtonMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveAppChooserButtonMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveAppChooserButtonMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveAppChooserButtonMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveAppChooserButtonMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveAppChooserButtonMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveAppChooserButtonMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveAppChooserButtonMethod "setWrapWidth" o = Gtk.ComboBox.ComboBoxSetWrapWidthMethodInfo
    ResolveAppChooserButtonMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveAppChooserButtonMethod t AppChooserButton, O.OverloadedMethod info AppChooserButton p) => OL.IsLabel t (AppChooserButton -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveAppChooserButtonMethod t AppChooserButton, O.OverloadedMethod info AppChooserButton p, R.HasField t AppChooserButton p) => R.HasField t AppChooserButton p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveAppChooserButtonMethod t AppChooserButton, O.OverloadedMethodInfo info AppChooserButton) => OL.IsLabel t (O.MethodProxy info AppChooserButton) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal AppChooserButton::custom-item-activated
-- | Emitted when a custom item, previously added with
-- 'GI.Gtk.Objects.AppChooserButton.appChooserButtonAppendCustomItem', is activated from the
-- dropdown menu.
type AppChooserButtonCustomItemActivatedCallback =
    T.Text
    -- ^ /@itemName@/: the name of the activated item
    -> IO ()

type C_AppChooserButtonCustomItemActivatedCallback =
    Ptr AppChooserButton ->                 -- object
    CString ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_AppChooserButtonCustomItemActivatedCallback`.
foreign import ccall "wrapper"
    mk_AppChooserButtonCustomItemActivatedCallback :: C_AppChooserButtonCustomItemActivatedCallback -> IO (FunPtr C_AppChooserButtonCustomItemActivatedCallback)

wrap_AppChooserButtonCustomItemActivatedCallback :: 
    GObject a => (a -> AppChooserButtonCustomItemActivatedCallback) ->
    C_AppChooserButtonCustomItemActivatedCallback
wrap_AppChooserButtonCustomItemActivatedCallback gi'cb gi'selfPtr itemName _ = do
    itemName' <- cstringToText itemName
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  itemName'


-- | Connect a signal handler for the [customItemActivated](#signal:customItemActivated) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' appChooserButton #customItemActivated callback
-- @
-- 
-- This signal admits a optional parameter @detail@.
-- If it's not @Nothing@, we will connect to “@custom-item-activated::detail@” instead.
-- 
onAppChooserButtonCustomItemActivated :: (IsAppChooserButton a, MonadIO m) => a -> P.Maybe T.Text -> ((?self :: a) => AppChooserButtonCustomItemActivatedCallback) -> m SignalHandlerId
onAppChooserButtonCustomItemActivated obj detail cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AppChooserButtonCustomItemActivatedCallback wrapped
    wrapped'' <- mk_AppChooserButtonCustomItemActivatedCallback wrapped'
    connectSignalFunPtr obj "custom-item-activated" wrapped'' SignalConnectBefore detail

-- | Connect a signal handler for the [customItemActivated](#signal:customItemActivated) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' appChooserButton #customItemActivated callback
-- @
-- 
-- This signal admits a optional parameter @detail@.
-- If it's not @Nothing@, we will connect to “@custom-item-activated::detail@” instead.
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterAppChooserButtonCustomItemActivated :: (IsAppChooserButton a, MonadIO m) => a -> P.Maybe T.Text -> ((?self :: a) => AppChooserButtonCustomItemActivatedCallback) -> m SignalHandlerId
afterAppChooserButtonCustomItemActivated obj detail cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_AppChooserButtonCustomItemActivatedCallback wrapped
    wrapped'' <- mk_AppChooserButtonCustomItemActivatedCallback wrapped'
    connectSignalFunPtr obj "custom-item-activated" wrapped'' SignalConnectAfter detail


#if defined(ENABLE_OVERLOADING)
data AppChooserButtonCustomItemActivatedSignalInfo
instance SignalInfo AppChooserButtonCustomItemActivatedSignalInfo where
    type HaskellCallbackType AppChooserButtonCustomItemActivatedSignalInfo = AppChooserButtonCustomItemActivatedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_AppChooserButtonCustomItemActivatedCallback cb
        cb'' <- mk_AppChooserButtonCustomItemActivatedCallback cb'
        connectSignalFunPtr obj "custom-item-activated" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserButton::custom-item-activated"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserButton.html#g:signal:customItemActivated"})

#endif

-- VVV Prop "heading"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just False)

-- | Get the value of the “@heading@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' appChooserButton #heading
-- @
getAppChooserButtonHeading :: (MonadIO m, IsAppChooserButton o) => o -> m (Maybe T.Text)
getAppChooserButtonHeading obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "heading"

-- | Set the value of the “@heading@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' appChooserButton [ #heading 'Data.GI.Base.Attributes.:=' value ]
-- @
setAppChooserButtonHeading :: (MonadIO m, IsAppChooserButton o) => o -> T.Text -> m ()
setAppChooserButtonHeading obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "heading" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@heading@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAppChooserButtonHeading :: (IsAppChooserButton o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructAppChooserButtonHeading val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "heading" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data AppChooserButtonHeadingPropertyInfo
instance AttrInfo AppChooserButtonHeadingPropertyInfo where
    type AttrAllowedOps AppChooserButtonHeadingPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint AppChooserButtonHeadingPropertyInfo = IsAppChooserButton
    type AttrSetTypeConstraint AppChooserButtonHeadingPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint AppChooserButtonHeadingPropertyInfo = (~) T.Text
    type AttrTransferType AppChooserButtonHeadingPropertyInfo = T.Text
    type AttrGetType AppChooserButtonHeadingPropertyInfo = (Maybe T.Text)
    type AttrLabel AppChooserButtonHeadingPropertyInfo = "heading"
    type AttrOrigin AppChooserButtonHeadingPropertyInfo = AppChooserButton
    attrGet = getAppChooserButtonHeading
    attrSet = setAppChooserButtonHeading
    attrTransfer _ v = do
        return v
    attrConstruct = constructAppChooserButtonHeading
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserButton.heading"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserButton.html#g:attr:heading"
        })
#endif

-- VVV Prop "show-default-item"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-default-item@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' appChooserButton #showDefaultItem
-- @
getAppChooserButtonShowDefaultItem :: (MonadIO m, IsAppChooserButton o) => o -> m Bool
getAppChooserButtonShowDefaultItem obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-default-item"

-- | Set the value of the “@show-default-item@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' appChooserButton [ #showDefaultItem 'Data.GI.Base.Attributes.:=' value ]
-- @
setAppChooserButtonShowDefaultItem :: (MonadIO m, IsAppChooserButton o) => o -> Bool -> m ()
setAppChooserButtonShowDefaultItem obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-default-item" val

-- | Construct a `GValueConstruct` with valid value for the “@show-default-item@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAppChooserButtonShowDefaultItem :: (IsAppChooserButton o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructAppChooserButtonShowDefaultItem val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-default-item" val

#if defined(ENABLE_OVERLOADING)
data AppChooserButtonShowDefaultItemPropertyInfo
instance AttrInfo AppChooserButtonShowDefaultItemPropertyInfo where
    type AttrAllowedOps AppChooserButtonShowDefaultItemPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint AppChooserButtonShowDefaultItemPropertyInfo = IsAppChooserButton
    type AttrSetTypeConstraint AppChooserButtonShowDefaultItemPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint AppChooserButtonShowDefaultItemPropertyInfo = (~) Bool
    type AttrTransferType AppChooserButtonShowDefaultItemPropertyInfo = Bool
    type AttrGetType AppChooserButtonShowDefaultItemPropertyInfo = Bool
    type AttrLabel AppChooserButtonShowDefaultItemPropertyInfo = "show-default-item"
    type AttrOrigin AppChooserButtonShowDefaultItemPropertyInfo = AppChooserButton
    attrGet = getAppChooserButtonShowDefaultItem
    attrSet = setAppChooserButtonShowDefaultItem
    attrTransfer _ v = do
        return v
    attrConstruct = constructAppChooserButtonShowDefaultItem
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserButton.showDefaultItem"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserButton.html#g:attr:showDefaultItem"
        })
#endif

-- VVV Prop "show-dialog-item"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstruct]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-dialog-item@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' appChooserButton #showDialogItem
-- @
getAppChooserButtonShowDialogItem :: (MonadIO m, IsAppChooserButton o) => o -> m Bool
getAppChooserButtonShowDialogItem obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-dialog-item"

-- | Set the value of the “@show-dialog-item@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' appChooserButton [ #showDialogItem 'Data.GI.Base.Attributes.:=' value ]
-- @
setAppChooserButtonShowDialogItem :: (MonadIO m, IsAppChooserButton o) => o -> Bool -> m ()
setAppChooserButtonShowDialogItem obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-dialog-item" val

-- | Construct a `GValueConstruct` with valid value for the “@show-dialog-item@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructAppChooserButtonShowDialogItem :: (IsAppChooserButton o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructAppChooserButtonShowDialogItem val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-dialog-item" val

#if defined(ENABLE_OVERLOADING)
data AppChooserButtonShowDialogItemPropertyInfo
instance AttrInfo AppChooserButtonShowDialogItemPropertyInfo where
    type AttrAllowedOps AppChooserButtonShowDialogItemPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint AppChooserButtonShowDialogItemPropertyInfo = IsAppChooserButton
    type AttrSetTypeConstraint AppChooserButtonShowDialogItemPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint AppChooserButtonShowDialogItemPropertyInfo = (~) Bool
    type AttrTransferType AppChooserButtonShowDialogItemPropertyInfo = Bool
    type AttrGetType AppChooserButtonShowDialogItemPropertyInfo = Bool
    type AttrLabel AppChooserButtonShowDialogItemPropertyInfo = "show-dialog-item"
    type AttrOrigin AppChooserButtonShowDialogItemPropertyInfo = AppChooserButton
    attrGet = getAppChooserButtonShowDialogItem
    attrSet = setAppChooserButtonShowDialogItem
    attrTransfer _ v = do
        return v
    attrConstruct = constructAppChooserButtonShowDialogItem
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserButton.showDialogItem"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserButton.html#g:attr:showDialogItem"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList AppChooserButton
type instance O.AttributeList AppChooserButton = AppChooserButtonAttributeList
type AppChooserButtonAttributeList = ('[ '("active", Gtk.ComboBox.ComboBoxActivePropertyInfo), '("activeId", Gtk.ComboBox.ComboBoxActiveIdPropertyInfo), '("addTearoffs", Gtk.ComboBox.ComboBoxAddTearoffsPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("buttonSensitivity", Gtk.ComboBox.ComboBoxButtonSensitivityPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("cellArea", Gtk.ComboBox.ComboBoxCellAreaPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("columnSpanColumn", Gtk.ComboBox.ComboBoxColumnSpanColumnPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("contentType", Gtk.AppChooser.AppChooserContentTypePropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("editingCanceled", Gtk.CellEditable.CellEditableEditingCanceledPropertyInfo), '("entryTextColumn", Gtk.ComboBox.ComboBoxEntryTextColumnPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasEntry", Gtk.ComboBox.ComboBoxHasEntryPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasFrame", Gtk.ComboBox.ComboBoxHasFramePropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heading", AppChooserButtonHeadingPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("idColumn", Gtk.ComboBox.ComboBoxIdColumnPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("model", Gtk.ComboBox.ComboBoxModelPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("popupFixedWidth", Gtk.ComboBox.ComboBoxPopupFixedWidthPropertyInfo), '("popupShown", Gtk.ComboBox.ComboBoxPopupShownPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("rowSpanColumn", Gtk.ComboBox.ComboBoxRowSpanColumnPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("showDefaultItem", AppChooserButtonShowDefaultItemPropertyInfo), '("showDialogItem", AppChooserButtonShowDialogItemPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tearoffTitle", Gtk.ComboBox.ComboBoxTearoffTitlePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("wrapWidth", Gtk.ComboBox.ComboBoxWrapWidthPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
appChooserButtonHeading :: AttrLabelProxy "heading"
appChooserButtonHeading = AttrLabelProxy

appChooserButtonShowDefaultItem :: AttrLabelProxy "showDefaultItem"
appChooserButtonShowDefaultItem = AttrLabelProxy

appChooserButtonShowDialogItem :: AttrLabelProxy "showDialogItem"
appChooserButtonShowDialogItem = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList AppChooserButton = AppChooserButtonSignalList
type AppChooserButtonSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("changed", Gtk.ComboBox.ComboBoxChangedSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("customItemActivated", AppChooserButtonCustomItemActivatedSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("editingDone", Gtk.CellEditable.CellEditableEditingDoneSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("formatEntryText", Gtk.ComboBox.ComboBoxFormatEntryTextSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveActive", Gtk.ComboBox.ComboBoxMoveActiveSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popdown", Gtk.ComboBox.ComboBoxPopdownSignalInfo), '("popup", Gtk.ComboBox.ComboBoxPopupSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("removeWidget", Gtk.CellEditable.CellEditableRemoveWidgetSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method AppChooserButton::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "content_type"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the content type to show applications for"
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
--               (TInterface Name { namespace = "Gtk" , name = "AppChooserButton" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_app_chooser_button_new" gtk_app_chooser_button_new :: 
    CString ->                              -- content_type : TBasicType TUTF8
    IO (Ptr AppChooserButton)

-- | Creates a new t'GI.Gtk.Objects.AppChooserButton.AppChooserButton' for applications
-- that can handle content of the given type.
-- 
-- /Since: 3.0/
appChooserButtonNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@contentType@/: the content type to show applications for
    -> m AppChooserButton
    -- ^ __Returns:__ a newly created t'GI.Gtk.Objects.AppChooserButton.AppChooserButton'
appChooserButtonNew contentType = liftIO $ do
    contentType' <- textToCString contentType
    result <- gtk_app_chooser_button_new contentType'
    checkUnexpectedReturnNULL "appChooserButtonNew" result
    result' <- (newObject AppChooserButton) result
    freeMem contentType'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method AppChooserButton::append_custom_item
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserButton"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the name of the custom item"
--                 , sinceVersion = Nothing
--                 }
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
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the label for the custom item"
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
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the icon for the custom item"
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

foreign import ccall "gtk_app_chooser_button_append_custom_item" gtk_app_chooser_button_append_custom_item :: 
    Ptr AppChooserButton ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserButton"})
    CString ->                              -- name : TBasicType TUTF8
    CString ->                              -- label : TBasicType TUTF8
    Ptr Gio.Icon.Icon ->                    -- icon : TInterface (Name {namespace = "Gio", name = "Icon"})
    IO ()

-- | Appends a custom item to the list of applications that is shown
-- in the popup; the item name must be unique per-widget.
-- Clients can use the provided name as a detail for the
-- [AppChooserButton::customItemActivated]("GI.Gtk.Objects.AppChooserButton#g:signal:customItemActivated") signal, to add a
-- callback for the activation of a particular custom item in the list.
-- See also 'GI.Gtk.Objects.AppChooserButton.appChooserButtonAppendSeparator'.
-- 
-- /Since: 3.0/
appChooserButtonAppendCustomItem ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserButton a, Gio.Icon.IsIcon b) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserButton.AppChooserButton'
    -> T.Text
    -- ^ /@name@/: the name of the custom item
    -> T.Text
    -- ^ /@label@/: the label for the custom item
    -> b
    -- ^ /@icon@/: the icon for the custom item
    -> m ()
appChooserButtonAppendCustomItem self name label icon = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    name' <- textToCString name
    label' <- textToCString label
    icon' <- unsafeManagedPtrCastPtr icon
    gtk_app_chooser_button_append_custom_item self' name' label' icon'
    touchManagedPtr self
    touchManagedPtr icon
    freeMem name'
    freeMem label'
    return ()

#if defined(ENABLE_OVERLOADING)
data AppChooserButtonAppendCustomItemMethodInfo
instance (signature ~ (T.Text -> T.Text -> b -> m ()), MonadIO m, IsAppChooserButton a, Gio.Icon.IsIcon b) => O.OverloadedMethod AppChooserButtonAppendCustomItemMethodInfo a signature where
    overloadedMethod = appChooserButtonAppendCustomItem

instance O.OverloadedMethodInfo AppChooserButtonAppendCustomItemMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserButton.appChooserButtonAppendCustomItem",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserButton.html#v:appChooserButtonAppendCustomItem"
        })


#endif

-- method AppChooserButton::append_separator
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserButton"
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

foreign import ccall "gtk_app_chooser_button_append_separator" gtk_app_chooser_button_append_separator :: 
    Ptr AppChooserButton ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserButton"})
    IO ()

-- | Appends a separator to the list of applications that is shown
-- in the popup.
-- 
-- /Since: 3.0/
appChooserButtonAppendSeparator ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserButton a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserButton.AppChooserButton'
    -> m ()
appChooserButtonAppendSeparator self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    gtk_app_chooser_button_append_separator self'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data AppChooserButtonAppendSeparatorMethodInfo
instance (signature ~ (m ()), MonadIO m, IsAppChooserButton a) => O.OverloadedMethod AppChooserButtonAppendSeparatorMethodInfo a signature where
    overloadedMethod = appChooserButtonAppendSeparator

instance O.OverloadedMethodInfo AppChooserButtonAppendSeparatorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserButton.appChooserButtonAppendSeparator",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserButton.html#v:appChooserButtonAppendSeparator"
        })


#endif

-- method AppChooserButton::get_heading
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserButton"
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
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_app_chooser_button_get_heading" gtk_app_chooser_button_get_heading :: 
    Ptr AppChooserButton ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserButton"})
    IO CString

-- | Returns the text to display at the top of the dialog.
appChooserButtonGetHeading ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserButton a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserButton.AppChooserButton'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the text to display at the top of the dialog,
    --     or 'P.Nothing', in which case a default text is displayed
appChooserButtonGetHeading self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_app_chooser_button_get_heading self'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr self
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data AppChooserButtonGetHeadingMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsAppChooserButton a) => O.OverloadedMethod AppChooserButtonGetHeadingMethodInfo a signature where
    overloadedMethod = appChooserButtonGetHeading

instance O.OverloadedMethodInfo AppChooserButtonGetHeadingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserButton.appChooserButtonGetHeading",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserButton.html#v:appChooserButtonGetHeading"
        })


#endif

-- method AppChooserButton::get_show_default_item
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserButton"
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

foreign import ccall "gtk_app_chooser_button_get_show_default_item" gtk_app_chooser_button_get_show_default_item :: 
    Ptr AppChooserButton ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserButton"})
    IO CInt

-- | Returns the current value of the [AppChooserButton:showDefaultItem]("GI.Gtk.Objects.AppChooserButton#g:attr:showDefaultItem")
-- property.
-- 
-- /Since: 3.2/
appChooserButtonGetShowDefaultItem ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserButton a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserButton.AppChooserButton'
    -> m Bool
    -- ^ __Returns:__ the value of [AppChooserButton:showDefaultItem]("GI.Gtk.Objects.AppChooserButton#g:attr:showDefaultItem")
appChooserButtonGetShowDefaultItem self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_app_chooser_button_get_show_default_item self'
    let result' = (/= 0) result
    touchManagedPtr self
    return result'

#if defined(ENABLE_OVERLOADING)
data AppChooserButtonGetShowDefaultItemMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsAppChooserButton a) => O.OverloadedMethod AppChooserButtonGetShowDefaultItemMethodInfo a signature where
    overloadedMethod = appChooserButtonGetShowDefaultItem

instance O.OverloadedMethodInfo AppChooserButtonGetShowDefaultItemMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserButton.appChooserButtonGetShowDefaultItem",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserButton.html#v:appChooserButtonGetShowDefaultItem"
        })


#endif

-- method AppChooserButton::get_show_dialog_item
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserButton"
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

foreign import ccall "gtk_app_chooser_button_get_show_dialog_item" gtk_app_chooser_button_get_show_dialog_item :: 
    Ptr AppChooserButton ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserButton"})
    IO CInt

-- | Returns the current value of the [AppChooserButton:showDialogItem]("GI.Gtk.Objects.AppChooserButton#g:attr:showDialogItem")
-- property.
-- 
-- /Since: 3.0/
appChooserButtonGetShowDialogItem ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserButton a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserButton.AppChooserButton'
    -> m Bool
    -- ^ __Returns:__ the value of [AppChooserButton:showDialogItem]("GI.Gtk.Objects.AppChooserButton#g:attr:showDialogItem")
appChooserButtonGetShowDialogItem self = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    result <- gtk_app_chooser_button_get_show_dialog_item self'
    let result' = (/= 0) result
    touchManagedPtr self
    return result'

#if defined(ENABLE_OVERLOADING)
data AppChooserButtonGetShowDialogItemMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsAppChooserButton a) => O.OverloadedMethod AppChooserButtonGetShowDialogItemMethodInfo a signature where
    overloadedMethod = appChooserButtonGetShowDialogItem

instance O.OverloadedMethodInfo AppChooserButtonGetShowDialogItemMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserButton.appChooserButtonGetShowDialogItem",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserButton.html#v:appChooserButtonGetShowDialogItem"
        })


#endif

-- method AppChooserButton::set_active_custom_item
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserButton"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the name of the custom item"
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

foreign import ccall "gtk_app_chooser_button_set_active_custom_item" gtk_app_chooser_button_set_active_custom_item :: 
    Ptr AppChooserButton ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserButton"})
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Selects a custom item previously added with
-- 'GI.Gtk.Objects.AppChooserButton.appChooserButtonAppendCustomItem'.
-- 
-- Use 'GI.Gtk.Interfaces.AppChooser.appChooserRefresh' to bring the selection
-- to its initial state.
-- 
-- /Since: 3.0/
appChooserButtonSetActiveCustomItem ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserButton a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserButton.AppChooserButton'
    -> T.Text
    -- ^ /@name@/: the name of the custom item
    -> m ()
appChooserButtonSetActiveCustomItem self name = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    name' <- textToCString name
    gtk_app_chooser_button_set_active_custom_item self' name'
    touchManagedPtr self
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data AppChooserButtonSetActiveCustomItemMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsAppChooserButton a) => O.OverloadedMethod AppChooserButtonSetActiveCustomItemMethodInfo a signature where
    overloadedMethod = appChooserButtonSetActiveCustomItem

instance O.OverloadedMethodInfo AppChooserButtonSetActiveCustomItemMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserButton.appChooserButtonSetActiveCustomItem",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserButton.html#v:appChooserButtonSetActiveCustomItem"
        })


#endif

-- method AppChooserButton::set_heading
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserButton"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "heading"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a string containing Pango markup"
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

foreign import ccall "gtk_app_chooser_button_set_heading" gtk_app_chooser_button_set_heading :: 
    Ptr AppChooserButton ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserButton"})
    CString ->                              -- heading : TBasicType TUTF8
    IO ()

-- | Sets the text to display at the top of the dialog.
-- If the heading is not set, the dialog displays a default text.
appChooserButtonSetHeading ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserButton a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserButton.AppChooserButton'
    -> T.Text
    -- ^ /@heading@/: a string containing Pango markup
    -> m ()
appChooserButtonSetHeading self heading = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    heading' <- textToCString heading
    gtk_app_chooser_button_set_heading self' heading'
    touchManagedPtr self
    freeMem heading'
    return ()

#if defined(ENABLE_OVERLOADING)
data AppChooserButtonSetHeadingMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsAppChooserButton a) => O.OverloadedMethod AppChooserButtonSetHeadingMethodInfo a signature where
    overloadedMethod = appChooserButtonSetHeading

instance O.OverloadedMethodInfo AppChooserButtonSetHeadingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserButton.appChooserButtonSetHeading",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserButton.html#v:appChooserButtonSetHeading"
        })


#endif

-- method AppChooserButton::set_show_default_item
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserButton"
--                 , sinceVersion = Nothing
--                 }
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
--                     Just "the new value for #GtkAppChooserButton:show-default-item"
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

foreign import ccall "gtk_app_chooser_button_set_show_default_item" gtk_app_chooser_button_set_show_default_item :: 
    Ptr AppChooserButton ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserButton"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Sets whether the dropdown menu of this button should show the
-- default application for the given content type at top.
-- 
-- /Since: 3.2/
appChooserButtonSetShowDefaultItem ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserButton a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserButton.AppChooserButton'
    -> Bool
    -- ^ /@setting@/: the new value for [AppChooserButton:showDefaultItem]("GI.Gtk.Objects.AppChooserButton#g:attr:showDefaultItem")
    -> m ()
appChooserButtonSetShowDefaultItem self setting = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    let setting' = (fromIntegral . fromEnum) setting
    gtk_app_chooser_button_set_show_default_item self' setting'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data AppChooserButtonSetShowDefaultItemMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsAppChooserButton a) => O.OverloadedMethod AppChooserButtonSetShowDefaultItemMethodInfo a signature where
    overloadedMethod = appChooserButtonSetShowDefaultItem

instance O.OverloadedMethodInfo AppChooserButtonSetShowDefaultItemMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserButton.appChooserButtonSetShowDefaultItem",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserButton.html#v:appChooserButtonSetShowDefaultItem"
        })


#endif

-- method AppChooserButton::set_show_dialog_item
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "self"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "AppChooserButton" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkAppChooserButton"
--                 , sinceVersion = Nothing
--                 }
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
--                     Just "the new value for #GtkAppChooserButton:show-dialog-item"
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

foreign import ccall "gtk_app_chooser_button_set_show_dialog_item" gtk_app_chooser_button_set_show_dialog_item :: 
    Ptr AppChooserButton ->                 -- self : TInterface (Name {namespace = "Gtk", name = "AppChooserButton"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Sets whether the dropdown menu of this button should show an
-- entry to trigger a t'GI.Gtk.Objects.AppChooserDialog.AppChooserDialog'.
-- 
-- /Since: 3.0/
appChooserButtonSetShowDialogItem ::
    (B.CallStack.HasCallStack, MonadIO m, IsAppChooserButton a) =>
    a
    -- ^ /@self@/: a t'GI.Gtk.Objects.AppChooserButton.AppChooserButton'
    -> Bool
    -- ^ /@setting@/: the new value for [AppChooserButton:showDialogItem]("GI.Gtk.Objects.AppChooserButton#g:attr:showDialogItem")
    -> m ()
appChooserButtonSetShowDialogItem self setting = liftIO $ do
    self' <- unsafeManagedPtrCastPtr self
    let setting' = (fromIntegral . fromEnum) setting
    gtk_app_chooser_button_set_show_dialog_item self' setting'
    touchManagedPtr self
    return ()

#if defined(ENABLE_OVERLOADING)
data AppChooserButtonSetShowDialogItemMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsAppChooserButton a) => O.OverloadedMethod AppChooserButtonSetShowDialogItemMethodInfo a signature where
    overloadedMethod = appChooserButtonSetShowDialogItem

instance O.OverloadedMethodInfo AppChooserButtonSetShowDialogItemMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.AppChooserButton.appChooserButtonSetShowDialogItem",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-AppChooserButton.html#v:appChooserButtonSetShowDialogItem"
        })


#endif


