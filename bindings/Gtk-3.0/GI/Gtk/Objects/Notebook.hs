{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.Notebook.Notebook' widget is a t'GI.Gtk.Objects.Container.Container' whose children are pages that
-- can be switched between using tab labels along one edge.
-- 
-- There are many configuration options for GtkNotebook. Among other
-- things, you can choose on which edge the tabs appear
-- (see 'GI.Gtk.Objects.Notebook.notebookSetTabPos'), whether, if there are too many
-- tabs to fit the notebook should be made bigger or scrolling
-- arrows added (see 'GI.Gtk.Objects.Notebook.notebookSetScrollable'), and whether there
-- will be a popup menu allowing the users to switch pages.
-- (see 'GI.Gtk.Objects.Notebook.notebookPopupEnable', 'GI.Gtk.Objects.Notebook.notebookPopupDisable')
-- 
-- = GtkNotebook as GtkBuildable
-- 
-- The GtkNotebook implementation of the t'GI.Gtk.Interfaces.Buildable.Buildable' interface
-- supports placing children into tabs by specifying “tab” as the
-- “type” attribute of a @\<child>@ element. Note that the content
-- of the tab must be created before the tab can be filled.
-- A tab child can be specified without specifying a @\<child>@
-- type attribute.
-- 
-- To add a child widget in the notebooks action area, specify
-- \"action-start\" or “action-end” as the “type” attribute of the
-- @\<child>@ element.
-- 
-- An example of a UI definition fragment with GtkNotebook:
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkNotebook">
-- >  <child>
-- >    <object class="GtkLabel" id="notebook-content">
-- >      <property name="label">Content</property>
-- >    </object>
-- >  </child>
-- >  <child type="tab">
-- >    <object class="GtkLabel" id="notebook-tab">
-- >      <property name="label">Tab</property>
-- >    </object>
-- >  </child>
-- ></object>
-- 
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >notebook
-- >├── header.top
-- >│   ├── [<action widget>]
-- >│   ├── tabs
-- >│   │   ├── [arrow]
-- >│   │   ├── tab
-- >│   │   │   ╰── <tab label>
-- >┊   ┊   ┊
-- >│   │   ├── tab[.reorderable-page]
-- >│   │   │   ╰── <tab label>
-- >│   │   ╰── [arrow]
-- >│   ╰── [<action widget>]
-- >│
-- >╰── stack
-- >    ├── <child>
-- >    ┊
-- >    ╰── <child>
-- 
-- 
-- GtkNotebook has a main CSS node with name notebook, a subnode
-- with name header and below that a subnode with name tabs which
-- contains one subnode per tab with name tab.
-- 
-- If action widgets are present, their CSS nodes are placed next
-- to the tabs node. If the notebook is scrollable, CSS nodes with
-- name arrow are placed as first and last child of the tabs node.
-- 
-- The main node gets the .frame style class when the notebook
-- has a border (see 'GI.Gtk.Objects.Notebook.notebookSetShowBorder').
-- 
-- The header node gets one of the style class .top, .bottom,
-- .left or .right, depending on where the tabs are placed. For
-- reorderable pages, the tab node gets the .reorderable-page class.
-- 
-- A tab node gets the .dnd style class while it is moved with drag-and-drop.
-- 
-- The nodes are always arranged from left-to-right, regarldess of text direction.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Notebook
    ( 

-- * Exported types
    Notebook(..)                            ,
    IsNotebook                              ,
    toNotebook                              ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [appendPage]("GI.Gtk.Objects.Notebook#g:method:appendPage"), [appendPageMenu]("GI.Gtk.Objects.Notebook#g:method:appendPageMenu"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [detachTab]("GI.Gtk.Objects.Notebook#g:method:detachTab"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [insertPage]("GI.Gtk.Objects.Notebook#g:method:insertPage"), [insertPageMenu]("GI.Gtk.Objects.Notebook#g:method:insertPageMenu"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [nextPage]("GI.Gtk.Objects.Notebook#g:method:nextPage"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [pageNum]("GI.Gtk.Objects.Notebook#g:method:pageNum"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [popupDisable]("GI.Gtk.Objects.Notebook#g:method:popupDisable"), [popupEnable]("GI.Gtk.Objects.Notebook#g:method:popupEnable"), [prependPage]("GI.Gtk.Objects.Notebook#g:method:prependPage"), [prependPageMenu]("GI.Gtk.Objects.Notebook#g:method:prependPageMenu"), [prevPage]("GI.Gtk.Objects.Notebook#g:method:prevPage"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removePage]("GI.Gtk.Objects.Notebook#g:method:removePage"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reorderChild]("GI.Gtk.Objects.Notebook#g:method:reorderChild"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getActionWidget]("GI.Gtk.Objects.Notebook#g:method:getActionWidget"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getCurrentPage]("GI.Gtk.Objects.Notebook#g:method:getCurrentPage"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getGroupName]("GI.Gtk.Objects.Notebook#g:method:getGroupName"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMenuLabel]("GI.Gtk.Objects.Notebook#g:method:getMenuLabel"), [getMenuLabelText]("GI.Gtk.Objects.Notebook#g:method:getMenuLabelText"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getNPages]("GI.Gtk.Objects.Notebook#g:method:getNPages"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getNthPage]("GI.Gtk.Objects.Notebook#g:method:getNthPage"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getScrollable]("GI.Gtk.Objects.Notebook#g:method:getScrollable"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getShowBorder]("GI.Gtk.Objects.Notebook#g:method:getShowBorder"), [getShowTabs]("GI.Gtk.Objects.Notebook#g:method:getShowTabs"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTabDetachable]("GI.Gtk.Objects.Notebook#g:method:getTabDetachable"), [getTabHborder]("GI.Gtk.Objects.Notebook#g:method:getTabHborder"), [getTabLabel]("GI.Gtk.Objects.Notebook#g:method:getTabLabel"), [getTabLabelText]("GI.Gtk.Objects.Notebook#g:method:getTabLabelText"), [getTabPos]("GI.Gtk.Objects.Notebook#g:method:getTabPos"), [getTabReorderable]("GI.Gtk.Objects.Notebook#g:method:getTabReorderable"), [getTabVborder]("GI.Gtk.Objects.Notebook#g:method:getTabVborder"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setActionWidget]("GI.Gtk.Objects.Notebook#g:method:setActionWidget"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setCurrentPage]("GI.Gtk.Objects.Notebook#g:method:setCurrentPage"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setGroupName]("GI.Gtk.Objects.Notebook#g:method:setGroupName"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMenuLabel]("GI.Gtk.Objects.Notebook#g:method:setMenuLabel"), [setMenuLabelText]("GI.Gtk.Objects.Notebook#g:method:setMenuLabelText"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setScrollable]("GI.Gtk.Objects.Notebook#g:method:setScrollable"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setShowBorder]("GI.Gtk.Objects.Notebook#g:method:setShowBorder"), [setShowTabs]("GI.Gtk.Objects.Notebook#g:method:setShowTabs"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTabDetachable]("GI.Gtk.Objects.Notebook#g:method:setTabDetachable"), [setTabLabel]("GI.Gtk.Objects.Notebook#g:method:setTabLabel"), [setTabLabelText]("GI.Gtk.Objects.Notebook#g:method:setTabLabelText"), [setTabPos]("GI.Gtk.Objects.Notebook#g:method:setTabPos"), [setTabReorderable]("GI.Gtk.Objects.Notebook#g:method:setTabReorderable"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveNotebookMethod                   ,
#endif

-- ** appendPage #method:appendPage#

#if defined(ENABLE_OVERLOADING)
    NotebookAppendPageMethodInfo            ,
#endif
    notebookAppendPage                      ,


-- ** appendPageMenu #method:appendPageMenu#

#if defined(ENABLE_OVERLOADING)
    NotebookAppendPageMenuMethodInfo        ,
#endif
    notebookAppendPageMenu                  ,


-- ** detachTab #method:detachTab#

#if defined(ENABLE_OVERLOADING)
    NotebookDetachTabMethodInfo             ,
#endif
    notebookDetachTab                       ,


-- ** getActionWidget #method:getActionWidget#

#if defined(ENABLE_OVERLOADING)
    NotebookGetActionWidgetMethodInfo       ,
#endif
    notebookGetActionWidget                 ,


-- ** getCurrentPage #method:getCurrentPage#

#if defined(ENABLE_OVERLOADING)
    NotebookGetCurrentPageMethodInfo        ,
#endif
    notebookGetCurrentPage                  ,


-- ** getGroupName #method:getGroupName#

#if defined(ENABLE_OVERLOADING)
    NotebookGetGroupNameMethodInfo          ,
#endif
    notebookGetGroupName                    ,


-- ** getMenuLabel #method:getMenuLabel#

#if defined(ENABLE_OVERLOADING)
    NotebookGetMenuLabelMethodInfo          ,
#endif
    notebookGetMenuLabel                    ,


-- ** getMenuLabelText #method:getMenuLabelText#

#if defined(ENABLE_OVERLOADING)
    NotebookGetMenuLabelTextMethodInfo      ,
#endif
    notebookGetMenuLabelText                ,


-- ** getNPages #method:getNPages#

#if defined(ENABLE_OVERLOADING)
    NotebookGetNPagesMethodInfo             ,
#endif
    notebookGetNPages                       ,


-- ** getNthPage #method:getNthPage#

#if defined(ENABLE_OVERLOADING)
    NotebookGetNthPageMethodInfo            ,
#endif
    notebookGetNthPage                      ,


-- ** getScrollable #method:getScrollable#

#if defined(ENABLE_OVERLOADING)
    NotebookGetScrollableMethodInfo         ,
#endif
    notebookGetScrollable                   ,


-- ** getShowBorder #method:getShowBorder#

#if defined(ENABLE_OVERLOADING)
    NotebookGetShowBorderMethodInfo         ,
#endif
    notebookGetShowBorder                   ,


-- ** getShowTabs #method:getShowTabs#

#if defined(ENABLE_OVERLOADING)
    NotebookGetShowTabsMethodInfo           ,
#endif
    notebookGetShowTabs                     ,


-- ** getTabDetachable #method:getTabDetachable#

#if defined(ENABLE_OVERLOADING)
    NotebookGetTabDetachableMethodInfo      ,
#endif
    notebookGetTabDetachable                ,


-- ** getTabHborder #method:getTabHborder#

#if defined(ENABLE_OVERLOADING)
    NotebookGetTabHborderMethodInfo         ,
#endif
    notebookGetTabHborder                   ,


-- ** getTabLabel #method:getTabLabel#

#if defined(ENABLE_OVERLOADING)
    NotebookGetTabLabelMethodInfo           ,
#endif
    notebookGetTabLabel                     ,


-- ** getTabLabelText #method:getTabLabelText#

#if defined(ENABLE_OVERLOADING)
    NotebookGetTabLabelTextMethodInfo       ,
#endif
    notebookGetTabLabelText                 ,


-- ** getTabPos #method:getTabPos#

#if defined(ENABLE_OVERLOADING)
    NotebookGetTabPosMethodInfo             ,
#endif
    notebookGetTabPos                       ,


-- ** getTabReorderable #method:getTabReorderable#

#if defined(ENABLE_OVERLOADING)
    NotebookGetTabReorderableMethodInfo     ,
#endif
    notebookGetTabReorderable               ,


-- ** getTabVborder #method:getTabVborder#

#if defined(ENABLE_OVERLOADING)
    NotebookGetTabVborderMethodInfo         ,
#endif
    notebookGetTabVborder                   ,


-- ** insertPage #method:insertPage#

#if defined(ENABLE_OVERLOADING)
    NotebookInsertPageMethodInfo            ,
#endif
    notebookInsertPage                      ,


-- ** insertPageMenu #method:insertPageMenu#

#if defined(ENABLE_OVERLOADING)
    NotebookInsertPageMenuMethodInfo        ,
#endif
    notebookInsertPageMenu                  ,


-- ** new #method:new#

    notebookNew                             ,


-- ** nextPage #method:nextPage#

#if defined(ENABLE_OVERLOADING)
    NotebookNextPageMethodInfo              ,
#endif
    notebookNextPage                        ,


-- ** pageNum #method:pageNum#

#if defined(ENABLE_OVERLOADING)
    NotebookPageNumMethodInfo               ,
#endif
    notebookPageNum                         ,


-- ** popupDisable #method:popupDisable#

#if defined(ENABLE_OVERLOADING)
    NotebookPopupDisableMethodInfo          ,
#endif
    notebookPopupDisable                    ,


-- ** popupEnable #method:popupEnable#

#if defined(ENABLE_OVERLOADING)
    NotebookPopupEnableMethodInfo           ,
#endif
    notebookPopupEnable                     ,


-- ** prependPage #method:prependPage#

#if defined(ENABLE_OVERLOADING)
    NotebookPrependPageMethodInfo           ,
#endif
    notebookPrependPage                     ,


-- ** prependPageMenu #method:prependPageMenu#

#if defined(ENABLE_OVERLOADING)
    NotebookPrependPageMenuMethodInfo       ,
#endif
    notebookPrependPageMenu                 ,


-- ** prevPage #method:prevPage#

#if defined(ENABLE_OVERLOADING)
    NotebookPrevPageMethodInfo              ,
#endif
    notebookPrevPage                        ,


-- ** removePage #method:removePage#

#if defined(ENABLE_OVERLOADING)
    NotebookRemovePageMethodInfo            ,
#endif
    notebookRemovePage                      ,


-- ** reorderChild #method:reorderChild#

#if defined(ENABLE_OVERLOADING)
    NotebookReorderChildMethodInfo          ,
#endif
    notebookReorderChild                    ,


-- ** setActionWidget #method:setActionWidget#

#if defined(ENABLE_OVERLOADING)
    NotebookSetActionWidgetMethodInfo       ,
#endif
    notebookSetActionWidget                 ,


-- ** setCurrentPage #method:setCurrentPage#

#if defined(ENABLE_OVERLOADING)
    NotebookSetCurrentPageMethodInfo        ,
#endif
    notebookSetCurrentPage                  ,


-- ** setGroupName #method:setGroupName#

#if defined(ENABLE_OVERLOADING)
    NotebookSetGroupNameMethodInfo          ,
#endif
    notebookSetGroupName                    ,


-- ** setMenuLabel #method:setMenuLabel#

#if defined(ENABLE_OVERLOADING)
    NotebookSetMenuLabelMethodInfo          ,
#endif
    notebookSetMenuLabel                    ,


-- ** setMenuLabelText #method:setMenuLabelText#

#if defined(ENABLE_OVERLOADING)
    NotebookSetMenuLabelTextMethodInfo      ,
#endif
    notebookSetMenuLabelText                ,


-- ** setScrollable #method:setScrollable#

#if defined(ENABLE_OVERLOADING)
    NotebookSetScrollableMethodInfo         ,
#endif
    notebookSetScrollable                   ,


-- ** setShowBorder #method:setShowBorder#

#if defined(ENABLE_OVERLOADING)
    NotebookSetShowBorderMethodInfo         ,
#endif
    notebookSetShowBorder                   ,


-- ** setShowTabs #method:setShowTabs#

#if defined(ENABLE_OVERLOADING)
    NotebookSetShowTabsMethodInfo           ,
#endif
    notebookSetShowTabs                     ,


-- ** setTabDetachable #method:setTabDetachable#

#if defined(ENABLE_OVERLOADING)
    NotebookSetTabDetachableMethodInfo      ,
#endif
    notebookSetTabDetachable                ,


-- ** setTabLabel #method:setTabLabel#

#if defined(ENABLE_OVERLOADING)
    NotebookSetTabLabelMethodInfo           ,
#endif
    notebookSetTabLabel                     ,


-- ** setTabLabelText #method:setTabLabelText#

#if defined(ENABLE_OVERLOADING)
    NotebookSetTabLabelTextMethodInfo       ,
#endif
    notebookSetTabLabelText                 ,


-- ** setTabPos #method:setTabPos#

#if defined(ENABLE_OVERLOADING)
    NotebookSetTabPosMethodInfo             ,
#endif
    notebookSetTabPos                       ,


-- ** setTabReorderable #method:setTabReorderable#

#if defined(ENABLE_OVERLOADING)
    NotebookSetTabReorderableMethodInfo     ,
#endif
    notebookSetTabReorderable               ,




 -- * Properties


-- ** enablePopup #attr:enablePopup#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    NotebookEnablePopupPropertyInfo         ,
#endif
    constructNotebookEnablePopup            ,
    getNotebookEnablePopup                  ,
#if defined(ENABLE_OVERLOADING)
    notebookEnablePopup                     ,
#endif
    setNotebookEnablePopup                  ,


-- ** groupName #attr:groupName#
-- | Group name for tab drag and drop.
-- 
-- /Since: 2.24/

#if defined(ENABLE_OVERLOADING)
    NotebookGroupNamePropertyInfo           ,
#endif
    clearNotebookGroupName                  ,
    constructNotebookGroupName              ,
    getNotebookGroupName                    ,
#if defined(ENABLE_OVERLOADING)
    notebookGroupName                       ,
#endif
    setNotebookGroupName                    ,


-- ** page #attr:page#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    NotebookPagePropertyInfo                ,
#endif
    constructNotebookPage                   ,
    getNotebookPage                         ,
#if defined(ENABLE_OVERLOADING)
    notebookPage                            ,
#endif
    setNotebookPage                         ,


-- ** scrollable #attr:scrollable#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    NotebookScrollablePropertyInfo          ,
#endif
    constructNotebookScrollable             ,
    getNotebookScrollable                   ,
#if defined(ENABLE_OVERLOADING)
    notebookScrollable                      ,
#endif
    setNotebookScrollable                   ,


-- ** showBorder #attr:showBorder#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    NotebookShowBorderPropertyInfo          ,
#endif
    constructNotebookShowBorder             ,
    getNotebookShowBorder                   ,
#if defined(ENABLE_OVERLOADING)
    notebookShowBorder                      ,
#endif
    setNotebookShowBorder                   ,


-- ** showTabs #attr:showTabs#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    NotebookShowTabsPropertyInfo            ,
#endif
    constructNotebookShowTabs               ,
    getNotebookShowTabs                     ,
#if defined(ENABLE_OVERLOADING)
    notebookShowTabs                        ,
#endif
    setNotebookShowTabs                     ,


-- ** tabPos #attr:tabPos#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    NotebookTabPosPropertyInfo              ,
#endif
    constructNotebookTabPos                 ,
    getNotebookTabPos                       ,
#if defined(ENABLE_OVERLOADING)
    notebookTabPos                          ,
#endif
    setNotebookTabPos                       ,




 -- * Signals


-- ** changeCurrentPage #signal:changeCurrentPage#

    NotebookChangeCurrentPageCallback       ,
#if defined(ENABLE_OVERLOADING)
    NotebookChangeCurrentPageSignalInfo     ,
#endif
    afterNotebookChangeCurrentPage          ,
    onNotebookChangeCurrentPage             ,


-- ** createWindow #signal:createWindow#

    NotebookCreateWindowCallback            ,
#if defined(ENABLE_OVERLOADING)
    NotebookCreateWindowSignalInfo          ,
#endif
    afterNotebookCreateWindow               ,
    onNotebookCreateWindow                  ,


-- ** focusTab #signal:focusTab#

    NotebookFocusTabCallback                ,
#if defined(ENABLE_OVERLOADING)
    NotebookFocusTabSignalInfo              ,
#endif
    afterNotebookFocusTab                   ,
    onNotebookFocusTab                      ,


-- ** moveFocusOut #signal:moveFocusOut#

    NotebookMoveFocusOutCallback            ,
#if defined(ENABLE_OVERLOADING)
    NotebookMoveFocusOutSignalInfo          ,
#endif
    afterNotebookMoveFocusOut               ,
    onNotebookMoveFocusOut                  ,


-- ** pageAdded #signal:pageAdded#

    NotebookPageAddedCallback               ,
#if defined(ENABLE_OVERLOADING)
    NotebookPageAddedSignalInfo             ,
#endif
    afterNotebookPageAdded                  ,
    onNotebookPageAdded                     ,


-- ** pageRemoved #signal:pageRemoved#

    NotebookPageRemovedCallback             ,
#if defined(ENABLE_OVERLOADING)
    NotebookPageRemovedSignalInfo           ,
#endif
    afterNotebookPageRemoved                ,
    onNotebookPageRemoved                   ,


-- ** pageReordered #signal:pageReordered#

    NotebookPageReorderedCallback           ,
#if defined(ENABLE_OVERLOADING)
    NotebookPageReorderedSignalInfo         ,
#endif
    afterNotebookPageReordered              ,
    onNotebookPageReordered                 ,


-- ** reorderTab #signal:reorderTab#

    NotebookReorderTabCallback              ,
#if defined(ENABLE_OVERLOADING)
    NotebookReorderTabSignalInfo            ,
#endif
    afterNotebookReorderTab                 ,
    onNotebookReorderTab                    ,


-- ** selectPage #signal:selectPage#

    NotebookSelectPageCallback              ,
#if defined(ENABLE_OVERLOADING)
    NotebookSelectPageSignalInfo            ,
#endif
    afterNotebookSelectPage                 ,
    onNotebookSelectPage                    ,


-- ** switchPage #signal:switchPage#

    NotebookSwitchPageCallback              ,
#if defined(ENABLE_OVERLOADING)
    NotebookSwitchPageSignalInfo            ,
#endif
    afterNotebookSwitchPage                 ,
    onNotebookSwitchPage                    ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype Notebook = Notebook (SP.ManagedPtr Notebook)
    deriving (Eq)

instance SP.ManagedPtrNewtype Notebook where
    toManagedPtr (Notebook p) = p

foreign import ccall "gtk_notebook_get_type"
    c_gtk_notebook_get_type :: IO B.Types.GType

instance B.Types.TypedObject Notebook where
    glibType = c_gtk_notebook_get_type

instance B.Types.GObject Notebook

-- | Type class for types which can be safely cast to `Notebook`, for instance with `toNotebook`.
class (SP.GObject o, O.IsDescendantOf Notebook o) => IsNotebook o
instance (SP.GObject o, O.IsDescendantOf Notebook o) => IsNotebook o

instance O.HasParentTypes Notebook
type instance O.ParentTypes Notebook = '[Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `Notebook`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toNotebook :: (MIO.MonadIO m, IsNotebook o) => o -> m Notebook
toNotebook = MIO.liftIO . B.ManagedPtr.unsafeCastTo Notebook

-- | Convert 'Notebook' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Notebook) where
    gvalueGType_ = c_gtk_notebook_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Notebook)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Notebook)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Notebook ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveNotebookMethod (t :: Symbol) (o :: *) :: * where
    ResolveNotebookMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveNotebookMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveNotebookMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveNotebookMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveNotebookMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveNotebookMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveNotebookMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveNotebookMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveNotebookMethod "appendPage" o = NotebookAppendPageMethodInfo
    ResolveNotebookMethod "appendPageMenu" o = NotebookAppendPageMenuMethodInfo
    ResolveNotebookMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveNotebookMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveNotebookMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveNotebookMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveNotebookMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveNotebookMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveNotebookMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveNotebookMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveNotebookMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveNotebookMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveNotebookMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveNotebookMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveNotebookMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveNotebookMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveNotebookMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveNotebookMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveNotebookMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveNotebookMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveNotebookMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveNotebookMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveNotebookMethod "detachTab" o = NotebookDetachTabMethodInfo
    ResolveNotebookMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveNotebookMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveNotebookMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveNotebookMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveNotebookMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveNotebookMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveNotebookMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveNotebookMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveNotebookMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveNotebookMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveNotebookMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveNotebookMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveNotebookMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveNotebookMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveNotebookMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveNotebookMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveNotebookMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveNotebookMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveNotebookMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveNotebookMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveNotebookMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveNotebookMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveNotebookMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveNotebookMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveNotebookMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveNotebookMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveNotebookMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveNotebookMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveNotebookMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveNotebookMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveNotebookMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveNotebookMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveNotebookMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveNotebookMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveNotebookMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveNotebookMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveNotebookMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveNotebookMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveNotebookMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveNotebookMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveNotebookMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveNotebookMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveNotebookMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveNotebookMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveNotebookMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveNotebookMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveNotebookMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveNotebookMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveNotebookMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveNotebookMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveNotebookMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveNotebookMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveNotebookMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveNotebookMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveNotebookMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveNotebookMethod "insertPage" o = NotebookInsertPageMethodInfo
    ResolveNotebookMethod "insertPageMenu" o = NotebookInsertPageMenuMethodInfo
    ResolveNotebookMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveNotebookMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveNotebookMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveNotebookMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveNotebookMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveNotebookMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveNotebookMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveNotebookMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveNotebookMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveNotebookMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveNotebookMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveNotebookMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveNotebookMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveNotebookMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveNotebookMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveNotebookMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveNotebookMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveNotebookMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveNotebookMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveNotebookMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveNotebookMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveNotebookMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveNotebookMethod "nextPage" o = NotebookNextPageMethodInfo
    ResolveNotebookMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveNotebookMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveNotebookMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveNotebookMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveNotebookMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveNotebookMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveNotebookMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveNotebookMethod "pageNum" o = NotebookPageNumMethodInfo
    ResolveNotebookMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveNotebookMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveNotebookMethod "popupDisable" o = NotebookPopupDisableMethodInfo
    ResolveNotebookMethod "popupEnable" o = NotebookPopupEnableMethodInfo
    ResolveNotebookMethod "prependPage" o = NotebookPrependPageMethodInfo
    ResolveNotebookMethod "prependPageMenu" o = NotebookPrependPageMenuMethodInfo
    ResolveNotebookMethod "prevPage" o = NotebookPrevPageMethodInfo
    ResolveNotebookMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveNotebookMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveNotebookMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveNotebookMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveNotebookMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveNotebookMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveNotebookMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveNotebookMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveNotebookMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveNotebookMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveNotebookMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveNotebookMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveNotebookMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveNotebookMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveNotebookMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveNotebookMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveNotebookMethod "removePage" o = NotebookRemovePageMethodInfo
    ResolveNotebookMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveNotebookMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveNotebookMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveNotebookMethod "reorderChild" o = NotebookReorderChildMethodInfo
    ResolveNotebookMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveNotebookMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveNotebookMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveNotebookMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveNotebookMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveNotebookMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveNotebookMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveNotebookMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveNotebookMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveNotebookMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveNotebookMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveNotebookMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveNotebookMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveNotebookMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveNotebookMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveNotebookMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveNotebookMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveNotebookMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveNotebookMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveNotebookMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveNotebookMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveNotebookMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveNotebookMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveNotebookMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveNotebookMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveNotebookMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveNotebookMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveNotebookMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveNotebookMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveNotebookMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveNotebookMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveNotebookMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveNotebookMethod "getActionWidget" o = NotebookGetActionWidgetMethodInfo
    ResolveNotebookMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveNotebookMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveNotebookMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveNotebookMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveNotebookMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveNotebookMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveNotebookMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveNotebookMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveNotebookMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveNotebookMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveNotebookMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveNotebookMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveNotebookMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveNotebookMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveNotebookMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveNotebookMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveNotebookMethod "getCurrentPage" o = NotebookGetCurrentPageMethodInfo
    ResolveNotebookMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveNotebookMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveNotebookMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveNotebookMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveNotebookMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveNotebookMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveNotebookMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveNotebookMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveNotebookMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveNotebookMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveNotebookMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveNotebookMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveNotebookMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveNotebookMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveNotebookMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveNotebookMethod "getGroupName" o = NotebookGetGroupNameMethodInfo
    ResolveNotebookMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveNotebookMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveNotebookMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveNotebookMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveNotebookMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveNotebookMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveNotebookMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveNotebookMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveNotebookMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveNotebookMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveNotebookMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveNotebookMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveNotebookMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveNotebookMethod "getMenuLabel" o = NotebookGetMenuLabelMethodInfo
    ResolveNotebookMethod "getMenuLabelText" o = NotebookGetMenuLabelTextMethodInfo
    ResolveNotebookMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveNotebookMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveNotebookMethod "getNPages" o = NotebookGetNPagesMethodInfo
    ResolveNotebookMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveNotebookMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveNotebookMethod "getNthPage" o = NotebookGetNthPageMethodInfo
    ResolveNotebookMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveNotebookMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveNotebookMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveNotebookMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveNotebookMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveNotebookMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveNotebookMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveNotebookMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveNotebookMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveNotebookMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveNotebookMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveNotebookMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveNotebookMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveNotebookMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveNotebookMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveNotebookMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveNotebookMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveNotebookMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveNotebookMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveNotebookMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveNotebookMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveNotebookMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveNotebookMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveNotebookMethod "getScrollable" o = NotebookGetScrollableMethodInfo
    ResolveNotebookMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveNotebookMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveNotebookMethod "getShowBorder" o = NotebookGetShowBorderMethodInfo
    ResolveNotebookMethod "getShowTabs" o = NotebookGetShowTabsMethodInfo
    ResolveNotebookMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveNotebookMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveNotebookMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveNotebookMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveNotebookMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveNotebookMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveNotebookMethod "getTabDetachable" o = NotebookGetTabDetachableMethodInfo
    ResolveNotebookMethod "getTabHborder" o = NotebookGetTabHborderMethodInfo
    ResolveNotebookMethod "getTabLabel" o = NotebookGetTabLabelMethodInfo
    ResolveNotebookMethod "getTabLabelText" o = NotebookGetTabLabelTextMethodInfo
    ResolveNotebookMethod "getTabPos" o = NotebookGetTabPosMethodInfo
    ResolveNotebookMethod "getTabReorderable" o = NotebookGetTabReorderableMethodInfo
    ResolveNotebookMethod "getTabVborder" o = NotebookGetTabVborderMethodInfo
    ResolveNotebookMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveNotebookMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveNotebookMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveNotebookMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveNotebookMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveNotebookMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveNotebookMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveNotebookMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveNotebookMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveNotebookMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveNotebookMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveNotebookMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveNotebookMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveNotebookMethod "setActionWidget" o = NotebookSetActionWidgetMethodInfo
    ResolveNotebookMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveNotebookMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveNotebookMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveNotebookMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveNotebookMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveNotebookMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveNotebookMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveNotebookMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveNotebookMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveNotebookMethod "setCurrentPage" o = NotebookSetCurrentPageMethodInfo
    ResolveNotebookMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveNotebookMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveNotebookMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveNotebookMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveNotebookMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveNotebookMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveNotebookMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveNotebookMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveNotebookMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveNotebookMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveNotebookMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveNotebookMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveNotebookMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveNotebookMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveNotebookMethod "setGroupName" o = NotebookSetGroupNameMethodInfo
    ResolveNotebookMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveNotebookMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveNotebookMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveNotebookMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveNotebookMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveNotebookMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveNotebookMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveNotebookMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveNotebookMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveNotebookMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveNotebookMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveNotebookMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveNotebookMethod "setMenuLabel" o = NotebookSetMenuLabelMethodInfo
    ResolveNotebookMethod "setMenuLabelText" o = NotebookSetMenuLabelTextMethodInfo
    ResolveNotebookMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveNotebookMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveNotebookMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveNotebookMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveNotebookMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveNotebookMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveNotebookMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveNotebookMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveNotebookMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveNotebookMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveNotebookMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveNotebookMethod "setScrollable" o = NotebookSetScrollableMethodInfo
    ResolveNotebookMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveNotebookMethod "setShowBorder" o = NotebookSetShowBorderMethodInfo
    ResolveNotebookMethod "setShowTabs" o = NotebookSetShowTabsMethodInfo
    ResolveNotebookMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveNotebookMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveNotebookMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveNotebookMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveNotebookMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveNotebookMethod "setTabDetachable" o = NotebookSetTabDetachableMethodInfo
    ResolveNotebookMethod "setTabLabel" o = NotebookSetTabLabelMethodInfo
    ResolveNotebookMethod "setTabLabelText" o = NotebookSetTabLabelTextMethodInfo
    ResolveNotebookMethod "setTabPos" o = NotebookSetTabPosMethodInfo
    ResolveNotebookMethod "setTabReorderable" o = NotebookSetTabReorderableMethodInfo
    ResolveNotebookMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveNotebookMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveNotebookMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveNotebookMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveNotebookMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveNotebookMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveNotebookMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveNotebookMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveNotebookMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveNotebookMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveNotebookMethod t Notebook, O.OverloadedMethod info Notebook p) => OL.IsLabel t (Notebook -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveNotebookMethod t Notebook, O.OverloadedMethod info Notebook p, R.HasField t Notebook p) => R.HasField t Notebook p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveNotebookMethod t Notebook, O.OverloadedMethodInfo info Notebook) => OL.IsLabel t (O.MethodProxy info Notebook) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Notebook::change-current-page
-- | /No description available in the introspection data./
type NotebookChangeCurrentPageCallback =
    Int32
    -> IO Bool

type C_NotebookChangeCurrentPageCallback =
    Ptr Notebook ->                         -- object
    Int32 ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_NotebookChangeCurrentPageCallback`.
foreign import ccall "wrapper"
    mk_NotebookChangeCurrentPageCallback :: C_NotebookChangeCurrentPageCallback -> IO (FunPtr C_NotebookChangeCurrentPageCallback)

wrap_NotebookChangeCurrentPageCallback :: 
    GObject a => (a -> NotebookChangeCurrentPageCallback) ->
    C_NotebookChangeCurrentPageCallback
wrap_NotebookChangeCurrentPageCallback gi'cb gi'selfPtr object _ = do
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  object
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [changeCurrentPage](#signal:changeCurrentPage) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' notebook #changeCurrentPage callback
-- @
-- 
-- 
onNotebookChangeCurrentPage :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookChangeCurrentPageCallback) -> m SignalHandlerId
onNotebookChangeCurrentPage obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookChangeCurrentPageCallback wrapped
    wrapped'' <- mk_NotebookChangeCurrentPageCallback wrapped'
    connectSignalFunPtr obj "change-current-page" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [changeCurrentPage](#signal:changeCurrentPage) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' notebook #changeCurrentPage callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterNotebookChangeCurrentPage :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookChangeCurrentPageCallback) -> m SignalHandlerId
afterNotebookChangeCurrentPage obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookChangeCurrentPageCallback wrapped
    wrapped'' <- mk_NotebookChangeCurrentPageCallback wrapped'
    connectSignalFunPtr obj "change-current-page" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data NotebookChangeCurrentPageSignalInfo
instance SignalInfo NotebookChangeCurrentPageSignalInfo where
    type HaskellCallbackType NotebookChangeCurrentPageSignalInfo = NotebookChangeCurrentPageCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_NotebookChangeCurrentPageCallback cb
        cb'' <- mk_NotebookChangeCurrentPageCallback cb'
        connectSignalFunPtr obj "change-current-page" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook::change-current-page"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:signal:changeCurrentPage"})

#endif

-- signal Notebook::create-window
-- | The [createWindow](#g:signal:createWindow) signal is emitted when a detachable
-- tab is dropped on the root window.
-- 
-- A handler for this signal can create a window containing
-- a notebook where the tab will be attached. It is also
-- responsible for moving\/resizing the window and adding the
-- necessary properties to the notebook (e.g. the
-- [Notebook:groupName]("GI.Gtk.Objects.Notebook#g:attr:groupName") ).
-- 
-- /Since: 2.12/
type NotebookCreateWindowCallback =
    Gtk.Widget.Widget
    -- ^ /@page@/: the tab of /@notebook@/ that is being detached
    -> Int32
    -- ^ /@x@/: the X coordinate where the drop happens
    -> Int32
    -- ^ /@y@/: the Y coordinate where the drop happens
    -> IO Notebook
    -- ^ __Returns:__ a t'GI.Gtk.Objects.Notebook.Notebook' that /@page@/ should be
    --     added to, or 'P.Nothing'.

type C_NotebookCreateWindowCallback =
    Ptr Notebook ->                         -- object
    Ptr Gtk.Widget.Widget ->
    Int32 ->
    Int32 ->
    Ptr () ->                               -- user_data
    IO (Ptr Notebook)

-- | Generate a function pointer callable from C code, from a `C_NotebookCreateWindowCallback`.
foreign import ccall "wrapper"
    mk_NotebookCreateWindowCallback :: C_NotebookCreateWindowCallback -> IO (FunPtr C_NotebookCreateWindowCallback)

wrap_NotebookCreateWindowCallback :: 
    GObject a => (a -> NotebookCreateWindowCallback) ->
    C_NotebookCreateWindowCallback
wrap_NotebookCreateWindowCallback gi'cb gi'selfPtr page x y _ = do
    page' <- (newObject Gtk.Widget.Widget) page
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  page' x y
    result' <- unsafeManagedPtrCastPtr result
    return result'


-- | Connect a signal handler for the [createWindow](#signal:createWindow) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' notebook #createWindow callback
-- @
-- 
-- 
onNotebookCreateWindow :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookCreateWindowCallback) -> m SignalHandlerId
onNotebookCreateWindow obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookCreateWindowCallback wrapped
    wrapped'' <- mk_NotebookCreateWindowCallback wrapped'
    connectSignalFunPtr obj "create-window" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [createWindow](#signal:createWindow) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' notebook #createWindow callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterNotebookCreateWindow :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookCreateWindowCallback) -> m SignalHandlerId
afterNotebookCreateWindow obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookCreateWindowCallback wrapped
    wrapped'' <- mk_NotebookCreateWindowCallback wrapped'
    connectSignalFunPtr obj "create-window" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data NotebookCreateWindowSignalInfo
instance SignalInfo NotebookCreateWindowSignalInfo where
    type HaskellCallbackType NotebookCreateWindowSignalInfo = NotebookCreateWindowCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_NotebookCreateWindowCallback cb
        cb'' <- mk_NotebookCreateWindowCallback cb'
        connectSignalFunPtr obj "create-window" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook::create-window"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:signal:createWindow"})

#endif

-- signal Notebook::focus-tab
-- | /No description available in the introspection data./
type NotebookFocusTabCallback =
    Gtk.Enums.NotebookTab
    -> IO Bool

type C_NotebookFocusTabCallback =
    Ptr Notebook ->                         -- object
    CUInt ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_NotebookFocusTabCallback`.
foreign import ccall "wrapper"
    mk_NotebookFocusTabCallback :: C_NotebookFocusTabCallback -> IO (FunPtr C_NotebookFocusTabCallback)

wrap_NotebookFocusTabCallback :: 
    GObject a => (a -> NotebookFocusTabCallback) ->
    C_NotebookFocusTabCallback
wrap_NotebookFocusTabCallback gi'cb gi'selfPtr object _ = do
    let object' = (toEnum . fromIntegral) object
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  object'
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [focusTab](#signal:focusTab) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' notebook #focusTab callback
-- @
-- 
-- 
onNotebookFocusTab :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookFocusTabCallback) -> m SignalHandlerId
onNotebookFocusTab obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookFocusTabCallback wrapped
    wrapped'' <- mk_NotebookFocusTabCallback wrapped'
    connectSignalFunPtr obj "focus-tab" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [focusTab](#signal:focusTab) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' notebook #focusTab callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterNotebookFocusTab :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookFocusTabCallback) -> m SignalHandlerId
afterNotebookFocusTab obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookFocusTabCallback wrapped
    wrapped'' <- mk_NotebookFocusTabCallback wrapped'
    connectSignalFunPtr obj "focus-tab" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data NotebookFocusTabSignalInfo
instance SignalInfo NotebookFocusTabSignalInfo where
    type HaskellCallbackType NotebookFocusTabSignalInfo = NotebookFocusTabCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_NotebookFocusTabCallback cb
        cb'' <- mk_NotebookFocusTabCallback cb'
        connectSignalFunPtr obj "focus-tab" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook::focus-tab"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:signal:focusTab"})

#endif

-- signal Notebook::move-focus-out
-- | /No description available in the introspection data./
type NotebookMoveFocusOutCallback =
    Gtk.Enums.DirectionType
    -> IO ()

type C_NotebookMoveFocusOutCallback =
    Ptr Notebook ->                         -- object
    CUInt ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_NotebookMoveFocusOutCallback`.
foreign import ccall "wrapper"
    mk_NotebookMoveFocusOutCallback :: C_NotebookMoveFocusOutCallback -> IO (FunPtr C_NotebookMoveFocusOutCallback)

wrap_NotebookMoveFocusOutCallback :: 
    GObject a => (a -> NotebookMoveFocusOutCallback) ->
    C_NotebookMoveFocusOutCallback
wrap_NotebookMoveFocusOutCallback gi'cb gi'selfPtr object _ = do
    let object' = (toEnum . fromIntegral) object
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  object'


-- | Connect a signal handler for the [moveFocusOut](#signal:moveFocusOut) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' notebook #moveFocusOut callback
-- @
-- 
-- 
onNotebookMoveFocusOut :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookMoveFocusOutCallback) -> m SignalHandlerId
onNotebookMoveFocusOut obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookMoveFocusOutCallback wrapped
    wrapped'' <- mk_NotebookMoveFocusOutCallback wrapped'
    connectSignalFunPtr obj "move-focus-out" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [moveFocusOut](#signal:moveFocusOut) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' notebook #moveFocusOut callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterNotebookMoveFocusOut :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookMoveFocusOutCallback) -> m SignalHandlerId
afterNotebookMoveFocusOut obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookMoveFocusOutCallback wrapped
    wrapped'' <- mk_NotebookMoveFocusOutCallback wrapped'
    connectSignalFunPtr obj "move-focus-out" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data NotebookMoveFocusOutSignalInfo
instance SignalInfo NotebookMoveFocusOutSignalInfo where
    type HaskellCallbackType NotebookMoveFocusOutSignalInfo = NotebookMoveFocusOutCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_NotebookMoveFocusOutCallback cb
        cb'' <- mk_NotebookMoveFocusOutCallback cb'
        connectSignalFunPtr obj "move-focus-out" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook::move-focus-out"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:signal:moveFocusOut"})

#endif

-- signal Notebook::page-added
-- | the [pageAdded](#g:signal:pageAdded) signal is emitted in the notebook
-- right after a page is added to the notebook.
-- 
-- /Since: 2.10/
type NotebookPageAddedCallback =
    Gtk.Widget.Widget
    -- ^ /@child@/: the child t'GI.Gtk.Objects.Widget.Widget' affected
    -> Word32
    -- ^ /@pageNum@/: the new page number for /@child@/
    -> IO ()

type C_NotebookPageAddedCallback =
    Ptr Notebook ->                         -- object
    Ptr Gtk.Widget.Widget ->
    Word32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_NotebookPageAddedCallback`.
foreign import ccall "wrapper"
    mk_NotebookPageAddedCallback :: C_NotebookPageAddedCallback -> IO (FunPtr C_NotebookPageAddedCallback)

wrap_NotebookPageAddedCallback :: 
    GObject a => (a -> NotebookPageAddedCallback) ->
    C_NotebookPageAddedCallback
wrap_NotebookPageAddedCallback gi'cb gi'selfPtr child pageNum _ = do
    child' <- (newObject Gtk.Widget.Widget) child
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  child' pageNum


-- | Connect a signal handler for the [pageAdded](#signal:pageAdded) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' notebook #pageAdded callback
-- @
-- 
-- 
onNotebookPageAdded :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookPageAddedCallback) -> m SignalHandlerId
onNotebookPageAdded obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookPageAddedCallback wrapped
    wrapped'' <- mk_NotebookPageAddedCallback wrapped'
    connectSignalFunPtr obj "page-added" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [pageAdded](#signal:pageAdded) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' notebook #pageAdded callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterNotebookPageAdded :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookPageAddedCallback) -> m SignalHandlerId
afterNotebookPageAdded obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookPageAddedCallback wrapped
    wrapped'' <- mk_NotebookPageAddedCallback wrapped'
    connectSignalFunPtr obj "page-added" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data NotebookPageAddedSignalInfo
instance SignalInfo NotebookPageAddedSignalInfo where
    type HaskellCallbackType NotebookPageAddedSignalInfo = NotebookPageAddedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_NotebookPageAddedCallback cb
        cb'' <- mk_NotebookPageAddedCallback cb'
        connectSignalFunPtr obj "page-added" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook::page-added"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:signal:pageAdded"})

#endif

-- signal Notebook::page-removed
-- | the [pageRemoved](#g:signal:pageRemoved) signal is emitted in the notebook
-- right after a page is removed from the notebook.
-- 
-- /Since: 2.10/
type NotebookPageRemovedCallback =
    Gtk.Widget.Widget
    -- ^ /@child@/: the child t'GI.Gtk.Objects.Widget.Widget' affected
    -> Word32
    -- ^ /@pageNum@/: the /@child@/ page number
    -> IO ()

type C_NotebookPageRemovedCallback =
    Ptr Notebook ->                         -- object
    Ptr Gtk.Widget.Widget ->
    Word32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_NotebookPageRemovedCallback`.
foreign import ccall "wrapper"
    mk_NotebookPageRemovedCallback :: C_NotebookPageRemovedCallback -> IO (FunPtr C_NotebookPageRemovedCallback)

wrap_NotebookPageRemovedCallback :: 
    GObject a => (a -> NotebookPageRemovedCallback) ->
    C_NotebookPageRemovedCallback
wrap_NotebookPageRemovedCallback gi'cb gi'selfPtr child pageNum _ = do
    child' <- (newObject Gtk.Widget.Widget) child
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  child' pageNum


-- | Connect a signal handler for the [pageRemoved](#signal:pageRemoved) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' notebook #pageRemoved callback
-- @
-- 
-- 
onNotebookPageRemoved :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookPageRemovedCallback) -> m SignalHandlerId
onNotebookPageRemoved obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookPageRemovedCallback wrapped
    wrapped'' <- mk_NotebookPageRemovedCallback wrapped'
    connectSignalFunPtr obj "page-removed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [pageRemoved](#signal:pageRemoved) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' notebook #pageRemoved callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterNotebookPageRemoved :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookPageRemovedCallback) -> m SignalHandlerId
afterNotebookPageRemoved obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookPageRemovedCallback wrapped
    wrapped'' <- mk_NotebookPageRemovedCallback wrapped'
    connectSignalFunPtr obj "page-removed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data NotebookPageRemovedSignalInfo
instance SignalInfo NotebookPageRemovedSignalInfo where
    type HaskellCallbackType NotebookPageRemovedSignalInfo = NotebookPageRemovedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_NotebookPageRemovedCallback cb
        cb'' <- mk_NotebookPageRemovedCallback cb'
        connectSignalFunPtr obj "page-removed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook::page-removed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:signal:pageRemoved"})

#endif

-- signal Notebook::page-reordered
-- | the [pageReordered](#g:signal:pageReordered) signal is emitted in the notebook
-- right after a page has been reordered.
-- 
-- /Since: 2.10/
type NotebookPageReorderedCallback =
    Gtk.Widget.Widget
    -- ^ /@child@/: the child t'GI.Gtk.Objects.Widget.Widget' affected
    -> Word32
    -- ^ /@pageNum@/: the new page number for /@child@/
    -> IO ()

type C_NotebookPageReorderedCallback =
    Ptr Notebook ->                         -- object
    Ptr Gtk.Widget.Widget ->
    Word32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_NotebookPageReorderedCallback`.
foreign import ccall "wrapper"
    mk_NotebookPageReorderedCallback :: C_NotebookPageReorderedCallback -> IO (FunPtr C_NotebookPageReorderedCallback)

wrap_NotebookPageReorderedCallback :: 
    GObject a => (a -> NotebookPageReorderedCallback) ->
    C_NotebookPageReorderedCallback
wrap_NotebookPageReorderedCallback gi'cb gi'selfPtr child pageNum _ = do
    child' <- (newObject Gtk.Widget.Widget) child
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  child' pageNum


-- | Connect a signal handler for the [pageReordered](#signal:pageReordered) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' notebook #pageReordered callback
-- @
-- 
-- 
onNotebookPageReordered :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookPageReorderedCallback) -> m SignalHandlerId
onNotebookPageReordered obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookPageReorderedCallback wrapped
    wrapped'' <- mk_NotebookPageReorderedCallback wrapped'
    connectSignalFunPtr obj "page-reordered" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [pageReordered](#signal:pageReordered) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' notebook #pageReordered callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterNotebookPageReordered :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookPageReorderedCallback) -> m SignalHandlerId
afterNotebookPageReordered obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookPageReorderedCallback wrapped
    wrapped'' <- mk_NotebookPageReorderedCallback wrapped'
    connectSignalFunPtr obj "page-reordered" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data NotebookPageReorderedSignalInfo
instance SignalInfo NotebookPageReorderedSignalInfo where
    type HaskellCallbackType NotebookPageReorderedSignalInfo = NotebookPageReorderedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_NotebookPageReorderedCallback cb
        cb'' <- mk_NotebookPageReorderedCallback cb'
        connectSignalFunPtr obj "page-reordered" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook::page-reordered"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:signal:pageReordered"})

#endif

-- signal Notebook::reorder-tab
-- | /No description available in the introspection data./
type NotebookReorderTabCallback =
    Gtk.Enums.DirectionType
    -> Bool
    -> IO Bool

type C_NotebookReorderTabCallback =
    Ptr Notebook ->                         -- object
    CUInt ->
    CInt ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_NotebookReorderTabCallback`.
foreign import ccall "wrapper"
    mk_NotebookReorderTabCallback :: C_NotebookReorderTabCallback -> IO (FunPtr C_NotebookReorderTabCallback)

wrap_NotebookReorderTabCallback :: 
    GObject a => (a -> NotebookReorderTabCallback) ->
    C_NotebookReorderTabCallback
wrap_NotebookReorderTabCallback gi'cb gi'selfPtr object p0 _ = do
    let object' = (toEnum . fromIntegral) object
    let p0' = (/= 0) p0
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  object' p0'
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [reorderTab](#signal:reorderTab) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' notebook #reorderTab callback
-- @
-- 
-- 
onNotebookReorderTab :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookReorderTabCallback) -> m SignalHandlerId
onNotebookReorderTab obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookReorderTabCallback wrapped
    wrapped'' <- mk_NotebookReorderTabCallback wrapped'
    connectSignalFunPtr obj "reorder-tab" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [reorderTab](#signal:reorderTab) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' notebook #reorderTab callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterNotebookReorderTab :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookReorderTabCallback) -> m SignalHandlerId
afterNotebookReorderTab obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookReorderTabCallback wrapped
    wrapped'' <- mk_NotebookReorderTabCallback wrapped'
    connectSignalFunPtr obj "reorder-tab" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data NotebookReorderTabSignalInfo
instance SignalInfo NotebookReorderTabSignalInfo where
    type HaskellCallbackType NotebookReorderTabSignalInfo = NotebookReorderTabCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_NotebookReorderTabCallback cb
        cb'' <- mk_NotebookReorderTabCallback cb'
        connectSignalFunPtr obj "reorder-tab" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook::reorder-tab"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:signal:reorderTab"})

#endif

-- signal Notebook::select-page
-- | /No description available in the introspection data./
type NotebookSelectPageCallback =
    Bool
    -> IO Bool

type C_NotebookSelectPageCallback =
    Ptr Notebook ->                         -- object
    CInt ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_NotebookSelectPageCallback`.
foreign import ccall "wrapper"
    mk_NotebookSelectPageCallback :: C_NotebookSelectPageCallback -> IO (FunPtr C_NotebookSelectPageCallback)

wrap_NotebookSelectPageCallback :: 
    GObject a => (a -> NotebookSelectPageCallback) ->
    C_NotebookSelectPageCallback
wrap_NotebookSelectPageCallback gi'cb gi'selfPtr object _ = do
    let object' = (/= 0) object
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  object'
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [selectPage](#signal:selectPage) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' notebook #selectPage callback
-- @
-- 
-- 
onNotebookSelectPage :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookSelectPageCallback) -> m SignalHandlerId
onNotebookSelectPage obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookSelectPageCallback wrapped
    wrapped'' <- mk_NotebookSelectPageCallback wrapped'
    connectSignalFunPtr obj "select-page" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [selectPage](#signal:selectPage) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' notebook #selectPage callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterNotebookSelectPage :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookSelectPageCallback) -> m SignalHandlerId
afterNotebookSelectPage obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookSelectPageCallback wrapped
    wrapped'' <- mk_NotebookSelectPageCallback wrapped'
    connectSignalFunPtr obj "select-page" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data NotebookSelectPageSignalInfo
instance SignalInfo NotebookSelectPageSignalInfo where
    type HaskellCallbackType NotebookSelectPageSignalInfo = NotebookSelectPageCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_NotebookSelectPageCallback cb
        cb'' <- mk_NotebookSelectPageCallback cb'
        connectSignalFunPtr obj "select-page" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook::select-page"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:signal:selectPage"})

#endif

-- signal Notebook::switch-page
-- | Emitted when the user or a function changes the current page.
type NotebookSwitchPageCallback =
    Gtk.Widget.Widget
    -- ^ /@page@/: the new current page
    -> Word32
    -- ^ /@pageNum@/: the index of the page
    -> IO ()

type C_NotebookSwitchPageCallback =
    Ptr Notebook ->                         -- object
    Ptr Gtk.Widget.Widget ->
    Word32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_NotebookSwitchPageCallback`.
foreign import ccall "wrapper"
    mk_NotebookSwitchPageCallback :: C_NotebookSwitchPageCallback -> IO (FunPtr C_NotebookSwitchPageCallback)

wrap_NotebookSwitchPageCallback :: 
    GObject a => (a -> NotebookSwitchPageCallback) ->
    C_NotebookSwitchPageCallback
wrap_NotebookSwitchPageCallback gi'cb gi'selfPtr page pageNum _ = do
    page' <- (newObject Gtk.Widget.Widget) page
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  page' pageNum


-- | Connect a signal handler for the [switchPage](#signal:switchPage) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' notebook #switchPage callback
-- @
-- 
-- 
onNotebookSwitchPage :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookSwitchPageCallback) -> m SignalHandlerId
onNotebookSwitchPage obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookSwitchPageCallback wrapped
    wrapped'' <- mk_NotebookSwitchPageCallback wrapped'
    connectSignalFunPtr obj "switch-page" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [switchPage](#signal:switchPage) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' notebook #switchPage callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterNotebookSwitchPage :: (IsNotebook a, MonadIO m) => a -> ((?self :: a) => NotebookSwitchPageCallback) -> m SignalHandlerId
afterNotebookSwitchPage obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_NotebookSwitchPageCallback wrapped
    wrapped'' <- mk_NotebookSwitchPageCallback wrapped'
    connectSignalFunPtr obj "switch-page" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data NotebookSwitchPageSignalInfo
instance SignalInfo NotebookSwitchPageSignalInfo where
    type HaskellCallbackType NotebookSwitchPageSignalInfo = NotebookSwitchPageCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_NotebookSwitchPageCallback cb
        cb'' <- mk_NotebookSwitchPageCallback cb'
        connectSignalFunPtr obj "switch-page" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook::switch-page"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:signal:switchPage"})

#endif

-- VVV Prop "enable-popup"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@enable-popup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' notebook #enablePopup
-- @
getNotebookEnablePopup :: (MonadIO m, IsNotebook o) => o -> m Bool
getNotebookEnablePopup obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "enable-popup"

-- | Set the value of the “@enable-popup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' notebook [ #enablePopup 'Data.GI.Base.Attributes.:=' value ]
-- @
setNotebookEnablePopup :: (MonadIO m, IsNotebook o) => o -> Bool -> m ()
setNotebookEnablePopup obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "enable-popup" val

-- | Construct a `GValueConstruct` with valid value for the “@enable-popup@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructNotebookEnablePopup :: (IsNotebook o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructNotebookEnablePopup val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "enable-popup" val

#if defined(ENABLE_OVERLOADING)
data NotebookEnablePopupPropertyInfo
instance AttrInfo NotebookEnablePopupPropertyInfo where
    type AttrAllowedOps NotebookEnablePopupPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint NotebookEnablePopupPropertyInfo = IsNotebook
    type AttrSetTypeConstraint NotebookEnablePopupPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint NotebookEnablePopupPropertyInfo = (~) Bool
    type AttrTransferType NotebookEnablePopupPropertyInfo = Bool
    type AttrGetType NotebookEnablePopupPropertyInfo = Bool
    type AttrLabel NotebookEnablePopupPropertyInfo = "enable-popup"
    type AttrOrigin NotebookEnablePopupPropertyInfo = Notebook
    attrGet = getNotebookEnablePopup
    attrSet = setNotebookEnablePopup
    attrTransfer _ v = do
        return v
    attrConstruct = constructNotebookEnablePopup
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.enablePopup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:attr:enablePopup"
        })
#endif

-- VVV Prop "group-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@group-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' notebook #groupName
-- @
getNotebookGroupName :: (MonadIO m, IsNotebook o) => o -> m (Maybe T.Text)
getNotebookGroupName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "group-name"

-- | Set the value of the “@group-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' notebook [ #groupName 'Data.GI.Base.Attributes.:=' value ]
-- @
setNotebookGroupName :: (MonadIO m, IsNotebook o) => o -> T.Text -> m ()
setNotebookGroupName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "group-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@group-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructNotebookGroupName :: (IsNotebook o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructNotebookGroupName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "group-name" (P.Just val)

-- | Set the value of the “@group-name@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #groupName
-- @
clearNotebookGroupName :: (MonadIO m, IsNotebook o) => o -> m ()
clearNotebookGroupName obj = liftIO $ B.Properties.setObjectPropertyString obj "group-name" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data NotebookGroupNamePropertyInfo
instance AttrInfo NotebookGroupNamePropertyInfo where
    type AttrAllowedOps NotebookGroupNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint NotebookGroupNamePropertyInfo = IsNotebook
    type AttrSetTypeConstraint NotebookGroupNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint NotebookGroupNamePropertyInfo = (~) T.Text
    type AttrTransferType NotebookGroupNamePropertyInfo = T.Text
    type AttrGetType NotebookGroupNamePropertyInfo = (Maybe T.Text)
    type AttrLabel NotebookGroupNamePropertyInfo = "group-name"
    type AttrOrigin NotebookGroupNamePropertyInfo = Notebook
    attrGet = getNotebookGroupName
    attrSet = setNotebookGroupName
    attrTransfer _ v = do
        return v
    attrConstruct = constructNotebookGroupName
    attrClear = clearNotebookGroupName
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.groupName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:attr:groupName"
        })
#endif

-- VVV Prop "page"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@page@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' notebook #page
-- @
getNotebookPage :: (MonadIO m, IsNotebook o) => o -> m Int32
getNotebookPage obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "page"

-- | Set the value of the “@page@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' notebook [ #page 'Data.GI.Base.Attributes.:=' value ]
-- @
setNotebookPage :: (MonadIO m, IsNotebook o) => o -> Int32 -> m ()
setNotebookPage obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "page" val

-- | Construct a `GValueConstruct` with valid value for the “@page@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructNotebookPage :: (IsNotebook o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructNotebookPage val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "page" val

#if defined(ENABLE_OVERLOADING)
data NotebookPagePropertyInfo
instance AttrInfo NotebookPagePropertyInfo where
    type AttrAllowedOps NotebookPagePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint NotebookPagePropertyInfo = IsNotebook
    type AttrSetTypeConstraint NotebookPagePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint NotebookPagePropertyInfo = (~) Int32
    type AttrTransferType NotebookPagePropertyInfo = Int32
    type AttrGetType NotebookPagePropertyInfo = Int32
    type AttrLabel NotebookPagePropertyInfo = "page"
    type AttrOrigin NotebookPagePropertyInfo = Notebook
    attrGet = getNotebookPage
    attrSet = setNotebookPage
    attrTransfer _ v = do
        return v
    attrConstruct = constructNotebookPage
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.page"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:attr:page"
        })
#endif

-- VVV Prop "scrollable"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@scrollable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' notebook #scrollable
-- @
getNotebookScrollable :: (MonadIO m, IsNotebook o) => o -> m Bool
getNotebookScrollable obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "scrollable"

-- | Set the value of the “@scrollable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' notebook [ #scrollable 'Data.GI.Base.Attributes.:=' value ]
-- @
setNotebookScrollable :: (MonadIO m, IsNotebook o) => o -> Bool -> m ()
setNotebookScrollable obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "scrollable" val

-- | Construct a `GValueConstruct` with valid value for the “@scrollable@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructNotebookScrollable :: (IsNotebook o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructNotebookScrollable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "scrollable" val

#if defined(ENABLE_OVERLOADING)
data NotebookScrollablePropertyInfo
instance AttrInfo NotebookScrollablePropertyInfo where
    type AttrAllowedOps NotebookScrollablePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint NotebookScrollablePropertyInfo = IsNotebook
    type AttrSetTypeConstraint NotebookScrollablePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint NotebookScrollablePropertyInfo = (~) Bool
    type AttrTransferType NotebookScrollablePropertyInfo = Bool
    type AttrGetType NotebookScrollablePropertyInfo = Bool
    type AttrLabel NotebookScrollablePropertyInfo = "scrollable"
    type AttrOrigin NotebookScrollablePropertyInfo = Notebook
    attrGet = getNotebookScrollable
    attrSet = setNotebookScrollable
    attrTransfer _ v = do
        return v
    attrConstruct = constructNotebookScrollable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.scrollable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:attr:scrollable"
        })
#endif

-- VVV Prop "show-border"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-border@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' notebook #showBorder
-- @
getNotebookShowBorder :: (MonadIO m, IsNotebook o) => o -> m Bool
getNotebookShowBorder obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-border"

-- | Set the value of the “@show-border@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' notebook [ #showBorder 'Data.GI.Base.Attributes.:=' value ]
-- @
setNotebookShowBorder :: (MonadIO m, IsNotebook o) => o -> Bool -> m ()
setNotebookShowBorder obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-border" val

-- | Construct a `GValueConstruct` with valid value for the “@show-border@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructNotebookShowBorder :: (IsNotebook o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructNotebookShowBorder val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-border" val

#if defined(ENABLE_OVERLOADING)
data NotebookShowBorderPropertyInfo
instance AttrInfo NotebookShowBorderPropertyInfo where
    type AttrAllowedOps NotebookShowBorderPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint NotebookShowBorderPropertyInfo = IsNotebook
    type AttrSetTypeConstraint NotebookShowBorderPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint NotebookShowBorderPropertyInfo = (~) Bool
    type AttrTransferType NotebookShowBorderPropertyInfo = Bool
    type AttrGetType NotebookShowBorderPropertyInfo = Bool
    type AttrLabel NotebookShowBorderPropertyInfo = "show-border"
    type AttrOrigin NotebookShowBorderPropertyInfo = Notebook
    attrGet = getNotebookShowBorder
    attrSet = setNotebookShowBorder
    attrTransfer _ v = do
        return v
    attrConstruct = constructNotebookShowBorder
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.showBorder"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:attr:showBorder"
        })
#endif

-- VVV Prop "show-tabs"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-tabs@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' notebook #showTabs
-- @
getNotebookShowTabs :: (MonadIO m, IsNotebook o) => o -> m Bool
getNotebookShowTabs obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-tabs"

-- | Set the value of the “@show-tabs@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' notebook [ #showTabs 'Data.GI.Base.Attributes.:=' value ]
-- @
setNotebookShowTabs :: (MonadIO m, IsNotebook o) => o -> Bool -> m ()
setNotebookShowTabs obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-tabs" val

-- | Construct a `GValueConstruct` with valid value for the “@show-tabs@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructNotebookShowTabs :: (IsNotebook o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructNotebookShowTabs val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-tabs" val

#if defined(ENABLE_OVERLOADING)
data NotebookShowTabsPropertyInfo
instance AttrInfo NotebookShowTabsPropertyInfo where
    type AttrAllowedOps NotebookShowTabsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint NotebookShowTabsPropertyInfo = IsNotebook
    type AttrSetTypeConstraint NotebookShowTabsPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint NotebookShowTabsPropertyInfo = (~) Bool
    type AttrTransferType NotebookShowTabsPropertyInfo = Bool
    type AttrGetType NotebookShowTabsPropertyInfo = Bool
    type AttrLabel NotebookShowTabsPropertyInfo = "show-tabs"
    type AttrOrigin NotebookShowTabsPropertyInfo = Notebook
    attrGet = getNotebookShowTabs
    attrSet = setNotebookShowTabs
    attrTransfer _ v = do
        return v
    attrConstruct = constructNotebookShowTabs
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.showTabs"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:attr:showTabs"
        })
#endif

-- VVV Prop "tab-pos"
   -- Type: TInterface (Name {namespace = "Gtk", name = "PositionType"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@tab-pos@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' notebook #tabPos
-- @
getNotebookTabPos :: (MonadIO m, IsNotebook o) => o -> m Gtk.Enums.PositionType
getNotebookTabPos obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "tab-pos"

-- | Set the value of the “@tab-pos@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' notebook [ #tabPos 'Data.GI.Base.Attributes.:=' value ]
-- @
setNotebookTabPos :: (MonadIO m, IsNotebook o) => o -> Gtk.Enums.PositionType -> m ()
setNotebookTabPos obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "tab-pos" val

-- | Construct a `GValueConstruct` with valid value for the “@tab-pos@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructNotebookTabPos :: (IsNotebook o, MIO.MonadIO m) => Gtk.Enums.PositionType -> m (GValueConstruct o)
constructNotebookTabPos val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "tab-pos" val

#if defined(ENABLE_OVERLOADING)
data NotebookTabPosPropertyInfo
instance AttrInfo NotebookTabPosPropertyInfo where
    type AttrAllowedOps NotebookTabPosPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint NotebookTabPosPropertyInfo = IsNotebook
    type AttrSetTypeConstraint NotebookTabPosPropertyInfo = (~) Gtk.Enums.PositionType
    type AttrTransferTypeConstraint NotebookTabPosPropertyInfo = (~) Gtk.Enums.PositionType
    type AttrTransferType NotebookTabPosPropertyInfo = Gtk.Enums.PositionType
    type AttrGetType NotebookTabPosPropertyInfo = Gtk.Enums.PositionType
    type AttrLabel NotebookTabPosPropertyInfo = "tab-pos"
    type AttrOrigin NotebookTabPosPropertyInfo = Notebook
    attrGet = getNotebookTabPos
    attrSet = setNotebookTabPos
    attrTransfer _ v = do
        return v
    attrConstruct = constructNotebookTabPos
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.tabPos"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#g:attr:tabPos"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Notebook
type instance O.AttributeList Notebook = NotebookAttributeList
type NotebookAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("enablePopup", NotebookEnablePopupPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("groupName", NotebookGroupNamePropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("page", NotebookPagePropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("scrollable", NotebookScrollablePropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("showBorder", NotebookShowBorderPropertyInfo), '("showTabs", NotebookShowTabsPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tabPos", NotebookTabPosPropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
notebookEnablePopup :: AttrLabelProxy "enablePopup"
notebookEnablePopup = AttrLabelProxy

notebookGroupName :: AttrLabelProxy "groupName"
notebookGroupName = AttrLabelProxy

notebookPage :: AttrLabelProxy "page"
notebookPage = AttrLabelProxy

notebookScrollable :: AttrLabelProxy "scrollable"
notebookScrollable = AttrLabelProxy

notebookShowBorder :: AttrLabelProxy "showBorder"
notebookShowBorder = AttrLabelProxy

notebookShowTabs :: AttrLabelProxy "showTabs"
notebookShowTabs = AttrLabelProxy

notebookTabPos :: AttrLabelProxy "tabPos"
notebookTabPos = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Notebook = NotebookSignalList
type NotebookSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("changeCurrentPage", NotebookChangeCurrentPageSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("createWindow", NotebookCreateWindowSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("focusTab", NotebookFocusTabSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("moveFocusOut", NotebookMoveFocusOutSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("pageAdded", NotebookPageAddedSignalInfo), '("pageRemoved", NotebookPageRemovedSignalInfo), '("pageReordered", NotebookPageReorderedSignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("reorderTab", NotebookReorderTabSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectPage", NotebookSelectPageSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("switchPage", NotebookSwitchPageSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Notebook::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Notebook" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_notebook_new" gtk_notebook_new :: 
    IO (Ptr Notebook)

-- | Creates a new t'GI.Gtk.Objects.Notebook.Notebook' widget with no pages.
notebookNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Notebook
    -- ^ __Returns:__ the newly created t'GI.Gtk.Objects.Notebook.Notebook'
notebookNew  = liftIO $ do
    result <- gtk_notebook_new
    checkUnexpectedReturnNULL "notebookNew" result
    result' <- (newObject Notebook) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Notebook::append_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText =
--                     Just "the #GtkWidget to use as the contents of the page"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tab_label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkWidget to be used as the label\n    for the page, or %NULL to use the default label, \8220page N\8221"
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

foreign import ccall "gtk_notebook_append_page" gtk_notebook_append_page :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.Widget.Widget ->                -- tab_label : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO Int32

-- | Appends a page to /@notebook@/.
notebookAppendPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: the t'GI.Gtk.Objects.Widget.Widget' to use as the contents of the page
    -> Maybe (c)
    -- ^ /@tabLabel@/: the t'GI.Gtk.Objects.Widget.Widget' to be used as the label
    --     for the page, or 'P.Nothing' to use the default label, “page N”
    -> m Int32
    -- ^ __Returns:__ the index (starting from 0) of the appended
    --     page in the notebook, or -1 if function fails
notebookAppendPage notebook child tabLabel = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    maybeTabLabel <- case tabLabel of
        Nothing -> return nullPtr
        Just jTabLabel -> do
            jTabLabel' <- unsafeManagedPtrCastPtr jTabLabel
            return jTabLabel'
    result <- gtk_notebook_append_page notebook' child' maybeTabLabel
    touchManagedPtr notebook
    touchManagedPtr child
    whenJust tabLabel touchManagedPtr
    return result

#if defined(ENABLE_OVERLOADING)
data NotebookAppendPageMethodInfo
instance (signature ~ (b -> Maybe (c) -> m Int32), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c) => O.OverloadedMethod NotebookAppendPageMethodInfo a signature where
    overloadedMethod = notebookAppendPage

instance O.OverloadedMethodInfo NotebookAppendPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookAppendPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookAppendPage"
        })


#endif

-- method Notebook::append_page_menu
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText =
--                     Just "the #GtkWidget to use as the contents of the page"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tab_label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkWidget to be used as the label\n    for the page, or %NULL to use the default label, \8220page N\8221"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "menu_label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the widget to use as a label for the\n    page-switch menu, if that is enabled. If %NULL, and @tab_label\n    is a #GtkLabel or %NULL, then the menu label will be a newly\n    created label with the same text as @tab_label; if @tab_label\n    is not a #GtkLabel, @menu_label must be specified if the\n    page-switch menu is to be used."
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

foreign import ccall "gtk_notebook_append_page_menu" gtk_notebook_append_page_menu :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.Widget.Widget ->                -- tab_label : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.Widget.Widget ->                -- menu_label : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO Int32

-- | Appends a page to /@notebook@/, specifying the widget to use as the
-- label in the popup menu.
notebookAppendPageMenu ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c, Gtk.Widget.IsWidget d) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: the t'GI.Gtk.Objects.Widget.Widget' to use as the contents of the page
    -> Maybe (c)
    -- ^ /@tabLabel@/: the t'GI.Gtk.Objects.Widget.Widget' to be used as the label
    --     for the page, or 'P.Nothing' to use the default label, “page N”
    -> Maybe (d)
    -- ^ /@menuLabel@/: the widget to use as a label for the
    --     page-switch menu, if that is enabled. If 'P.Nothing', and /@tabLabel@/
    --     is a t'GI.Gtk.Objects.Label.Label' or 'P.Nothing', then the menu label will be a newly
    --     created label with the same text as /@tabLabel@/; if /@tabLabel@/
    --     is not a t'GI.Gtk.Objects.Label.Label', /@menuLabel@/ must be specified if the
    --     page-switch menu is to be used.
    -> m Int32
    -- ^ __Returns:__ the index (starting from 0) of the appended
    --     page in the notebook, or -1 if function fails
notebookAppendPageMenu notebook child tabLabel menuLabel = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    maybeTabLabel <- case tabLabel of
        Nothing -> return nullPtr
        Just jTabLabel -> do
            jTabLabel' <- unsafeManagedPtrCastPtr jTabLabel
            return jTabLabel'
    maybeMenuLabel <- case menuLabel of
        Nothing -> return nullPtr
        Just jMenuLabel -> do
            jMenuLabel' <- unsafeManagedPtrCastPtr jMenuLabel
            return jMenuLabel'
    result <- gtk_notebook_append_page_menu notebook' child' maybeTabLabel maybeMenuLabel
    touchManagedPtr notebook
    touchManagedPtr child
    whenJust tabLabel touchManagedPtr
    whenJust menuLabel touchManagedPtr
    return result

#if defined(ENABLE_OVERLOADING)
data NotebookAppendPageMenuMethodInfo
instance (signature ~ (b -> Maybe (c) -> Maybe (d) -> m Int32), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c, Gtk.Widget.IsWidget d) => O.OverloadedMethod NotebookAppendPageMenuMethodInfo a signature where
    overloadedMethod = notebookAppendPageMenu

instance O.OverloadedMethodInfo NotebookAppendPageMenuMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookAppendPageMenu",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookAppendPageMenu"
        })


#endif

-- method Notebook::detach_tab
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a child" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_detach_tab" gtk_notebook_detach_tab :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Removes the child from the notebook.
-- 
-- This function is very similar to 'GI.Gtk.Objects.Container.containerRemove',
-- but additionally informs the notebook that the removal
-- is happening as part of a tab DND operation, which should
-- not be cancelled.
-- 
-- /Since: 3.16/
notebookDetachTab ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: a child
    -> m ()
notebookDetachTab notebook child = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    gtk_notebook_detach_tab notebook' child'
    touchManagedPtr notebook
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookDetachTabMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) => O.OverloadedMethod NotebookDetachTabMethodInfo a signature where
    overloadedMethod = notebookDetachTab

instance O.OverloadedMethodInfo NotebookDetachTabMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookDetachTab",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookDetachTab"
        })


#endif

-- method Notebook::get_action_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pack_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PackType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "pack type of the action widget to receive"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_notebook_get_action_widget" gtk_notebook_get_action_widget :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    CUInt ->                                -- pack_type : TInterface (Name {namespace = "Gtk", name = "PackType"})
    IO (Ptr Gtk.Widget.Widget)

-- | Gets one of the action widgets. See 'GI.Gtk.Objects.Notebook.notebookSetActionWidget'.
-- 
-- /Since: 2.20/
notebookGetActionWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> Gtk.Enums.PackType
    -- ^ /@packType@/: pack type of the action widget to receive
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ The action widget with the given
    -- /@packType@/ or 'P.Nothing' when this action widget has not been set
notebookGetActionWidget notebook packType = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    let packType' = (fromIntegral . fromEnum) packType
    result <- gtk_notebook_get_action_widget notebook' packType'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr notebook
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data NotebookGetActionWidgetMethodInfo
instance (signature ~ (Gtk.Enums.PackType -> m (Maybe Gtk.Widget.Widget)), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookGetActionWidgetMethodInfo a signature where
    overloadedMethod = notebookGetActionWidget

instance O.OverloadedMethodInfo NotebookGetActionWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetActionWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetActionWidget"
        })


#endif

-- method Notebook::get_current_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_get_current_page" gtk_notebook_get_current_page :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    IO Int32

-- | Returns the page number of the current page.
notebookGetCurrentPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> m Int32
    -- ^ __Returns:__ the index (starting from 0) of the current
    --     page in the notebook. If the notebook has no pages,
    --     then -1 will be returned.
notebookGetCurrentPage notebook = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    result <- gtk_notebook_get_current_page notebook'
    touchManagedPtr notebook
    return result

#if defined(ENABLE_OVERLOADING)
data NotebookGetCurrentPageMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookGetCurrentPageMethodInfo a signature where
    overloadedMethod = notebookGetCurrentPage

instance O.OverloadedMethodInfo NotebookGetCurrentPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetCurrentPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetCurrentPage"
        })


#endif

-- method Notebook::get_group_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_get_group_name" gtk_notebook_get_group_name :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    IO CString

-- | Gets the current group name for /@notebook@/.
-- 
-- /Since: 2.24/
notebookGetGroupName ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the group name, or 'P.Nothing' if none is set
notebookGetGroupName notebook = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    result <- gtk_notebook_get_group_name notebook'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr notebook
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data NotebookGetGroupNameMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookGetGroupNameMethodInfo a signature where
    overloadedMethod = notebookGetGroupName

instance O.OverloadedMethodInfo NotebookGetGroupNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetGroupName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetGroupName"
        })


#endif

-- method Notebook::get_menu_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a widget contained in a page of @notebook"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_notebook_get_menu_label" gtk_notebook_get_menu_label :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO (Ptr Gtk.Widget.Widget)

-- | Retrieves the menu label widget of the page containing /@child@/.
notebookGetMenuLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: a widget contained in a page of /@notebook@/
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the menu label, or 'P.Nothing' if the
    -- notebook page does not have a menu label other than the default (the tab
    -- label).
notebookGetMenuLabel notebook child = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    result <- gtk_notebook_get_menu_label notebook' child'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr notebook
    touchManagedPtr child
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data NotebookGetMenuLabelMethodInfo
instance (signature ~ (b -> m (Maybe Gtk.Widget.Widget)), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) => O.OverloadedMethod NotebookGetMenuLabelMethodInfo a signature where
    overloadedMethod = notebookGetMenuLabel

instance O.OverloadedMethodInfo NotebookGetMenuLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetMenuLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetMenuLabel"
        })


#endif

-- method Notebook::get_menu_label_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the child widget of a page of the notebook."
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

foreign import ccall "gtk_notebook_get_menu_label_text" gtk_notebook_get_menu_label_text :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO CString

-- | Retrieves the text of the menu label for the page containing
-- /@child@/.
notebookGetMenuLabelText ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: the child widget of a page of the notebook.
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the text of the tab label, or 'P.Nothing' if the widget does
    -- not have a menu label other than the default menu label, or the menu label
    -- widget is not a t'GI.Gtk.Objects.Label.Label'. The string is owned by the widget and must not be
    -- freed.
notebookGetMenuLabelText notebook child = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    result <- gtk_notebook_get_menu_label_text notebook' child'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr notebook
    touchManagedPtr child
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data NotebookGetMenuLabelTextMethodInfo
instance (signature ~ (b -> m (Maybe T.Text)), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) => O.OverloadedMethod NotebookGetMenuLabelTextMethodInfo a signature where
    overloadedMethod = notebookGetMenuLabelText

instance O.OverloadedMethodInfo NotebookGetMenuLabelTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetMenuLabelText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetMenuLabelText"
        })


#endif

-- method Notebook::get_n_pages
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_get_n_pages" gtk_notebook_get_n_pages :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    IO Int32

-- | Gets the number of pages in a notebook.
-- 
-- /Since: 2.2/
notebookGetNPages ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> m Int32
    -- ^ __Returns:__ the number of pages in the notebook
notebookGetNPages notebook = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    result <- gtk_notebook_get_n_pages notebook'
    touchManagedPtr notebook
    return result

#if defined(ENABLE_OVERLOADING)
data NotebookGetNPagesMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookGetNPagesMethodInfo a signature where
    overloadedMethod = notebookGetNPages

instance O.OverloadedMethodInfo NotebookGetNPagesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetNPages",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetNPages"
        })


#endif

-- method Notebook::get_nth_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page_num"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the index of a page in the notebook, or -1\n    to get the last page"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_notebook_get_nth_page" gtk_notebook_get_nth_page :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Int32 ->                                -- page_num : TBasicType TInt
    IO (Ptr Gtk.Widget.Widget)

-- | Returns the child widget contained in page number /@pageNum@/.
notebookGetNthPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> Int32
    -- ^ /@pageNum@/: the index of a page in the notebook, or -1
    --     to get the last page
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the child widget, or 'P.Nothing' if /@pageNum@/
    -- is out of bounds
notebookGetNthPage notebook pageNum = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    result <- gtk_notebook_get_nth_page notebook' pageNum
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr notebook
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data NotebookGetNthPageMethodInfo
instance (signature ~ (Int32 -> m (Maybe Gtk.Widget.Widget)), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookGetNthPageMethodInfo a signature where
    overloadedMethod = notebookGetNthPage

instance O.OverloadedMethodInfo NotebookGetNthPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetNthPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetNthPage"
        })


#endif

-- method Notebook::get_scrollable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_get_scrollable" gtk_notebook_get_scrollable :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    IO CInt

-- | Returns whether the tab label area has arrows for scrolling.
-- See 'GI.Gtk.Objects.Notebook.notebookSetScrollable'.
notebookGetScrollable ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if arrows for scrolling are present
notebookGetScrollable notebook = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    result <- gtk_notebook_get_scrollable notebook'
    let result' = (/= 0) result
    touchManagedPtr notebook
    return result'

#if defined(ENABLE_OVERLOADING)
data NotebookGetScrollableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookGetScrollableMethodInfo a signature where
    overloadedMethod = notebookGetScrollable

instance O.OverloadedMethodInfo NotebookGetScrollableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetScrollable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetScrollable"
        })


#endif

-- method Notebook::get_show_border
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_get_show_border" gtk_notebook_get_show_border :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    IO CInt

-- | Returns whether a bevel will be drawn around the notebook pages.
-- See 'GI.Gtk.Objects.Notebook.notebookSetShowBorder'.
notebookGetShowBorder ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the bevel is drawn
notebookGetShowBorder notebook = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    result <- gtk_notebook_get_show_border notebook'
    let result' = (/= 0) result
    touchManagedPtr notebook
    return result'

#if defined(ENABLE_OVERLOADING)
data NotebookGetShowBorderMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookGetShowBorderMethodInfo a signature where
    overloadedMethod = notebookGetShowBorder

instance O.OverloadedMethodInfo NotebookGetShowBorderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetShowBorder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetShowBorder"
        })


#endif

-- method Notebook::get_show_tabs
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_get_show_tabs" gtk_notebook_get_show_tabs :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    IO CInt

-- | Returns whether the tabs of the notebook are shown.
-- See 'GI.Gtk.Objects.Notebook.notebookSetShowTabs'.
notebookGetShowTabs ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the tabs are shown
notebookGetShowTabs notebook = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    result <- gtk_notebook_get_show_tabs notebook'
    let result' = (/= 0) result
    touchManagedPtr notebook
    return result'

#if defined(ENABLE_OVERLOADING)
data NotebookGetShowTabsMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookGetShowTabsMethodInfo a signature where
    overloadedMethod = notebookGetShowTabs

instance O.OverloadedMethodInfo NotebookGetShowTabsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetShowTabs",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetShowTabs"
        })


#endif

-- method Notebook::get_tab_detachable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a child #GtkWidget" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_get_tab_detachable" gtk_notebook_get_tab_detachable :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO CInt

-- | Returns whether the tab contents can be detached from /@notebook@/.
-- 
-- /Since: 2.10/
notebookGetTabDetachable ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: a child t'GI.Gtk.Objects.Widget.Widget'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the tab is detachable.
notebookGetTabDetachable notebook child = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    result <- gtk_notebook_get_tab_detachable notebook' child'
    let result' = (/= 0) result
    touchManagedPtr notebook
    touchManagedPtr child
    return result'

#if defined(ENABLE_OVERLOADING)
data NotebookGetTabDetachableMethodInfo
instance (signature ~ (b -> m Bool), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) => O.OverloadedMethod NotebookGetTabDetachableMethodInfo a signature where
    overloadedMethod = notebookGetTabDetachable

instance O.OverloadedMethodInfo NotebookGetTabDetachableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetTabDetachable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetTabDetachable"
        })


#endif

-- method Notebook::get_tab_hborder
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_get_tab_hborder" gtk_notebook_get_tab_hborder :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    IO Word16

{-# DEPRECATED notebookGetTabHborder ["(Since version 3.4)","this function returns zero"] #-}
-- | Returns the horizontal width of a tab border.
-- 
-- /Since: 2.22/
notebookGetTabHborder ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> m Word16
    -- ^ __Returns:__ horizontal width of a tab border
notebookGetTabHborder notebook = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    result <- gtk_notebook_get_tab_hborder notebook'
    touchManagedPtr notebook
    return result

#if defined(ENABLE_OVERLOADING)
data NotebookGetTabHborderMethodInfo
instance (signature ~ (m Word16), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookGetTabHborderMethodInfo a signature where
    overloadedMethod = notebookGetTabHborder

instance O.OverloadedMethodInfo NotebookGetTabHborderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetTabHborder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetTabHborder"
        })


#endif

-- method Notebook::get_tab_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the page" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_get_tab_label" gtk_notebook_get_tab_label :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO (Ptr Gtk.Widget.Widget)

-- | Returns the tab label widget for the page /@child@/.
-- 'P.Nothing' is returned if /@child@/ is not in /@notebook@/ or
-- if no tab label has specifically been set for /@child@/.
notebookGetTabLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: the page
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the tab label
notebookGetTabLabel notebook child = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    result <- gtk_notebook_get_tab_label notebook' child'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr notebook
    touchManagedPtr child
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data NotebookGetTabLabelMethodInfo
instance (signature ~ (b -> m (Maybe Gtk.Widget.Widget)), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) => O.OverloadedMethod NotebookGetTabLabelMethodInfo a signature where
    overloadedMethod = notebookGetTabLabel

instance O.OverloadedMethodInfo NotebookGetTabLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetTabLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetTabLabel"
        })


#endif

-- method Notebook::get_tab_label_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a widget contained in a page of @notebook"
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

foreign import ccall "gtk_notebook_get_tab_label_text" gtk_notebook_get_tab_label_text :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO CString

-- | Retrieves the text of the tab label for the page containing
-- /@child@/.
notebookGetTabLabelText ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: a widget contained in a page of /@notebook@/
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the text of the tab label, or 'P.Nothing' if the tab label
    -- widget is not a t'GI.Gtk.Objects.Label.Label'. The string is owned by the widget and must not be
    -- freed.
notebookGetTabLabelText notebook child = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    result <- gtk_notebook_get_tab_label_text notebook' child'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        return result''
    touchManagedPtr notebook
    touchManagedPtr child
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data NotebookGetTabLabelTextMethodInfo
instance (signature ~ (b -> m (Maybe T.Text)), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) => O.OverloadedMethod NotebookGetTabLabelTextMethodInfo a signature where
    overloadedMethod = notebookGetTabLabelText

instance O.OverloadedMethodInfo NotebookGetTabLabelTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetTabLabelText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetTabLabelText"
        })


#endif

-- method Notebook::get_tab_pos
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_get_tab_pos" gtk_notebook_get_tab_pos :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    IO CUInt

-- | Gets the edge at which the tabs for switching pages in the
-- notebook are drawn.
notebookGetTabPos ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> m Gtk.Enums.PositionType
    -- ^ __Returns:__ the edge at which the tabs are drawn
notebookGetTabPos notebook = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    result <- gtk_notebook_get_tab_pos notebook'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr notebook
    return result'

#if defined(ENABLE_OVERLOADING)
data NotebookGetTabPosMethodInfo
instance (signature ~ (m Gtk.Enums.PositionType), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookGetTabPosMethodInfo a signature where
    overloadedMethod = notebookGetTabPos

instance O.OverloadedMethodInfo NotebookGetTabPosMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetTabPos",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetTabPos"
        })


#endif

-- method Notebook::get_tab_reorderable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a child #GtkWidget" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_get_tab_reorderable" gtk_notebook_get_tab_reorderable :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO CInt

-- | Gets whether the tab can be reordered via drag and drop or not.
-- 
-- /Since: 2.10/
notebookGetTabReorderable ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: a child t'GI.Gtk.Objects.Widget.Widget'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the tab is reorderable.
notebookGetTabReorderable notebook child = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    result <- gtk_notebook_get_tab_reorderable notebook' child'
    let result' = (/= 0) result
    touchManagedPtr notebook
    touchManagedPtr child
    return result'

#if defined(ENABLE_OVERLOADING)
data NotebookGetTabReorderableMethodInfo
instance (signature ~ (b -> m Bool), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) => O.OverloadedMethod NotebookGetTabReorderableMethodInfo a signature where
    overloadedMethod = notebookGetTabReorderable

instance O.OverloadedMethodInfo NotebookGetTabReorderableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetTabReorderable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetTabReorderable"
        })


#endif

-- method Notebook::get_tab_vborder
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_get_tab_vborder" gtk_notebook_get_tab_vborder :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    IO Word16

{-# DEPRECATED notebookGetTabVborder ["(Since version 3.4)","this function returns zero"] #-}
-- | Returns the vertical width of a tab border.
-- 
-- /Since: 2.22/
notebookGetTabVborder ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> m Word16
    -- ^ __Returns:__ vertical width of a tab border
notebookGetTabVborder notebook = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    result <- gtk_notebook_get_tab_vborder notebook'
    touchManagedPtr notebook
    return result

#if defined(ENABLE_OVERLOADING)
data NotebookGetTabVborderMethodInfo
instance (signature ~ (m Word16), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookGetTabVborderMethodInfo a signature where
    overloadedMethod = notebookGetTabVborder

instance O.OverloadedMethodInfo NotebookGetTabVborderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookGetTabVborder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookGetTabVborder"
        })


#endif

-- method Notebook::insert_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText =
--                     Just "the #GtkWidget to use as the contents of the page"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tab_label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkWidget to be used as the label\n    for the page, or %NULL to use the default label, \8220page N\8221"
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
--                 { rawDocText =
--                     Just
--                       "the index (starting at 0) at which to insert the page,\n    or -1 to append the page after all other pages"
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

foreign import ccall "gtk_notebook_insert_page" gtk_notebook_insert_page :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.Widget.Widget ->                -- tab_label : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- position : TBasicType TInt
    IO Int32

-- | Insert a page into /@notebook@/ at the given position.
notebookInsertPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: the t'GI.Gtk.Objects.Widget.Widget' to use as the contents of the page
    -> Maybe (c)
    -- ^ /@tabLabel@/: the t'GI.Gtk.Objects.Widget.Widget' to be used as the label
    --     for the page, or 'P.Nothing' to use the default label, “page N”
    -> Int32
    -- ^ /@position@/: the index (starting at 0) at which to insert the page,
    --     or -1 to append the page after all other pages
    -> m Int32
    -- ^ __Returns:__ the index (starting from 0) of the inserted
    --     page in the notebook, or -1 if function fails
notebookInsertPage notebook child tabLabel position = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    maybeTabLabel <- case tabLabel of
        Nothing -> return nullPtr
        Just jTabLabel -> do
            jTabLabel' <- unsafeManagedPtrCastPtr jTabLabel
            return jTabLabel'
    result <- gtk_notebook_insert_page notebook' child' maybeTabLabel position
    touchManagedPtr notebook
    touchManagedPtr child
    whenJust tabLabel touchManagedPtr
    return result

#if defined(ENABLE_OVERLOADING)
data NotebookInsertPageMethodInfo
instance (signature ~ (b -> Maybe (c) -> Int32 -> m Int32), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c) => O.OverloadedMethod NotebookInsertPageMethodInfo a signature where
    overloadedMethod = notebookInsertPage

instance O.OverloadedMethodInfo NotebookInsertPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookInsertPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookInsertPage"
        })


#endif

-- method Notebook::insert_page_menu
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText =
--                     Just "the #GtkWidget to use as the contents of the page"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tab_label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkWidget to be used as the label\n    for the page, or %NULL to use the default label, \8220page N\8221"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "menu_label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the widget to use as a label for the\n    page-switch menu, if that is enabled. If %NULL, and @tab_label\n    is a #GtkLabel or %NULL, then the menu label will be a newly\n    created label with the same text as @tab_label; if @tab_label\n    is not a #GtkLabel, @menu_label must be specified if the\n    page-switch menu is to be used."
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
--                 { rawDocText =
--                     Just
--                       "the index (starting at 0) at which to insert the page,\n    or -1 to append the page after all other pages."
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

foreign import ccall "gtk_notebook_insert_page_menu" gtk_notebook_insert_page_menu :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.Widget.Widget ->                -- tab_label : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.Widget.Widget ->                -- menu_label : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- position : TBasicType TInt
    IO Int32

-- | Insert a page into /@notebook@/ at the given position, specifying
-- the widget to use as the label in the popup menu.
notebookInsertPageMenu ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c, Gtk.Widget.IsWidget d) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: the t'GI.Gtk.Objects.Widget.Widget' to use as the contents of the page
    -> Maybe (c)
    -- ^ /@tabLabel@/: the t'GI.Gtk.Objects.Widget.Widget' to be used as the label
    --     for the page, or 'P.Nothing' to use the default label, “page N”
    -> Maybe (d)
    -- ^ /@menuLabel@/: the widget to use as a label for the
    --     page-switch menu, if that is enabled. If 'P.Nothing', and /@tabLabel@/
    --     is a t'GI.Gtk.Objects.Label.Label' or 'P.Nothing', then the menu label will be a newly
    --     created label with the same text as /@tabLabel@/; if /@tabLabel@/
    --     is not a t'GI.Gtk.Objects.Label.Label', /@menuLabel@/ must be specified if the
    --     page-switch menu is to be used.
    -> Int32
    -- ^ /@position@/: the index (starting at 0) at which to insert the page,
    --     or -1 to append the page after all other pages.
    -> m Int32
    -- ^ __Returns:__ the index (starting from 0) of the inserted
    --     page in the notebook
notebookInsertPageMenu notebook child tabLabel menuLabel position = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    maybeTabLabel <- case tabLabel of
        Nothing -> return nullPtr
        Just jTabLabel -> do
            jTabLabel' <- unsafeManagedPtrCastPtr jTabLabel
            return jTabLabel'
    maybeMenuLabel <- case menuLabel of
        Nothing -> return nullPtr
        Just jMenuLabel -> do
            jMenuLabel' <- unsafeManagedPtrCastPtr jMenuLabel
            return jMenuLabel'
    result <- gtk_notebook_insert_page_menu notebook' child' maybeTabLabel maybeMenuLabel position
    touchManagedPtr notebook
    touchManagedPtr child
    whenJust tabLabel touchManagedPtr
    whenJust menuLabel touchManagedPtr
    return result

#if defined(ENABLE_OVERLOADING)
data NotebookInsertPageMenuMethodInfo
instance (signature ~ (b -> Maybe (c) -> Maybe (d) -> Int32 -> m Int32), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c, Gtk.Widget.IsWidget d) => O.OverloadedMethod NotebookInsertPageMenuMethodInfo a signature where
    overloadedMethod = notebookInsertPageMenu

instance O.OverloadedMethodInfo NotebookInsertPageMenuMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookInsertPageMenu",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookInsertPageMenu"
        })


#endif

-- method Notebook::next_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_next_page" gtk_notebook_next_page :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    IO ()

-- | Switches to the next page. Nothing happens if the current page is
-- the last page.
notebookNextPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> m ()
notebookNextPage notebook = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    gtk_notebook_next_page notebook'
    touchManagedPtr notebook
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookNextPageMethodInfo
instance (signature ~ (m ()), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookNextPageMethodInfo a signature where
    overloadedMethod = notebookNextPage

instance O.OverloadedMethodInfo NotebookNextPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookNextPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookNextPage"
        })


#endif

-- method Notebook::page_num
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_notebook_page_num" gtk_notebook_page_num :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO Int32

-- | Finds the index of the page which contains the given child
-- widget.
notebookPageNum ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> m Int32
    -- ^ __Returns:__ the index of the page containing /@child@/, or
    --     -1 if /@child@/ is not in the notebook
notebookPageNum notebook child = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    result <- gtk_notebook_page_num notebook' child'
    touchManagedPtr notebook
    touchManagedPtr child
    return result

#if defined(ENABLE_OVERLOADING)
data NotebookPageNumMethodInfo
instance (signature ~ (b -> m Int32), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) => O.OverloadedMethod NotebookPageNumMethodInfo a signature where
    overloadedMethod = notebookPageNum

instance O.OverloadedMethodInfo NotebookPageNumMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookPageNum",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookPageNum"
        })


#endif

-- method Notebook::popup_disable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_popup_disable" gtk_notebook_popup_disable :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    IO ()

-- | Disables the popup menu.
notebookPopupDisable ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> m ()
notebookPopupDisable notebook = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    gtk_notebook_popup_disable notebook'
    touchManagedPtr notebook
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookPopupDisableMethodInfo
instance (signature ~ (m ()), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookPopupDisableMethodInfo a signature where
    overloadedMethod = notebookPopupDisable

instance O.OverloadedMethodInfo NotebookPopupDisableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookPopupDisable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookPopupDisable"
        })


#endif

-- method Notebook::popup_enable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_popup_enable" gtk_notebook_popup_enable :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    IO ()

-- | Enables the popup menu: if the user clicks with the right
-- mouse button on the tab labels, a menu with all the pages
-- will be popped up.
notebookPopupEnable ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> m ()
notebookPopupEnable notebook = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    gtk_notebook_popup_enable notebook'
    touchManagedPtr notebook
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookPopupEnableMethodInfo
instance (signature ~ (m ()), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookPopupEnableMethodInfo a signature where
    overloadedMethod = notebookPopupEnable

instance O.OverloadedMethodInfo NotebookPopupEnableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookPopupEnable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookPopupEnable"
        })


#endif

-- method Notebook::prepend_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText =
--                     Just "the #GtkWidget to use as the contents of the page"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tab_label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkWidget to be used as the label\n    for the page, or %NULL to use the default label, \8220page N\8221"
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

foreign import ccall "gtk_notebook_prepend_page" gtk_notebook_prepend_page :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.Widget.Widget ->                -- tab_label : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO Int32

-- | Prepends a page to /@notebook@/.
notebookPrependPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: the t'GI.Gtk.Objects.Widget.Widget' to use as the contents of the page
    -> Maybe (c)
    -- ^ /@tabLabel@/: the t'GI.Gtk.Objects.Widget.Widget' to be used as the label
    --     for the page, or 'P.Nothing' to use the default label, “page N”
    -> m Int32
    -- ^ __Returns:__ the index (starting from 0) of the prepended
    --     page in the notebook, or -1 if function fails
notebookPrependPage notebook child tabLabel = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    maybeTabLabel <- case tabLabel of
        Nothing -> return nullPtr
        Just jTabLabel -> do
            jTabLabel' <- unsafeManagedPtrCastPtr jTabLabel
            return jTabLabel'
    result <- gtk_notebook_prepend_page notebook' child' maybeTabLabel
    touchManagedPtr notebook
    touchManagedPtr child
    whenJust tabLabel touchManagedPtr
    return result

#if defined(ENABLE_OVERLOADING)
data NotebookPrependPageMethodInfo
instance (signature ~ (b -> Maybe (c) -> m Int32), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c) => O.OverloadedMethod NotebookPrependPageMethodInfo a signature where
    overloadedMethod = notebookPrependPage

instance O.OverloadedMethodInfo NotebookPrependPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookPrependPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookPrependPage"
        })


#endif

-- method Notebook::prepend_page_menu
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText =
--                     Just "the #GtkWidget to use as the contents of the page"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tab_label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GtkWidget to be used as the label\n    for the page, or %NULL to use the default label, \8220page N\8221"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "menu_label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the widget to use as a label for the\n    page-switch menu, if that is enabled. If %NULL, and @tab_label\n    is a #GtkLabel or %NULL, then the menu label will be a newly\n    created label with the same text as @tab_label; if @tab_label\n    is not a #GtkLabel, @menu_label must be specified if the\n    page-switch menu is to be used."
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

foreign import ccall "gtk_notebook_prepend_page_menu" gtk_notebook_prepend_page_menu :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.Widget.Widget ->                -- tab_label : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.Widget.Widget ->                -- menu_label : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO Int32

-- | Prepends a page to /@notebook@/, specifying the widget to use as the
-- label in the popup menu.
notebookPrependPageMenu ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c, Gtk.Widget.IsWidget d) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: the t'GI.Gtk.Objects.Widget.Widget' to use as the contents of the page
    -> Maybe (c)
    -- ^ /@tabLabel@/: the t'GI.Gtk.Objects.Widget.Widget' to be used as the label
    --     for the page, or 'P.Nothing' to use the default label, “page N”
    -> Maybe (d)
    -- ^ /@menuLabel@/: the widget to use as a label for the
    --     page-switch menu, if that is enabled. If 'P.Nothing', and /@tabLabel@/
    --     is a t'GI.Gtk.Objects.Label.Label' or 'P.Nothing', then the menu label will be a newly
    --     created label with the same text as /@tabLabel@/; if /@tabLabel@/
    --     is not a t'GI.Gtk.Objects.Label.Label', /@menuLabel@/ must be specified if the
    --     page-switch menu is to be used.
    -> m Int32
    -- ^ __Returns:__ the index (starting from 0) of the prepended
    --     page in the notebook, or -1 if function fails
notebookPrependPageMenu notebook child tabLabel menuLabel = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    maybeTabLabel <- case tabLabel of
        Nothing -> return nullPtr
        Just jTabLabel -> do
            jTabLabel' <- unsafeManagedPtrCastPtr jTabLabel
            return jTabLabel'
    maybeMenuLabel <- case menuLabel of
        Nothing -> return nullPtr
        Just jMenuLabel -> do
            jMenuLabel' <- unsafeManagedPtrCastPtr jMenuLabel
            return jMenuLabel'
    result <- gtk_notebook_prepend_page_menu notebook' child' maybeTabLabel maybeMenuLabel
    touchManagedPtr notebook
    touchManagedPtr child
    whenJust tabLabel touchManagedPtr
    whenJust menuLabel touchManagedPtr
    return result

#if defined(ENABLE_OVERLOADING)
data NotebookPrependPageMenuMethodInfo
instance (signature ~ (b -> Maybe (c) -> Maybe (d) -> m Int32), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c, Gtk.Widget.IsWidget d) => O.OverloadedMethod NotebookPrependPageMenuMethodInfo a signature where
    overloadedMethod = notebookPrependPageMenu

instance O.OverloadedMethodInfo NotebookPrependPageMenuMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookPrependPageMenu",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookPrependPageMenu"
        })


#endif

-- method Notebook::prev_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_prev_page" gtk_notebook_prev_page :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    IO ()

-- | Switches to the previous page. Nothing happens if the current page
-- is the first page.
notebookPrevPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> m ()
notebookPrevPage notebook = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    gtk_notebook_prev_page notebook'
    touchManagedPtr notebook
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookPrevPageMethodInfo
instance (signature ~ (m ()), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookPrevPageMethodInfo a signature where
    overloadedMethod = notebookPrevPage

instance O.OverloadedMethodInfo NotebookPrevPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookPrevPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookPrevPage"
        })


#endif

-- method Notebook::remove_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page_num"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the index of a notebook page, starting\n    from 0. If -1, the last page will be removed."
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

foreign import ccall "gtk_notebook_remove_page" gtk_notebook_remove_page :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Int32 ->                                -- page_num : TBasicType TInt
    IO ()

-- | Removes a page from the notebook given its index
-- in the notebook.
notebookRemovePage ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> Int32
    -- ^ /@pageNum@/: the index of a notebook page, starting
    --     from 0. If -1, the last page will be removed.
    -> m ()
notebookRemovePage notebook pageNum = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    gtk_notebook_remove_page notebook' pageNum
    touchManagedPtr notebook
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookRemovePageMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookRemovePageMethodInfo a signature where
    overloadedMethod = notebookRemovePage

instance O.OverloadedMethodInfo NotebookRemovePageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookRemovePage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookRemovePage"
        })


#endif

-- method Notebook::reorder_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the child to move" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the new position, or -1 to move to the end"
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

foreign import ccall "gtk_notebook_reorder_child" gtk_notebook_reorder_child :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- position : TBasicType TInt
    IO ()

-- | Reorders the page containing /@child@/, so that it appears in position
-- /@position@/. If /@position@/ is greater than or equal to the number of
-- children in the list or negative, /@child@/ will be moved to the end
-- of the list.
notebookReorderChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: the child to move
    -> Int32
    -- ^ /@position@/: the new position, or -1 to move to the end
    -> m ()
notebookReorderChild notebook child position = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    gtk_notebook_reorder_child notebook' child' position
    touchManagedPtr notebook
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookReorderChildMethodInfo
instance (signature ~ (b -> Int32 -> m ()), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) => O.OverloadedMethod NotebookReorderChildMethodInfo a signature where
    overloadedMethod = notebookReorderChild

instance O.OverloadedMethodInfo NotebookReorderChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookReorderChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookReorderChild"
        })


#endif

-- method Notebook::set_action_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a #GtkWidget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pack_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PackType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "pack type of the action widget"
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

foreign import ccall "gtk_notebook_set_action_widget" gtk_notebook_set_action_widget :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CUInt ->                                -- pack_type : TInterface (Name {namespace = "Gtk", name = "PackType"})
    IO ()

-- | Sets /@widget@/ as one of the action widgets. Depending on the pack type
-- the widget will be placed before or after the tabs. You can use
-- a t'GI.Gtk.Objects.Box.Box' if you need to pack more than one widget on the same side.
-- 
-- Note that action widgets are “internal” children of the notebook and thus
-- not included in the list returned from 'GI.Gtk.Objects.Container.containerForeach'.
-- 
-- /Since: 2.20/
notebookSetActionWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> Gtk.Enums.PackType
    -- ^ /@packType@/: pack type of the action widget
    -> m ()
notebookSetActionWidget notebook widget packType = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    widget' <- unsafeManagedPtrCastPtr widget
    let packType' = (fromIntegral . fromEnum) packType
    gtk_notebook_set_action_widget notebook' widget' packType'
    touchManagedPtr notebook
    touchManagedPtr widget
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookSetActionWidgetMethodInfo
instance (signature ~ (b -> Gtk.Enums.PackType -> m ()), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) => O.OverloadedMethod NotebookSetActionWidgetMethodInfo a signature where
    overloadedMethod = notebookSetActionWidget

instance O.OverloadedMethodInfo NotebookSetActionWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookSetActionWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookSetActionWidget"
        })


#endif

-- method Notebook::set_current_page
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "page_num"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "index of the page to switch to, starting from 0.\n    If negative, the last page will be used. If greater\n    than the number of pages in the notebook, nothing\n    will be done."
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

foreign import ccall "gtk_notebook_set_current_page" gtk_notebook_set_current_page :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Int32 ->                                -- page_num : TBasicType TInt
    IO ()

-- | Switches to the page number /@pageNum@/.
-- 
-- Note that due to historical reasons, GtkNotebook refuses
-- to switch to a page unless the child widget is visible.
-- Therefore, it is recommended to show child widgets before
-- adding them to a notebook.
notebookSetCurrentPage ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> Int32
    -- ^ /@pageNum@/: index of the page to switch to, starting from 0.
    --     If negative, the last page will be used. If greater
    --     than the number of pages in the notebook, nothing
    --     will be done.
    -> m ()
notebookSetCurrentPage notebook pageNum = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    gtk_notebook_set_current_page notebook' pageNum
    touchManagedPtr notebook
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookSetCurrentPageMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookSetCurrentPageMethodInfo a signature where
    overloadedMethod = notebookSetCurrentPage

instance O.OverloadedMethodInfo NotebookSetCurrentPageMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookSetCurrentPage",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookSetCurrentPage"
        })


#endif

-- method Notebook::set_group_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the name of the notebook group,\n    or %NULL to unset it"
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

foreign import ccall "gtk_notebook_set_group_name" gtk_notebook_set_group_name :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    CString ->                              -- group_name : TBasicType TUTF8
    IO ()

-- | Sets a group name for /@notebook@/.
-- 
-- Notebooks with the same name will be able to exchange tabs
-- via drag and drop. A notebook with a 'P.Nothing' group name will
-- not be able to exchange tabs with any other notebook.
-- 
-- /Since: 2.24/
notebookSetGroupName ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> Maybe (T.Text)
    -- ^ /@groupName@/: the name of the notebook group,
    --     or 'P.Nothing' to unset it
    -> m ()
notebookSetGroupName notebook groupName = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    maybeGroupName <- case groupName of
        Nothing -> return nullPtr
        Just jGroupName -> do
            jGroupName' <- textToCString jGroupName
            return jGroupName'
    gtk_notebook_set_group_name notebook' maybeGroupName
    touchManagedPtr notebook
    freeMem maybeGroupName
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookSetGroupNameMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookSetGroupNameMethodInfo a signature where
    overloadedMethod = notebookSetGroupName

instance O.OverloadedMethodInfo NotebookSetGroupNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookSetGroupName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookSetGroupName"
        })


#endif

-- method Notebook::set_menu_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the child widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "menu_label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the menu label, or %NULL for default"
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

foreign import ccall "gtk_notebook_set_menu_label" gtk_notebook_set_menu_label :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.Widget.Widget ->                -- menu_label : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Changes the menu label for the page containing /@child@/.
notebookSetMenuLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: the child widget
    -> Maybe (c)
    -- ^ /@menuLabel@/: the menu label, or 'P.Nothing' for default
    -> m ()
notebookSetMenuLabel notebook child menuLabel = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    maybeMenuLabel <- case menuLabel of
        Nothing -> return nullPtr
        Just jMenuLabel -> do
            jMenuLabel' <- unsafeManagedPtrCastPtr jMenuLabel
            return jMenuLabel'
    gtk_notebook_set_menu_label notebook' child' maybeMenuLabel
    touchManagedPtr notebook
    touchManagedPtr child
    whenJust menuLabel touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookSetMenuLabelMethodInfo
instance (signature ~ (b -> Maybe (c) -> m ()), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c) => O.OverloadedMethod NotebookSetMenuLabelMethodInfo a signature where
    overloadedMethod = notebookSetMenuLabel

instance O.OverloadedMethodInfo NotebookSetMenuLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookSetMenuLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookSetMenuLabel"
        })


#endif

-- method Notebook::set_menu_label_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the child widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "menu_text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the label text" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_set_menu_label_text" gtk_notebook_set_menu_label_text :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- menu_text : TBasicType TUTF8
    IO ()

-- | Creates a new label and sets it as the menu label of /@child@/.
notebookSetMenuLabelText ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: the child widget
    -> T.Text
    -- ^ /@menuText@/: the label text
    -> m ()
notebookSetMenuLabelText notebook child menuText = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    menuText' <- textToCString menuText
    gtk_notebook_set_menu_label_text notebook' child' menuText'
    touchManagedPtr notebook
    touchManagedPtr child
    freeMem menuText'
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookSetMenuLabelTextMethodInfo
instance (signature ~ (b -> T.Text -> m ()), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) => O.OverloadedMethod NotebookSetMenuLabelTextMethodInfo a signature where
    overloadedMethod = notebookSetMenuLabelText

instance O.OverloadedMethodInfo NotebookSetMenuLabelTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookSetMenuLabelText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookSetMenuLabelText"
        })


#endif

-- method Notebook::set_scrollable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "scrollable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if scroll arrows should be added"
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

foreign import ccall "gtk_notebook_set_scrollable" gtk_notebook_set_scrollable :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    CInt ->                                 -- scrollable : TBasicType TBoolean
    IO ()

-- | Sets whether the tab label area will have arrows for
-- scrolling if there are too many tabs to fit in the area.
notebookSetScrollable ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> Bool
    -- ^ /@scrollable@/: 'P.True' if scroll arrows should be added
    -> m ()
notebookSetScrollable notebook scrollable = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    let scrollable' = (fromIntegral . fromEnum) scrollable
    gtk_notebook_set_scrollable notebook' scrollable'
    touchManagedPtr notebook
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookSetScrollableMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookSetScrollableMethodInfo a signature where
    overloadedMethod = notebookSetScrollable

instance O.OverloadedMethodInfo NotebookSetScrollableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookSetScrollable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookSetScrollable"
        })


#endif

-- method Notebook::set_show_border
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "show_border"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if a bevel should be drawn around the notebook"
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

foreign import ccall "gtk_notebook_set_show_border" gtk_notebook_set_show_border :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    CInt ->                                 -- show_border : TBasicType TBoolean
    IO ()

-- | Sets whether a bevel will be drawn around the notebook pages.
-- This only has a visual effect when the tabs are not shown.
-- See 'GI.Gtk.Objects.Notebook.notebookSetShowTabs'.
notebookSetShowBorder ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> Bool
    -- ^ /@showBorder@/: 'P.True' if a bevel should be drawn around the notebook
    -> m ()
notebookSetShowBorder notebook showBorder = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    let showBorder' = (fromIntegral . fromEnum) showBorder
    gtk_notebook_set_show_border notebook' showBorder'
    touchManagedPtr notebook
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookSetShowBorderMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookSetShowBorderMethodInfo a signature where
    overloadedMethod = notebookSetShowBorder

instance O.OverloadedMethodInfo NotebookSetShowBorderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookSetShowBorder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookSetShowBorder"
        })


#endif

-- method Notebook::set_show_tabs
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "show_tabs"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if the tabs should be shown"
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

foreign import ccall "gtk_notebook_set_show_tabs" gtk_notebook_set_show_tabs :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    CInt ->                                 -- show_tabs : TBasicType TBoolean
    IO ()

-- | Sets whether to show the tabs for the notebook or not.
notebookSetShowTabs ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> Bool
    -- ^ /@showTabs@/: 'P.True' if the tabs should be shown
    -> m ()
notebookSetShowTabs notebook showTabs = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    let showTabs' = (fromIntegral . fromEnum) showTabs
    gtk_notebook_set_show_tabs notebook' showTabs'
    touchManagedPtr notebook
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookSetShowTabsMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookSetShowTabsMethodInfo a signature where
    overloadedMethod = notebookSetShowTabs

instance O.OverloadedMethodInfo NotebookSetShowTabsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookSetShowTabs",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookSetShowTabs"
        })


#endif

-- method Notebook::set_tab_detachable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a child #GtkWidget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detachable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether the tab is detachable or not"
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

foreign import ccall "gtk_notebook_set_tab_detachable" gtk_notebook_set_tab_detachable :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CInt ->                                 -- detachable : TBasicType TBoolean
    IO ()

-- | Sets whether the tab can be detached from /@notebook@/ to another
-- notebook or widget.
-- 
-- Note that 2 notebooks must share a common group identificator
-- (see 'GI.Gtk.Objects.Notebook.notebookSetGroupName') to allow automatic tabs
-- interchange between them.
-- 
-- If you want a widget to interact with a notebook through DnD
-- (i.e.: accept dragged tabs from it) it must be set as a drop
-- destination and accept the target “GTK_NOTEBOOK_TAB”. The notebook
-- will fill the selection with a GtkWidget** pointing to the child
-- widget that corresponds to the dropped tab.
-- 
-- Note that you should use 'GI.Gtk.Objects.Notebook.notebookDetachTab' instead
-- of 'GI.Gtk.Objects.Container.containerRemove' if you want to remove the tab from
-- the source notebook as part of accepting a drop. Otherwise,
-- the source notebook will think that the dragged tab was
-- removed from underneath the ongoing drag operation, and
-- will initiate a drag cancel animation.
-- 
-- 
-- === /C code/
-- >
-- > static void
-- > on_drag_data_received (GtkWidget        *widget,
-- >                        GdkDragContext   *context,
-- >                        gint              x,
-- >                        gint              y,
-- >                        GtkSelectionData *data,
-- >                        guint             info,
-- >                        guint             time,
-- >                        gpointer          user_data)
-- > {
-- >   GtkWidget *notebook;
-- >   GtkWidget **child;
-- >
-- >   notebook = gtk_drag_get_source_widget (context);
-- >   child = (void*) gtk_selection_data_get_data (data);
-- >
-- >   // process_widget (*child);
-- >
-- >   gtk_notebook_detach_tab (GTK_NOTEBOOK (notebook), *child);
-- > }
-- 
-- 
-- If you want a notebook to accept drags from other widgets,
-- you will have to set your own DnD code to do it.
-- 
-- /Since: 2.10/
notebookSetTabDetachable ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: a child t'GI.Gtk.Objects.Widget.Widget'
    -> Bool
    -- ^ /@detachable@/: whether the tab is detachable or not
    -> m ()
notebookSetTabDetachable notebook child detachable = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    let detachable' = (fromIntegral . fromEnum) detachable
    gtk_notebook_set_tab_detachable notebook' child' detachable'
    touchManagedPtr notebook
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookSetTabDetachableMethodInfo
instance (signature ~ (b -> Bool -> m ()), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) => O.OverloadedMethod NotebookSetTabDetachableMethodInfo a signature where
    overloadedMethod = notebookSetTabDetachable

instance O.OverloadedMethodInfo NotebookSetTabDetachableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookSetTabDetachable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookSetTabDetachable"
        })


#endif

-- method Notebook::set_tab_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the page" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tab_label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the tab label widget to use, or %NULL\n    for default tab label"
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

foreign import ccall "gtk_notebook_set_tab_label" gtk_notebook_set_tab_label :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.Widget.Widget ->                -- tab_label : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Changes the tab label for /@child@/.
-- If 'P.Nothing' is specified for /@tabLabel@/, then the page will
-- have the label “page N”.
notebookSetTabLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: the page
    -> Maybe (c)
    -- ^ /@tabLabel@/: the tab label widget to use, or 'P.Nothing'
    --     for default tab label
    -> m ()
notebookSetTabLabel notebook child tabLabel = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    maybeTabLabel <- case tabLabel of
        Nothing -> return nullPtr
        Just jTabLabel -> do
            jTabLabel' <- unsafeManagedPtrCastPtr jTabLabel
            return jTabLabel'
    gtk_notebook_set_tab_label notebook' child' maybeTabLabel
    touchManagedPtr notebook
    touchManagedPtr child
    whenJust tabLabel touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookSetTabLabelMethodInfo
instance (signature ~ (b -> Maybe (c) -> m ()), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b, Gtk.Widget.IsWidget c) => O.OverloadedMethod NotebookSetTabLabelMethodInfo a signature where
    overloadedMethod = notebookSetTabLabel

instance O.OverloadedMethodInfo NotebookSetTabLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookSetTabLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookSetTabLabel"
        })


#endif

-- method Notebook::set_tab_label_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the page" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "tab_text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the label text" , sinceVersion = Nothing }
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

foreign import ccall "gtk_notebook_set_tab_label_text" gtk_notebook_set_tab_label_text :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- tab_text : TBasicType TUTF8
    IO ()

-- | Creates a new label and sets it as the tab label for the page
-- containing /@child@/.
notebookSetTabLabelText ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: the page
    -> T.Text
    -- ^ /@tabText@/: the label text
    -> m ()
notebookSetTabLabelText notebook child tabText = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    tabText' <- textToCString tabText
    gtk_notebook_set_tab_label_text notebook' child' tabText'
    touchManagedPtr notebook
    touchManagedPtr child
    freeMem tabText'
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookSetTabLabelTextMethodInfo
instance (signature ~ (b -> T.Text -> m ()), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) => O.OverloadedMethod NotebookSetTabLabelTextMethodInfo a signature where
    overloadedMethod = notebookSetTabLabelText

instance O.OverloadedMethodInfo NotebookSetTabLabelTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookSetTabLabelText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookSetTabLabelText"
        })


#endif

-- method Notebook::set_tab_pos
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook." , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the edge to draw the tabs at"
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

foreign import ccall "gtk_notebook_set_tab_pos" gtk_notebook_set_tab_pos :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    CUInt ->                                -- pos : TInterface (Name {namespace = "Gtk", name = "PositionType"})
    IO ()

-- | Sets the edge at which the tabs for switching pages in the
-- notebook are drawn.
notebookSetTabPos ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'.
    -> Gtk.Enums.PositionType
    -- ^ /@pos@/: the edge to draw the tabs at
    -> m ()
notebookSetTabPos notebook pos = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    let pos' = (fromIntegral . fromEnum) pos
    gtk_notebook_set_tab_pos notebook' pos'
    touchManagedPtr notebook
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookSetTabPosMethodInfo
instance (signature ~ (Gtk.Enums.PositionType -> m ()), MonadIO m, IsNotebook a) => O.OverloadedMethod NotebookSetTabPosMethodInfo a signature where
    overloadedMethod = notebookSetTabPos

instance O.OverloadedMethodInfo NotebookSetTabPosMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookSetTabPos",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookSetTabPos"
        })


#endif

-- method Notebook::set_tab_reorderable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "notebook"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Notebook" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkNotebook" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a child #GtkWidget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "reorderable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether the tab is reorderable or not"
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

foreign import ccall "gtk_notebook_set_tab_reorderable" gtk_notebook_set_tab_reorderable :: 
    Ptr Notebook ->                         -- notebook : TInterface (Name {namespace = "Gtk", name = "Notebook"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CInt ->                                 -- reorderable : TBasicType TBoolean
    IO ()

-- | Sets whether the notebook tab can be reordered
-- via drag and drop or not.
-- 
-- /Since: 2.10/
notebookSetTabReorderable ::
    (B.CallStack.HasCallStack, MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@notebook@/: a t'GI.Gtk.Objects.Notebook.Notebook'
    -> b
    -- ^ /@child@/: a child t'GI.Gtk.Objects.Widget.Widget'
    -> Bool
    -- ^ /@reorderable@/: whether the tab is reorderable or not
    -> m ()
notebookSetTabReorderable notebook child reorderable = liftIO $ do
    notebook' <- unsafeManagedPtrCastPtr notebook
    child' <- unsafeManagedPtrCastPtr child
    let reorderable' = (fromIntegral . fromEnum) reorderable
    gtk_notebook_set_tab_reorderable notebook' child' reorderable'
    touchManagedPtr notebook
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data NotebookSetTabReorderableMethodInfo
instance (signature ~ (b -> Bool -> m ()), MonadIO m, IsNotebook a, Gtk.Widget.IsWidget b) => O.OverloadedMethod NotebookSetTabReorderableMethodInfo a signature where
    overloadedMethod = notebookSetTabReorderable

instance O.OverloadedMethodInfo NotebookSetTabReorderableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Notebook.notebookSetTabReorderable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Notebook.html#v:notebookSetTabReorderable"
        })


#endif


