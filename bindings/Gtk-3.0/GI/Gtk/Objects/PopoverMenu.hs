{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- GtkPopoverMenu is a subclass of t'GI.Gtk.Objects.Popover.Popover' that treats its
-- children like menus and allows switching between them. It is
-- meant to be used primarily together with t'GI.Gtk.Objects.ModelButton.ModelButton', but
-- any widget can be used, such as t'GI.Gtk.Objects.SpinButton.SpinButton' or t'GI.Gtk.Objects.Scale.Scale'.
-- In this respect, GtkPopoverMenu is more flexible than popovers
-- that are created from a t'GI.Gio.Objects.MenuModel.MenuModel' with 'GI.Gtk.Objects.Popover.popoverNewFromModel'.
-- 
-- To add a child as a submenu, set the t'GI.Gtk.Objects.PopoverMenu.PopoverMenu':@/submenu/@
-- child property to the name of the submenu. To let the user open
-- this submenu, add a t'GI.Gtk.Objects.ModelButton.ModelButton' whose [ModelButton:menuName]("GI.Gtk.Objects.ModelButton#g:attr:menuName")
-- property is set to the name you\'ve given to the submenu.
-- 
-- By convention, the first child of a submenu should be a t'GI.Gtk.Objects.ModelButton.ModelButton'
-- to switch back to the parent menu. Such a button should use the
-- [ModelButton:inverted]("GI.Gtk.Objects.ModelButton#g:attr:inverted") and [ModelButton:centered]("GI.Gtk.Objects.ModelButton#g:attr:centered") properties
-- to achieve a title-like appearance and place the submenu indicator
-- at the opposite side. To switch back to the main menu, use \"main\"
-- as the menu name.
-- 
-- = Example
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkPopoverMenu">
-- >  <child>
-- >    <object class="GtkBox">
-- >      <property name="visible">True</property>
-- >      <property name="margin">10</property>
-- >      <child>
-- >        <object class="GtkModelButton">
-- >          <property name="visible">True</property>
-- >          <property name="action-name">win.frob</property>
-- >          <property name="text" translatable="yes">Frob</property>
-- >        </object>
-- >      </child>
-- >      <child>
-- >        <object class="GtkModelButton">
-- >          <property name="visible">True</property>
-- >          <property name="menu-name">more</property>
-- >          <property name="text" translatable="yes">More</property>
-- >        </object>
-- >      </child>
-- >    </object>
-- >  </child>
-- >  <child>
-- >    <object class="GtkBox">
-- >      <property name="visible">True</property>
-- >      <property name="margin">10</property>
-- >      <child>
-- >        <object class="GtkModelButton">
-- >          <property name="visible">True</property>
-- >          <property name="action-name">win.foo</property>
-- >          <property name="text" translatable="yes">Foo</property>
-- >        </object>
-- >      </child>
-- >      <child>
-- >        <object class="GtkModelButton">
-- >          <property name="visible">True</property>
-- >          <property name="action-name">win.bar</property>
-- >          <property name="text" translatable="yes">Bar</property>
-- >        </object>
-- >      </child>
-- >    </object>
-- >    <packing>
-- >      <property name="submenu">more</property>
-- >    </packing>
-- >  </child>
-- ></object>
-- 
-- 
-- Just like normal popovers created using gtk_popover_new_from_model,
-- t'GI.Gtk.Objects.PopoverMenu.PopoverMenu' instances have a single css node called \"popover\"
-- and get the .menu style class.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.PopoverMenu
    ( 

-- * Exported types
    PopoverMenu(..)                         ,
    IsPopoverMenu                           ,
    toPopoverMenu                           ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindModel]("GI.Gtk.Objects.Popover#g:method:bindModel"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [openSubmenu]("GI.Gtk.Objects.PopoverMenu#g:method:openSubmenu"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [popdown]("GI.Gtk.Objects.Popover#g:method:popdown"), [popup]("GI.Gtk.Objects.Popover#g:method:popup"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getConstrainTo]("GI.Gtk.Objects.Popover#g:method:getConstrainTo"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDefaultWidget]("GI.Gtk.Objects.Popover#g:method:getDefaultWidget"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModal]("GI.Gtk.Objects.Popover#g:method:getModal"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPointingTo]("GI.Gtk.Objects.Popover#g:method:getPointingTo"), [getPosition]("GI.Gtk.Objects.Popover#g:method:getPosition"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRelativeTo]("GI.Gtk.Objects.Popover#g:method:getRelativeTo"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getTransitionsEnabled]("GI.Gtk.Objects.Popover#g:method:getTransitionsEnabled"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setConstrainTo]("GI.Gtk.Objects.Popover#g:method:setConstrainTo"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDefaultWidget]("GI.Gtk.Objects.Popover#g:method:setDefaultWidget"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setModal]("GI.Gtk.Objects.Popover#g:method:setModal"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPointingTo]("GI.Gtk.Objects.Popover#g:method:setPointingTo"), [setPosition]("GI.Gtk.Objects.Popover#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setRelativeTo]("GI.Gtk.Objects.Popover#g:method:setRelativeTo"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setTransitionsEnabled]("GI.Gtk.Objects.Popover#g:method:setTransitionsEnabled"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolvePopoverMenuMethod                ,
#endif

-- ** new #method:new#

    popoverMenuNew                          ,


-- ** openSubmenu #method:openSubmenu#

#if defined(ENABLE_OVERLOADING)
    PopoverMenuOpenSubmenuMethodInfo        ,
#endif
    popoverMenuOpenSubmenu                  ,




 -- * Properties


-- ** visibleSubmenu #attr:visibleSubmenu#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    PopoverMenuVisibleSubmenuPropertyInfo   ,
#endif
    clearPopoverMenuVisibleSubmenu          ,
    constructPopoverMenuVisibleSubmenu      ,
    getPopoverMenuVisibleSubmenu            ,
#if defined(ENABLE_OVERLOADING)
    popoverMenuVisibleSubmenu               ,
#endif
    setPopoverMenuVisibleSubmenu            ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.Bin as Gtk.Bin
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.Popover as Gtk.Popover
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype PopoverMenu = PopoverMenu (SP.ManagedPtr PopoverMenu)
    deriving (Eq)

instance SP.ManagedPtrNewtype PopoverMenu where
    toManagedPtr (PopoverMenu p) = p

foreign import ccall "gtk_popover_menu_get_type"
    c_gtk_popover_menu_get_type :: IO B.Types.GType

instance B.Types.TypedObject PopoverMenu where
    glibType = c_gtk_popover_menu_get_type

instance B.Types.GObject PopoverMenu

-- | Type class for types which can be safely cast to `PopoverMenu`, for instance with `toPopoverMenu`.
class (SP.GObject o, O.IsDescendantOf PopoverMenu o) => IsPopoverMenu o
instance (SP.GObject o, O.IsDescendantOf PopoverMenu o) => IsPopoverMenu o

instance O.HasParentTypes PopoverMenu
type instance O.ParentTypes PopoverMenu = '[Gtk.Popover.Popover, Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `PopoverMenu`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toPopoverMenu :: (MIO.MonadIO m, IsPopoverMenu o) => o -> m PopoverMenu
toPopoverMenu = MIO.liftIO . B.ManagedPtr.unsafeCastTo PopoverMenu

-- | Convert 'PopoverMenu' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe PopoverMenu) where
    gvalueGType_ = c_gtk_popover_menu_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr PopoverMenu)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr PopoverMenu)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject PopoverMenu ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolvePopoverMenuMethod (t :: Symbol) (o :: *) :: * where
    ResolvePopoverMenuMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolvePopoverMenuMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolvePopoverMenuMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolvePopoverMenuMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolvePopoverMenuMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolvePopoverMenuMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolvePopoverMenuMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolvePopoverMenuMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolvePopoverMenuMethod "bindModel" o = Gtk.Popover.PopoverBindModelMethodInfo
    ResolvePopoverMenuMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolvePopoverMenuMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolvePopoverMenuMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolvePopoverMenuMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolvePopoverMenuMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolvePopoverMenuMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolvePopoverMenuMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolvePopoverMenuMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolvePopoverMenuMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolvePopoverMenuMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolvePopoverMenuMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolvePopoverMenuMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolvePopoverMenuMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolvePopoverMenuMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolvePopoverMenuMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolvePopoverMenuMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolvePopoverMenuMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolvePopoverMenuMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolvePopoverMenuMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolvePopoverMenuMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolvePopoverMenuMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolvePopoverMenuMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolvePopoverMenuMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolvePopoverMenuMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolvePopoverMenuMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolvePopoverMenuMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolvePopoverMenuMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolvePopoverMenuMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolvePopoverMenuMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolvePopoverMenuMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolvePopoverMenuMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolvePopoverMenuMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolvePopoverMenuMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolvePopoverMenuMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolvePopoverMenuMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolvePopoverMenuMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolvePopoverMenuMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolvePopoverMenuMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolvePopoverMenuMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolvePopoverMenuMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolvePopoverMenuMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolvePopoverMenuMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolvePopoverMenuMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolvePopoverMenuMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolvePopoverMenuMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolvePopoverMenuMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolvePopoverMenuMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolvePopoverMenuMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolvePopoverMenuMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolvePopoverMenuMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolvePopoverMenuMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolvePopoverMenuMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolvePopoverMenuMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolvePopoverMenuMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolvePopoverMenuMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolvePopoverMenuMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolvePopoverMenuMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolvePopoverMenuMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolvePopoverMenuMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolvePopoverMenuMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolvePopoverMenuMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolvePopoverMenuMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolvePopoverMenuMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolvePopoverMenuMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolvePopoverMenuMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolvePopoverMenuMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolvePopoverMenuMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolvePopoverMenuMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolvePopoverMenuMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolvePopoverMenuMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolvePopoverMenuMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolvePopoverMenuMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolvePopoverMenuMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolvePopoverMenuMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolvePopoverMenuMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolvePopoverMenuMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolvePopoverMenuMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolvePopoverMenuMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolvePopoverMenuMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolvePopoverMenuMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolvePopoverMenuMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolvePopoverMenuMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolvePopoverMenuMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolvePopoverMenuMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolvePopoverMenuMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolvePopoverMenuMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolvePopoverMenuMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolvePopoverMenuMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolvePopoverMenuMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolvePopoverMenuMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolvePopoverMenuMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolvePopoverMenuMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolvePopoverMenuMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolvePopoverMenuMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolvePopoverMenuMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolvePopoverMenuMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolvePopoverMenuMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolvePopoverMenuMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolvePopoverMenuMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolvePopoverMenuMethod "openSubmenu" o = PopoverMenuOpenSubmenuMethodInfo
    ResolvePopoverMenuMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolvePopoverMenuMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolvePopoverMenuMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolvePopoverMenuMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolvePopoverMenuMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolvePopoverMenuMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolvePopoverMenuMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolvePopoverMenuMethod "popdown" o = Gtk.Popover.PopoverPopdownMethodInfo
    ResolvePopoverMenuMethod "popup" o = Gtk.Popover.PopoverPopupMethodInfo
    ResolvePopoverMenuMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolvePopoverMenuMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolvePopoverMenuMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolvePopoverMenuMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolvePopoverMenuMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolvePopoverMenuMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolvePopoverMenuMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolvePopoverMenuMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolvePopoverMenuMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolvePopoverMenuMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolvePopoverMenuMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolvePopoverMenuMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolvePopoverMenuMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolvePopoverMenuMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolvePopoverMenuMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolvePopoverMenuMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolvePopoverMenuMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolvePopoverMenuMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolvePopoverMenuMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolvePopoverMenuMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolvePopoverMenuMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolvePopoverMenuMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolvePopoverMenuMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolvePopoverMenuMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolvePopoverMenuMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolvePopoverMenuMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolvePopoverMenuMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolvePopoverMenuMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolvePopoverMenuMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolvePopoverMenuMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolvePopoverMenuMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolvePopoverMenuMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolvePopoverMenuMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolvePopoverMenuMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolvePopoverMenuMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolvePopoverMenuMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolvePopoverMenuMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolvePopoverMenuMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolvePopoverMenuMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolvePopoverMenuMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolvePopoverMenuMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolvePopoverMenuMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolvePopoverMenuMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolvePopoverMenuMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolvePopoverMenuMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolvePopoverMenuMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolvePopoverMenuMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolvePopoverMenuMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolvePopoverMenuMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolvePopoverMenuMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolvePopoverMenuMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolvePopoverMenuMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolvePopoverMenuMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolvePopoverMenuMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolvePopoverMenuMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolvePopoverMenuMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolvePopoverMenuMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolvePopoverMenuMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolvePopoverMenuMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolvePopoverMenuMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolvePopoverMenuMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolvePopoverMenuMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolvePopoverMenuMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolvePopoverMenuMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolvePopoverMenuMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolvePopoverMenuMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolvePopoverMenuMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolvePopoverMenuMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolvePopoverMenuMethod "getConstrainTo" o = Gtk.Popover.PopoverGetConstrainToMethodInfo
    ResolvePopoverMenuMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolvePopoverMenuMethod "getDefaultWidget" o = Gtk.Popover.PopoverGetDefaultWidgetMethodInfo
    ResolvePopoverMenuMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolvePopoverMenuMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolvePopoverMenuMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolvePopoverMenuMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolvePopoverMenuMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolvePopoverMenuMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolvePopoverMenuMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolvePopoverMenuMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolvePopoverMenuMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolvePopoverMenuMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolvePopoverMenuMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolvePopoverMenuMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolvePopoverMenuMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolvePopoverMenuMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolvePopoverMenuMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolvePopoverMenuMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolvePopoverMenuMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolvePopoverMenuMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolvePopoverMenuMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolvePopoverMenuMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolvePopoverMenuMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolvePopoverMenuMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolvePopoverMenuMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolvePopoverMenuMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolvePopoverMenuMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolvePopoverMenuMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolvePopoverMenuMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolvePopoverMenuMethod "getModal" o = Gtk.Popover.PopoverGetModalMethodInfo
    ResolvePopoverMenuMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolvePopoverMenuMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolvePopoverMenuMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolvePopoverMenuMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolvePopoverMenuMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolvePopoverMenuMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolvePopoverMenuMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolvePopoverMenuMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolvePopoverMenuMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolvePopoverMenuMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolvePopoverMenuMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolvePopoverMenuMethod "getPointingTo" o = Gtk.Popover.PopoverGetPointingToMethodInfo
    ResolvePopoverMenuMethod "getPosition" o = Gtk.Popover.PopoverGetPositionMethodInfo
    ResolvePopoverMenuMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolvePopoverMenuMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolvePopoverMenuMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolvePopoverMenuMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolvePopoverMenuMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolvePopoverMenuMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolvePopoverMenuMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolvePopoverMenuMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolvePopoverMenuMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolvePopoverMenuMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolvePopoverMenuMethod "getRelativeTo" o = Gtk.Popover.PopoverGetRelativeToMethodInfo
    ResolvePopoverMenuMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolvePopoverMenuMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolvePopoverMenuMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolvePopoverMenuMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolvePopoverMenuMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolvePopoverMenuMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolvePopoverMenuMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolvePopoverMenuMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolvePopoverMenuMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolvePopoverMenuMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolvePopoverMenuMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolvePopoverMenuMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolvePopoverMenuMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolvePopoverMenuMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolvePopoverMenuMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolvePopoverMenuMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolvePopoverMenuMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolvePopoverMenuMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolvePopoverMenuMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolvePopoverMenuMethod "getTransitionsEnabled" o = Gtk.Popover.PopoverGetTransitionsEnabledMethodInfo
    ResolvePopoverMenuMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolvePopoverMenuMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolvePopoverMenuMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolvePopoverMenuMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolvePopoverMenuMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolvePopoverMenuMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolvePopoverMenuMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolvePopoverMenuMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolvePopoverMenuMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolvePopoverMenuMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolvePopoverMenuMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolvePopoverMenuMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolvePopoverMenuMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolvePopoverMenuMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolvePopoverMenuMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolvePopoverMenuMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolvePopoverMenuMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolvePopoverMenuMethod "setConstrainTo" o = Gtk.Popover.PopoverSetConstrainToMethodInfo
    ResolvePopoverMenuMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolvePopoverMenuMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolvePopoverMenuMethod "setDefaultWidget" o = Gtk.Popover.PopoverSetDefaultWidgetMethodInfo
    ResolvePopoverMenuMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolvePopoverMenuMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolvePopoverMenuMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolvePopoverMenuMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolvePopoverMenuMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolvePopoverMenuMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolvePopoverMenuMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolvePopoverMenuMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolvePopoverMenuMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolvePopoverMenuMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolvePopoverMenuMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolvePopoverMenuMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolvePopoverMenuMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolvePopoverMenuMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolvePopoverMenuMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolvePopoverMenuMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolvePopoverMenuMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolvePopoverMenuMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolvePopoverMenuMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolvePopoverMenuMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolvePopoverMenuMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolvePopoverMenuMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolvePopoverMenuMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolvePopoverMenuMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolvePopoverMenuMethod "setModal" o = Gtk.Popover.PopoverSetModalMethodInfo
    ResolvePopoverMenuMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolvePopoverMenuMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolvePopoverMenuMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolvePopoverMenuMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolvePopoverMenuMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolvePopoverMenuMethod "setPointingTo" o = Gtk.Popover.PopoverSetPointingToMethodInfo
    ResolvePopoverMenuMethod "setPosition" o = Gtk.Popover.PopoverSetPositionMethodInfo
    ResolvePopoverMenuMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolvePopoverMenuMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolvePopoverMenuMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolvePopoverMenuMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolvePopoverMenuMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolvePopoverMenuMethod "setRelativeTo" o = Gtk.Popover.PopoverSetRelativeToMethodInfo
    ResolvePopoverMenuMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolvePopoverMenuMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolvePopoverMenuMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolvePopoverMenuMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolvePopoverMenuMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolvePopoverMenuMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolvePopoverMenuMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolvePopoverMenuMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolvePopoverMenuMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolvePopoverMenuMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolvePopoverMenuMethod "setTransitionsEnabled" o = Gtk.Popover.PopoverSetTransitionsEnabledMethodInfo
    ResolvePopoverMenuMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolvePopoverMenuMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolvePopoverMenuMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolvePopoverMenuMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolvePopoverMenuMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolvePopoverMenuMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolvePopoverMenuMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolvePopoverMenuMethod t PopoverMenu, O.OverloadedMethod info PopoverMenu p) => OL.IsLabel t (PopoverMenu -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolvePopoverMenuMethod t PopoverMenu, O.OverloadedMethod info PopoverMenu p, R.HasField t PopoverMenu p) => R.HasField t PopoverMenu p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolvePopoverMenuMethod t PopoverMenu, O.OverloadedMethodInfo info PopoverMenu) => OL.IsLabel t (O.MethodProxy info PopoverMenu) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "visible-submenu"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@visible-submenu@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' popoverMenu #visibleSubmenu
-- @
getPopoverMenuVisibleSubmenu :: (MonadIO m, IsPopoverMenu o) => o -> m (Maybe T.Text)
getPopoverMenuVisibleSubmenu obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "visible-submenu"

-- | Set the value of the “@visible-submenu@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' popoverMenu [ #visibleSubmenu 'Data.GI.Base.Attributes.:=' value ]
-- @
setPopoverMenuVisibleSubmenu :: (MonadIO m, IsPopoverMenu o) => o -> T.Text -> m ()
setPopoverMenuVisibleSubmenu obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "visible-submenu" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@visible-submenu@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructPopoverMenuVisibleSubmenu :: (IsPopoverMenu o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructPopoverMenuVisibleSubmenu val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "visible-submenu" (P.Just val)

-- | Set the value of the “@visible-submenu@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #visibleSubmenu
-- @
clearPopoverMenuVisibleSubmenu :: (MonadIO m, IsPopoverMenu o) => o -> m ()
clearPopoverMenuVisibleSubmenu obj = liftIO $ B.Properties.setObjectPropertyString obj "visible-submenu" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data PopoverMenuVisibleSubmenuPropertyInfo
instance AttrInfo PopoverMenuVisibleSubmenuPropertyInfo where
    type AttrAllowedOps PopoverMenuVisibleSubmenuPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint PopoverMenuVisibleSubmenuPropertyInfo = IsPopoverMenu
    type AttrSetTypeConstraint PopoverMenuVisibleSubmenuPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint PopoverMenuVisibleSubmenuPropertyInfo = (~) T.Text
    type AttrTransferType PopoverMenuVisibleSubmenuPropertyInfo = T.Text
    type AttrGetType PopoverMenuVisibleSubmenuPropertyInfo = (Maybe T.Text)
    type AttrLabel PopoverMenuVisibleSubmenuPropertyInfo = "visible-submenu"
    type AttrOrigin PopoverMenuVisibleSubmenuPropertyInfo = PopoverMenu
    attrGet = getPopoverMenuVisibleSubmenu
    attrSet = setPopoverMenuVisibleSubmenu
    attrTransfer _ v = do
        return v
    attrConstruct = constructPopoverMenuVisibleSubmenu
    attrClear = clearPopoverMenuVisibleSubmenu
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PopoverMenu.visibleSubmenu"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PopoverMenu.html#g:attr:visibleSubmenu"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList PopoverMenu
type instance O.AttributeList PopoverMenu = PopoverMenuAttributeList
type PopoverMenuAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("constrainTo", Gtk.Popover.PopoverConstrainToPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("modal", Gtk.Popover.PopoverModalPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("pointingTo", Gtk.Popover.PopoverPointingToPropertyInfo), '("position", Gtk.Popover.PopoverPositionPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("relativeTo", Gtk.Popover.PopoverRelativeToPropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("transitionsEnabled", Gtk.Popover.PopoverTransitionsEnabledPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("visibleSubmenu", PopoverMenuVisibleSubmenuPropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
popoverMenuVisibleSubmenu :: AttrLabelProxy "visibleSubmenu"
popoverMenuVisibleSubmenu = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList PopoverMenu = PopoverMenuSignalList
type PopoverMenuSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("closed", Gtk.Popover.PopoverClosedSignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method PopoverMenu::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "PopoverMenu" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_popover_menu_new" gtk_popover_menu_new :: 
    IO (Ptr PopoverMenu)

-- | Creates a new popover menu.
-- 
-- /Since: 3.16/
popoverMenuNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m PopoverMenu
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.PopoverMenu.PopoverMenu'
popoverMenuNew  = liftIO $ do
    result <- gtk_popover_menu_new
    checkUnexpectedReturnNULL "popoverMenuNew" result
    result' <- (newObject PopoverMenu) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method PopoverMenu::open_submenu
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "popover"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "PopoverMenu" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkPopoverMenu" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the name of the menu to switch to"
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

foreign import ccall "gtk_popover_menu_open_submenu" gtk_popover_menu_open_submenu :: 
    Ptr PopoverMenu ->                      -- popover : TInterface (Name {namespace = "Gtk", name = "PopoverMenu"})
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Opens a submenu of the /@popover@/. The /@name@/
-- must be one of the names given to the submenus
-- of /@popover@/ with t'GI.Gtk.Objects.PopoverMenu.PopoverMenu':@/submenu/@, or
-- \"main\" to switch back to the main menu.
-- 
-- t'GI.Gtk.Objects.ModelButton.ModelButton' will open submenus automatically
-- when the [ModelButton:menuName]("GI.Gtk.Objects.ModelButton#g:attr:menuName") property is set,
-- so this function is only needed when you are using
-- other kinds of widgets to initiate menu changes.
-- 
-- /Since: 3.16/
popoverMenuOpenSubmenu ::
    (B.CallStack.HasCallStack, MonadIO m, IsPopoverMenu a) =>
    a
    -- ^ /@popover@/: a t'GI.Gtk.Objects.PopoverMenu.PopoverMenu'
    -> T.Text
    -- ^ /@name@/: the name of the menu to switch to
    -> m ()
popoverMenuOpenSubmenu popover name = liftIO $ do
    popover' <- unsafeManagedPtrCastPtr popover
    name' <- textToCString name
    gtk_popover_menu_open_submenu popover' name'
    touchManagedPtr popover
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data PopoverMenuOpenSubmenuMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsPopoverMenu a) => O.OverloadedMethod PopoverMenuOpenSubmenuMethodInfo a signature where
    overloadedMethod = popoverMenuOpenSubmenu

instance O.OverloadedMethodInfo PopoverMenuOpenSubmenuMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.PopoverMenu.popoverMenuOpenSubmenu",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-PopoverMenu.html#v:popoverMenuOpenSubmenu"
        })


#endif


