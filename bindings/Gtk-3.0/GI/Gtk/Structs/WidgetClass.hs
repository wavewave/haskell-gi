{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.WidgetClass
    ( 

-- * Exported types
    WidgetClass(..)                         ,
    newZeroWidgetClass                      ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindTemplateCallbackFull]("GI.Gtk.Structs.WidgetClass#g:method:bindTemplateCallbackFull"), [bindTemplateChildFull]("GI.Gtk.Structs.WidgetClass#g:method:bindTemplateChildFull"), [findStyleProperty]("GI.Gtk.Structs.WidgetClass#g:method:findStyleProperty"), [installStyleProperty]("GI.Gtk.Structs.WidgetClass#g:method:installStyleProperty").
-- 
-- ==== Getters
-- [getCssName]("GI.Gtk.Structs.WidgetClass#g:method:getCssName").
-- 
-- ==== Setters
-- [setAccessibleRole]("GI.Gtk.Structs.WidgetClass#g:method:setAccessibleRole"), [setAccessibleType]("GI.Gtk.Structs.WidgetClass#g:method:setAccessibleType"), [setConnectFunc]("GI.Gtk.Structs.WidgetClass#g:method:setConnectFunc"), [setCssName]("GI.Gtk.Structs.WidgetClass#g:method:setCssName"), [setTemplate]("GI.Gtk.Structs.WidgetClass#g:method:setTemplate"), [setTemplateFromResource]("GI.Gtk.Structs.WidgetClass#g:method:setTemplateFromResource").

#if defined(ENABLE_OVERLOADING)
    ResolveWidgetClassMethod                ,
#endif

-- ** bindTemplateCallbackFull #method:bindTemplateCallbackFull#

#if defined(ENABLE_OVERLOADING)
    WidgetClassBindTemplateCallbackFullMethodInfo,
#endif
    widgetClassBindTemplateCallbackFull     ,


-- ** bindTemplateChildFull #method:bindTemplateChildFull#

#if defined(ENABLE_OVERLOADING)
    WidgetClassBindTemplateChildFullMethodInfo,
#endif
    widgetClassBindTemplateChildFull        ,


-- ** findStyleProperty #method:findStyleProperty#

#if defined(ENABLE_OVERLOADING)
    WidgetClassFindStylePropertyMethodInfo  ,
#endif
    widgetClassFindStyleProperty            ,


-- ** getCssName #method:getCssName#

#if defined(ENABLE_OVERLOADING)
    WidgetClassGetCssNameMethodInfo         ,
#endif
    widgetClassGetCssName                   ,


-- ** installStyleProperty #method:installStyleProperty#

#if defined(ENABLE_OVERLOADING)
    WidgetClassInstallStylePropertyMethodInfo,
#endif
    widgetClassInstallStyleProperty         ,


-- ** setAccessibleRole #method:setAccessibleRole#

#if defined(ENABLE_OVERLOADING)
    WidgetClassSetAccessibleRoleMethodInfo  ,
#endif
    widgetClassSetAccessibleRole            ,


-- ** setAccessibleType #method:setAccessibleType#

#if defined(ENABLE_OVERLOADING)
    WidgetClassSetAccessibleTypeMethodInfo  ,
#endif
    widgetClassSetAccessibleType            ,


-- ** setConnectFunc #method:setConnectFunc#

#if defined(ENABLE_OVERLOADING)
    WidgetClassSetConnectFuncMethodInfo     ,
#endif
    widgetClassSetConnectFunc               ,


-- ** setCssName #method:setCssName#

#if defined(ENABLE_OVERLOADING)
    WidgetClassSetCssNameMethodInfo         ,
#endif
    widgetClassSetCssName                   ,


-- ** setTemplate #method:setTemplate#

#if defined(ENABLE_OVERLOADING)
    WidgetClassSetTemplateMethodInfo        ,
#endif
    widgetClassSetTemplate                  ,


-- ** setTemplateFromResource #method:setTemplateFromResource#

#if defined(ENABLE_OVERLOADING)
    WidgetClassSetTemplateFromResourceMethodInfo,
#endif
    widgetClassSetTemplateFromResource      ,




 -- * Properties


-- ** activateSignal #attr:activateSignal#
-- | The signal to emit when a widget of this class is
--   activated, 'GI.Gtk.Objects.Widget.widgetActivate' handles the emission.
--   Implementation of this signal is optional.

    getWidgetClassActivateSignal            ,
    setWidgetClassActivateSignal            ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_activateSignal              ,
#endif


-- ** adjustBaselineAllocation #attr:adjustBaselineAllocation#
-- | /No description available in the introspection data./

    clearWidgetClassAdjustBaselineAllocation,
    getWidgetClassAdjustBaselineAllocation  ,
    setWidgetClassAdjustBaselineAllocation  ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_adjustBaselineAllocation    ,
#endif


-- ** adjustBaselineRequest #attr:adjustBaselineRequest#
-- | /No description available in the introspection data./

    clearWidgetClassAdjustBaselineRequest   ,
    getWidgetClassAdjustBaselineRequest     ,
    setWidgetClassAdjustBaselineRequest     ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_adjustBaselineRequest       ,
#endif


-- ** adjustSizeAllocation #attr:adjustSizeAllocation#
-- | /No description available in the introspection data./

    clearWidgetClassAdjustSizeAllocation    ,
    getWidgetClassAdjustSizeAllocation      ,
    setWidgetClassAdjustSizeAllocation      ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_adjustSizeAllocation        ,
#endif


-- ** adjustSizeRequest #attr:adjustSizeRequest#
-- | /No description available in the introspection data./

    clearWidgetClassAdjustSizeRequest       ,
    getWidgetClassAdjustSizeRequest         ,
    setWidgetClassAdjustSizeRequest         ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_adjustSizeRequest           ,
#endif


-- ** buttonPressEvent #attr:buttonPressEvent#
-- | /No description available in the introspection data./

    clearWidgetClassButtonPressEvent        ,
    getWidgetClassButtonPressEvent          ,
    setWidgetClassButtonPressEvent          ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_buttonPressEvent            ,
#endif


-- ** buttonReleaseEvent #attr:buttonReleaseEvent#
-- | /No description available in the introspection data./

    clearWidgetClassButtonReleaseEvent      ,
    getWidgetClassButtonReleaseEvent        ,
    setWidgetClassButtonReleaseEvent        ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_buttonReleaseEvent          ,
#endif


-- ** canActivateAccel #attr:canActivateAccel#
-- | /No description available in the introspection data./

    clearWidgetClassCanActivateAccel        ,
    getWidgetClassCanActivateAccel          ,
    setWidgetClassCanActivateAccel          ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_canActivateAccel            ,
#endif


-- ** childNotify #attr:childNotify#
-- | /No description available in the introspection data./

    clearWidgetClassChildNotify             ,
    getWidgetClassChildNotify               ,
    setWidgetClassChildNotify               ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_childNotify                 ,
#endif


-- ** compositedChanged #attr:compositedChanged#
-- | /No description available in the introspection data./

    clearWidgetClassCompositedChanged       ,
    getWidgetClassCompositedChanged         ,
    setWidgetClassCompositedChanged         ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_compositedChanged           ,
#endif


-- ** computeExpand #attr:computeExpand#
-- | /No description available in the introspection data./

    clearWidgetClassComputeExpand           ,
    getWidgetClassComputeExpand             ,
    setWidgetClassComputeExpand             ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_computeExpand               ,
#endif


-- ** configureEvent #attr:configureEvent#
-- | /No description available in the introspection data./

    clearWidgetClassConfigureEvent          ,
    getWidgetClassConfigureEvent            ,
    setWidgetClassConfigureEvent            ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_configureEvent              ,
#endif


-- ** damageEvent #attr:damageEvent#
-- | /No description available in the introspection data./

    clearWidgetClassDamageEvent             ,
    getWidgetClassDamageEvent               ,
    setWidgetClassDamageEvent               ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_damageEvent                 ,
#endif


-- ** deleteEvent #attr:deleteEvent#
-- | /No description available in the introspection data./

    clearWidgetClassDeleteEvent             ,
    getWidgetClassDeleteEvent               ,
    setWidgetClassDeleteEvent               ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_deleteEvent                 ,
#endif


-- ** destroy #attr:destroy#
-- | /No description available in the introspection data./

    clearWidgetClassDestroy                 ,
    getWidgetClassDestroy                   ,
    setWidgetClassDestroy                   ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_destroy                     ,
#endif


-- ** destroyEvent #attr:destroyEvent#
-- | /No description available in the introspection data./

    clearWidgetClassDestroyEvent            ,
    getWidgetClassDestroyEvent              ,
    setWidgetClassDestroyEvent              ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_destroyEvent                ,
#endif


-- ** directionChanged #attr:directionChanged#
-- | /No description available in the introspection data./

    clearWidgetClassDirectionChanged        ,
    getWidgetClassDirectionChanged          ,
    setWidgetClassDirectionChanged          ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_directionChanged            ,
#endif


-- ** dispatchChildPropertiesChanged #attr:dispatchChildPropertiesChanged#
-- | /No description available in the introspection data./

    clearWidgetClassDispatchChildPropertiesChanged,
    getWidgetClassDispatchChildPropertiesChanged,
    setWidgetClassDispatchChildPropertiesChanged,
#if defined(ENABLE_OVERLOADING)
    widgetClass_dispatchChildPropertiesChanged,
#endif


-- ** dragBegin #attr:dragBegin#
-- | /No description available in the introspection data./

    clearWidgetClassDragBegin               ,
    getWidgetClassDragBegin                 ,
    setWidgetClassDragBegin                 ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_dragBegin                   ,
#endif


-- ** dragDataDelete #attr:dragDataDelete#
-- | /No description available in the introspection data./

    clearWidgetClassDragDataDelete          ,
    getWidgetClassDragDataDelete            ,
    setWidgetClassDragDataDelete            ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_dragDataDelete              ,
#endif


-- ** dragDataGet #attr:dragDataGet#
-- | /No description available in the introspection data./

    clearWidgetClassDragDataGet             ,
    getWidgetClassDragDataGet               ,
    setWidgetClassDragDataGet               ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_dragDataGet                 ,
#endif


-- ** dragDataReceived #attr:dragDataReceived#
-- | /No description available in the introspection data./

    clearWidgetClassDragDataReceived        ,
    getWidgetClassDragDataReceived          ,
    setWidgetClassDragDataReceived          ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_dragDataReceived            ,
#endif


-- ** dragDrop #attr:dragDrop#
-- | /No description available in the introspection data./

    clearWidgetClassDragDrop                ,
    getWidgetClassDragDrop                  ,
    setWidgetClassDragDrop                  ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_dragDrop                    ,
#endif


-- ** dragEnd #attr:dragEnd#
-- | /No description available in the introspection data./

    clearWidgetClassDragEnd                 ,
    getWidgetClassDragEnd                   ,
    setWidgetClassDragEnd                   ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_dragEnd                     ,
#endif


-- ** dragFailed #attr:dragFailed#
-- | /No description available in the introspection data./

    clearWidgetClassDragFailed              ,
    getWidgetClassDragFailed                ,
    setWidgetClassDragFailed                ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_dragFailed                  ,
#endif


-- ** dragLeave #attr:dragLeave#
-- | /No description available in the introspection data./

    clearWidgetClassDragLeave               ,
    getWidgetClassDragLeave                 ,
    setWidgetClassDragLeave                 ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_dragLeave                   ,
#endif


-- ** dragMotion #attr:dragMotion#
-- | /No description available in the introspection data./

    clearWidgetClassDragMotion              ,
    getWidgetClassDragMotion                ,
    setWidgetClassDragMotion                ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_dragMotion                  ,
#endif


-- ** draw #attr:draw#
-- | /No description available in the introspection data./

    clearWidgetClassDraw                    ,
    getWidgetClassDraw                      ,
    setWidgetClassDraw                      ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_draw                        ,
#endif


-- ** enterNotifyEvent #attr:enterNotifyEvent#
-- | /No description available in the introspection data./

    clearWidgetClassEnterNotifyEvent        ,
    getWidgetClassEnterNotifyEvent          ,
    setWidgetClassEnterNotifyEvent          ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_enterNotifyEvent            ,
#endif


-- ** event #attr:event#
-- | /No description available in the introspection data./

    clearWidgetClassEvent                   ,
    getWidgetClassEvent                     ,
    setWidgetClassEvent                     ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_event                       ,
#endif


-- ** focus #attr:focus#
-- | /No description available in the introspection data./

    clearWidgetClassFocus                   ,
    getWidgetClassFocus                     ,
    setWidgetClassFocus                     ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_focus                       ,
#endif


-- ** focusInEvent #attr:focusInEvent#
-- | /No description available in the introspection data./

    clearWidgetClassFocusInEvent            ,
    getWidgetClassFocusInEvent              ,
    setWidgetClassFocusInEvent              ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_focusInEvent                ,
#endif


-- ** focusOutEvent #attr:focusOutEvent#
-- | /No description available in the introspection data./

    clearWidgetClassFocusOutEvent           ,
    getWidgetClassFocusOutEvent             ,
    setWidgetClassFocusOutEvent             ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_focusOutEvent               ,
#endif


-- ** getAccessible #attr:getAccessible#
-- | /No description available in the introspection data./

    clearWidgetClassGetAccessible           ,
    getWidgetClassGetAccessible             ,
    setWidgetClassGetAccessible             ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_getAccessible               ,
#endif


-- ** getPreferredHeight #attr:getPreferredHeight#
-- | /No description available in the introspection data./

    clearWidgetClassGetPreferredHeight      ,
    getWidgetClassGetPreferredHeight        ,
    setWidgetClassGetPreferredHeight        ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_getPreferredHeight          ,
#endif


-- ** getPreferredHeightAndBaselineForWidth #attr:getPreferredHeightAndBaselineForWidth#
-- | /No description available in the introspection data./

    clearWidgetClassGetPreferredHeightAndBaselineForWidth,
    getWidgetClassGetPreferredHeightAndBaselineForWidth,
    setWidgetClassGetPreferredHeightAndBaselineForWidth,
#if defined(ENABLE_OVERLOADING)
    widgetClass_getPreferredHeightAndBaselineForWidth,
#endif


-- ** getPreferredHeightForWidth #attr:getPreferredHeightForWidth#
-- | /No description available in the introspection data./

    clearWidgetClassGetPreferredHeightForWidth,
    getWidgetClassGetPreferredHeightForWidth,
    setWidgetClassGetPreferredHeightForWidth,
#if defined(ENABLE_OVERLOADING)
    widgetClass_getPreferredHeightForWidth  ,
#endif


-- ** getPreferredWidth #attr:getPreferredWidth#
-- | /No description available in the introspection data./

    clearWidgetClassGetPreferredWidth       ,
    getWidgetClassGetPreferredWidth         ,
    setWidgetClassGetPreferredWidth         ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_getPreferredWidth           ,
#endif


-- ** getPreferredWidthForHeight #attr:getPreferredWidthForHeight#
-- | /No description available in the introspection data./

    clearWidgetClassGetPreferredWidthForHeight,
    getWidgetClassGetPreferredWidthForHeight,
    setWidgetClassGetPreferredWidthForHeight,
#if defined(ENABLE_OVERLOADING)
    widgetClass_getPreferredWidthForHeight  ,
#endif


-- ** getRequestMode #attr:getRequestMode#
-- | /No description available in the introspection data./

    clearWidgetClassGetRequestMode          ,
    getWidgetClassGetRequestMode            ,
    setWidgetClassGetRequestMode            ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_getRequestMode              ,
#endif


-- ** grabBrokenEvent #attr:grabBrokenEvent#
-- | /No description available in the introspection data./

    clearWidgetClassGrabBrokenEvent         ,
    getWidgetClassGrabBrokenEvent           ,
    setWidgetClassGrabBrokenEvent           ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_grabBrokenEvent             ,
#endif


-- ** grabFocus #attr:grabFocus#
-- | /No description available in the introspection data./

    clearWidgetClassGrabFocus               ,
    getWidgetClassGrabFocus                 ,
    setWidgetClassGrabFocus                 ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_grabFocus                   ,
#endif


-- ** grabNotify #attr:grabNotify#
-- | /No description available in the introspection data./

    clearWidgetClassGrabNotify              ,
    getWidgetClassGrabNotify                ,
    setWidgetClassGrabNotify                ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_grabNotify                  ,
#endif


-- ** hide #attr:hide#
-- | /No description available in the introspection data./

    clearWidgetClassHide                    ,
    getWidgetClassHide                      ,
    setWidgetClassHide                      ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_hide                        ,
#endif


-- ** hierarchyChanged #attr:hierarchyChanged#
-- | /No description available in the introspection data./

    clearWidgetClassHierarchyChanged        ,
    getWidgetClassHierarchyChanged          ,
    setWidgetClassHierarchyChanged          ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_hierarchyChanged            ,
#endif


-- ** keyPressEvent #attr:keyPressEvent#
-- | /No description available in the introspection data./

    clearWidgetClassKeyPressEvent           ,
    getWidgetClassKeyPressEvent             ,
    setWidgetClassKeyPressEvent             ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_keyPressEvent               ,
#endif


-- ** keyReleaseEvent #attr:keyReleaseEvent#
-- | /No description available in the introspection data./

    clearWidgetClassKeyReleaseEvent         ,
    getWidgetClassKeyReleaseEvent           ,
    setWidgetClassKeyReleaseEvent           ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_keyReleaseEvent             ,
#endif


-- ** keynavFailed #attr:keynavFailed#
-- | /No description available in the introspection data./

    clearWidgetClassKeynavFailed            ,
    getWidgetClassKeynavFailed              ,
    setWidgetClassKeynavFailed              ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_keynavFailed                ,
#endif


-- ** leaveNotifyEvent #attr:leaveNotifyEvent#
-- | /No description available in the introspection data./

    clearWidgetClassLeaveNotifyEvent        ,
    getWidgetClassLeaveNotifyEvent          ,
    setWidgetClassLeaveNotifyEvent          ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_leaveNotifyEvent            ,
#endif


-- ** map #attr:map#
-- | /No description available in the introspection data./

    clearWidgetClassMap                     ,
    getWidgetClassMap                       ,
    setWidgetClassMap                       ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_map                         ,
#endif


-- ** mapEvent #attr:mapEvent#
-- | /No description available in the introspection data./

    clearWidgetClassMapEvent                ,
    getWidgetClassMapEvent                  ,
    setWidgetClassMapEvent                  ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_mapEvent                    ,
#endif


-- ** mnemonicActivate #attr:mnemonicActivate#
-- | /No description available in the introspection data./

    clearWidgetClassMnemonicActivate        ,
    getWidgetClassMnemonicActivate          ,
    setWidgetClassMnemonicActivate          ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_mnemonicActivate            ,
#endif


-- ** motionNotifyEvent #attr:motionNotifyEvent#
-- | /No description available in the introspection data./

    clearWidgetClassMotionNotifyEvent       ,
    getWidgetClassMotionNotifyEvent         ,
    setWidgetClassMotionNotifyEvent         ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_motionNotifyEvent           ,
#endif


-- ** moveFocus #attr:moveFocus#
-- | /No description available in the introspection data./

    clearWidgetClassMoveFocus               ,
    getWidgetClassMoveFocus                 ,
    setWidgetClassMoveFocus                 ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_moveFocus                   ,
#endif


-- ** parentClass #attr:parentClass#
-- | The object class structure needs to be the first
--   element in the widget class structure in order for the class mechanism
--   to work correctly. This allows a GtkWidgetClass pointer to be cast to
--   a GObjectClass pointer.

    getWidgetClassParentClass               ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_parentClass                 ,
#endif


-- ** parentSet #attr:parentSet#
-- | /No description available in the introspection data./

    clearWidgetClassParentSet               ,
    getWidgetClassParentSet                 ,
    setWidgetClassParentSet                 ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_parentSet                   ,
#endif


-- ** popupMenu #attr:popupMenu#
-- | /No description available in the introspection data./

    clearWidgetClassPopupMenu               ,
    getWidgetClassPopupMenu                 ,
    setWidgetClassPopupMenu                 ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_popupMenu                   ,
#endif


-- ** propertyNotifyEvent #attr:propertyNotifyEvent#
-- | /No description available in the introspection data./

    clearWidgetClassPropertyNotifyEvent     ,
    getWidgetClassPropertyNotifyEvent       ,
    setWidgetClassPropertyNotifyEvent       ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_propertyNotifyEvent         ,
#endif


-- ** proximityInEvent #attr:proximityInEvent#
-- | /No description available in the introspection data./

    clearWidgetClassProximityInEvent        ,
    getWidgetClassProximityInEvent          ,
    setWidgetClassProximityInEvent          ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_proximityInEvent            ,
#endif


-- ** proximityOutEvent #attr:proximityOutEvent#
-- | /No description available in the introspection data./

    clearWidgetClassProximityOutEvent       ,
    getWidgetClassProximityOutEvent         ,
    setWidgetClassProximityOutEvent         ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_proximityOutEvent           ,
#endif


-- ** queryTooltip #attr:queryTooltip#
-- | /No description available in the introspection data./

    clearWidgetClassQueryTooltip            ,
    getWidgetClassQueryTooltip              ,
    setWidgetClassQueryTooltip              ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_queryTooltip                ,
#endif


-- ** queueDrawRegion #attr:queueDrawRegion#
-- | /No description available in the introspection data./

    clearWidgetClassQueueDrawRegion         ,
    getWidgetClassQueueDrawRegion           ,
    setWidgetClassQueueDrawRegion           ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_queueDrawRegion             ,
#endif


-- ** realize #attr:realize#
-- | /No description available in the introspection data./

    clearWidgetClassRealize                 ,
    getWidgetClassRealize                   ,
    setWidgetClassRealize                   ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_realize                     ,
#endif


-- ** screenChanged #attr:screenChanged#
-- | /No description available in the introspection data./

    clearWidgetClassScreenChanged           ,
    getWidgetClassScreenChanged             ,
    setWidgetClassScreenChanged             ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_screenChanged               ,
#endif


-- ** scrollEvent #attr:scrollEvent#
-- | /No description available in the introspection data./

    clearWidgetClassScrollEvent             ,
    getWidgetClassScrollEvent               ,
    setWidgetClassScrollEvent               ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_scrollEvent                 ,
#endif


-- ** selectionClearEvent #attr:selectionClearEvent#
-- | /No description available in the introspection data./

    clearWidgetClassSelectionClearEvent     ,
    getWidgetClassSelectionClearEvent       ,
    setWidgetClassSelectionClearEvent       ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_selectionClearEvent         ,
#endif


-- ** selectionGet #attr:selectionGet#
-- | /No description available in the introspection data./

    clearWidgetClassSelectionGet            ,
    getWidgetClassSelectionGet              ,
    setWidgetClassSelectionGet              ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_selectionGet                ,
#endif


-- ** selectionNotifyEvent #attr:selectionNotifyEvent#
-- | /No description available in the introspection data./

    clearWidgetClassSelectionNotifyEvent    ,
    getWidgetClassSelectionNotifyEvent      ,
    setWidgetClassSelectionNotifyEvent      ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_selectionNotifyEvent        ,
#endif


-- ** selectionReceived #attr:selectionReceived#
-- | /No description available in the introspection data./

    clearWidgetClassSelectionReceived       ,
    getWidgetClassSelectionReceived         ,
    setWidgetClassSelectionReceived         ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_selectionReceived           ,
#endif


-- ** selectionRequestEvent #attr:selectionRequestEvent#
-- | /No description available in the introspection data./

    clearWidgetClassSelectionRequestEvent   ,
    getWidgetClassSelectionRequestEvent     ,
    setWidgetClassSelectionRequestEvent     ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_selectionRequestEvent       ,
#endif


-- ** show #attr:show#
-- | /No description available in the introspection data./

    clearWidgetClassShow                    ,
    getWidgetClassShow                      ,
    setWidgetClassShow                      ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_show                        ,
#endif


-- ** showAll #attr:showAll#
-- | /No description available in the introspection data./

    clearWidgetClassShowAll                 ,
    getWidgetClassShowAll                   ,
    setWidgetClassShowAll                   ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_showAll                     ,
#endif


-- ** showHelp #attr:showHelp#
-- | /No description available in the introspection data./

    clearWidgetClassShowHelp                ,
    getWidgetClassShowHelp                  ,
    setWidgetClassShowHelp                  ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_showHelp                    ,
#endif


-- ** sizeAllocate #attr:sizeAllocate#
-- | /No description available in the introspection data./

    clearWidgetClassSizeAllocate            ,
    getWidgetClassSizeAllocate              ,
    setWidgetClassSizeAllocate              ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_sizeAllocate                ,
#endif


-- ** stateChanged #attr:stateChanged#
-- | /No description available in the introspection data./

    clearWidgetClassStateChanged            ,
    getWidgetClassStateChanged              ,
    setWidgetClassStateChanged              ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_stateChanged                ,
#endif


-- ** stateFlagsChanged #attr:stateFlagsChanged#
-- | /No description available in the introspection data./

    clearWidgetClassStateFlagsChanged       ,
    getWidgetClassStateFlagsChanged         ,
    setWidgetClassStateFlagsChanged         ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_stateFlagsChanged           ,
#endif


-- ** styleSet #attr:styleSet#
-- | /No description available in the introspection data./

    clearWidgetClassStyleSet                ,
    getWidgetClassStyleSet                  ,
    setWidgetClassStyleSet                  ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_styleSet                    ,
#endif


-- ** styleUpdated #attr:styleUpdated#
-- | /No description available in the introspection data./

    clearWidgetClassStyleUpdated            ,
    getWidgetClassStyleUpdated              ,
    setWidgetClassStyleUpdated              ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_styleUpdated                ,
#endif


-- ** touchEvent #attr:touchEvent#
-- | /No description available in the introspection data./

    clearWidgetClassTouchEvent              ,
    getWidgetClassTouchEvent                ,
    setWidgetClassTouchEvent                ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_touchEvent                  ,
#endif


-- ** unmap #attr:unmap#
-- | /No description available in the introspection data./

    clearWidgetClassUnmap                   ,
    getWidgetClassUnmap                     ,
    setWidgetClassUnmap                     ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_unmap                       ,
#endif


-- ** unmapEvent #attr:unmapEvent#
-- | /No description available in the introspection data./

    clearWidgetClassUnmapEvent              ,
    getWidgetClassUnmapEvent                ,
    setWidgetClassUnmapEvent                ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_unmapEvent                  ,
#endif


-- ** unrealize #attr:unrealize#
-- | /No description available in the introspection data./

    clearWidgetClassUnrealize               ,
    getWidgetClassUnrealize                 ,
    setWidgetClassUnrealize                 ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_unrealize                   ,
#endif


-- ** visibilityNotifyEvent #attr:visibilityNotifyEvent#
-- | /No description available in the introspection data./

    clearWidgetClassVisibilityNotifyEvent   ,
    getWidgetClassVisibilityNotifyEvent     ,
    setWidgetClassVisibilityNotifyEvent     ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_visibilityNotifyEvent       ,
#endif


-- ** windowStateEvent #attr:windowStateEvent#
-- | /No description available in the introspection data./

    clearWidgetClassWindowStateEvent        ,
    getWidgetClassWindowStateEvent          ,
    setWidgetClassWindowStateEvent          ,
#if defined(ENABLE_OVERLOADING)
    widgetClass_windowStateEvent            ,
#endif




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

import qualified GI.Atk.Enums as Atk.Enums
import qualified GI.GLib.Callbacks as GLib.Callbacks
import qualified GI.GLib.Structs.Bytes as GLib.Bytes
import qualified GI.GObject.Callbacks as GObject.Callbacks
import qualified GI.GObject.Structs.InitiallyUnownedClass as GObject.InitiallyUnownedClass
import qualified GI.Gtk.Callbacks as Gtk.Callbacks

-- | Memory-managed wrapper type.
newtype WidgetClass = WidgetClass (SP.ManagedPtr WidgetClass)
    deriving (Eq)

instance SP.ManagedPtrNewtype WidgetClass where
    toManagedPtr (WidgetClass p) = p

instance BoxedPtr WidgetClass where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 824 >=> B.ManagedPtr.wrapPtr WidgetClass)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr WidgetClass where
    boxedPtrCalloc = callocBytes 824


-- | Construct a `WidgetClass` struct initialized to zero.
newZeroWidgetClass :: MonadIO m => m WidgetClass
newZeroWidgetClass = liftIO $ boxedPtrCalloc >>= wrapPtr WidgetClass

instance tag ~ 'AttrSet => Constructible WidgetClass tag where
    new _ attrs = do
        o <- newZeroWidgetClass
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@parent_class@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #parentClass
-- @
getWidgetClassParentClass :: MonadIO m => WidgetClass -> m GObject.InitiallyUnownedClass.InitiallyUnownedClass
getWidgetClassParentClass s = liftIO $ withManagedPtr s $ \ptr -> do
    let val = ptr `plusPtr` 0 :: (Ptr GObject.InitiallyUnownedClass.InitiallyUnownedClass)
    val' <- (newPtr GObject.InitiallyUnownedClass.InitiallyUnownedClass) val
    return val'

#if defined(ENABLE_OVERLOADING)
data WidgetClassParentClassFieldInfo
instance AttrInfo WidgetClassParentClassFieldInfo where
    type AttrBaseTypeConstraint WidgetClassParentClassFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassParentClassFieldInfo = '[ 'AttrGet]
    type AttrSetTypeConstraint WidgetClassParentClassFieldInfo = (~) (Ptr GObject.InitiallyUnownedClass.InitiallyUnownedClass)
    type AttrTransferTypeConstraint WidgetClassParentClassFieldInfo = (~)(Ptr GObject.InitiallyUnownedClass.InitiallyUnownedClass)
    type AttrTransferType WidgetClassParentClassFieldInfo = (Ptr GObject.InitiallyUnownedClass.InitiallyUnownedClass)
    type AttrGetType WidgetClassParentClassFieldInfo = GObject.InitiallyUnownedClass.InitiallyUnownedClass
    type AttrLabel WidgetClassParentClassFieldInfo = "parent_class"
    type AttrOrigin WidgetClassParentClassFieldInfo = WidgetClass
    attrGet = getWidgetClassParentClass
    attrSet = undefined
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.parentClass"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:parentClass"
        })

widgetClass_parentClass :: AttrLabelProxy "parentClass"
widgetClass_parentClass = AttrLabelProxy

#endif


-- | Get the value of the “@activate_signal@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #activateSignal
-- @
getWidgetClassActivateSignal :: MonadIO m => WidgetClass -> m Word32
getWidgetClassActivateSignal s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 136) :: IO Word32
    return val

-- | Set the value of the “@activate_signal@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #activateSignal 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassActivateSignal :: MonadIO m => WidgetClass -> Word32 -> m ()
setWidgetClassActivateSignal s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 136) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data WidgetClassActivateSignalFieldInfo
instance AttrInfo WidgetClassActivateSignalFieldInfo where
    type AttrBaseTypeConstraint WidgetClassActivateSignalFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassActivateSignalFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint WidgetClassActivateSignalFieldInfo = (~) Word32
    type AttrTransferTypeConstraint WidgetClassActivateSignalFieldInfo = (~)Word32
    type AttrTransferType WidgetClassActivateSignalFieldInfo = Word32
    type AttrGetType WidgetClassActivateSignalFieldInfo = Word32
    type AttrLabel WidgetClassActivateSignalFieldInfo = "activate_signal"
    type AttrOrigin WidgetClassActivateSignalFieldInfo = WidgetClass
    attrGet = getWidgetClassActivateSignal
    attrSet = setWidgetClassActivateSignal
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.activateSignal"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:activateSignal"
        })

widgetClass_activateSignal :: AttrLabelProxy "activateSignal"
widgetClass_activateSignal = AttrLabelProxy

#endif


-- | Get the value of the “@dispatch_child_properties_changed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #dispatchChildPropertiesChanged
-- @
getWidgetClassDispatchChildPropertiesChanged :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassDispatchChildPropertiesChangedFieldCallback)
getWidgetClassDispatchChildPropertiesChanged s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 144) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassDispatchChildPropertiesChangedFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassDispatchChildPropertiesChangedFieldCallback val'
        return val''
    return result

-- | Set the value of the “@dispatch_child_properties_changed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #dispatchChildPropertiesChanged 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassDispatchChildPropertiesChanged :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassDispatchChildPropertiesChangedFieldCallback -> m ()
setWidgetClassDispatchChildPropertiesChanged s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 144) (val :: FunPtr Gtk.Callbacks.C_WidgetClassDispatchChildPropertiesChangedFieldCallback)

-- | Set the value of the “@dispatch_child_properties_changed@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #dispatchChildPropertiesChanged
-- @
clearWidgetClassDispatchChildPropertiesChanged :: MonadIO m => WidgetClass -> m ()
clearWidgetClassDispatchChildPropertiesChanged s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 144) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassDispatchChildPropertiesChangedFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassDispatchChildPropertiesChangedFieldInfo
instance AttrInfo WidgetClassDispatchChildPropertiesChangedFieldInfo where
    type AttrBaseTypeConstraint WidgetClassDispatchChildPropertiesChangedFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassDispatchChildPropertiesChangedFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassDispatchChildPropertiesChangedFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassDispatchChildPropertiesChangedFieldCallback)
    type AttrTransferTypeConstraint WidgetClassDispatchChildPropertiesChangedFieldInfo = (~)Gtk.Callbacks.WidgetClassDispatchChildPropertiesChangedFieldCallback
    type AttrTransferType WidgetClassDispatchChildPropertiesChangedFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassDispatchChildPropertiesChangedFieldCallback)
    type AttrGetType WidgetClassDispatchChildPropertiesChangedFieldInfo = Maybe Gtk.Callbacks.WidgetClassDispatchChildPropertiesChangedFieldCallback
    type AttrLabel WidgetClassDispatchChildPropertiesChangedFieldInfo = "dispatch_child_properties_changed"
    type AttrOrigin WidgetClassDispatchChildPropertiesChangedFieldInfo = WidgetClass
    attrGet = getWidgetClassDispatchChildPropertiesChanged
    attrSet = setWidgetClassDispatchChildPropertiesChanged
    attrConstruct = undefined
    attrClear = clearWidgetClassDispatchChildPropertiesChanged
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassDispatchChildPropertiesChangedFieldCallback (Gtk.Callbacks.wrap_WidgetClassDispatchChildPropertiesChangedFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.dispatchChildPropertiesChanged"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:dispatchChildPropertiesChanged"
        })

widgetClass_dispatchChildPropertiesChanged :: AttrLabelProxy "dispatchChildPropertiesChanged"
widgetClass_dispatchChildPropertiesChanged = AttrLabelProxy

#endif


-- | Get the value of the “@destroy@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #destroy
-- @
getWidgetClassDestroy :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassDestroyFieldCallback)
getWidgetClassDestroy s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 152) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassDestroyFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassDestroyFieldCallback val'
        return val''
    return result

-- | Set the value of the “@destroy@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #destroy 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassDestroy :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassDestroyFieldCallback -> m ()
setWidgetClassDestroy s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 152) (val :: FunPtr Gtk.Callbacks.C_WidgetClassDestroyFieldCallback)

-- | Set the value of the “@destroy@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #destroy
-- @
clearWidgetClassDestroy :: MonadIO m => WidgetClass -> m ()
clearWidgetClassDestroy s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 152) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassDestroyFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassDestroyFieldInfo
instance AttrInfo WidgetClassDestroyFieldInfo where
    type AttrBaseTypeConstraint WidgetClassDestroyFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassDestroyFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassDestroyFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassDestroyFieldCallback)
    type AttrTransferTypeConstraint WidgetClassDestroyFieldInfo = (~)Gtk.Callbacks.WidgetClassDestroyFieldCallback
    type AttrTransferType WidgetClassDestroyFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassDestroyFieldCallback)
    type AttrGetType WidgetClassDestroyFieldInfo = Maybe Gtk.Callbacks.WidgetClassDestroyFieldCallback
    type AttrLabel WidgetClassDestroyFieldInfo = "destroy"
    type AttrOrigin WidgetClassDestroyFieldInfo = WidgetClass
    attrGet = getWidgetClassDestroy
    attrSet = setWidgetClassDestroy
    attrConstruct = undefined
    attrClear = clearWidgetClassDestroy
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassDestroyFieldCallback (Gtk.Callbacks.wrap_WidgetClassDestroyFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.destroy"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:destroy"
        })

widgetClass_destroy :: AttrLabelProxy "destroy"
widgetClass_destroy = AttrLabelProxy

#endif


-- | Get the value of the “@show@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #show
-- @
getWidgetClassShow :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassShowFieldCallback)
getWidgetClassShow s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 160) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassShowFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassShowFieldCallback val'
        return val''
    return result

-- | Set the value of the “@show@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #show 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassShow :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassShowFieldCallback -> m ()
setWidgetClassShow s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 160) (val :: FunPtr Gtk.Callbacks.C_WidgetClassShowFieldCallback)

-- | Set the value of the “@show@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #show
-- @
clearWidgetClassShow :: MonadIO m => WidgetClass -> m ()
clearWidgetClassShow s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 160) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassShowFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassShowFieldInfo
instance AttrInfo WidgetClassShowFieldInfo where
    type AttrBaseTypeConstraint WidgetClassShowFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassShowFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassShowFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassShowFieldCallback)
    type AttrTransferTypeConstraint WidgetClassShowFieldInfo = (~)Gtk.Callbacks.WidgetClassShowFieldCallback
    type AttrTransferType WidgetClassShowFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassShowFieldCallback)
    type AttrGetType WidgetClassShowFieldInfo = Maybe Gtk.Callbacks.WidgetClassShowFieldCallback
    type AttrLabel WidgetClassShowFieldInfo = "show"
    type AttrOrigin WidgetClassShowFieldInfo = WidgetClass
    attrGet = getWidgetClassShow
    attrSet = setWidgetClassShow
    attrConstruct = undefined
    attrClear = clearWidgetClassShow
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassShowFieldCallback (Gtk.Callbacks.wrap_WidgetClassShowFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.show"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:show"
        })

widgetClass_show :: AttrLabelProxy "show"
widgetClass_show = AttrLabelProxy

#endif


-- | Get the value of the “@show_all@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #showAll
-- @
getWidgetClassShowAll :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassShowAllFieldCallback)
getWidgetClassShowAll s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 168) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassShowAllFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassShowAllFieldCallback val'
        return val''
    return result

-- | Set the value of the “@show_all@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #showAll 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassShowAll :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassShowAllFieldCallback -> m ()
setWidgetClassShowAll s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 168) (val :: FunPtr Gtk.Callbacks.C_WidgetClassShowAllFieldCallback)

-- | Set the value of the “@show_all@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #showAll
-- @
clearWidgetClassShowAll :: MonadIO m => WidgetClass -> m ()
clearWidgetClassShowAll s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 168) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassShowAllFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassShowAllFieldInfo
instance AttrInfo WidgetClassShowAllFieldInfo where
    type AttrBaseTypeConstraint WidgetClassShowAllFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassShowAllFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassShowAllFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassShowAllFieldCallback)
    type AttrTransferTypeConstraint WidgetClassShowAllFieldInfo = (~)Gtk.Callbacks.WidgetClassShowAllFieldCallback
    type AttrTransferType WidgetClassShowAllFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassShowAllFieldCallback)
    type AttrGetType WidgetClassShowAllFieldInfo = Maybe Gtk.Callbacks.WidgetClassShowAllFieldCallback
    type AttrLabel WidgetClassShowAllFieldInfo = "show_all"
    type AttrOrigin WidgetClassShowAllFieldInfo = WidgetClass
    attrGet = getWidgetClassShowAll
    attrSet = setWidgetClassShowAll
    attrConstruct = undefined
    attrClear = clearWidgetClassShowAll
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassShowAllFieldCallback (Gtk.Callbacks.wrap_WidgetClassShowAllFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.showAll"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:showAll"
        })

widgetClass_showAll :: AttrLabelProxy "showAll"
widgetClass_showAll = AttrLabelProxy

#endif


-- | Get the value of the “@hide@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #hide
-- @
getWidgetClassHide :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassHideFieldCallback)
getWidgetClassHide s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 176) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassHideFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassHideFieldCallback val'
        return val''
    return result

-- | Set the value of the “@hide@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #hide 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassHide :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassHideFieldCallback -> m ()
setWidgetClassHide s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 176) (val :: FunPtr Gtk.Callbacks.C_WidgetClassHideFieldCallback)

-- | Set the value of the “@hide@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #hide
-- @
clearWidgetClassHide :: MonadIO m => WidgetClass -> m ()
clearWidgetClassHide s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 176) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassHideFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassHideFieldInfo
instance AttrInfo WidgetClassHideFieldInfo where
    type AttrBaseTypeConstraint WidgetClassHideFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassHideFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassHideFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassHideFieldCallback)
    type AttrTransferTypeConstraint WidgetClassHideFieldInfo = (~)Gtk.Callbacks.WidgetClassHideFieldCallback
    type AttrTransferType WidgetClassHideFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassHideFieldCallback)
    type AttrGetType WidgetClassHideFieldInfo = Maybe Gtk.Callbacks.WidgetClassHideFieldCallback
    type AttrLabel WidgetClassHideFieldInfo = "hide"
    type AttrOrigin WidgetClassHideFieldInfo = WidgetClass
    attrGet = getWidgetClassHide
    attrSet = setWidgetClassHide
    attrConstruct = undefined
    attrClear = clearWidgetClassHide
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassHideFieldCallback (Gtk.Callbacks.wrap_WidgetClassHideFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.hide"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:hide"
        })

widgetClass_hide :: AttrLabelProxy "hide"
widgetClass_hide = AttrLabelProxy

#endif


-- | Get the value of the “@map@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #map
-- @
getWidgetClassMap :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassMapFieldCallback)
getWidgetClassMap s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 184) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassMapFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassMapFieldCallback val'
        return val''
    return result

-- | Set the value of the “@map@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #map 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassMap :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassMapFieldCallback -> m ()
setWidgetClassMap s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 184) (val :: FunPtr Gtk.Callbacks.C_WidgetClassMapFieldCallback)

-- | Set the value of the “@map@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #map
-- @
clearWidgetClassMap :: MonadIO m => WidgetClass -> m ()
clearWidgetClassMap s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 184) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassMapFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassMapFieldInfo
instance AttrInfo WidgetClassMapFieldInfo where
    type AttrBaseTypeConstraint WidgetClassMapFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassMapFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassMapFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassMapFieldCallback)
    type AttrTransferTypeConstraint WidgetClassMapFieldInfo = (~)Gtk.Callbacks.WidgetClassMapFieldCallback
    type AttrTransferType WidgetClassMapFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassMapFieldCallback)
    type AttrGetType WidgetClassMapFieldInfo = Maybe Gtk.Callbacks.WidgetClassMapFieldCallback
    type AttrLabel WidgetClassMapFieldInfo = "map"
    type AttrOrigin WidgetClassMapFieldInfo = WidgetClass
    attrGet = getWidgetClassMap
    attrSet = setWidgetClassMap
    attrConstruct = undefined
    attrClear = clearWidgetClassMap
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassMapFieldCallback (Gtk.Callbacks.wrap_WidgetClassMapFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.map"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:map"
        })

widgetClass_map :: AttrLabelProxy "map"
widgetClass_map = AttrLabelProxy

#endif


-- | Get the value of the “@unmap@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #unmap
-- @
getWidgetClassUnmap :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassUnmapFieldCallback)
getWidgetClassUnmap s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 192) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassUnmapFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassUnmapFieldCallback val'
        return val''
    return result

-- | Set the value of the “@unmap@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #unmap 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassUnmap :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassUnmapFieldCallback -> m ()
setWidgetClassUnmap s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 192) (val :: FunPtr Gtk.Callbacks.C_WidgetClassUnmapFieldCallback)

-- | Set the value of the “@unmap@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #unmap
-- @
clearWidgetClassUnmap :: MonadIO m => WidgetClass -> m ()
clearWidgetClassUnmap s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 192) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassUnmapFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassUnmapFieldInfo
instance AttrInfo WidgetClassUnmapFieldInfo where
    type AttrBaseTypeConstraint WidgetClassUnmapFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassUnmapFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassUnmapFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassUnmapFieldCallback)
    type AttrTransferTypeConstraint WidgetClassUnmapFieldInfo = (~)Gtk.Callbacks.WidgetClassUnmapFieldCallback
    type AttrTransferType WidgetClassUnmapFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassUnmapFieldCallback)
    type AttrGetType WidgetClassUnmapFieldInfo = Maybe Gtk.Callbacks.WidgetClassUnmapFieldCallback
    type AttrLabel WidgetClassUnmapFieldInfo = "unmap"
    type AttrOrigin WidgetClassUnmapFieldInfo = WidgetClass
    attrGet = getWidgetClassUnmap
    attrSet = setWidgetClassUnmap
    attrConstruct = undefined
    attrClear = clearWidgetClassUnmap
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassUnmapFieldCallback (Gtk.Callbacks.wrap_WidgetClassUnmapFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.unmap"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:unmap"
        })

widgetClass_unmap :: AttrLabelProxy "unmap"
widgetClass_unmap = AttrLabelProxy

#endif


-- | Get the value of the “@realize@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #realize
-- @
getWidgetClassRealize :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassRealizeFieldCallback)
getWidgetClassRealize s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 200) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassRealizeFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassRealizeFieldCallback val'
        return val''
    return result

-- | Set the value of the “@realize@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #realize 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassRealize :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassRealizeFieldCallback -> m ()
setWidgetClassRealize s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 200) (val :: FunPtr Gtk.Callbacks.C_WidgetClassRealizeFieldCallback)

-- | Set the value of the “@realize@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #realize
-- @
clearWidgetClassRealize :: MonadIO m => WidgetClass -> m ()
clearWidgetClassRealize s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 200) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassRealizeFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassRealizeFieldInfo
instance AttrInfo WidgetClassRealizeFieldInfo where
    type AttrBaseTypeConstraint WidgetClassRealizeFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassRealizeFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassRealizeFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassRealizeFieldCallback)
    type AttrTransferTypeConstraint WidgetClassRealizeFieldInfo = (~)Gtk.Callbacks.WidgetClassRealizeFieldCallback
    type AttrTransferType WidgetClassRealizeFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassRealizeFieldCallback)
    type AttrGetType WidgetClassRealizeFieldInfo = Maybe Gtk.Callbacks.WidgetClassRealizeFieldCallback
    type AttrLabel WidgetClassRealizeFieldInfo = "realize"
    type AttrOrigin WidgetClassRealizeFieldInfo = WidgetClass
    attrGet = getWidgetClassRealize
    attrSet = setWidgetClassRealize
    attrConstruct = undefined
    attrClear = clearWidgetClassRealize
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassRealizeFieldCallback (Gtk.Callbacks.wrap_WidgetClassRealizeFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.realize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:realize"
        })

widgetClass_realize :: AttrLabelProxy "realize"
widgetClass_realize = AttrLabelProxy

#endif


-- | Get the value of the “@unrealize@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #unrealize
-- @
getWidgetClassUnrealize :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassUnrealizeFieldCallback)
getWidgetClassUnrealize s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 208) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassUnrealizeFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassUnrealizeFieldCallback val'
        return val''
    return result

-- | Set the value of the “@unrealize@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #unrealize 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassUnrealize :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassUnrealizeFieldCallback -> m ()
setWidgetClassUnrealize s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 208) (val :: FunPtr Gtk.Callbacks.C_WidgetClassUnrealizeFieldCallback)

-- | Set the value of the “@unrealize@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #unrealize
-- @
clearWidgetClassUnrealize :: MonadIO m => WidgetClass -> m ()
clearWidgetClassUnrealize s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 208) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassUnrealizeFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassUnrealizeFieldInfo
instance AttrInfo WidgetClassUnrealizeFieldInfo where
    type AttrBaseTypeConstraint WidgetClassUnrealizeFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassUnrealizeFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassUnrealizeFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassUnrealizeFieldCallback)
    type AttrTransferTypeConstraint WidgetClassUnrealizeFieldInfo = (~)Gtk.Callbacks.WidgetClassUnrealizeFieldCallback
    type AttrTransferType WidgetClassUnrealizeFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassUnrealizeFieldCallback)
    type AttrGetType WidgetClassUnrealizeFieldInfo = Maybe Gtk.Callbacks.WidgetClassUnrealizeFieldCallback
    type AttrLabel WidgetClassUnrealizeFieldInfo = "unrealize"
    type AttrOrigin WidgetClassUnrealizeFieldInfo = WidgetClass
    attrGet = getWidgetClassUnrealize
    attrSet = setWidgetClassUnrealize
    attrConstruct = undefined
    attrClear = clearWidgetClassUnrealize
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassUnrealizeFieldCallback (Gtk.Callbacks.wrap_WidgetClassUnrealizeFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.unrealize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:unrealize"
        })

widgetClass_unrealize :: AttrLabelProxy "unrealize"
widgetClass_unrealize = AttrLabelProxy

#endif


-- | Get the value of the “@size_allocate@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #sizeAllocate
-- @
getWidgetClassSizeAllocate :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassSizeAllocateFieldCallback)
getWidgetClassSizeAllocate s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 216) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassSizeAllocateFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassSizeAllocateFieldCallback val'
        return val''
    return result

-- | Set the value of the “@size_allocate@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #sizeAllocate 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassSizeAllocate :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassSizeAllocateFieldCallback -> m ()
setWidgetClassSizeAllocate s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 216) (val :: FunPtr Gtk.Callbacks.C_WidgetClassSizeAllocateFieldCallback)

-- | Set the value of the “@size_allocate@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #sizeAllocate
-- @
clearWidgetClassSizeAllocate :: MonadIO m => WidgetClass -> m ()
clearWidgetClassSizeAllocate s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 216) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassSizeAllocateFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassSizeAllocateFieldInfo
instance AttrInfo WidgetClassSizeAllocateFieldInfo where
    type AttrBaseTypeConstraint WidgetClassSizeAllocateFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassSizeAllocateFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassSizeAllocateFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassSizeAllocateFieldCallback)
    type AttrTransferTypeConstraint WidgetClassSizeAllocateFieldInfo = (~)Gtk.Callbacks.WidgetClassSizeAllocateFieldCallback
    type AttrTransferType WidgetClassSizeAllocateFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassSizeAllocateFieldCallback)
    type AttrGetType WidgetClassSizeAllocateFieldInfo = Maybe Gtk.Callbacks.WidgetClassSizeAllocateFieldCallback
    type AttrLabel WidgetClassSizeAllocateFieldInfo = "size_allocate"
    type AttrOrigin WidgetClassSizeAllocateFieldInfo = WidgetClass
    attrGet = getWidgetClassSizeAllocate
    attrSet = setWidgetClassSizeAllocate
    attrConstruct = undefined
    attrClear = clearWidgetClassSizeAllocate
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassSizeAllocateFieldCallback (Gtk.Callbacks.wrap_WidgetClassSizeAllocateFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.sizeAllocate"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:sizeAllocate"
        })

widgetClass_sizeAllocate :: AttrLabelProxy "sizeAllocate"
widgetClass_sizeAllocate = AttrLabelProxy

#endif


-- | Get the value of the “@state_changed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #stateChanged
-- @
getWidgetClassStateChanged :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassStateChangedFieldCallback)
getWidgetClassStateChanged s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 224) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassStateChangedFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassStateChangedFieldCallback val'
        return val''
    return result

-- | Set the value of the “@state_changed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #stateChanged 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassStateChanged :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassStateChangedFieldCallback -> m ()
setWidgetClassStateChanged s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 224) (val :: FunPtr Gtk.Callbacks.C_WidgetClassStateChangedFieldCallback)

-- | Set the value of the “@state_changed@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #stateChanged
-- @
clearWidgetClassStateChanged :: MonadIO m => WidgetClass -> m ()
clearWidgetClassStateChanged s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 224) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassStateChangedFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassStateChangedFieldInfo
instance AttrInfo WidgetClassStateChangedFieldInfo where
    type AttrBaseTypeConstraint WidgetClassStateChangedFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassStateChangedFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassStateChangedFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassStateChangedFieldCallback)
    type AttrTransferTypeConstraint WidgetClassStateChangedFieldInfo = (~)Gtk.Callbacks.WidgetClassStateChangedFieldCallback
    type AttrTransferType WidgetClassStateChangedFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassStateChangedFieldCallback)
    type AttrGetType WidgetClassStateChangedFieldInfo = Maybe Gtk.Callbacks.WidgetClassStateChangedFieldCallback
    type AttrLabel WidgetClassStateChangedFieldInfo = "state_changed"
    type AttrOrigin WidgetClassStateChangedFieldInfo = WidgetClass
    attrGet = getWidgetClassStateChanged
    attrSet = setWidgetClassStateChanged
    attrConstruct = undefined
    attrClear = clearWidgetClassStateChanged
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassStateChangedFieldCallback (Gtk.Callbacks.wrap_WidgetClassStateChangedFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.stateChanged"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:stateChanged"
        })

widgetClass_stateChanged :: AttrLabelProxy "stateChanged"
widgetClass_stateChanged = AttrLabelProxy

#endif


-- | Get the value of the “@state_flags_changed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #stateFlagsChanged
-- @
getWidgetClassStateFlagsChanged :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassStateFlagsChangedFieldCallback)
getWidgetClassStateFlagsChanged s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 232) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassStateFlagsChangedFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassStateFlagsChangedFieldCallback val'
        return val''
    return result

-- | Set the value of the “@state_flags_changed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #stateFlagsChanged 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassStateFlagsChanged :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassStateFlagsChangedFieldCallback -> m ()
setWidgetClassStateFlagsChanged s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 232) (val :: FunPtr Gtk.Callbacks.C_WidgetClassStateFlagsChangedFieldCallback)

-- | Set the value of the “@state_flags_changed@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #stateFlagsChanged
-- @
clearWidgetClassStateFlagsChanged :: MonadIO m => WidgetClass -> m ()
clearWidgetClassStateFlagsChanged s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 232) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassStateFlagsChangedFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassStateFlagsChangedFieldInfo
instance AttrInfo WidgetClassStateFlagsChangedFieldInfo where
    type AttrBaseTypeConstraint WidgetClassStateFlagsChangedFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassStateFlagsChangedFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassStateFlagsChangedFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassStateFlagsChangedFieldCallback)
    type AttrTransferTypeConstraint WidgetClassStateFlagsChangedFieldInfo = (~)Gtk.Callbacks.WidgetClassStateFlagsChangedFieldCallback
    type AttrTransferType WidgetClassStateFlagsChangedFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassStateFlagsChangedFieldCallback)
    type AttrGetType WidgetClassStateFlagsChangedFieldInfo = Maybe Gtk.Callbacks.WidgetClassStateFlagsChangedFieldCallback
    type AttrLabel WidgetClassStateFlagsChangedFieldInfo = "state_flags_changed"
    type AttrOrigin WidgetClassStateFlagsChangedFieldInfo = WidgetClass
    attrGet = getWidgetClassStateFlagsChanged
    attrSet = setWidgetClassStateFlagsChanged
    attrConstruct = undefined
    attrClear = clearWidgetClassStateFlagsChanged
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassStateFlagsChangedFieldCallback (Gtk.Callbacks.wrap_WidgetClassStateFlagsChangedFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.stateFlagsChanged"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:stateFlagsChanged"
        })

widgetClass_stateFlagsChanged :: AttrLabelProxy "stateFlagsChanged"
widgetClass_stateFlagsChanged = AttrLabelProxy

#endif


-- | Get the value of the “@parent_set@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #parentSet
-- @
getWidgetClassParentSet :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassParentSetFieldCallback)
getWidgetClassParentSet s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 240) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassParentSetFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassParentSetFieldCallback val'
        return val''
    return result

-- | Set the value of the “@parent_set@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #parentSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassParentSet :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassParentSetFieldCallback -> m ()
setWidgetClassParentSet s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 240) (val :: FunPtr Gtk.Callbacks.C_WidgetClassParentSetFieldCallback)

-- | Set the value of the “@parent_set@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #parentSet
-- @
clearWidgetClassParentSet :: MonadIO m => WidgetClass -> m ()
clearWidgetClassParentSet s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 240) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassParentSetFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassParentSetFieldInfo
instance AttrInfo WidgetClassParentSetFieldInfo where
    type AttrBaseTypeConstraint WidgetClassParentSetFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassParentSetFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassParentSetFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassParentSetFieldCallback)
    type AttrTransferTypeConstraint WidgetClassParentSetFieldInfo = (~)Gtk.Callbacks.WidgetClassParentSetFieldCallback
    type AttrTransferType WidgetClassParentSetFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassParentSetFieldCallback)
    type AttrGetType WidgetClassParentSetFieldInfo = Maybe Gtk.Callbacks.WidgetClassParentSetFieldCallback
    type AttrLabel WidgetClassParentSetFieldInfo = "parent_set"
    type AttrOrigin WidgetClassParentSetFieldInfo = WidgetClass
    attrGet = getWidgetClassParentSet
    attrSet = setWidgetClassParentSet
    attrConstruct = undefined
    attrClear = clearWidgetClassParentSet
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassParentSetFieldCallback (Gtk.Callbacks.wrap_WidgetClassParentSetFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.parentSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:parentSet"
        })

widgetClass_parentSet :: AttrLabelProxy "parentSet"
widgetClass_parentSet = AttrLabelProxy

#endif


-- | Get the value of the “@hierarchy_changed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #hierarchyChanged
-- @
getWidgetClassHierarchyChanged :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassHierarchyChangedFieldCallback)
getWidgetClassHierarchyChanged s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 248) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassHierarchyChangedFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassHierarchyChangedFieldCallback val'
        return val''
    return result

-- | Set the value of the “@hierarchy_changed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #hierarchyChanged 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassHierarchyChanged :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassHierarchyChangedFieldCallback -> m ()
setWidgetClassHierarchyChanged s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 248) (val :: FunPtr Gtk.Callbacks.C_WidgetClassHierarchyChangedFieldCallback)

-- | Set the value of the “@hierarchy_changed@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #hierarchyChanged
-- @
clearWidgetClassHierarchyChanged :: MonadIO m => WidgetClass -> m ()
clearWidgetClassHierarchyChanged s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 248) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassHierarchyChangedFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassHierarchyChangedFieldInfo
instance AttrInfo WidgetClassHierarchyChangedFieldInfo where
    type AttrBaseTypeConstraint WidgetClassHierarchyChangedFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassHierarchyChangedFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassHierarchyChangedFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassHierarchyChangedFieldCallback)
    type AttrTransferTypeConstraint WidgetClassHierarchyChangedFieldInfo = (~)Gtk.Callbacks.WidgetClassHierarchyChangedFieldCallback
    type AttrTransferType WidgetClassHierarchyChangedFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassHierarchyChangedFieldCallback)
    type AttrGetType WidgetClassHierarchyChangedFieldInfo = Maybe Gtk.Callbacks.WidgetClassHierarchyChangedFieldCallback
    type AttrLabel WidgetClassHierarchyChangedFieldInfo = "hierarchy_changed"
    type AttrOrigin WidgetClassHierarchyChangedFieldInfo = WidgetClass
    attrGet = getWidgetClassHierarchyChanged
    attrSet = setWidgetClassHierarchyChanged
    attrConstruct = undefined
    attrClear = clearWidgetClassHierarchyChanged
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassHierarchyChangedFieldCallback (Gtk.Callbacks.wrap_WidgetClassHierarchyChangedFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.hierarchyChanged"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:hierarchyChanged"
        })

widgetClass_hierarchyChanged :: AttrLabelProxy "hierarchyChanged"
widgetClass_hierarchyChanged = AttrLabelProxy

#endif


-- | Get the value of the “@style_set@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #styleSet
-- @
getWidgetClassStyleSet :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassStyleSetFieldCallback)
getWidgetClassStyleSet s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 256) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassStyleSetFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassStyleSetFieldCallback val'
        return val''
    return result

-- | Set the value of the “@style_set@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #styleSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassStyleSet :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassStyleSetFieldCallback -> m ()
setWidgetClassStyleSet s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 256) (val :: FunPtr Gtk.Callbacks.C_WidgetClassStyleSetFieldCallback)

-- | Set the value of the “@style_set@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #styleSet
-- @
clearWidgetClassStyleSet :: MonadIO m => WidgetClass -> m ()
clearWidgetClassStyleSet s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 256) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassStyleSetFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassStyleSetFieldInfo
instance AttrInfo WidgetClassStyleSetFieldInfo where
    type AttrBaseTypeConstraint WidgetClassStyleSetFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassStyleSetFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassStyleSetFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassStyleSetFieldCallback)
    type AttrTransferTypeConstraint WidgetClassStyleSetFieldInfo = (~)Gtk.Callbacks.WidgetClassStyleSetFieldCallback
    type AttrTransferType WidgetClassStyleSetFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassStyleSetFieldCallback)
    type AttrGetType WidgetClassStyleSetFieldInfo = Maybe Gtk.Callbacks.WidgetClassStyleSetFieldCallback
    type AttrLabel WidgetClassStyleSetFieldInfo = "style_set"
    type AttrOrigin WidgetClassStyleSetFieldInfo = WidgetClass
    attrGet = getWidgetClassStyleSet
    attrSet = setWidgetClassStyleSet
    attrConstruct = undefined
    attrClear = clearWidgetClassStyleSet
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassStyleSetFieldCallback (Gtk.Callbacks.wrap_WidgetClassStyleSetFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.styleSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:styleSet"
        })

widgetClass_styleSet :: AttrLabelProxy "styleSet"
widgetClass_styleSet = AttrLabelProxy

#endif


-- | Get the value of the “@direction_changed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #directionChanged
-- @
getWidgetClassDirectionChanged :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassDirectionChangedFieldCallback)
getWidgetClassDirectionChanged s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 264) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassDirectionChangedFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassDirectionChangedFieldCallback val'
        return val''
    return result

-- | Set the value of the “@direction_changed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #directionChanged 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassDirectionChanged :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassDirectionChangedFieldCallback -> m ()
setWidgetClassDirectionChanged s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 264) (val :: FunPtr Gtk.Callbacks.C_WidgetClassDirectionChangedFieldCallback)

-- | Set the value of the “@direction_changed@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #directionChanged
-- @
clearWidgetClassDirectionChanged :: MonadIO m => WidgetClass -> m ()
clearWidgetClassDirectionChanged s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 264) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassDirectionChangedFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassDirectionChangedFieldInfo
instance AttrInfo WidgetClassDirectionChangedFieldInfo where
    type AttrBaseTypeConstraint WidgetClassDirectionChangedFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassDirectionChangedFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassDirectionChangedFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassDirectionChangedFieldCallback)
    type AttrTransferTypeConstraint WidgetClassDirectionChangedFieldInfo = (~)Gtk.Callbacks.WidgetClassDirectionChangedFieldCallback
    type AttrTransferType WidgetClassDirectionChangedFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassDirectionChangedFieldCallback)
    type AttrGetType WidgetClassDirectionChangedFieldInfo = Maybe Gtk.Callbacks.WidgetClassDirectionChangedFieldCallback
    type AttrLabel WidgetClassDirectionChangedFieldInfo = "direction_changed"
    type AttrOrigin WidgetClassDirectionChangedFieldInfo = WidgetClass
    attrGet = getWidgetClassDirectionChanged
    attrSet = setWidgetClassDirectionChanged
    attrConstruct = undefined
    attrClear = clearWidgetClassDirectionChanged
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassDirectionChangedFieldCallback (Gtk.Callbacks.wrap_WidgetClassDirectionChangedFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.directionChanged"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:directionChanged"
        })

widgetClass_directionChanged :: AttrLabelProxy "directionChanged"
widgetClass_directionChanged = AttrLabelProxy

#endif


-- | Get the value of the “@grab_notify@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #grabNotify
-- @
getWidgetClassGrabNotify :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassGrabNotifyFieldCallback)
getWidgetClassGrabNotify s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 272) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassGrabNotifyFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassGrabNotifyFieldCallback val'
        return val''
    return result

-- | Set the value of the “@grab_notify@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #grabNotify 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassGrabNotify :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassGrabNotifyFieldCallback -> m ()
setWidgetClassGrabNotify s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 272) (val :: FunPtr Gtk.Callbacks.C_WidgetClassGrabNotifyFieldCallback)

-- | Set the value of the “@grab_notify@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #grabNotify
-- @
clearWidgetClassGrabNotify :: MonadIO m => WidgetClass -> m ()
clearWidgetClassGrabNotify s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 272) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassGrabNotifyFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassGrabNotifyFieldInfo
instance AttrInfo WidgetClassGrabNotifyFieldInfo where
    type AttrBaseTypeConstraint WidgetClassGrabNotifyFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassGrabNotifyFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassGrabNotifyFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassGrabNotifyFieldCallback)
    type AttrTransferTypeConstraint WidgetClassGrabNotifyFieldInfo = (~)Gtk.Callbacks.WidgetClassGrabNotifyFieldCallback
    type AttrTransferType WidgetClassGrabNotifyFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassGrabNotifyFieldCallback)
    type AttrGetType WidgetClassGrabNotifyFieldInfo = Maybe Gtk.Callbacks.WidgetClassGrabNotifyFieldCallback
    type AttrLabel WidgetClassGrabNotifyFieldInfo = "grab_notify"
    type AttrOrigin WidgetClassGrabNotifyFieldInfo = WidgetClass
    attrGet = getWidgetClassGrabNotify
    attrSet = setWidgetClassGrabNotify
    attrConstruct = undefined
    attrClear = clearWidgetClassGrabNotify
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassGrabNotifyFieldCallback (Gtk.Callbacks.wrap_WidgetClassGrabNotifyFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.grabNotify"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:grabNotify"
        })

widgetClass_grabNotify :: AttrLabelProxy "grabNotify"
widgetClass_grabNotify = AttrLabelProxy

#endif


-- | Get the value of the “@child_notify@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #childNotify
-- @
getWidgetClassChildNotify :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassChildNotifyFieldCallback)
getWidgetClassChildNotify s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 280) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassChildNotifyFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassChildNotifyFieldCallback val'
        return val''
    return result

-- | Set the value of the “@child_notify@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #childNotify 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassChildNotify :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassChildNotifyFieldCallback -> m ()
setWidgetClassChildNotify s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 280) (val :: FunPtr Gtk.Callbacks.C_WidgetClassChildNotifyFieldCallback)

-- | Set the value of the “@child_notify@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #childNotify
-- @
clearWidgetClassChildNotify :: MonadIO m => WidgetClass -> m ()
clearWidgetClassChildNotify s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 280) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassChildNotifyFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassChildNotifyFieldInfo
instance AttrInfo WidgetClassChildNotifyFieldInfo where
    type AttrBaseTypeConstraint WidgetClassChildNotifyFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassChildNotifyFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassChildNotifyFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassChildNotifyFieldCallback)
    type AttrTransferTypeConstraint WidgetClassChildNotifyFieldInfo = (~)Gtk.Callbacks.WidgetClassChildNotifyFieldCallback
    type AttrTransferType WidgetClassChildNotifyFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassChildNotifyFieldCallback)
    type AttrGetType WidgetClassChildNotifyFieldInfo = Maybe Gtk.Callbacks.WidgetClassChildNotifyFieldCallback
    type AttrLabel WidgetClassChildNotifyFieldInfo = "child_notify"
    type AttrOrigin WidgetClassChildNotifyFieldInfo = WidgetClass
    attrGet = getWidgetClassChildNotify
    attrSet = setWidgetClassChildNotify
    attrConstruct = undefined
    attrClear = clearWidgetClassChildNotify
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassChildNotifyFieldCallback (Gtk.Callbacks.wrap_WidgetClassChildNotifyFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.childNotify"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:childNotify"
        })

widgetClass_childNotify :: AttrLabelProxy "childNotify"
widgetClass_childNotify = AttrLabelProxy

#endif


-- | Get the value of the “@draw@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #draw
-- @
getWidgetClassDraw :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassDrawFieldCallback)
getWidgetClassDraw s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 288) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassDrawFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassDrawFieldCallback val'
        return val''
    return result

-- | Set the value of the “@draw@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #draw 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassDraw :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassDrawFieldCallback -> m ()
setWidgetClassDraw s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 288) (val :: FunPtr Gtk.Callbacks.C_WidgetClassDrawFieldCallback)

-- | Set the value of the “@draw@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #draw
-- @
clearWidgetClassDraw :: MonadIO m => WidgetClass -> m ()
clearWidgetClassDraw s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 288) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassDrawFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassDrawFieldInfo
instance AttrInfo WidgetClassDrawFieldInfo where
    type AttrBaseTypeConstraint WidgetClassDrawFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassDrawFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassDrawFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassDrawFieldCallback)
    type AttrTransferTypeConstraint WidgetClassDrawFieldInfo = (~)Gtk.Callbacks.WidgetClassDrawFieldCallback
    type AttrTransferType WidgetClassDrawFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassDrawFieldCallback)
    type AttrGetType WidgetClassDrawFieldInfo = Maybe Gtk.Callbacks.WidgetClassDrawFieldCallback
    type AttrLabel WidgetClassDrawFieldInfo = "draw"
    type AttrOrigin WidgetClassDrawFieldInfo = WidgetClass
    attrGet = getWidgetClassDraw
    attrSet = setWidgetClassDraw
    attrConstruct = undefined
    attrClear = clearWidgetClassDraw
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassDrawFieldCallback (Gtk.Callbacks.wrap_WidgetClassDrawFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.draw"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:draw"
        })

widgetClass_draw :: AttrLabelProxy "draw"
widgetClass_draw = AttrLabelProxy

#endif


-- | Get the value of the “@get_request_mode@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #getRequestMode
-- @
getWidgetClassGetRequestMode :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassGetRequestModeFieldCallback)
getWidgetClassGetRequestMode s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 296) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassGetRequestModeFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassGetRequestModeFieldCallback val'
        return val''
    return result

-- | Set the value of the “@get_request_mode@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #getRequestMode 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassGetRequestMode :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassGetRequestModeFieldCallback -> m ()
setWidgetClassGetRequestMode s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 296) (val :: FunPtr Gtk.Callbacks.C_WidgetClassGetRequestModeFieldCallback)

-- | Set the value of the “@get_request_mode@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #getRequestMode
-- @
clearWidgetClassGetRequestMode :: MonadIO m => WidgetClass -> m ()
clearWidgetClassGetRequestMode s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 296) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassGetRequestModeFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassGetRequestModeFieldInfo
instance AttrInfo WidgetClassGetRequestModeFieldInfo where
    type AttrBaseTypeConstraint WidgetClassGetRequestModeFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassGetRequestModeFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassGetRequestModeFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassGetRequestModeFieldCallback)
    type AttrTransferTypeConstraint WidgetClassGetRequestModeFieldInfo = (~)Gtk.Callbacks.WidgetClassGetRequestModeFieldCallback
    type AttrTransferType WidgetClassGetRequestModeFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassGetRequestModeFieldCallback)
    type AttrGetType WidgetClassGetRequestModeFieldInfo = Maybe Gtk.Callbacks.WidgetClassGetRequestModeFieldCallback
    type AttrLabel WidgetClassGetRequestModeFieldInfo = "get_request_mode"
    type AttrOrigin WidgetClassGetRequestModeFieldInfo = WidgetClass
    attrGet = getWidgetClassGetRequestMode
    attrSet = setWidgetClassGetRequestMode
    attrConstruct = undefined
    attrClear = clearWidgetClassGetRequestMode
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassGetRequestModeFieldCallback (Gtk.Callbacks.wrap_WidgetClassGetRequestModeFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.getRequestMode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:getRequestMode"
        })

widgetClass_getRequestMode :: AttrLabelProxy "getRequestMode"
widgetClass_getRequestMode = AttrLabelProxy

#endif


-- | Get the value of the “@get_preferred_height@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #getPreferredHeight
-- @
getWidgetClassGetPreferredHeight :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassGetPreferredHeightFieldCallback)
getWidgetClassGetPreferredHeight s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 304) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassGetPreferredHeightFieldCallback val'
        return val''
    return result

-- | Set the value of the “@get_preferred_height@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #getPreferredHeight 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassGetPreferredHeight :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightFieldCallback -> m ()
setWidgetClassGetPreferredHeight s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 304) (val :: FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightFieldCallback)

-- | Set the value of the “@get_preferred_height@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #getPreferredHeight
-- @
clearWidgetClassGetPreferredHeight :: MonadIO m => WidgetClass -> m ()
clearWidgetClassGetPreferredHeight s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 304) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassGetPreferredHeightFieldInfo
instance AttrInfo WidgetClassGetPreferredHeightFieldInfo where
    type AttrBaseTypeConstraint WidgetClassGetPreferredHeightFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassGetPreferredHeightFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassGetPreferredHeightFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightFieldCallback)
    type AttrTransferTypeConstraint WidgetClassGetPreferredHeightFieldInfo = (~)Gtk.Callbacks.WidgetClassGetPreferredHeightFieldCallback
    type AttrTransferType WidgetClassGetPreferredHeightFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightFieldCallback)
    type AttrGetType WidgetClassGetPreferredHeightFieldInfo = Maybe Gtk.Callbacks.WidgetClassGetPreferredHeightFieldCallback
    type AttrLabel WidgetClassGetPreferredHeightFieldInfo = "get_preferred_height"
    type AttrOrigin WidgetClassGetPreferredHeightFieldInfo = WidgetClass
    attrGet = getWidgetClassGetPreferredHeight
    attrSet = setWidgetClassGetPreferredHeight
    attrConstruct = undefined
    attrClear = clearWidgetClassGetPreferredHeight
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassGetPreferredHeightFieldCallback (Gtk.Callbacks.wrap_WidgetClassGetPreferredHeightFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.getPreferredHeight"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:getPreferredHeight"
        })

widgetClass_getPreferredHeight :: AttrLabelProxy "getPreferredHeight"
widgetClass_getPreferredHeight = AttrLabelProxy

#endif


-- | Get the value of the “@get_preferred_width_for_height@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #getPreferredWidthForHeight
-- @
getWidgetClassGetPreferredWidthForHeight :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassGetPreferredWidthForHeightFieldCallback)
getWidgetClassGetPreferredWidthForHeight s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 312) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredWidthForHeightFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassGetPreferredWidthForHeightFieldCallback val'
        return val''
    return result

-- | Set the value of the “@get_preferred_width_for_height@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #getPreferredWidthForHeight 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassGetPreferredWidthForHeight :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredWidthForHeightFieldCallback -> m ()
setWidgetClassGetPreferredWidthForHeight s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 312) (val :: FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredWidthForHeightFieldCallback)

-- | Set the value of the “@get_preferred_width_for_height@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #getPreferredWidthForHeight
-- @
clearWidgetClassGetPreferredWidthForHeight :: MonadIO m => WidgetClass -> m ()
clearWidgetClassGetPreferredWidthForHeight s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 312) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredWidthForHeightFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassGetPreferredWidthForHeightFieldInfo
instance AttrInfo WidgetClassGetPreferredWidthForHeightFieldInfo where
    type AttrBaseTypeConstraint WidgetClassGetPreferredWidthForHeightFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassGetPreferredWidthForHeightFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassGetPreferredWidthForHeightFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredWidthForHeightFieldCallback)
    type AttrTransferTypeConstraint WidgetClassGetPreferredWidthForHeightFieldInfo = (~)Gtk.Callbacks.WidgetClassGetPreferredWidthForHeightFieldCallback
    type AttrTransferType WidgetClassGetPreferredWidthForHeightFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredWidthForHeightFieldCallback)
    type AttrGetType WidgetClassGetPreferredWidthForHeightFieldInfo = Maybe Gtk.Callbacks.WidgetClassGetPreferredWidthForHeightFieldCallback
    type AttrLabel WidgetClassGetPreferredWidthForHeightFieldInfo = "get_preferred_width_for_height"
    type AttrOrigin WidgetClassGetPreferredWidthForHeightFieldInfo = WidgetClass
    attrGet = getWidgetClassGetPreferredWidthForHeight
    attrSet = setWidgetClassGetPreferredWidthForHeight
    attrConstruct = undefined
    attrClear = clearWidgetClassGetPreferredWidthForHeight
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassGetPreferredWidthForHeightFieldCallback (Gtk.Callbacks.wrap_WidgetClassGetPreferredWidthForHeightFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.getPreferredWidthForHeight"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:getPreferredWidthForHeight"
        })

widgetClass_getPreferredWidthForHeight :: AttrLabelProxy "getPreferredWidthForHeight"
widgetClass_getPreferredWidthForHeight = AttrLabelProxy

#endif


-- | Get the value of the “@get_preferred_width@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #getPreferredWidth
-- @
getWidgetClassGetPreferredWidth :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassGetPreferredWidthFieldCallback)
getWidgetClassGetPreferredWidth s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 320) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredWidthFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassGetPreferredWidthFieldCallback val'
        return val''
    return result

-- | Set the value of the “@get_preferred_width@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #getPreferredWidth 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassGetPreferredWidth :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredWidthFieldCallback -> m ()
setWidgetClassGetPreferredWidth s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 320) (val :: FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredWidthFieldCallback)

-- | Set the value of the “@get_preferred_width@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #getPreferredWidth
-- @
clearWidgetClassGetPreferredWidth :: MonadIO m => WidgetClass -> m ()
clearWidgetClassGetPreferredWidth s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 320) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredWidthFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassGetPreferredWidthFieldInfo
instance AttrInfo WidgetClassGetPreferredWidthFieldInfo where
    type AttrBaseTypeConstraint WidgetClassGetPreferredWidthFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassGetPreferredWidthFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassGetPreferredWidthFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredWidthFieldCallback)
    type AttrTransferTypeConstraint WidgetClassGetPreferredWidthFieldInfo = (~)Gtk.Callbacks.WidgetClassGetPreferredWidthFieldCallback
    type AttrTransferType WidgetClassGetPreferredWidthFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredWidthFieldCallback)
    type AttrGetType WidgetClassGetPreferredWidthFieldInfo = Maybe Gtk.Callbacks.WidgetClassGetPreferredWidthFieldCallback
    type AttrLabel WidgetClassGetPreferredWidthFieldInfo = "get_preferred_width"
    type AttrOrigin WidgetClassGetPreferredWidthFieldInfo = WidgetClass
    attrGet = getWidgetClassGetPreferredWidth
    attrSet = setWidgetClassGetPreferredWidth
    attrConstruct = undefined
    attrClear = clearWidgetClassGetPreferredWidth
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassGetPreferredWidthFieldCallback (Gtk.Callbacks.wrap_WidgetClassGetPreferredWidthFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.getPreferredWidth"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:getPreferredWidth"
        })

widgetClass_getPreferredWidth :: AttrLabelProxy "getPreferredWidth"
widgetClass_getPreferredWidth = AttrLabelProxy

#endif


-- | Get the value of the “@get_preferred_height_for_width@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #getPreferredHeightForWidth
-- @
getWidgetClassGetPreferredHeightForWidth :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassGetPreferredHeightForWidthFieldCallback)
getWidgetClassGetPreferredHeightForWidth s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 328) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightForWidthFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassGetPreferredHeightForWidthFieldCallback val'
        return val''
    return result

-- | Set the value of the “@get_preferred_height_for_width@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #getPreferredHeightForWidth 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassGetPreferredHeightForWidth :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightForWidthFieldCallback -> m ()
setWidgetClassGetPreferredHeightForWidth s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 328) (val :: FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightForWidthFieldCallback)

-- | Set the value of the “@get_preferred_height_for_width@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #getPreferredHeightForWidth
-- @
clearWidgetClassGetPreferredHeightForWidth :: MonadIO m => WidgetClass -> m ()
clearWidgetClassGetPreferredHeightForWidth s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 328) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightForWidthFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassGetPreferredHeightForWidthFieldInfo
instance AttrInfo WidgetClassGetPreferredHeightForWidthFieldInfo where
    type AttrBaseTypeConstraint WidgetClassGetPreferredHeightForWidthFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassGetPreferredHeightForWidthFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassGetPreferredHeightForWidthFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightForWidthFieldCallback)
    type AttrTransferTypeConstraint WidgetClassGetPreferredHeightForWidthFieldInfo = (~)Gtk.Callbacks.WidgetClassGetPreferredHeightForWidthFieldCallback
    type AttrTransferType WidgetClassGetPreferredHeightForWidthFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightForWidthFieldCallback)
    type AttrGetType WidgetClassGetPreferredHeightForWidthFieldInfo = Maybe Gtk.Callbacks.WidgetClassGetPreferredHeightForWidthFieldCallback
    type AttrLabel WidgetClassGetPreferredHeightForWidthFieldInfo = "get_preferred_height_for_width"
    type AttrOrigin WidgetClassGetPreferredHeightForWidthFieldInfo = WidgetClass
    attrGet = getWidgetClassGetPreferredHeightForWidth
    attrSet = setWidgetClassGetPreferredHeightForWidth
    attrConstruct = undefined
    attrClear = clearWidgetClassGetPreferredHeightForWidth
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassGetPreferredHeightForWidthFieldCallback (Gtk.Callbacks.wrap_WidgetClassGetPreferredHeightForWidthFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.getPreferredHeightForWidth"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:getPreferredHeightForWidth"
        })

widgetClass_getPreferredHeightForWidth :: AttrLabelProxy "getPreferredHeightForWidth"
widgetClass_getPreferredHeightForWidth = AttrLabelProxy

#endif


-- | Get the value of the “@mnemonic_activate@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #mnemonicActivate
-- @
getWidgetClassMnemonicActivate :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassMnemonicActivateFieldCallback)
getWidgetClassMnemonicActivate s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 336) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassMnemonicActivateFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassMnemonicActivateFieldCallback val'
        return val''
    return result

-- | Set the value of the “@mnemonic_activate@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #mnemonicActivate 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassMnemonicActivate :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassMnemonicActivateFieldCallback -> m ()
setWidgetClassMnemonicActivate s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 336) (val :: FunPtr Gtk.Callbacks.C_WidgetClassMnemonicActivateFieldCallback)

-- | Set the value of the “@mnemonic_activate@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #mnemonicActivate
-- @
clearWidgetClassMnemonicActivate :: MonadIO m => WidgetClass -> m ()
clearWidgetClassMnemonicActivate s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 336) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassMnemonicActivateFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassMnemonicActivateFieldInfo
instance AttrInfo WidgetClassMnemonicActivateFieldInfo where
    type AttrBaseTypeConstraint WidgetClassMnemonicActivateFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassMnemonicActivateFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassMnemonicActivateFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassMnemonicActivateFieldCallback)
    type AttrTransferTypeConstraint WidgetClassMnemonicActivateFieldInfo = (~)Gtk.Callbacks.WidgetClassMnemonicActivateFieldCallback
    type AttrTransferType WidgetClassMnemonicActivateFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassMnemonicActivateFieldCallback)
    type AttrGetType WidgetClassMnemonicActivateFieldInfo = Maybe Gtk.Callbacks.WidgetClassMnemonicActivateFieldCallback
    type AttrLabel WidgetClassMnemonicActivateFieldInfo = "mnemonic_activate"
    type AttrOrigin WidgetClassMnemonicActivateFieldInfo = WidgetClass
    attrGet = getWidgetClassMnemonicActivate
    attrSet = setWidgetClassMnemonicActivate
    attrConstruct = undefined
    attrClear = clearWidgetClassMnemonicActivate
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassMnemonicActivateFieldCallback (Gtk.Callbacks.wrap_WidgetClassMnemonicActivateFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.mnemonicActivate"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:mnemonicActivate"
        })

widgetClass_mnemonicActivate :: AttrLabelProxy "mnemonicActivate"
widgetClass_mnemonicActivate = AttrLabelProxy

#endif


-- | Get the value of the “@grab_focus@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #grabFocus
-- @
getWidgetClassGrabFocus :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassGrabFocusFieldCallback)
getWidgetClassGrabFocus s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 344) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassGrabFocusFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassGrabFocusFieldCallback val'
        return val''
    return result

-- | Set the value of the “@grab_focus@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #grabFocus 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassGrabFocus :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassGrabFocusFieldCallback -> m ()
setWidgetClassGrabFocus s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 344) (val :: FunPtr Gtk.Callbacks.C_WidgetClassGrabFocusFieldCallback)

-- | Set the value of the “@grab_focus@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #grabFocus
-- @
clearWidgetClassGrabFocus :: MonadIO m => WidgetClass -> m ()
clearWidgetClassGrabFocus s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 344) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassGrabFocusFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassGrabFocusFieldInfo
instance AttrInfo WidgetClassGrabFocusFieldInfo where
    type AttrBaseTypeConstraint WidgetClassGrabFocusFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassGrabFocusFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassGrabFocusFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassGrabFocusFieldCallback)
    type AttrTransferTypeConstraint WidgetClassGrabFocusFieldInfo = (~)Gtk.Callbacks.WidgetClassGrabFocusFieldCallback
    type AttrTransferType WidgetClassGrabFocusFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassGrabFocusFieldCallback)
    type AttrGetType WidgetClassGrabFocusFieldInfo = Maybe Gtk.Callbacks.WidgetClassGrabFocusFieldCallback
    type AttrLabel WidgetClassGrabFocusFieldInfo = "grab_focus"
    type AttrOrigin WidgetClassGrabFocusFieldInfo = WidgetClass
    attrGet = getWidgetClassGrabFocus
    attrSet = setWidgetClassGrabFocus
    attrConstruct = undefined
    attrClear = clearWidgetClassGrabFocus
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassGrabFocusFieldCallback (Gtk.Callbacks.wrap_WidgetClassGrabFocusFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.grabFocus"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:grabFocus"
        })

widgetClass_grabFocus :: AttrLabelProxy "grabFocus"
widgetClass_grabFocus = AttrLabelProxy

#endif


-- | Get the value of the “@focus@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #focus
-- @
getWidgetClassFocus :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassFocusFieldCallback)
getWidgetClassFocus s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 352) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassFocusFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassFocusFieldCallback val'
        return val''
    return result

-- | Set the value of the “@focus@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #focus 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassFocus :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassFocusFieldCallback -> m ()
setWidgetClassFocus s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 352) (val :: FunPtr Gtk.Callbacks.C_WidgetClassFocusFieldCallback)

-- | Set the value of the “@focus@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #focus
-- @
clearWidgetClassFocus :: MonadIO m => WidgetClass -> m ()
clearWidgetClassFocus s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 352) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassFocusFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassFocusFieldInfo
instance AttrInfo WidgetClassFocusFieldInfo where
    type AttrBaseTypeConstraint WidgetClassFocusFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassFocusFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassFocusFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassFocusFieldCallback)
    type AttrTransferTypeConstraint WidgetClassFocusFieldInfo = (~)Gtk.Callbacks.WidgetClassFocusFieldCallback
    type AttrTransferType WidgetClassFocusFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassFocusFieldCallback)
    type AttrGetType WidgetClassFocusFieldInfo = Maybe Gtk.Callbacks.WidgetClassFocusFieldCallback
    type AttrLabel WidgetClassFocusFieldInfo = "focus"
    type AttrOrigin WidgetClassFocusFieldInfo = WidgetClass
    attrGet = getWidgetClassFocus
    attrSet = setWidgetClassFocus
    attrConstruct = undefined
    attrClear = clearWidgetClassFocus
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassFocusFieldCallback (Gtk.Callbacks.wrap_WidgetClassFocusFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.focus"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:focus"
        })

widgetClass_focus :: AttrLabelProxy "focus"
widgetClass_focus = AttrLabelProxy

#endif


-- | Get the value of the “@move_focus@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #moveFocus
-- @
getWidgetClassMoveFocus :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassMoveFocusFieldCallback)
getWidgetClassMoveFocus s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 360) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassMoveFocusFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassMoveFocusFieldCallback val'
        return val''
    return result

-- | Set the value of the “@move_focus@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #moveFocus 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassMoveFocus :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassMoveFocusFieldCallback -> m ()
setWidgetClassMoveFocus s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 360) (val :: FunPtr Gtk.Callbacks.C_WidgetClassMoveFocusFieldCallback)

-- | Set the value of the “@move_focus@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #moveFocus
-- @
clearWidgetClassMoveFocus :: MonadIO m => WidgetClass -> m ()
clearWidgetClassMoveFocus s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 360) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassMoveFocusFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassMoveFocusFieldInfo
instance AttrInfo WidgetClassMoveFocusFieldInfo where
    type AttrBaseTypeConstraint WidgetClassMoveFocusFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassMoveFocusFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassMoveFocusFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassMoveFocusFieldCallback)
    type AttrTransferTypeConstraint WidgetClassMoveFocusFieldInfo = (~)Gtk.Callbacks.WidgetClassMoveFocusFieldCallback
    type AttrTransferType WidgetClassMoveFocusFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassMoveFocusFieldCallback)
    type AttrGetType WidgetClassMoveFocusFieldInfo = Maybe Gtk.Callbacks.WidgetClassMoveFocusFieldCallback
    type AttrLabel WidgetClassMoveFocusFieldInfo = "move_focus"
    type AttrOrigin WidgetClassMoveFocusFieldInfo = WidgetClass
    attrGet = getWidgetClassMoveFocus
    attrSet = setWidgetClassMoveFocus
    attrConstruct = undefined
    attrClear = clearWidgetClassMoveFocus
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassMoveFocusFieldCallback (Gtk.Callbacks.wrap_WidgetClassMoveFocusFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.moveFocus"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:moveFocus"
        })

widgetClass_moveFocus :: AttrLabelProxy "moveFocus"
widgetClass_moveFocus = AttrLabelProxy

#endif


-- | Get the value of the “@keynav_failed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #keynavFailed
-- @
getWidgetClassKeynavFailed :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassKeynavFailedFieldCallback)
getWidgetClassKeynavFailed s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 368) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassKeynavFailedFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassKeynavFailedFieldCallback val'
        return val''
    return result

-- | Set the value of the “@keynav_failed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #keynavFailed 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassKeynavFailed :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassKeynavFailedFieldCallback -> m ()
setWidgetClassKeynavFailed s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 368) (val :: FunPtr Gtk.Callbacks.C_WidgetClassKeynavFailedFieldCallback)

-- | Set the value of the “@keynav_failed@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #keynavFailed
-- @
clearWidgetClassKeynavFailed :: MonadIO m => WidgetClass -> m ()
clearWidgetClassKeynavFailed s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 368) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassKeynavFailedFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassKeynavFailedFieldInfo
instance AttrInfo WidgetClassKeynavFailedFieldInfo where
    type AttrBaseTypeConstraint WidgetClassKeynavFailedFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassKeynavFailedFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassKeynavFailedFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassKeynavFailedFieldCallback)
    type AttrTransferTypeConstraint WidgetClassKeynavFailedFieldInfo = (~)Gtk.Callbacks.WidgetClassKeynavFailedFieldCallback
    type AttrTransferType WidgetClassKeynavFailedFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassKeynavFailedFieldCallback)
    type AttrGetType WidgetClassKeynavFailedFieldInfo = Maybe Gtk.Callbacks.WidgetClassKeynavFailedFieldCallback
    type AttrLabel WidgetClassKeynavFailedFieldInfo = "keynav_failed"
    type AttrOrigin WidgetClassKeynavFailedFieldInfo = WidgetClass
    attrGet = getWidgetClassKeynavFailed
    attrSet = setWidgetClassKeynavFailed
    attrConstruct = undefined
    attrClear = clearWidgetClassKeynavFailed
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassKeynavFailedFieldCallback (Gtk.Callbacks.wrap_WidgetClassKeynavFailedFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.keynavFailed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:keynavFailed"
        })

widgetClass_keynavFailed :: AttrLabelProxy "keynavFailed"
widgetClass_keynavFailed = AttrLabelProxy

#endif


-- | Get the value of the “@event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #event
-- @
getWidgetClassEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassEventFieldCallback)
getWidgetClassEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 376) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #event 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassEventFieldCallback -> m ()
setWidgetClassEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 376) (val :: FunPtr Gtk.Callbacks.C_WidgetClassEventFieldCallback)

-- | Set the value of the “@event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #event
-- @
clearWidgetClassEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 376) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassEventFieldInfo
instance AttrInfo WidgetClassEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassEventFieldInfo = (~)Gtk.Callbacks.WidgetClassEventFieldCallback
    type AttrTransferType WidgetClassEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassEventFieldCallback)
    type AttrGetType WidgetClassEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassEventFieldCallback
    type AttrLabel WidgetClassEventFieldInfo = "event"
    type AttrOrigin WidgetClassEventFieldInfo = WidgetClass
    attrGet = getWidgetClassEvent
    attrSet = setWidgetClassEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.event"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:event"
        })

widgetClass_event :: AttrLabelProxy "event"
widgetClass_event = AttrLabelProxy

#endif


-- | Get the value of the “@button_press_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #buttonPressEvent
-- @
getWidgetClassButtonPressEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassButtonPressEventFieldCallback)
getWidgetClassButtonPressEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 384) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassButtonPressEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassButtonPressEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@button_press_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #buttonPressEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassButtonPressEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassButtonPressEventFieldCallback -> m ()
setWidgetClassButtonPressEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 384) (val :: FunPtr Gtk.Callbacks.C_WidgetClassButtonPressEventFieldCallback)

-- | Set the value of the “@button_press_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #buttonPressEvent
-- @
clearWidgetClassButtonPressEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassButtonPressEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 384) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassButtonPressEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassButtonPressEventFieldInfo
instance AttrInfo WidgetClassButtonPressEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassButtonPressEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassButtonPressEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassButtonPressEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassButtonPressEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassButtonPressEventFieldInfo = (~)Gtk.Callbacks.WidgetClassButtonPressEventFieldCallback
    type AttrTransferType WidgetClassButtonPressEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassButtonPressEventFieldCallback)
    type AttrGetType WidgetClassButtonPressEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassButtonPressEventFieldCallback
    type AttrLabel WidgetClassButtonPressEventFieldInfo = "button_press_event"
    type AttrOrigin WidgetClassButtonPressEventFieldInfo = WidgetClass
    attrGet = getWidgetClassButtonPressEvent
    attrSet = setWidgetClassButtonPressEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassButtonPressEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassButtonPressEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassButtonPressEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.buttonPressEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:buttonPressEvent"
        })

widgetClass_buttonPressEvent :: AttrLabelProxy "buttonPressEvent"
widgetClass_buttonPressEvent = AttrLabelProxy

#endif


-- | Get the value of the “@button_release_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #buttonReleaseEvent
-- @
getWidgetClassButtonReleaseEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassButtonReleaseEventFieldCallback)
getWidgetClassButtonReleaseEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 392) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassButtonReleaseEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassButtonReleaseEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@button_release_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #buttonReleaseEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassButtonReleaseEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassButtonReleaseEventFieldCallback -> m ()
setWidgetClassButtonReleaseEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 392) (val :: FunPtr Gtk.Callbacks.C_WidgetClassButtonReleaseEventFieldCallback)

-- | Set the value of the “@button_release_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #buttonReleaseEvent
-- @
clearWidgetClassButtonReleaseEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassButtonReleaseEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 392) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassButtonReleaseEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassButtonReleaseEventFieldInfo
instance AttrInfo WidgetClassButtonReleaseEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassButtonReleaseEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassButtonReleaseEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassButtonReleaseEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassButtonReleaseEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassButtonReleaseEventFieldInfo = (~)Gtk.Callbacks.WidgetClassButtonReleaseEventFieldCallback
    type AttrTransferType WidgetClassButtonReleaseEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassButtonReleaseEventFieldCallback)
    type AttrGetType WidgetClassButtonReleaseEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassButtonReleaseEventFieldCallback
    type AttrLabel WidgetClassButtonReleaseEventFieldInfo = "button_release_event"
    type AttrOrigin WidgetClassButtonReleaseEventFieldInfo = WidgetClass
    attrGet = getWidgetClassButtonReleaseEvent
    attrSet = setWidgetClassButtonReleaseEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassButtonReleaseEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassButtonReleaseEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassButtonReleaseEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.buttonReleaseEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:buttonReleaseEvent"
        })

widgetClass_buttonReleaseEvent :: AttrLabelProxy "buttonReleaseEvent"
widgetClass_buttonReleaseEvent = AttrLabelProxy

#endif


-- | Get the value of the “@scroll_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #scrollEvent
-- @
getWidgetClassScrollEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassScrollEventFieldCallback)
getWidgetClassScrollEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 400) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassScrollEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassScrollEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@scroll_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #scrollEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassScrollEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassScrollEventFieldCallback -> m ()
setWidgetClassScrollEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 400) (val :: FunPtr Gtk.Callbacks.C_WidgetClassScrollEventFieldCallback)

-- | Set the value of the “@scroll_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #scrollEvent
-- @
clearWidgetClassScrollEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassScrollEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 400) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassScrollEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassScrollEventFieldInfo
instance AttrInfo WidgetClassScrollEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassScrollEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassScrollEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassScrollEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassScrollEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassScrollEventFieldInfo = (~)Gtk.Callbacks.WidgetClassScrollEventFieldCallback
    type AttrTransferType WidgetClassScrollEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassScrollEventFieldCallback)
    type AttrGetType WidgetClassScrollEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassScrollEventFieldCallback
    type AttrLabel WidgetClassScrollEventFieldInfo = "scroll_event"
    type AttrOrigin WidgetClassScrollEventFieldInfo = WidgetClass
    attrGet = getWidgetClassScrollEvent
    attrSet = setWidgetClassScrollEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassScrollEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassScrollEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassScrollEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.scrollEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:scrollEvent"
        })

widgetClass_scrollEvent :: AttrLabelProxy "scrollEvent"
widgetClass_scrollEvent = AttrLabelProxy

#endif


-- | Get the value of the “@motion_notify_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #motionNotifyEvent
-- @
getWidgetClassMotionNotifyEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassMotionNotifyEventFieldCallback)
getWidgetClassMotionNotifyEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 408) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassMotionNotifyEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassMotionNotifyEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@motion_notify_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #motionNotifyEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassMotionNotifyEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassMotionNotifyEventFieldCallback -> m ()
setWidgetClassMotionNotifyEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 408) (val :: FunPtr Gtk.Callbacks.C_WidgetClassMotionNotifyEventFieldCallback)

-- | Set the value of the “@motion_notify_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #motionNotifyEvent
-- @
clearWidgetClassMotionNotifyEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassMotionNotifyEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 408) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassMotionNotifyEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassMotionNotifyEventFieldInfo
instance AttrInfo WidgetClassMotionNotifyEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassMotionNotifyEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassMotionNotifyEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassMotionNotifyEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassMotionNotifyEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassMotionNotifyEventFieldInfo = (~)Gtk.Callbacks.WidgetClassMotionNotifyEventFieldCallback
    type AttrTransferType WidgetClassMotionNotifyEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassMotionNotifyEventFieldCallback)
    type AttrGetType WidgetClassMotionNotifyEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassMotionNotifyEventFieldCallback
    type AttrLabel WidgetClassMotionNotifyEventFieldInfo = "motion_notify_event"
    type AttrOrigin WidgetClassMotionNotifyEventFieldInfo = WidgetClass
    attrGet = getWidgetClassMotionNotifyEvent
    attrSet = setWidgetClassMotionNotifyEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassMotionNotifyEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassMotionNotifyEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassMotionNotifyEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.motionNotifyEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:motionNotifyEvent"
        })

widgetClass_motionNotifyEvent :: AttrLabelProxy "motionNotifyEvent"
widgetClass_motionNotifyEvent = AttrLabelProxy

#endif


-- | Get the value of the “@delete_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #deleteEvent
-- @
getWidgetClassDeleteEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassDeleteEventFieldCallback)
getWidgetClassDeleteEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 416) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassDeleteEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassDeleteEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@delete_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #deleteEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassDeleteEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassDeleteEventFieldCallback -> m ()
setWidgetClassDeleteEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 416) (val :: FunPtr Gtk.Callbacks.C_WidgetClassDeleteEventFieldCallback)

-- | Set the value of the “@delete_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #deleteEvent
-- @
clearWidgetClassDeleteEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassDeleteEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 416) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassDeleteEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassDeleteEventFieldInfo
instance AttrInfo WidgetClassDeleteEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassDeleteEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassDeleteEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassDeleteEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassDeleteEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassDeleteEventFieldInfo = (~)Gtk.Callbacks.WidgetClassDeleteEventFieldCallback
    type AttrTransferType WidgetClassDeleteEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassDeleteEventFieldCallback)
    type AttrGetType WidgetClassDeleteEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassDeleteEventFieldCallback
    type AttrLabel WidgetClassDeleteEventFieldInfo = "delete_event"
    type AttrOrigin WidgetClassDeleteEventFieldInfo = WidgetClass
    attrGet = getWidgetClassDeleteEvent
    attrSet = setWidgetClassDeleteEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassDeleteEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassDeleteEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassDeleteEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.deleteEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:deleteEvent"
        })

widgetClass_deleteEvent :: AttrLabelProxy "deleteEvent"
widgetClass_deleteEvent = AttrLabelProxy

#endif


-- | Get the value of the “@destroy_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #destroyEvent
-- @
getWidgetClassDestroyEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassDestroyEventFieldCallback)
getWidgetClassDestroyEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 424) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassDestroyEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassDestroyEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@destroy_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #destroyEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassDestroyEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassDestroyEventFieldCallback -> m ()
setWidgetClassDestroyEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 424) (val :: FunPtr Gtk.Callbacks.C_WidgetClassDestroyEventFieldCallback)

-- | Set the value of the “@destroy_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #destroyEvent
-- @
clearWidgetClassDestroyEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassDestroyEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 424) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassDestroyEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassDestroyEventFieldInfo
instance AttrInfo WidgetClassDestroyEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassDestroyEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassDestroyEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassDestroyEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassDestroyEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassDestroyEventFieldInfo = (~)Gtk.Callbacks.WidgetClassDestroyEventFieldCallback
    type AttrTransferType WidgetClassDestroyEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassDestroyEventFieldCallback)
    type AttrGetType WidgetClassDestroyEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassDestroyEventFieldCallback
    type AttrLabel WidgetClassDestroyEventFieldInfo = "destroy_event"
    type AttrOrigin WidgetClassDestroyEventFieldInfo = WidgetClass
    attrGet = getWidgetClassDestroyEvent
    attrSet = setWidgetClassDestroyEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassDestroyEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassDestroyEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassDestroyEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.destroyEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:destroyEvent"
        })

widgetClass_destroyEvent :: AttrLabelProxy "destroyEvent"
widgetClass_destroyEvent = AttrLabelProxy

#endif


-- | Get the value of the “@key_press_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #keyPressEvent
-- @
getWidgetClassKeyPressEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassKeyPressEventFieldCallback)
getWidgetClassKeyPressEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 432) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassKeyPressEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassKeyPressEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@key_press_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #keyPressEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassKeyPressEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassKeyPressEventFieldCallback -> m ()
setWidgetClassKeyPressEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 432) (val :: FunPtr Gtk.Callbacks.C_WidgetClassKeyPressEventFieldCallback)

-- | Set the value of the “@key_press_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #keyPressEvent
-- @
clearWidgetClassKeyPressEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassKeyPressEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 432) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassKeyPressEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassKeyPressEventFieldInfo
instance AttrInfo WidgetClassKeyPressEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassKeyPressEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassKeyPressEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassKeyPressEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassKeyPressEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassKeyPressEventFieldInfo = (~)Gtk.Callbacks.WidgetClassKeyPressEventFieldCallback
    type AttrTransferType WidgetClassKeyPressEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassKeyPressEventFieldCallback)
    type AttrGetType WidgetClassKeyPressEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassKeyPressEventFieldCallback
    type AttrLabel WidgetClassKeyPressEventFieldInfo = "key_press_event"
    type AttrOrigin WidgetClassKeyPressEventFieldInfo = WidgetClass
    attrGet = getWidgetClassKeyPressEvent
    attrSet = setWidgetClassKeyPressEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassKeyPressEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassKeyPressEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassKeyPressEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.keyPressEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:keyPressEvent"
        })

widgetClass_keyPressEvent :: AttrLabelProxy "keyPressEvent"
widgetClass_keyPressEvent = AttrLabelProxy

#endif


-- | Get the value of the “@key_release_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #keyReleaseEvent
-- @
getWidgetClassKeyReleaseEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassKeyReleaseEventFieldCallback)
getWidgetClassKeyReleaseEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 440) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassKeyReleaseEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassKeyReleaseEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@key_release_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #keyReleaseEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassKeyReleaseEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassKeyReleaseEventFieldCallback -> m ()
setWidgetClassKeyReleaseEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 440) (val :: FunPtr Gtk.Callbacks.C_WidgetClassKeyReleaseEventFieldCallback)

-- | Set the value of the “@key_release_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #keyReleaseEvent
-- @
clearWidgetClassKeyReleaseEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassKeyReleaseEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 440) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassKeyReleaseEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassKeyReleaseEventFieldInfo
instance AttrInfo WidgetClassKeyReleaseEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassKeyReleaseEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassKeyReleaseEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassKeyReleaseEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassKeyReleaseEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassKeyReleaseEventFieldInfo = (~)Gtk.Callbacks.WidgetClassKeyReleaseEventFieldCallback
    type AttrTransferType WidgetClassKeyReleaseEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassKeyReleaseEventFieldCallback)
    type AttrGetType WidgetClassKeyReleaseEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassKeyReleaseEventFieldCallback
    type AttrLabel WidgetClassKeyReleaseEventFieldInfo = "key_release_event"
    type AttrOrigin WidgetClassKeyReleaseEventFieldInfo = WidgetClass
    attrGet = getWidgetClassKeyReleaseEvent
    attrSet = setWidgetClassKeyReleaseEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassKeyReleaseEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassKeyReleaseEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassKeyReleaseEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.keyReleaseEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:keyReleaseEvent"
        })

widgetClass_keyReleaseEvent :: AttrLabelProxy "keyReleaseEvent"
widgetClass_keyReleaseEvent = AttrLabelProxy

#endif


-- | Get the value of the “@enter_notify_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #enterNotifyEvent
-- @
getWidgetClassEnterNotifyEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassEnterNotifyEventFieldCallback)
getWidgetClassEnterNotifyEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 448) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassEnterNotifyEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassEnterNotifyEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@enter_notify_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #enterNotifyEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassEnterNotifyEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassEnterNotifyEventFieldCallback -> m ()
setWidgetClassEnterNotifyEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 448) (val :: FunPtr Gtk.Callbacks.C_WidgetClassEnterNotifyEventFieldCallback)

-- | Set the value of the “@enter_notify_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #enterNotifyEvent
-- @
clearWidgetClassEnterNotifyEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassEnterNotifyEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 448) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassEnterNotifyEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassEnterNotifyEventFieldInfo
instance AttrInfo WidgetClassEnterNotifyEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassEnterNotifyEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassEnterNotifyEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassEnterNotifyEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassEnterNotifyEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassEnterNotifyEventFieldInfo = (~)Gtk.Callbacks.WidgetClassEnterNotifyEventFieldCallback
    type AttrTransferType WidgetClassEnterNotifyEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassEnterNotifyEventFieldCallback)
    type AttrGetType WidgetClassEnterNotifyEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassEnterNotifyEventFieldCallback
    type AttrLabel WidgetClassEnterNotifyEventFieldInfo = "enter_notify_event"
    type AttrOrigin WidgetClassEnterNotifyEventFieldInfo = WidgetClass
    attrGet = getWidgetClassEnterNotifyEvent
    attrSet = setWidgetClassEnterNotifyEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassEnterNotifyEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassEnterNotifyEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassEnterNotifyEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.enterNotifyEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:enterNotifyEvent"
        })

widgetClass_enterNotifyEvent :: AttrLabelProxy "enterNotifyEvent"
widgetClass_enterNotifyEvent = AttrLabelProxy

#endif


-- | Get the value of the “@leave_notify_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #leaveNotifyEvent
-- @
getWidgetClassLeaveNotifyEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassLeaveNotifyEventFieldCallback)
getWidgetClassLeaveNotifyEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 456) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassLeaveNotifyEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassLeaveNotifyEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@leave_notify_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #leaveNotifyEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassLeaveNotifyEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassLeaveNotifyEventFieldCallback -> m ()
setWidgetClassLeaveNotifyEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 456) (val :: FunPtr Gtk.Callbacks.C_WidgetClassLeaveNotifyEventFieldCallback)

-- | Set the value of the “@leave_notify_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #leaveNotifyEvent
-- @
clearWidgetClassLeaveNotifyEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassLeaveNotifyEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 456) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassLeaveNotifyEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassLeaveNotifyEventFieldInfo
instance AttrInfo WidgetClassLeaveNotifyEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassLeaveNotifyEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassLeaveNotifyEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassLeaveNotifyEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassLeaveNotifyEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassLeaveNotifyEventFieldInfo = (~)Gtk.Callbacks.WidgetClassLeaveNotifyEventFieldCallback
    type AttrTransferType WidgetClassLeaveNotifyEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassLeaveNotifyEventFieldCallback)
    type AttrGetType WidgetClassLeaveNotifyEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassLeaveNotifyEventFieldCallback
    type AttrLabel WidgetClassLeaveNotifyEventFieldInfo = "leave_notify_event"
    type AttrOrigin WidgetClassLeaveNotifyEventFieldInfo = WidgetClass
    attrGet = getWidgetClassLeaveNotifyEvent
    attrSet = setWidgetClassLeaveNotifyEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassLeaveNotifyEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassLeaveNotifyEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassLeaveNotifyEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.leaveNotifyEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:leaveNotifyEvent"
        })

widgetClass_leaveNotifyEvent :: AttrLabelProxy "leaveNotifyEvent"
widgetClass_leaveNotifyEvent = AttrLabelProxy

#endif


-- | Get the value of the “@configure_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #configureEvent
-- @
getWidgetClassConfigureEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassConfigureEventFieldCallback)
getWidgetClassConfigureEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 464) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassConfigureEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassConfigureEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@configure_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #configureEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassConfigureEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassConfigureEventFieldCallback -> m ()
setWidgetClassConfigureEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 464) (val :: FunPtr Gtk.Callbacks.C_WidgetClassConfigureEventFieldCallback)

-- | Set the value of the “@configure_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #configureEvent
-- @
clearWidgetClassConfigureEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassConfigureEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 464) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassConfigureEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassConfigureEventFieldInfo
instance AttrInfo WidgetClassConfigureEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassConfigureEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassConfigureEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassConfigureEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassConfigureEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassConfigureEventFieldInfo = (~)Gtk.Callbacks.WidgetClassConfigureEventFieldCallback
    type AttrTransferType WidgetClassConfigureEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassConfigureEventFieldCallback)
    type AttrGetType WidgetClassConfigureEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassConfigureEventFieldCallback
    type AttrLabel WidgetClassConfigureEventFieldInfo = "configure_event"
    type AttrOrigin WidgetClassConfigureEventFieldInfo = WidgetClass
    attrGet = getWidgetClassConfigureEvent
    attrSet = setWidgetClassConfigureEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassConfigureEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassConfigureEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassConfigureEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.configureEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:configureEvent"
        })

widgetClass_configureEvent :: AttrLabelProxy "configureEvent"
widgetClass_configureEvent = AttrLabelProxy

#endif


-- | Get the value of the “@focus_in_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #focusInEvent
-- @
getWidgetClassFocusInEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassFocusInEventFieldCallback)
getWidgetClassFocusInEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 472) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassFocusInEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassFocusInEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@focus_in_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #focusInEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassFocusInEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassFocusInEventFieldCallback -> m ()
setWidgetClassFocusInEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 472) (val :: FunPtr Gtk.Callbacks.C_WidgetClassFocusInEventFieldCallback)

-- | Set the value of the “@focus_in_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #focusInEvent
-- @
clearWidgetClassFocusInEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassFocusInEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 472) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassFocusInEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassFocusInEventFieldInfo
instance AttrInfo WidgetClassFocusInEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassFocusInEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassFocusInEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassFocusInEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassFocusInEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassFocusInEventFieldInfo = (~)Gtk.Callbacks.WidgetClassFocusInEventFieldCallback
    type AttrTransferType WidgetClassFocusInEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassFocusInEventFieldCallback)
    type AttrGetType WidgetClassFocusInEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassFocusInEventFieldCallback
    type AttrLabel WidgetClassFocusInEventFieldInfo = "focus_in_event"
    type AttrOrigin WidgetClassFocusInEventFieldInfo = WidgetClass
    attrGet = getWidgetClassFocusInEvent
    attrSet = setWidgetClassFocusInEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassFocusInEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassFocusInEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassFocusInEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.focusInEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:focusInEvent"
        })

widgetClass_focusInEvent :: AttrLabelProxy "focusInEvent"
widgetClass_focusInEvent = AttrLabelProxy

#endif


-- | Get the value of the “@focus_out_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #focusOutEvent
-- @
getWidgetClassFocusOutEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassFocusOutEventFieldCallback)
getWidgetClassFocusOutEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 480) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassFocusOutEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassFocusOutEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@focus_out_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #focusOutEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassFocusOutEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassFocusOutEventFieldCallback -> m ()
setWidgetClassFocusOutEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 480) (val :: FunPtr Gtk.Callbacks.C_WidgetClassFocusOutEventFieldCallback)

-- | Set the value of the “@focus_out_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #focusOutEvent
-- @
clearWidgetClassFocusOutEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassFocusOutEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 480) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassFocusOutEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassFocusOutEventFieldInfo
instance AttrInfo WidgetClassFocusOutEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassFocusOutEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassFocusOutEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassFocusOutEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassFocusOutEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassFocusOutEventFieldInfo = (~)Gtk.Callbacks.WidgetClassFocusOutEventFieldCallback
    type AttrTransferType WidgetClassFocusOutEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassFocusOutEventFieldCallback)
    type AttrGetType WidgetClassFocusOutEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassFocusOutEventFieldCallback
    type AttrLabel WidgetClassFocusOutEventFieldInfo = "focus_out_event"
    type AttrOrigin WidgetClassFocusOutEventFieldInfo = WidgetClass
    attrGet = getWidgetClassFocusOutEvent
    attrSet = setWidgetClassFocusOutEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassFocusOutEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassFocusOutEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassFocusOutEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.focusOutEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:focusOutEvent"
        })

widgetClass_focusOutEvent :: AttrLabelProxy "focusOutEvent"
widgetClass_focusOutEvent = AttrLabelProxy

#endif


-- | Get the value of the “@map_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #mapEvent
-- @
getWidgetClassMapEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassMapEventFieldCallback)
getWidgetClassMapEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 488) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassMapEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassMapEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@map_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #mapEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassMapEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassMapEventFieldCallback -> m ()
setWidgetClassMapEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 488) (val :: FunPtr Gtk.Callbacks.C_WidgetClassMapEventFieldCallback)

-- | Set the value of the “@map_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #mapEvent
-- @
clearWidgetClassMapEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassMapEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 488) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassMapEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassMapEventFieldInfo
instance AttrInfo WidgetClassMapEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassMapEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassMapEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassMapEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassMapEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassMapEventFieldInfo = (~)Gtk.Callbacks.WidgetClassMapEventFieldCallback
    type AttrTransferType WidgetClassMapEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassMapEventFieldCallback)
    type AttrGetType WidgetClassMapEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassMapEventFieldCallback
    type AttrLabel WidgetClassMapEventFieldInfo = "map_event"
    type AttrOrigin WidgetClassMapEventFieldInfo = WidgetClass
    attrGet = getWidgetClassMapEvent
    attrSet = setWidgetClassMapEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassMapEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassMapEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassMapEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.mapEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:mapEvent"
        })

widgetClass_mapEvent :: AttrLabelProxy "mapEvent"
widgetClass_mapEvent = AttrLabelProxy

#endif


-- | Get the value of the “@unmap_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #unmapEvent
-- @
getWidgetClassUnmapEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassUnmapEventFieldCallback)
getWidgetClassUnmapEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 496) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassUnmapEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassUnmapEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@unmap_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #unmapEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassUnmapEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassUnmapEventFieldCallback -> m ()
setWidgetClassUnmapEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 496) (val :: FunPtr Gtk.Callbacks.C_WidgetClassUnmapEventFieldCallback)

-- | Set the value of the “@unmap_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #unmapEvent
-- @
clearWidgetClassUnmapEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassUnmapEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 496) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassUnmapEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassUnmapEventFieldInfo
instance AttrInfo WidgetClassUnmapEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassUnmapEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassUnmapEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassUnmapEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassUnmapEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassUnmapEventFieldInfo = (~)Gtk.Callbacks.WidgetClassUnmapEventFieldCallback
    type AttrTransferType WidgetClassUnmapEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassUnmapEventFieldCallback)
    type AttrGetType WidgetClassUnmapEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassUnmapEventFieldCallback
    type AttrLabel WidgetClassUnmapEventFieldInfo = "unmap_event"
    type AttrOrigin WidgetClassUnmapEventFieldInfo = WidgetClass
    attrGet = getWidgetClassUnmapEvent
    attrSet = setWidgetClassUnmapEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassUnmapEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassUnmapEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassUnmapEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.unmapEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:unmapEvent"
        })

widgetClass_unmapEvent :: AttrLabelProxy "unmapEvent"
widgetClass_unmapEvent = AttrLabelProxy

#endif


-- | Get the value of the “@property_notify_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #propertyNotifyEvent
-- @
getWidgetClassPropertyNotifyEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassPropertyNotifyEventFieldCallback)
getWidgetClassPropertyNotifyEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 504) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassPropertyNotifyEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassPropertyNotifyEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@property_notify_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #propertyNotifyEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassPropertyNotifyEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassPropertyNotifyEventFieldCallback -> m ()
setWidgetClassPropertyNotifyEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 504) (val :: FunPtr Gtk.Callbacks.C_WidgetClassPropertyNotifyEventFieldCallback)

-- | Set the value of the “@property_notify_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #propertyNotifyEvent
-- @
clearWidgetClassPropertyNotifyEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassPropertyNotifyEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 504) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassPropertyNotifyEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassPropertyNotifyEventFieldInfo
instance AttrInfo WidgetClassPropertyNotifyEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassPropertyNotifyEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassPropertyNotifyEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassPropertyNotifyEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassPropertyNotifyEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassPropertyNotifyEventFieldInfo = (~)Gtk.Callbacks.WidgetClassPropertyNotifyEventFieldCallback
    type AttrTransferType WidgetClassPropertyNotifyEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassPropertyNotifyEventFieldCallback)
    type AttrGetType WidgetClassPropertyNotifyEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassPropertyNotifyEventFieldCallback
    type AttrLabel WidgetClassPropertyNotifyEventFieldInfo = "property_notify_event"
    type AttrOrigin WidgetClassPropertyNotifyEventFieldInfo = WidgetClass
    attrGet = getWidgetClassPropertyNotifyEvent
    attrSet = setWidgetClassPropertyNotifyEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassPropertyNotifyEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassPropertyNotifyEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassPropertyNotifyEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.propertyNotifyEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:propertyNotifyEvent"
        })

widgetClass_propertyNotifyEvent :: AttrLabelProxy "propertyNotifyEvent"
widgetClass_propertyNotifyEvent = AttrLabelProxy

#endif


-- | Get the value of the “@selection_clear_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #selectionClearEvent
-- @
getWidgetClassSelectionClearEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassSelectionClearEventFieldCallback)
getWidgetClassSelectionClearEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 512) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassSelectionClearEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassSelectionClearEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@selection_clear_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #selectionClearEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassSelectionClearEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassSelectionClearEventFieldCallback -> m ()
setWidgetClassSelectionClearEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 512) (val :: FunPtr Gtk.Callbacks.C_WidgetClassSelectionClearEventFieldCallback)

-- | Set the value of the “@selection_clear_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #selectionClearEvent
-- @
clearWidgetClassSelectionClearEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassSelectionClearEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 512) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassSelectionClearEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassSelectionClearEventFieldInfo
instance AttrInfo WidgetClassSelectionClearEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassSelectionClearEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassSelectionClearEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassSelectionClearEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassSelectionClearEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassSelectionClearEventFieldInfo = (~)Gtk.Callbacks.WidgetClassSelectionClearEventFieldCallback
    type AttrTransferType WidgetClassSelectionClearEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassSelectionClearEventFieldCallback)
    type AttrGetType WidgetClassSelectionClearEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassSelectionClearEventFieldCallback
    type AttrLabel WidgetClassSelectionClearEventFieldInfo = "selection_clear_event"
    type AttrOrigin WidgetClassSelectionClearEventFieldInfo = WidgetClass
    attrGet = getWidgetClassSelectionClearEvent
    attrSet = setWidgetClassSelectionClearEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassSelectionClearEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassSelectionClearEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassSelectionClearEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.selectionClearEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:selectionClearEvent"
        })

widgetClass_selectionClearEvent :: AttrLabelProxy "selectionClearEvent"
widgetClass_selectionClearEvent = AttrLabelProxy

#endif


-- | Get the value of the “@selection_request_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #selectionRequestEvent
-- @
getWidgetClassSelectionRequestEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassSelectionRequestEventFieldCallback)
getWidgetClassSelectionRequestEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 520) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassSelectionRequestEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassSelectionRequestEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@selection_request_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #selectionRequestEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassSelectionRequestEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassSelectionRequestEventFieldCallback -> m ()
setWidgetClassSelectionRequestEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 520) (val :: FunPtr Gtk.Callbacks.C_WidgetClassSelectionRequestEventFieldCallback)

-- | Set the value of the “@selection_request_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #selectionRequestEvent
-- @
clearWidgetClassSelectionRequestEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassSelectionRequestEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 520) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassSelectionRequestEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassSelectionRequestEventFieldInfo
instance AttrInfo WidgetClassSelectionRequestEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassSelectionRequestEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassSelectionRequestEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassSelectionRequestEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassSelectionRequestEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassSelectionRequestEventFieldInfo = (~)Gtk.Callbacks.WidgetClassSelectionRequestEventFieldCallback
    type AttrTransferType WidgetClassSelectionRequestEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassSelectionRequestEventFieldCallback)
    type AttrGetType WidgetClassSelectionRequestEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassSelectionRequestEventFieldCallback
    type AttrLabel WidgetClassSelectionRequestEventFieldInfo = "selection_request_event"
    type AttrOrigin WidgetClassSelectionRequestEventFieldInfo = WidgetClass
    attrGet = getWidgetClassSelectionRequestEvent
    attrSet = setWidgetClassSelectionRequestEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassSelectionRequestEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassSelectionRequestEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassSelectionRequestEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.selectionRequestEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:selectionRequestEvent"
        })

widgetClass_selectionRequestEvent :: AttrLabelProxy "selectionRequestEvent"
widgetClass_selectionRequestEvent = AttrLabelProxy

#endif


-- | Get the value of the “@selection_notify_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #selectionNotifyEvent
-- @
getWidgetClassSelectionNotifyEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassSelectionNotifyEventFieldCallback)
getWidgetClassSelectionNotifyEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 528) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassSelectionNotifyEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassSelectionNotifyEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@selection_notify_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #selectionNotifyEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassSelectionNotifyEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassSelectionNotifyEventFieldCallback -> m ()
setWidgetClassSelectionNotifyEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 528) (val :: FunPtr Gtk.Callbacks.C_WidgetClassSelectionNotifyEventFieldCallback)

-- | Set the value of the “@selection_notify_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #selectionNotifyEvent
-- @
clearWidgetClassSelectionNotifyEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassSelectionNotifyEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 528) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassSelectionNotifyEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassSelectionNotifyEventFieldInfo
instance AttrInfo WidgetClassSelectionNotifyEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassSelectionNotifyEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassSelectionNotifyEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassSelectionNotifyEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassSelectionNotifyEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassSelectionNotifyEventFieldInfo = (~)Gtk.Callbacks.WidgetClassSelectionNotifyEventFieldCallback
    type AttrTransferType WidgetClassSelectionNotifyEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassSelectionNotifyEventFieldCallback)
    type AttrGetType WidgetClassSelectionNotifyEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassSelectionNotifyEventFieldCallback
    type AttrLabel WidgetClassSelectionNotifyEventFieldInfo = "selection_notify_event"
    type AttrOrigin WidgetClassSelectionNotifyEventFieldInfo = WidgetClass
    attrGet = getWidgetClassSelectionNotifyEvent
    attrSet = setWidgetClassSelectionNotifyEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassSelectionNotifyEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassSelectionNotifyEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassSelectionNotifyEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.selectionNotifyEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:selectionNotifyEvent"
        })

widgetClass_selectionNotifyEvent :: AttrLabelProxy "selectionNotifyEvent"
widgetClass_selectionNotifyEvent = AttrLabelProxy

#endif


-- | Get the value of the “@proximity_in_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #proximityInEvent
-- @
getWidgetClassProximityInEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassProximityInEventFieldCallback)
getWidgetClassProximityInEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 536) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassProximityInEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassProximityInEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@proximity_in_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #proximityInEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassProximityInEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassProximityInEventFieldCallback -> m ()
setWidgetClassProximityInEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 536) (val :: FunPtr Gtk.Callbacks.C_WidgetClassProximityInEventFieldCallback)

-- | Set the value of the “@proximity_in_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #proximityInEvent
-- @
clearWidgetClassProximityInEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassProximityInEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 536) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassProximityInEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassProximityInEventFieldInfo
instance AttrInfo WidgetClassProximityInEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassProximityInEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassProximityInEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassProximityInEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassProximityInEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassProximityInEventFieldInfo = (~)Gtk.Callbacks.WidgetClassProximityInEventFieldCallback
    type AttrTransferType WidgetClassProximityInEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassProximityInEventFieldCallback)
    type AttrGetType WidgetClassProximityInEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassProximityInEventFieldCallback
    type AttrLabel WidgetClassProximityInEventFieldInfo = "proximity_in_event"
    type AttrOrigin WidgetClassProximityInEventFieldInfo = WidgetClass
    attrGet = getWidgetClassProximityInEvent
    attrSet = setWidgetClassProximityInEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassProximityInEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassProximityInEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassProximityInEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.proximityInEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:proximityInEvent"
        })

widgetClass_proximityInEvent :: AttrLabelProxy "proximityInEvent"
widgetClass_proximityInEvent = AttrLabelProxy

#endif


-- | Get the value of the “@proximity_out_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #proximityOutEvent
-- @
getWidgetClassProximityOutEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassProximityOutEventFieldCallback)
getWidgetClassProximityOutEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 544) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassProximityOutEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassProximityOutEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@proximity_out_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #proximityOutEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassProximityOutEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassProximityOutEventFieldCallback -> m ()
setWidgetClassProximityOutEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 544) (val :: FunPtr Gtk.Callbacks.C_WidgetClassProximityOutEventFieldCallback)

-- | Set the value of the “@proximity_out_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #proximityOutEvent
-- @
clearWidgetClassProximityOutEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassProximityOutEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 544) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassProximityOutEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassProximityOutEventFieldInfo
instance AttrInfo WidgetClassProximityOutEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassProximityOutEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassProximityOutEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassProximityOutEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassProximityOutEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassProximityOutEventFieldInfo = (~)Gtk.Callbacks.WidgetClassProximityOutEventFieldCallback
    type AttrTransferType WidgetClassProximityOutEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassProximityOutEventFieldCallback)
    type AttrGetType WidgetClassProximityOutEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassProximityOutEventFieldCallback
    type AttrLabel WidgetClassProximityOutEventFieldInfo = "proximity_out_event"
    type AttrOrigin WidgetClassProximityOutEventFieldInfo = WidgetClass
    attrGet = getWidgetClassProximityOutEvent
    attrSet = setWidgetClassProximityOutEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassProximityOutEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassProximityOutEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassProximityOutEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.proximityOutEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:proximityOutEvent"
        })

widgetClass_proximityOutEvent :: AttrLabelProxy "proximityOutEvent"
widgetClass_proximityOutEvent = AttrLabelProxy

#endif


-- | Get the value of the “@visibility_notify_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #visibilityNotifyEvent
-- @
getWidgetClassVisibilityNotifyEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassVisibilityNotifyEventFieldCallback)
getWidgetClassVisibilityNotifyEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 552) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassVisibilityNotifyEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassVisibilityNotifyEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@visibility_notify_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #visibilityNotifyEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassVisibilityNotifyEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassVisibilityNotifyEventFieldCallback -> m ()
setWidgetClassVisibilityNotifyEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 552) (val :: FunPtr Gtk.Callbacks.C_WidgetClassVisibilityNotifyEventFieldCallback)

-- | Set the value of the “@visibility_notify_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #visibilityNotifyEvent
-- @
clearWidgetClassVisibilityNotifyEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassVisibilityNotifyEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 552) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassVisibilityNotifyEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassVisibilityNotifyEventFieldInfo
instance AttrInfo WidgetClassVisibilityNotifyEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassVisibilityNotifyEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassVisibilityNotifyEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassVisibilityNotifyEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassVisibilityNotifyEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassVisibilityNotifyEventFieldInfo = (~)Gtk.Callbacks.WidgetClassVisibilityNotifyEventFieldCallback
    type AttrTransferType WidgetClassVisibilityNotifyEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassVisibilityNotifyEventFieldCallback)
    type AttrGetType WidgetClassVisibilityNotifyEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassVisibilityNotifyEventFieldCallback
    type AttrLabel WidgetClassVisibilityNotifyEventFieldInfo = "visibility_notify_event"
    type AttrOrigin WidgetClassVisibilityNotifyEventFieldInfo = WidgetClass
    attrGet = getWidgetClassVisibilityNotifyEvent
    attrSet = setWidgetClassVisibilityNotifyEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassVisibilityNotifyEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassVisibilityNotifyEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassVisibilityNotifyEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.visibilityNotifyEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:visibilityNotifyEvent"
        })

widgetClass_visibilityNotifyEvent :: AttrLabelProxy "visibilityNotifyEvent"
widgetClass_visibilityNotifyEvent = AttrLabelProxy

#endif


-- | Get the value of the “@window_state_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #windowStateEvent
-- @
getWidgetClassWindowStateEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassWindowStateEventFieldCallback)
getWidgetClassWindowStateEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 560) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassWindowStateEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassWindowStateEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@window_state_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #windowStateEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassWindowStateEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassWindowStateEventFieldCallback -> m ()
setWidgetClassWindowStateEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 560) (val :: FunPtr Gtk.Callbacks.C_WidgetClassWindowStateEventFieldCallback)

-- | Set the value of the “@window_state_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #windowStateEvent
-- @
clearWidgetClassWindowStateEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassWindowStateEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 560) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassWindowStateEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassWindowStateEventFieldInfo
instance AttrInfo WidgetClassWindowStateEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassWindowStateEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassWindowStateEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassWindowStateEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassWindowStateEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassWindowStateEventFieldInfo = (~)Gtk.Callbacks.WidgetClassWindowStateEventFieldCallback
    type AttrTransferType WidgetClassWindowStateEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassWindowStateEventFieldCallback)
    type AttrGetType WidgetClassWindowStateEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassWindowStateEventFieldCallback
    type AttrLabel WidgetClassWindowStateEventFieldInfo = "window_state_event"
    type AttrOrigin WidgetClassWindowStateEventFieldInfo = WidgetClass
    attrGet = getWidgetClassWindowStateEvent
    attrSet = setWidgetClassWindowStateEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassWindowStateEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassWindowStateEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassWindowStateEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.windowStateEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:windowStateEvent"
        })

widgetClass_windowStateEvent :: AttrLabelProxy "windowStateEvent"
widgetClass_windowStateEvent = AttrLabelProxy

#endif


-- | Get the value of the “@damage_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #damageEvent
-- @
getWidgetClassDamageEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassDamageEventFieldCallback)
getWidgetClassDamageEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 568) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassDamageEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassDamageEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@damage_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #damageEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassDamageEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassDamageEventFieldCallback -> m ()
setWidgetClassDamageEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 568) (val :: FunPtr Gtk.Callbacks.C_WidgetClassDamageEventFieldCallback)

-- | Set the value of the “@damage_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #damageEvent
-- @
clearWidgetClassDamageEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassDamageEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 568) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassDamageEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassDamageEventFieldInfo
instance AttrInfo WidgetClassDamageEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassDamageEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassDamageEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassDamageEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassDamageEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassDamageEventFieldInfo = (~)Gtk.Callbacks.WidgetClassDamageEventFieldCallback
    type AttrTransferType WidgetClassDamageEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassDamageEventFieldCallback)
    type AttrGetType WidgetClassDamageEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassDamageEventFieldCallback
    type AttrLabel WidgetClassDamageEventFieldInfo = "damage_event"
    type AttrOrigin WidgetClassDamageEventFieldInfo = WidgetClass
    attrGet = getWidgetClassDamageEvent
    attrSet = setWidgetClassDamageEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassDamageEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassDamageEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassDamageEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.damageEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:damageEvent"
        })

widgetClass_damageEvent :: AttrLabelProxy "damageEvent"
widgetClass_damageEvent = AttrLabelProxy

#endif


-- | Get the value of the “@grab_broken_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #grabBrokenEvent
-- @
getWidgetClassGrabBrokenEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassGrabBrokenEventFieldCallback)
getWidgetClassGrabBrokenEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 576) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassGrabBrokenEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassGrabBrokenEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@grab_broken_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #grabBrokenEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassGrabBrokenEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassGrabBrokenEventFieldCallback -> m ()
setWidgetClassGrabBrokenEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 576) (val :: FunPtr Gtk.Callbacks.C_WidgetClassGrabBrokenEventFieldCallback)

-- | Set the value of the “@grab_broken_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #grabBrokenEvent
-- @
clearWidgetClassGrabBrokenEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassGrabBrokenEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 576) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassGrabBrokenEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassGrabBrokenEventFieldInfo
instance AttrInfo WidgetClassGrabBrokenEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassGrabBrokenEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassGrabBrokenEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassGrabBrokenEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassGrabBrokenEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassGrabBrokenEventFieldInfo = (~)Gtk.Callbacks.WidgetClassGrabBrokenEventFieldCallback
    type AttrTransferType WidgetClassGrabBrokenEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassGrabBrokenEventFieldCallback)
    type AttrGetType WidgetClassGrabBrokenEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassGrabBrokenEventFieldCallback
    type AttrLabel WidgetClassGrabBrokenEventFieldInfo = "grab_broken_event"
    type AttrOrigin WidgetClassGrabBrokenEventFieldInfo = WidgetClass
    attrGet = getWidgetClassGrabBrokenEvent
    attrSet = setWidgetClassGrabBrokenEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassGrabBrokenEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassGrabBrokenEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassGrabBrokenEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.grabBrokenEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:grabBrokenEvent"
        })

widgetClass_grabBrokenEvent :: AttrLabelProxy "grabBrokenEvent"
widgetClass_grabBrokenEvent = AttrLabelProxy

#endif


-- | Get the value of the “@selection_get@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #selectionGet
-- @
getWidgetClassSelectionGet :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassSelectionGetFieldCallback)
getWidgetClassSelectionGet s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 584) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassSelectionGetFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassSelectionGetFieldCallback val'
        return val''
    return result

-- | Set the value of the “@selection_get@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #selectionGet 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassSelectionGet :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassSelectionGetFieldCallback -> m ()
setWidgetClassSelectionGet s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 584) (val :: FunPtr Gtk.Callbacks.C_WidgetClassSelectionGetFieldCallback)

-- | Set the value of the “@selection_get@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #selectionGet
-- @
clearWidgetClassSelectionGet :: MonadIO m => WidgetClass -> m ()
clearWidgetClassSelectionGet s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 584) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassSelectionGetFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassSelectionGetFieldInfo
instance AttrInfo WidgetClassSelectionGetFieldInfo where
    type AttrBaseTypeConstraint WidgetClassSelectionGetFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassSelectionGetFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassSelectionGetFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassSelectionGetFieldCallback)
    type AttrTransferTypeConstraint WidgetClassSelectionGetFieldInfo = (~)Gtk.Callbacks.WidgetClassSelectionGetFieldCallback
    type AttrTransferType WidgetClassSelectionGetFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassSelectionGetFieldCallback)
    type AttrGetType WidgetClassSelectionGetFieldInfo = Maybe Gtk.Callbacks.WidgetClassSelectionGetFieldCallback
    type AttrLabel WidgetClassSelectionGetFieldInfo = "selection_get"
    type AttrOrigin WidgetClassSelectionGetFieldInfo = WidgetClass
    attrGet = getWidgetClassSelectionGet
    attrSet = setWidgetClassSelectionGet
    attrConstruct = undefined
    attrClear = clearWidgetClassSelectionGet
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassSelectionGetFieldCallback (Gtk.Callbacks.wrap_WidgetClassSelectionGetFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.selectionGet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:selectionGet"
        })

widgetClass_selectionGet :: AttrLabelProxy "selectionGet"
widgetClass_selectionGet = AttrLabelProxy

#endif


-- | Get the value of the “@selection_received@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #selectionReceived
-- @
getWidgetClassSelectionReceived :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassSelectionReceivedFieldCallback)
getWidgetClassSelectionReceived s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 592) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassSelectionReceivedFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassSelectionReceivedFieldCallback val'
        return val''
    return result

-- | Set the value of the “@selection_received@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #selectionReceived 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassSelectionReceived :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassSelectionReceivedFieldCallback -> m ()
setWidgetClassSelectionReceived s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 592) (val :: FunPtr Gtk.Callbacks.C_WidgetClassSelectionReceivedFieldCallback)

-- | Set the value of the “@selection_received@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #selectionReceived
-- @
clearWidgetClassSelectionReceived :: MonadIO m => WidgetClass -> m ()
clearWidgetClassSelectionReceived s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 592) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassSelectionReceivedFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassSelectionReceivedFieldInfo
instance AttrInfo WidgetClassSelectionReceivedFieldInfo where
    type AttrBaseTypeConstraint WidgetClassSelectionReceivedFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassSelectionReceivedFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassSelectionReceivedFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassSelectionReceivedFieldCallback)
    type AttrTransferTypeConstraint WidgetClassSelectionReceivedFieldInfo = (~)Gtk.Callbacks.WidgetClassSelectionReceivedFieldCallback
    type AttrTransferType WidgetClassSelectionReceivedFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassSelectionReceivedFieldCallback)
    type AttrGetType WidgetClassSelectionReceivedFieldInfo = Maybe Gtk.Callbacks.WidgetClassSelectionReceivedFieldCallback
    type AttrLabel WidgetClassSelectionReceivedFieldInfo = "selection_received"
    type AttrOrigin WidgetClassSelectionReceivedFieldInfo = WidgetClass
    attrGet = getWidgetClassSelectionReceived
    attrSet = setWidgetClassSelectionReceived
    attrConstruct = undefined
    attrClear = clearWidgetClassSelectionReceived
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassSelectionReceivedFieldCallback (Gtk.Callbacks.wrap_WidgetClassSelectionReceivedFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.selectionReceived"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:selectionReceived"
        })

widgetClass_selectionReceived :: AttrLabelProxy "selectionReceived"
widgetClass_selectionReceived = AttrLabelProxy

#endif


-- | Get the value of the “@drag_begin@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #dragBegin
-- @
getWidgetClassDragBegin :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassDragBeginFieldCallback)
getWidgetClassDragBegin s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 600) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassDragBeginFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassDragBeginFieldCallback val'
        return val''
    return result

-- | Set the value of the “@drag_begin@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #dragBegin 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassDragBegin :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassDragBeginFieldCallback -> m ()
setWidgetClassDragBegin s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 600) (val :: FunPtr Gtk.Callbacks.C_WidgetClassDragBeginFieldCallback)

-- | Set the value of the “@drag_begin@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #dragBegin
-- @
clearWidgetClassDragBegin :: MonadIO m => WidgetClass -> m ()
clearWidgetClassDragBegin s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 600) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassDragBeginFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassDragBeginFieldInfo
instance AttrInfo WidgetClassDragBeginFieldInfo where
    type AttrBaseTypeConstraint WidgetClassDragBeginFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassDragBeginFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassDragBeginFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassDragBeginFieldCallback)
    type AttrTransferTypeConstraint WidgetClassDragBeginFieldInfo = (~)Gtk.Callbacks.WidgetClassDragBeginFieldCallback
    type AttrTransferType WidgetClassDragBeginFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassDragBeginFieldCallback)
    type AttrGetType WidgetClassDragBeginFieldInfo = Maybe Gtk.Callbacks.WidgetClassDragBeginFieldCallback
    type AttrLabel WidgetClassDragBeginFieldInfo = "drag_begin"
    type AttrOrigin WidgetClassDragBeginFieldInfo = WidgetClass
    attrGet = getWidgetClassDragBegin
    attrSet = setWidgetClassDragBegin
    attrConstruct = undefined
    attrClear = clearWidgetClassDragBegin
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassDragBeginFieldCallback (Gtk.Callbacks.wrap_WidgetClassDragBeginFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.dragBegin"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:dragBegin"
        })

widgetClass_dragBegin :: AttrLabelProxy "dragBegin"
widgetClass_dragBegin = AttrLabelProxy

#endif


-- | Get the value of the “@drag_end@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #dragEnd
-- @
getWidgetClassDragEnd :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassDragEndFieldCallback)
getWidgetClassDragEnd s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 608) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassDragEndFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassDragEndFieldCallback val'
        return val''
    return result

-- | Set the value of the “@drag_end@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #dragEnd 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassDragEnd :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassDragEndFieldCallback -> m ()
setWidgetClassDragEnd s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 608) (val :: FunPtr Gtk.Callbacks.C_WidgetClassDragEndFieldCallback)

-- | Set the value of the “@drag_end@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #dragEnd
-- @
clearWidgetClassDragEnd :: MonadIO m => WidgetClass -> m ()
clearWidgetClassDragEnd s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 608) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassDragEndFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassDragEndFieldInfo
instance AttrInfo WidgetClassDragEndFieldInfo where
    type AttrBaseTypeConstraint WidgetClassDragEndFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassDragEndFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassDragEndFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassDragEndFieldCallback)
    type AttrTransferTypeConstraint WidgetClassDragEndFieldInfo = (~)Gtk.Callbacks.WidgetClassDragEndFieldCallback
    type AttrTransferType WidgetClassDragEndFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassDragEndFieldCallback)
    type AttrGetType WidgetClassDragEndFieldInfo = Maybe Gtk.Callbacks.WidgetClassDragEndFieldCallback
    type AttrLabel WidgetClassDragEndFieldInfo = "drag_end"
    type AttrOrigin WidgetClassDragEndFieldInfo = WidgetClass
    attrGet = getWidgetClassDragEnd
    attrSet = setWidgetClassDragEnd
    attrConstruct = undefined
    attrClear = clearWidgetClassDragEnd
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassDragEndFieldCallback (Gtk.Callbacks.wrap_WidgetClassDragEndFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.dragEnd"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:dragEnd"
        })

widgetClass_dragEnd :: AttrLabelProxy "dragEnd"
widgetClass_dragEnd = AttrLabelProxy

#endif


-- | Get the value of the “@drag_data_get@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #dragDataGet
-- @
getWidgetClassDragDataGet :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassDragDataGetFieldCallback)
getWidgetClassDragDataGet s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 616) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassDragDataGetFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassDragDataGetFieldCallback val'
        return val''
    return result

-- | Set the value of the “@drag_data_get@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #dragDataGet 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassDragDataGet :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassDragDataGetFieldCallback -> m ()
setWidgetClassDragDataGet s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 616) (val :: FunPtr Gtk.Callbacks.C_WidgetClassDragDataGetFieldCallback)

-- | Set the value of the “@drag_data_get@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #dragDataGet
-- @
clearWidgetClassDragDataGet :: MonadIO m => WidgetClass -> m ()
clearWidgetClassDragDataGet s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 616) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassDragDataGetFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassDragDataGetFieldInfo
instance AttrInfo WidgetClassDragDataGetFieldInfo where
    type AttrBaseTypeConstraint WidgetClassDragDataGetFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassDragDataGetFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassDragDataGetFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassDragDataGetFieldCallback)
    type AttrTransferTypeConstraint WidgetClassDragDataGetFieldInfo = (~)Gtk.Callbacks.WidgetClassDragDataGetFieldCallback
    type AttrTransferType WidgetClassDragDataGetFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassDragDataGetFieldCallback)
    type AttrGetType WidgetClassDragDataGetFieldInfo = Maybe Gtk.Callbacks.WidgetClassDragDataGetFieldCallback
    type AttrLabel WidgetClassDragDataGetFieldInfo = "drag_data_get"
    type AttrOrigin WidgetClassDragDataGetFieldInfo = WidgetClass
    attrGet = getWidgetClassDragDataGet
    attrSet = setWidgetClassDragDataGet
    attrConstruct = undefined
    attrClear = clearWidgetClassDragDataGet
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassDragDataGetFieldCallback (Gtk.Callbacks.wrap_WidgetClassDragDataGetFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.dragDataGet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:dragDataGet"
        })

widgetClass_dragDataGet :: AttrLabelProxy "dragDataGet"
widgetClass_dragDataGet = AttrLabelProxy

#endif


-- | Get the value of the “@drag_data_delete@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #dragDataDelete
-- @
getWidgetClassDragDataDelete :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassDragDataDeleteFieldCallback)
getWidgetClassDragDataDelete s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 624) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassDragDataDeleteFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassDragDataDeleteFieldCallback val'
        return val''
    return result

-- | Set the value of the “@drag_data_delete@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #dragDataDelete 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassDragDataDelete :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassDragDataDeleteFieldCallback -> m ()
setWidgetClassDragDataDelete s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 624) (val :: FunPtr Gtk.Callbacks.C_WidgetClassDragDataDeleteFieldCallback)

-- | Set the value of the “@drag_data_delete@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #dragDataDelete
-- @
clearWidgetClassDragDataDelete :: MonadIO m => WidgetClass -> m ()
clearWidgetClassDragDataDelete s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 624) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassDragDataDeleteFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassDragDataDeleteFieldInfo
instance AttrInfo WidgetClassDragDataDeleteFieldInfo where
    type AttrBaseTypeConstraint WidgetClassDragDataDeleteFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassDragDataDeleteFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassDragDataDeleteFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassDragDataDeleteFieldCallback)
    type AttrTransferTypeConstraint WidgetClassDragDataDeleteFieldInfo = (~)Gtk.Callbacks.WidgetClassDragDataDeleteFieldCallback
    type AttrTransferType WidgetClassDragDataDeleteFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassDragDataDeleteFieldCallback)
    type AttrGetType WidgetClassDragDataDeleteFieldInfo = Maybe Gtk.Callbacks.WidgetClassDragDataDeleteFieldCallback
    type AttrLabel WidgetClassDragDataDeleteFieldInfo = "drag_data_delete"
    type AttrOrigin WidgetClassDragDataDeleteFieldInfo = WidgetClass
    attrGet = getWidgetClassDragDataDelete
    attrSet = setWidgetClassDragDataDelete
    attrConstruct = undefined
    attrClear = clearWidgetClassDragDataDelete
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassDragDataDeleteFieldCallback (Gtk.Callbacks.wrap_WidgetClassDragDataDeleteFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.dragDataDelete"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:dragDataDelete"
        })

widgetClass_dragDataDelete :: AttrLabelProxy "dragDataDelete"
widgetClass_dragDataDelete = AttrLabelProxy

#endif


-- | Get the value of the “@drag_leave@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #dragLeave
-- @
getWidgetClassDragLeave :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassDragLeaveFieldCallback)
getWidgetClassDragLeave s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 632) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassDragLeaveFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassDragLeaveFieldCallback val'
        return val''
    return result

-- | Set the value of the “@drag_leave@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #dragLeave 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassDragLeave :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassDragLeaveFieldCallback -> m ()
setWidgetClassDragLeave s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 632) (val :: FunPtr Gtk.Callbacks.C_WidgetClassDragLeaveFieldCallback)

-- | Set the value of the “@drag_leave@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #dragLeave
-- @
clearWidgetClassDragLeave :: MonadIO m => WidgetClass -> m ()
clearWidgetClassDragLeave s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 632) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassDragLeaveFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassDragLeaveFieldInfo
instance AttrInfo WidgetClassDragLeaveFieldInfo where
    type AttrBaseTypeConstraint WidgetClassDragLeaveFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassDragLeaveFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassDragLeaveFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassDragLeaveFieldCallback)
    type AttrTransferTypeConstraint WidgetClassDragLeaveFieldInfo = (~)Gtk.Callbacks.WidgetClassDragLeaveFieldCallback
    type AttrTransferType WidgetClassDragLeaveFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassDragLeaveFieldCallback)
    type AttrGetType WidgetClassDragLeaveFieldInfo = Maybe Gtk.Callbacks.WidgetClassDragLeaveFieldCallback
    type AttrLabel WidgetClassDragLeaveFieldInfo = "drag_leave"
    type AttrOrigin WidgetClassDragLeaveFieldInfo = WidgetClass
    attrGet = getWidgetClassDragLeave
    attrSet = setWidgetClassDragLeave
    attrConstruct = undefined
    attrClear = clearWidgetClassDragLeave
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassDragLeaveFieldCallback (Gtk.Callbacks.wrap_WidgetClassDragLeaveFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.dragLeave"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:dragLeave"
        })

widgetClass_dragLeave :: AttrLabelProxy "dragLeave"
widgetClass_dragLeave = AttrLabelProxy

#endif


-- | Get the value of the “@drag_motion@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #dragMotion
-- @
getWidgetClassDragMotion :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassDragMotionFieldCallback)
getWidgetClassDragMotion s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 640) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassDragMotionFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassDragMotionFieldCallback val'
        return val''
    return result

-- | Set the value of the “@drag_motion@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #dragMotion 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassDragMotion :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassDragMotionFieldCallback -> m ()
setWidgetClassDragMotion s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 640) (val :: FunPtr Gtk.Callbacks.C_WidgetClassDragMotionFieldCallback)

-- | Set the value of the “@drag_motion@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #dragMotion
-- @
clearWidgetClassDragMotion :: MonadIO m => WidgetClass -> m ()
clearWidgetClassDragMotion s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 640) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassDragMotionFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassDragMotionFieldInfo
instance AttrInfo WidgetClassDragMotionFieldInfo where
    type AttrBaseTypeConstraint WidgetClassDragMotionFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassDragMotionFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassDragMotionFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassDragMotionFieldCallback)
    type AttrTransferTypeConstraint WidgetClassDragMotionFieldInfo = (~)Gtk.Callbacks.WidgetClassDragMotionFieldCallback
    type AttrTransferType WidgetClassDragMotionFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassDragMotionFieldCallback)
    type AttrGetType WidgetClassDragMotionFieldInfo = Maybe Gtk.Callbacks.WidgetClassDragMotionFieldCallback
    type AttrLabel WidgetClassDragMotionFieldInfo = "drag_motion"
    type AttrOrigin WidgetClassDragMotionFieldInfo = WidgetClass
    attrGet = getWidgetClassDragMotion
    attrSet = setWidgetClassDragMotion
    attrConstruct = undefined
    attrClear = clearWidgetClassDragMotion
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassDragMotionFieldCallback (Gtk.Callbacks.wrap_WidgetClassDragMotionFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.dragMotion"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:dragMotion"
        })

widgetClass_dragMotion :: AttrLabelProxy "dragMotion"
widgetClass_dragMotion = AttrLabelProxy

#endif


-- | Get the value of the “@drag_drop@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #dragDrop
-- @
getWidgetClassDragDrop :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassDragDropFieldCallback)
getWidgetClassDragDrop s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 648) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassDragDropFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassDragDropFieldCallback val'
        return val''
    return result

-- | Set the value of the “@drag_drop@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #dragDrop 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassDragDrop :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassDragDropFieldCallback -> m ()
setWidgetClassDragDrop s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 648) (val :: FunPtr Gtk.Callbacks.C_WidgetClassDragDropFieldCallback)

-- | Set the value of the “@drag_drop@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #dragDrop
-- @
clearWidgetClassDragDrop :: MonadIO m => WidgetClass -> m ()
clearWidgetClassDragDrop s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 648) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassDragDropFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassDragDropFieldInfo
instance AttrInfo WidgetClassDragDropFieldInfo where
    type AttrBaseTypeConstraint WidgetClassDragDropFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassDragDropFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassDragDropFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassDragDropFieldCallback)
    type AttrTransferTypeConstraint WidgetClassDragDropFieldInfo = (~)Gtk.Callbacks.WidgetClassDragDropFieldCallback
    type AttrTransferType WidgetClassDragDropFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassDragDropFieldCallback)
    type AttrGetType WidgetClassDragDropFieldInfo = Maybe Gtk.Callbacks.WidgetClassDragDropFieldCallback
    type AttrLabel WidgetClassDragDropFieldInfo = "drag_drop"
    type AttrOrigin WidgetClassDragDropFieldInfo = WidgetClass
    attrGet = getWidgetClassDragDrop
    attrSet = setWidgetClassDragDrop
    attrConstruct = undefined
    attrClear = clearWidgetClassDragDrop
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassDragDropFieldCallback (Gtk.Callbacks.wrap_WidgetClassDragDropFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.dragDrop"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:dragDrop"
        })

widgetClass_dragDrop :: AttrLabelProxy "dragDrop"
widgetClass_dragDrop = AttrLabelProxy

#endif


-- | Get the value of the “@drag_data_received@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #dragDataReceived
-- @
getWidgetClassDragDataReceived :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassDragDataReceivedFieldCallback)
getWidgetClassDragDataReceived s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 656) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassDragDataReceivedFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassDragDataReceivedFieldCallback val'
        return val''
    return result

-- | Set the value of the “@drag_data_received@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #dragDataReceived 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassDragDataReceived :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassDragDataReceivedFieldCallback -> m ()
setWidgetClassDragDataReceived s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 656) (val :: FunPtr Gtk.Callbacks.C_WidgetClassDragDataReceivedFieldCallback)

-- | Set the value of the “@drag_data_received@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #dragDataReceived
-- @
clearWidgetClassDragDataReceived :: MonadIO m => WidgetClass -> m ()
clearWidgetClassDragDataReceived s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 656) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassDragDataReceivedFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassDragDataReceivedFieldInfo
instance AttrInfo WidgetClassDragDataReceivedFieldInfo where
    type AttrBaseTypeConstraint WidgetClassDragDataReceivedFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassDragDataReceivedFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassDragDataReceivedFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassDragDataReceivedFieldCallback)
    type AttrTransferTypeConstraint WidgetClassDragDataReceivedFieldInfo = (~)Gtk.Callbacks.WidgetClassDragDataReceivedFieldCallback
    type AttrTransferType WidgetClassDragDataReceivedFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassDragDataReceivedFieldCallback)
    type AttrGetType WidgetClassDragDataReceivedFieldInfo = Maybe Gtk.Callbacks.WidgetClassDragDataReceivedFieldCallback
    type AttrLabel WidgetClassDragDataReceivedFieldInfo = "drag_data_received"
    type AttrOrigin WidgetClassDragDataReceivedFieldInfo = WidgetClass
    attrGet = getWidgetClassDragDataReceived
    attrSet = setWidgetClassDragDataReceived
    attrConstruct = undefined
    attrClear = clearWidgetClassDragDataReceived
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassDragDataReceivedFieldCallback (Gtk.Callbacks.wrap_WidgetClassDragDataReceivedFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.dragDataReceived"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:dragDataReceived"
        })

widgetClass_dragDataReceived :: AttrLabelProxy "dragDataReceived"
widgetClass_dragDataReceived = AttrLabelProxy

#endif


-- | Get the value of the “@drag_failed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #dragFailed
-- @
getWidgetClassDragFailed :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassDragFailedFieldCallback)
getWidgetClassDragFailed s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 664) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassDragFailedFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassDragFailedFieldCallback val'
        return val''
    return result

-- | Set the value of the “@drag_failed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #dragFailed 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassDragFailed :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassDragFailedFieldCallback -> m ()
setWidgetClassDragFailed s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 664) (val :: FunPtr Gtk.Callbacks.C_WidgetClassDragFailedFieldCallback)

-- | Set the value of the “@drag_failed@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #dragFailed
-- @
clearWidgetClassDragFailed :: MonadIO m => WidgetClass -> m ()
clearWidgetClassDragFailed s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 664) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassDragFailedFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassDragFailedFieldInfo
instance AttrInfo WidgetClassDragFailedFieldInfo where
    type AttrBaseTypeConstraint WidgetClassDragFailedFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassDragFailedFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassDragFailedFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassDragFailedFieldCallback)
    type AttrTransferTypeConstraint WidgetClassDragFailedFieldInfo = (~)Gtk.Callbacks.WidgetClassDragFailedFieldCallback
    type AttrTransferType WidgetClassDragFailedFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassDragFailedFieldCallback)
    type AttrGetType WidgetClassDragFailedFieldInfo = Maybe Gtk.Callbacks.WidgetClassDragFailedFieldCallback
    type AttrLabel WidgetClassDragFailedFieldInfo = "drag_failed"
    type AttrOrigin WidgetClassDragFailedFieldInfo = WidgetClass
    attrGet = getWidgetClassDragFailed
    attrSet = setWidgetClassDragFailed
    attrConstruct = undefined
    attrClear = clearWidgetClassDragFailed
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassDragFailedFieldCallback (Gtk.Callbacks.wrap_WidgetClassDragFailedFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.dragFailed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:dragFailed"
        })

widgetClass_dragFailed :: AttrLabelProxy "dragFailed"
widgetClass_dragFailed = AttrLabelProxy

#endif


-- | Get the value of the “@popup_menu@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #popupMenu
-- @
getWidgetClassPopupMenu :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassPopupMenuFieldCallback)
getWidgetClassPopupMenu s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 672) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassPopupMenuFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassPopupMenuFieldCallback val'
        return val''
    return result

-- | Set the value of the “@popup_menu@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #popupMenu 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassPopupMenu :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassPopupMenuFieldCallback -> m ()
setWidgetClassPopupMenu s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 672) (val :: FunPtr Gtk.Callbacks.C_WidgetClassPopupMenuFieldCallback)

-- | Set the value of the “@popup_menu@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #popupMenu
-- @
clearWidgetClassPopupMenu :: MonadIO m => WidgetClass -> m ()
clearWidgetClassPopupMenu s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 672) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassPopupMenuFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassPopupMenuFieldInfo
instance AttrInfo WidgetClassPopupMenuFieldInfo where
    type AttrBaseTypeConstraint WidgetClassPopupMenuFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassPopupMenuFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassPopupMenuFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassPopupMenuFieldCallback)
    type AttrTransferTypeConstraint WidgetClassPopupMenuFieldInfo = (~)Gtk.Callbacks.WidgetClassPopupMenuFieldCallback
    type AttrTransferType WidgetClassPopupMenuFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassPopupMenuFieldCallback)
    type AttrGetType WidgetClassPopupMenuFieldInfo = Maybe Gtk.Callbacks.WidgetClassPopupMenuFieldCallback
    type AttrLabel WidgetClassPopupMenuFieldInfo = "popup_menu"
    type AttrOrigin WidgetClassPopupMenuFieldInfo = WidgetClass
    attrGet = getWidgetClassPopupMenu
    attrSet = setWidgetClassPopupMenu
    attrConstruct = undefined
    attrClear = clearWidgetClassPopupMenu
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassPopupMenuFieldCallback (Gtk.Callbacks.wrap_WidgetClassPopupMenuFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.popupMenu"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:popupMenu"
        })

widgetClass_popupMenu :: AttrLabelProxy "popupMenu"
widgetClass_popupMenu = AttrLabelProxy

#endif


-- | Get the value of the “@show_help@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #showHelp
-- @
getWidgetClassShowHelp :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassShowHelpFieldCallback)
getWidgetClassShowHelp s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 680) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassShowHelpFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassShowHelpFieldCallback val'
        return val''
    return result

-- | Set the value of the “@show_help@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #showHelp 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassShowHelp :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassShowHelpFieldCallback -> m ()
setWidgetClassShowHelp s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 680) (val :: FunPtr Gtk.Callbacks.C_WidgetClassShowHelpFieldCallback)

-- | Set the value of the “@show_help@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #showHelp
-- @
clearWidgetClassShowHelp :: MonadIO m => WidgetClass -> m ()
clearWidgetClassShowHelp s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 680) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassShowHelpFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassShowHelpFieldInfo
instance AttrInfo WidgetClassShowHelpFieldInfo where
    type AttrBaseTypeConstraint WidgetClassShowHelpFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassShowHelpFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassShowHelpFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassShowHelpFieldCallback)
    type AttrTransferTypeConstraint WidgetClassShowHelpFieldInfo = (~)Gtk.Callbacks.WidgetClassShowHelpFieldCallback
    type AttrTransferType WidgetClassShowHelpFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassShowHelpFieldCallback)
    type AttrGetType WidgetClassShowHelpFieldInfo = Maybe Gtk.Callbacks.WidgetClassShowHelpFieldCallback
    type AttrLabel WidgetClassShowHelpFieldInfo = "show_help"
    type AttrOrigin WidgetClassShowHelpFieldInfo = WidgetClass
    attrGet = getWidgetClassShowHelp
    attrSet = setWidgetClassShowHelp
    attrConstruct = undefined
    attrClear = clearWidgetClassShowHelp
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassShowHelpFieldCallback (Gtk.Callbacks.wrap_WidgetClassShowHelpFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.showHelp"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:showHelp"
        })

widgetClass_showHelp :: AttrLabelProxy "showHelp"
widgetClass_showHelp = AttrLabelProxy

#endif


-- | Get the value of the “@get_accessible@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #getAccessible
-- @
getWidgetClassGetAccessible :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassGetAccessibleFieldCallback)
getWidgetClassGetAccessible s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 688) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassGetAccessibleFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassGetAccessibleFieldCallback val'
        return val''
    return result

-- | Set the value of the “@get_accessible@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #getAccessible 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassGetAccessible :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassGetAccessibleFieldCallback -> m ()
setWidgetClassGetAccessible s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 688) (val :: FunPtr Gtk.Callbacks.C_WidgetClassGetAccessibleFieldCallback)

-- | Set the value of the “@get_accessible@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #getAccessible
-- @
clearWidgetClassGetAccessible :: MonadIO m => WidgetClass -> m ()
clearWidgetClassGetAccessible s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 688) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassGetAccessibleFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassGetAccessibleFieldInfo
instance AttrInfo WidgetClassGetAccessibleFieldInfo where
    type AttrBaseTypeConstraint WidgetClassGetAccessibleFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassGetAccessibleFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassGetAccessibleFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassGetAccessibleFieldCallback)
    type AttrTransferTypeConstraint WidgetClassGetAccessibleFieldInfo = (~)Gtk.Callbacks.WidgetClassGetAccessibleFieldCallback
    type AttrTransferType WidgetClassGetAccessibleFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassGetAccessibleFieldCallback)
    type AttrGetType WidgetClassGetAccessibleFieldInfo = Maybe Gtk.Callbacks.WidgetClassGetAccessibleFieldCallback
    type AttrLabel WidgetClassGetAccessibleFieldInfo = "get_accessible"
    type AttrOrigin WidgetClassGetAccessibleFieldInfo = WidgetClass
    attrGet = getWidgetClassGetAccessible
    attrSet = setWidgetClassGetAccessible
    attrConstruct = undefined
    attrClear = clearWidgetClassGetAccessible
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassGetAccessibleFieldCallback (Gtk.Callbacks.wrap_WidgetClassGetAccessibleFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.getAccessible"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:getAccessible"
        })

widgetClass_getAccessible :: AttrLabelProxy "getAccessible"
widgetClass_getAccessible = AttrLabelProxy

#endif


-- | Get the value of the “@screen_changed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #screenChanged
-- @
getWidgetClassScreenChanged :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassScreenChangedFieldCallback)
getWidgetClassScreenChanged s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 696) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassScreenChangedFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassScreenChangedFieldCallback val'
        return val''
    return result

-- | Set the value of the “@screen_changed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #screenChanged 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassScreenChanged :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassScreenChangedFieldCallback -> m ()
setWidgetClassScreenChanged s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 696) (val :: FunPtr Gtk.Callbacks.C_WidgetClassScreenChangedFieldCallback)

-- | Set the value of the “@screen_changed@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #screenChanged
-- @
clearWidgetClassScreenChanged :: MonadIO m => WidgetClass -> m ()
clearWidgetClassScreenChanged s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 696) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassScreenChangedFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassScreenChangedFieldInfo
instance AttrInfo WidgetClassScreenChangedFieldInfo where
    type AttrBaseTypeConstraint WidgetClassScreenChangedFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassScreenChangedFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassScreenChangedFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassScreenChangedFieldCallback)
    type AttrTransferTypeConstraint WidgetClassScreenChangedFieldInfo = (~)Gtk.Callbacks.WidgetClassScreenChangedFieldCallback
    type AttrTransferType WidgetClassScreenChangedFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassScreenChangedFieldCallback)
    type AttrGetType WidgetClassScreenChangedFieldInfo = Maybe Gtk.Callbacks.WidgetClassScreenChangedFieldCallback
    type AttrLabel WidgetClassScreenChangedFieldInfo = "screen_changed"
    type AttrOrigin WidgetClassScreenChangedFieldInfo = WidgetClass
    attrGet = getWidgetClassScreenChanged
    attrSet = setWidgetClassScreenChanged
    attrConstruct = undefined
    attrClear = clearWidgetClassScreenChanged
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassScreenChangedFieldCallback (Gtk.Callbacks.wrap_WidgetClassScreenChangedFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.screenChanged"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:screenChanged"
        })

widgetClass_screenChanged :: AttrLabelProxy "screenChanged"
widgetClass_screenChanged = AttrLabelProxy

#endif


-- | Get the value of the “@can_activate_accel@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #canActivateAccel
-- @
getWidgetClassCanActivateAccel :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassCanActivateAccelFieldCallback)
getWidgetClassCanActivateAccel s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 704) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassCanActivateAccelFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassCanActivateAccelFieldCallback val'
        return val''
    return result

-- | Set the value of the “@can_activate_accel@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #canActivateAccel 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassCanActivateAccel :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassCanActivateAccelFieldCallback -> m ()
setWidgetClassCanActivateAccel s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 704) (val :: FunPtr Gtk.Callbacks.C_WidgetClassCanActivateAccelFieldCallback)

-- | Set the value of the “@can_activate_accel@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #canActivateAccel
-- @
clearWidgetClassCanActivateAccel :: MonadIO m => WidgetClass -> m ()
clearWidgetClassCanActivateAccel s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 704) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassCanActivateAccelFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassCanActivateAccelFieldInfo
instance AttrInfo WidgetClassCanActivateAccelFieldInfo where
    type AttrBaseTypeConstraint WidgetClassCanActivateAccelFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassCanActivateAccelFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassCanActivateAccelFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassCanActivateAccelFieldCallback)
    type AttrTransferTypeConstraint WidgetClassCanActivateAccelFieldInfo = (~)Gtk.Callbacks.WidgetClassCanActivateAccelFieldCallback
    type AttrTransferType WidgetClassCanActivateAccelFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassCanActivateAccelFieldCallback)
    type AttrGetType WidgetClassCanActivateAccelFieldInfo = Maybe Gtk.Callbacks.WidgetClassCanActivateAccelFieldCallback
    type AttrLabel WidgetClassCanActivateAccelFieldInfo = "can_activate_accel"
    type AttrOrigin WidgetClassCanActivateAccelFieldInfo = WidgetClass
    attrGet = getWidgetClassCanActivateAccel
    attrSet = setWidgetClassCanActivateAccel
    attrConstruct = undefined
    attrClear = clearWidgetClassCanActivateAccel
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassCanActivateAccelFieldCallback (Gtk.Callbacks.wrap_WidgetClassCanActivateAccelFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.canActivateAccel"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:canActivateAccel"
        })

widgetClass_canActivateAccel :: AttrLabelProxy "canActivateAccel"
widgetClass_canActivateAccel = AttrLabelProxy

#endif


-- | Get the value of the “@composited_changed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #compositedChanged
-- @
getWidgetClassCompositedChanged :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassCompositedChangedFieldCallback)
getWidgetClassCompositedChanged s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 712) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassCompositedChangedFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassCompositedChangedFieldCallback val'
        return val''
    return result

-- | Set the value of the “@composited_changed@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #compositedChanged 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassCompositedChanged :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassCompositedChangedFieldCallback -> m ()
setWidgetClassCompositedChanged s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 712) (val :: FunPtr Gtk.Callbacks.C_WidgetClassCompositedChangedFieldCallback)

-- | Set the value of the “@composited_changed@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #compositedChanged
-- @
clearWidgetClassCompositedChanged :: MonadIO m => WidgetClass -> m ()
clearWidgetClassCompositedChanged s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 712) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassCompositedChangedFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassCompositedChangedFieldInfo
instance AttrInfo WidgetClassCompositedChangedFieldInfo where
    type AttrBaseTypeConstraint WidgetClassCompositedChangedFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassCompositedChangedFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassCompositedChangedFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassCompositedChangedFieldCallback)
    type AttrTransferTypeConstraint WidgetClassCompositedChangedFieldInfo = (~)Gtk.Callbacks.WidgetClassCompositedChangedFieldCallback
    type AttrTransferType WidgetClassCompositedChangedFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassCompositedChangedFieldCallback)
    type AttrGetType WidgetClassCompositedChangedFieldInfo = Maybe Gtk.Callbacks.WidgetClassCompositedChangedFieldCallback
    type AttrLabel WidgetClassCompositedChangedFieldInfo = "composited_changed"
    type AttrOrigin WidgetClassCompositedChangedFieldInfo = WidgetClass
    attrGet = getWidgetClassCompositedChanged
    attrSet = setWidgetClassCompositedChanged
    attrConstruct = undefined
    attrClear = clearWidgetClassCompositedChanged
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassCompositedChangedFieldCallback (Gtk.Callbacks.wrap_WidgetClassCompositedChangedFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.compositedChanged"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:compositedChanged"
        })

widgetClass_compositedChanged :: AttrLabelProxy "compositedChanged"
widgetClass_compositedChanged = AttrLabelProxy

#endif


-- | Get the value of the “@query_tooltip@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #queryTooltip
-- @
getWidgetClassQueryTooltip :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassQueryTooltipFieldCallback)
getWidgetClassQueryTooltip s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 720) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassQueryTooltipFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassQueryTooltipFieldCallback val'
        return val''
    return result

-- | Set the value of the “@query_tooltip@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #queryTooltip 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassQueryTooltip :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassQueryTooltipFieldCallback -> m ()
setWidgetClassQueryTooltip s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 720) (val :: FunPtr Gtk.Callbacks.C_WidgetClassQueryTooltipFieldCallback)

-- | Set the value of the “@query_tooltip@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #queryTooltip
-- @
clearWidgetClassQueryTooltip :: MonadIO m => WidgetClass -> m ()
clearWidgetClassQueryTooltip s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 720) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassQueryTooltipFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassQueryTooltipFieldInfo
instance AttrInfo WidgetClassQueryTooltipFieldInfo where
    type AttrBaseTypeConstraint WidgetClassQueryTooltipFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassQueryTooltipFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassQueryTooltipFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassQueryTooltipFieldCallback)
    type AttrTransferTypeConstraint WidgetClassQueryTooltipFieldInfo = (~)Gtk.Callbacks.WidgetClassQueryTooltipFieldCallback
    type AttrTransferType WidgetClassQueryTooltipFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassQueryTooltipFieldCallback)
    type AttrGetType WidgetClassQueryTooltipFieldInfo = Maybe Gtk.Callbacks.WidgetClassQueryTooltipFieldCallback
    type AttrLabel WidgetClassQueryTooltipFieldInfo = "query_tooltip"
    type AttrOrigin WidgetClassQueryTooltipFieldInfo = WidgetClass
    attrGet = getWidgetClassQueryTooltip
    attrSet = setWidgetClassQueryTooltip
    attrConstruct = undefined
    attrClear = clearWidgetClassQueryTooltip
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassQueryTooltipFieldCallback (Gtk.Callbacks.wrap_WidgetClassQueryTooltipFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.queryTooltip"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:queryTooltip"
        })

widgetClass_queryTooltip :: AttrLabelProxy "queryTooltip"
widgetClass_queryTooltip = AttrLabelProxy

#endif


-- | Get the value of the “@compute_expand@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #computeExpand
-- @
getWidgetClassComputeExpand :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassComputeExpandFieldCallback)
getWidgetClassComputeExpand s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 728) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassComputeExpandFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassComputeExpandFieldCallback val'
        return val''
    return result

-- | Set the value of the “@compute_expand@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #computeExpand 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassComputeExpand :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassComputeExpandFieldCallback -> m ()
setWidgetClassComputeExpand s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 728) (val :: FunPtr Gtk.Callbacks.C_WidgetClassComputeExpandFieldCallback)

-- | Set the value of the “@compute_expand@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #computeExpand
-- @
clearWidgetClassComputeExpand :: MonadIO m => WidgetClass -> m ()
clearWidgetClassComputeExpand s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 728) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassComputeExpandFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassComputeExpandFieldInfo
instance AttrInfo WidgetClassComputeExpandFieldInfo where
    type AttrBaseTypeConstraint WidgetClassComputeExpandFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassComputeExpandFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassComputeExpandFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassComputeExpandFieldCallback)
    type AttrTransferTypeConstraint WidgetClassComputeExpandFieldInfo = (~)Gtk.Callbacks.WidgetClassComputeExpandFieldCallback
    type AttrTransferType WidgetClassComputeExpandFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassComputeExpandFieldCallback)
    type AttrGetType WidgetClassComputeExpandFieldInfo = Maybe Gtk.Callbacks.WidgetClassComputeExpandFieldCallback
    type AttrLabel WidgetClassComputeExpandFieldInfo = "compute_expand"
    type AttrOrigin WidgetClassComputeExpandFieldInfo = WidgetClass
    attrGet = getWidgetClassComputeExpand
    attrSet = setWidgetClassComputeExpand
    attrConstruct = undefined
    attrClear = clearWidgetClassComputeExpand
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassComputeExpandFieldCallback (Gtk.Callbacks.wrap_WidgetClassComputeExpandFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.computeExpand"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:computeExpand"
        })

widgetClass_computeExpand :: AttrLabelProxy "computeExpand"
widgetClass_computeExpand = AttrLabelProxy

#endif


-- | Get the value of the “@adjust_size_request@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #adjustSizeRequest
-- @
getWidgetClassAdjustSizeRequest :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassAdjustSizeRequestFieldCallback)
getWidgetClassAdjustSizeRequest s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 736) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassAdjustSizeRequestFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassAdjustSizeRequestFieldCallback val'
        return val''
    return result

-- | Set the value of the “@adjust_size_request@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #adjustSizeRequest 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassAdjustSizeRequest :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassAdjustSizeRequestFieldCallback -> m ()
setWidgetClassAdjustSizeRequest s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 736) (val :: FunPtr Gtk.Callbacks.C_WidgetClassAdjustSizeRequestFieldCallback)

-- | Set the value of the “@adjust_size_request@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #adjustSizeRequest
-- @
clearWidgetClassAdjustSizeRequest :: MonadIO m => WidgetClass -> m ()
clearWidgetClassAdjustSizeRequest s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 736) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassAdjustSizeRequestFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassAdjustSizeRequestFieldInfo
instance AttrInfo WidgetClassAdjustSizeRequestFieldInfo where
    type AttrBaseTypeConstraint WidgetClassAdjustSizeRequestFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassAdjustSizeRequestFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassAdjustSizeRequestFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassAdjustSizeRequestFieldCallback)
    type AttrTransferTypeConstraint WidgetClassAdjustSizeRequestFieldInfo = (~)Gtk.Callbacks.WidgetClassAdjustSizeRequestFieldCallback
    type AttrTransferType WidgetClassAdjustSizeRequestFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassAdjustSizeRequestFieldCallback)
    type AttrGetType WidgetClassAdjustSizeRequestFieldInfo = Maybe Gtk.Callbacks.WidgetClassAdjustSizeRequestFieldCallback
    type AttrLabel WidgetClassAdjustSizeRequestFieldInfo = "adjust_size_request"
    type AttrOrigin WidgetClassAdjustSizeRequestFieldInfo = WidgetClass
    attrGet = getWidgetClassAdjustSizeRequest
    attrSet = setWidgetClassAdjustSizeRequest
    attrConstruct = undefined
    attrClear = clearWidgetClassAdjustSizeRequest
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassAdjustSizeRequestFieldCallback (Gtk.Callbacks.wrap_WidgetClassAdjustSizeRequestFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.adjustSizeRequest"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:adjustSizeRequest"
        })

widgetClass_adjustSizeRequest :: AttrLabelProxy "adjustSizeRequest"
widgetClass_adjustSizeRequest = AttrLabelProxy

#endif


-- | Get the value of the “@adjust_size_allocation@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #adjustSizeAllocation
-- @
getWidgetClassAdjustSizeAllocation :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassAdjustSizeAllocationFieldCallback)
getWidgetClassAdjustSizeAllocation s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 744) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassAdjustSizeAllocationFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassAdjustSizeAllocationFieldCallback val'
        return val''
    return result

-- | Set the value of the “@adjust_size_allocation@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #adjustSizeAllocation 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassAdjustSizeAllocation :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassAdjustSizeAllocationFieldCallback -> m ()
setWidgetClassAdjustSizeAllocation s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 744) (val :: FunPtr Gtk.Callbacks.C_WidgetClassAdjustSizeAllocationFieldCallback)

-- | Set the value of the “@adjust_size_allocation@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #adjustSizeAllocation
-- @
clearWidgetClassAdjustSizeAllocation :: MonadIO m => WidgetClass -> m ()
clearWidgetClassAdjustSizeAllocation s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 744) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassAdjustSizeAllocationFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassAdjustSizeAllocationFieldInfo
instance AttrInfo WidgetClassAdjustSizeAllocationFieldInfo where
    type AttrBaseTypeConstraint WidgetClassAdjustSizeAllocationFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassAdjustSizeAllocationFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassAdjustSizeAllocationFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassAdjustSizeAllocationFieldCallback)
    type AttrTransferTypeConstraint WidgetClassAdjustSizeAllocationFieldInfo = (~)Gtk.Callbacks.WidgetClassAdjustSizeAllocationFieldCallback
    type AttrTransferType WidgetClassAdjustSizeAllocationFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassAdjustSizeAllocationFieldCallback)
    type AttrGetType WidgetClassAdjustSizeAllocationFieldInfo = Maybe Gtk.Callbacks.WidgetClassAdjustSizeAllocationFieldCallback
    type AttrLabel WidgetClassAdjustSizeAllocationFieldInfo = "adjust_size_allocation"
    type AttrOrigin WidgetClassAdjustSizeAllocationFieldInfo = WidgetClass
    attrGet = getWidgetClassAdjustSizeAllocation
    attrSet = setWidgetClassAdjustSizeAllocation
    attrConstruct = undefined
    attrClear = clearWidgetClassAdjustSizeAllocation
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassAdjustSizeAllocationFieldCallback (Gtk.Callbacks.wrap_WidgetClassAdjustSizeAllocationFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.adjustSizeAllocation"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:adjustSizeAllocation"
        })

widgetClass_adjustSizeAllocation :: AttrLabelProxy "adjustSizeAllocation"
widgetClass_adjustSizeAllocation = AttrLabelProxy

#endif


-- | Get the value of the “@style_updated@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #styleUpdated
-- @
getWidgetClassStyleUpdated :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassStyleUpdatedFieldCallback)
getWidgetClassStyleUpdated s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 752) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassStyleUpdatedFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassStyleUpdatedFieldCallback val'
        return val''
    return result

-- | Set the value of the “@style_updated@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #styleUpdated 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassStyleUpdated :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassStyleUpdatedFieldCallback -> m ()
setWidgetClassStyleUpdated s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 752) (val :: FunPtr Gtk.Callbacks.C_WidgetClassStyleUpdatedFieldCallback)

-- | Set the value of the “@style_updated@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #styleUpdated
-- @
clearWidgetClassStyleUpdated :: MonadIO m => WidgetClass -> m ()
clearWidgetClassStyleUpdated s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 752) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassStyleUpdatedFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassStyleUpdatedFieldInfo
instance AttrInfo WidgetClassStyleUpdatedFieldInfo where
    type AttrBaseTypeConstraint WidgetClassStyleUpdatedFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassStyleUpdatedFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassStyleUpdatedFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassStyleUpdatedFieldCallback)
    type AttrTransferTypeConstraint WidgetClassStyleUpdatedFieldInfo = (~)Gtk.Callbacks.WidgetClassStyleUpdatedFieldCallback
    type AttrTransferType WidgetClassStyleUpdatedFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassStyleUpdatedFieldCallback)
    type AttrGetType WidgetClassStyleUpdatedFieldInfo = Maybe Gtk.Callbacks.WidgetClassStyleUpdatedFieldCallback
    type AttrLabel WidgetClassStyleUpdatedFieldInfo = "style_updated"
    type AttrOrigin WidgetClassStyleUpdatedFieldInfo = WidgetClass
    attrGet = getWidgetClassStyleUpdated
    attrSet = setWidgetClassStyleUpdated
    attrConstruct = undefined
    attrClear = clearWidgetClassStyleUpdated
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassStyleUpdatedFieldCallback (Gtk.Callbacks.wrap_WidgetClassStyleUpdatedFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.styleUpdated"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:styleUpdated"
        })

widgetClass_styleUpdated :: AttrLabelProxy "styleUpdated"
widgetClass_styleUpdated = AttrLabelProxy

#endif


-- | Get the value of the “@touch_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #touchEvent
-- @
getWidgetClassTouchEvent :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassTouchEventFieldCallback)
getWidgetClassTouchEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 760) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassTouchEventFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassTouchEventFieldCallback val'
        return val''
    return result

-- | Set the value of the “@touch_event@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #touchEvent 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassTouchEvent :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassTouchEventFieldCallback -> m ()
setWidgetClassTouchEvent s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 760) (val :: FunPtr Gtk.Callbacks.C_WidgetClassTouchEventFieldCallback)

-- | Set the value of the “@touch_event@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #touchEvent
-- @
clearWidgetClassTouchEvent :: MonadIO m => WidgetClass -> m ()
clearWidgetClassTouchEvent s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 760) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassTouchEventFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassTouchEventFieldInfo
instance AttrInfo WidgetClassTouchEventFieldInfo where
    type AttrBaseTypeConstraint WidgetClassTouchEventFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassTouchEventFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassTouchEventFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassTouchEventFieldCallback)
    type AttrTransferTypeConstraint WidgetClassTouchEventFieldInfo = (~)Gtk.Callbacks.WidgetClassTouchEventFieldCallback
    type AttrTransferType WidgetClassTouchEventFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassTouchEventFieldCallback)
    type AttrGetType WidgetClassTouchEventFieldInfo = Maybe Gtk.Callbacks.WidgetClassTouchEventFieldCallback
    type AttrLabel WidgetClassTouchEventFieldInfo = "touch_event"
    type AttrOrigin WidgetClassTouchEventFieldInfo = WidgetClass
    attrGet = getWidgetClassTouchEvent
    attrSet = setWidgetClassTouchEvent
    attrConstruct = undefined
    attrClear = clearWidgetClassTouchEvent
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassTouchEventFieldCallback (Gtk.Callbacks.wrap_WidgetClassTouchEventFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.touchEvent"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:touchEvent"
        })

widgetClass_touchEvent :: AttrLabelProxy "touchEvent"
widgetClass_touchEvent = AttrLabelProxy

#endif


-- | Get the value of the “@get_preferred_height_and_baseline_for_width@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #getPreferredHeightAndBaselineForWidth
-- @
getWidgetClassGetPreferredHeightAndBaselineForWidth :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassGetPreferredHeightAndBaselineForWidthFieldCallback)
getWidgetClassGetPreferredHeightAndBaselineForWidth s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 768) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightAndBaselineForWidthFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassGetPreferredHeightAndBaselineForWidthFieldCallback val'
        return val''
    return result

-- | Set the value of the “@get_preferred_height_and_baseline_for_width@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #getPreferredHeightAndBaselineForWidth 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassGetPreferredHeightAndBaselineForWidth :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightAndBaselineForWidthFieldCallback -> m ()
setWidgetClassGetPreferredHeightAndBaselineForWidth s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 768) (val :: FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightAndBaselineForWidthFieldCallback)

-- | Set the value of the “@get_preferred_height_and_baseline_for_width@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #getPreferredHeightAndBaselineForWidth
-- @
clearWidgetClassGetPreferredHeightAndBaselineForWidth :: MonadIO m => WidgetClass -> m ()
clearWidgetClassGetPreferredHeightAndBaselineForWidth s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 768) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightAndBaselineForWidthFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassGetPreferredHeightAndBaselineForWidthFieldInfo
instance AttrInfo WidgetClassGetPreferredHeightAndBaselineForWidthFieldInfo where
    type AttrBaseTypeConstraint WidgetClassGetPreferredHeightAndBaselineForWidthFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassGetPreferredHeightAndBaselineForWidthFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassGetPreferredHeightAndBaselineForWidthFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightAndBaselineForWidthFieldCallback)
    type AttrTransferTypeConstraint WidgetClassGetPreferredHeightAndBaselineForWidthFieldInfo = (~)Gtk.Callbacks.WidgetClassGetPreferredHeightAndBaselineForWidthFieldCallback
    type AttrTransferType WidgetClassGetPreferredHeightAndBaselineForWidthFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassGetPreferredHeightAndBaselineForWidthFieldCallback)
    type AttrGetType WidgetClassGetPreferredHeightAndBaselineForWidthFieldInfo = Maybe Gtk.Callbacks.WidgetClassGetPreferredHeightAndBaselineForWidthFieldCallback
    type AttrLabel WidgetClassGetPreferredHeightAndBaselineForWidthFieldInfo = "get_preferred_height_and_baseline_for_width"
    type AttrOrigin WidgetClassGetPreferredHeightAndBaselineForWidthFieldInfo = WidgetClass
    attrGet = getWidgetClassGetPreferredHeightAndBaselineForWidth
    attrSet = setWidgetClassGetPreferredHeightAndBaselineForWidth
    attrConstruct = undefined
    attrClear = clearWidgetClassGetPreferredHeightAndBaselineForWidth
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassGetPreferredHeightAndBaselineForWidthFieldCallback (Gtk.Callbacks.wrap_WidgetClassGetPreferredHeightAndBaselineForWidthFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.getPreferredHeightAndBaselineForWidth"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:getPreferredHeightAndBaselineForWidth"
        })

widgetClass_getPreferredHeightAndBaselineForWidth :: AttrLabelProxy "getPreferredHeightAndBaselineForWidth"
widgetClass_getPreferredHeightAndBaselineForWidth = AttrLabelProxy

#endif


-- | Get the value of the “@adjust_baseline_request@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #adjustBaselineRequest
-- @
getWidgetClassAdjustBaselineRequest :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassAdjustBaselineRequestFieldCallback)
getWidgetClassAdjustBaselineRequest s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 776) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassAdjustBaselineRequestFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassAdjustBaselineRequestFieldCallback val'
        return val''
    return result

-- | Set the value of the “@adjust_baseline_request@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #adjustBaselineRequest 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassAdjustBaselineRequest :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassAdjustBaselineRequestFieldCallback -> m ()
setWidgetClassAdjustBaselineRequest s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 776) (val :: FunPtr Gtk.Callbacks.C_WidgetClassAdjustBaselineRequestFieldCallback)

-- | Set the value of the “@adjust_baseline_request@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #adjustBaselineRequest
-- @
clearWidgetClassAdjustBaselineRequest :: MonadIO m => WidgetClass -> m ()
clearWidgetClassAdjustBaselineRequest s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 776) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassAdjustBaselineRequestFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassAdjustBaselineRequestFieldInfo
instance AttrInfo WidgetClassAdjustBaselineRequestFieldInfo where
    type AttrBaseTypeConstraint WidgetClassAdjustBaselineRequestFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassAdjustBaselineRequestFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassAdjustBaselineRequestFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassAdjustBaselineRequestFieldCallback)
    type AttrTransferTypeConstraint WidgetClassAdjustBaselineRequestFieldInfo = (~)Gtk.Callbacks.WidgetClassAdjustBaselineRequestFieldCallback
    type AttrTransferType WidgetClassAdjustBaselineRequestFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassAdjustBaselineRequestFieldCallback)
    type AttrGetType WidgetClassAdjustBaselineRequestFieldInfo = Maybe Gtk.Callbacks.WidgetClassAdjustBaselineRequestFieldCallback
    type AttrLabel WidgetClassAdjustBaselineRequestFieldInfo = "adjust_baseline_request"
    type AttrOrigin WidgetClassAdjustBaselineRequestFieldInfo = WidgetClass
    attrGet = getWidgetClassAdjustBaselineRequest
    attrSet = setWidgetClassAdjustBaselineRequest
    attrConstruct = undefined
    attrClear = clearWidgetClassAdjustBaselineRequest
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassAdjustBaselineRequestFieldCallback (Gtk.Callbacks.wrap_WidgetClassAdjustBaselineRequestFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.adjustBaselineRequest"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:adjustBaselineRequest"
        })

widgetClass_adjustBaselineRequest :: AttrLabelProxy "adjustBaselineRequest"
widgetClass_adjustBaselineRequest = AttrLabelProxy

#endif


-- | Get the value of the “@adjust_baseline_allocation@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #adjustBaselineAllocation
-- @
getWidgetClassAdjustBaselineAllocation :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassAdjustBaselineAllocationFieldCallback)
getWidgetClassAdjustBaselineAllocation s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 784) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassAdjustBaselineAllocationFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassAdjustBaselineAllocationFieldCallback val'
        return val''
    return result

-- | Set the value of the “@adjust_baseline_allocation@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #adjustBaselineAllocation 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassAdjustBaselineAllocation :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassAdjustBaselineAllocationFieldCallback -> m ()
setWidgetClassAdjustBaselineAllocation s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 784) (val :: FunPtr Gtk.Callbacks.C_WidgetClassAdjustBaselineAllocationFieldCallback)

-- | Set the value of the “@adjust_baseline_allocation@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #adjustBaselineAllocation
-- @
clearWidgetClassAdjustBaselineAllocation :: MonadIO m => WidgetClass -> m ()
clearWidgetClassAdjustBaselineAllocation s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 784) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassAdjustBaselineAllocationFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassAdjustBaselineAllocationFieldInfo
instance AttrInfo WidgetClassAdjustBaselineAllocationFieldInfo where
    type AttrBaseTypeConstraint WidgetClassAdjustBaselineAllocationFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassAdjustBaselineAllocationFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassAdjustBaselineAllocationFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassAdjustBaselineAllocationFieldCallback)
    type AttrTransferTypeConstraint WidgetClassAdjustBaselineAllocationFieldInfo = (~)Gtk.Callbacks.WidgetClassAdjustBaselineAllocationFieldCallback
    type AttrTransferType WidgetClassAdjustBaselineAllocationFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassAdjustBaselineAllocationFieldCallback)
    type AttrGetType WidgetClassAdjustBaselineAllocationFieldInfo = Maybe Gtk.Callbacks.WidgetClassAdjustBaselineAllocationFieldCallback
    type AttrLabel WidgetClassAdjustBaselineAllocationFieldInfo = "adjust_baseline_allocation"
    type AttrOrigin WidgetClassAdjustBaselineAllocationFieldInfo = WidgetClass
    attrGet = getWidgetClassAdjustBaselineAllocation
    attrSet = setWidgetClassAdjustBaselineAllocation
    attrConstruct = undefined
    attrClear = clearWidgetClassAdjustBaselineAllocation
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassAdjustBaselineAllocationFieldCallback (Gtk.Callbacks.wrap_WidgetClassAdjustBaselineAllocationFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.adjustBaselineAllocation"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:adjustBaselineAllocation"
        })

widgetClass_adjustBaselineAllocation :: AttrLabelProxy "adjustBaselineAllocation"
widgetClass_adjustBaselineAllocation = AttrLabelProxy

#endif


-- | Get the value of the “@queue_draw_region@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' widgetClass #queueDrawRegion
-- @
getWidgetClassQueueDrawRegion :: MonadIO m => WidgetClass -> m (Maybe Gtk.Callbacks.WidgetClassQueueDrawRegionFieldCallback)
getWidgetClassQueueDrawRegion s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 792) :: IO (FunPtr Gtk.Callbacks.C_WidgetClassQueueDrawRegionFieldCallback)
    result <- SP.convertFunPtrIfNonNull val $ \val' -> do
        let val'' = Gtk.Callbacks.dynamic_WidgetClassQueueDrawRegionFieldCallback val'
        return val''
    return result

-- | Set the value of the “@queue_draw_region@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' widgetClass [ #queueDrawRegion 'Data.GI.Base.Attributes.:=' value ]
-- @
setWidgetClassQueueDrawRegion :: MonadIO m => WidgetClass -> FunPtr Gtk.Callbacks.C_WidgetClassQueueDrawRegionFieldCallback -> m ()
setWidgetClassQueueDrawRegion s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 792) (val :: FunPtr Gtk.Callbacks.C_WidgetClassQueueDrawRegionFieldCallback)

-- | Set the value of the “@queue_draw_region@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #queueDrawRegion
-- @
clearWidgetClassQueueDrawRegion :: MonadIO m => WidgetClass -> m ()
clearWidgetClassQueueDrawRegion s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 792) (FP.nullFunPtr :: FunPtr Gtk.Callbacks.C_WidgetClassQueueDrawRegionFieldCallback)

#if defined(ENABLE_OVERLOADING)
data WidgetClassQueueDrawRegionFieldInfo
instance AttrInfo WidgetClassQueueDrawRegionFieldInfo where
    type AttrBaseTypeConstraint WidgetClassQueueDrawRegionFieldInfo = (~) WidgetClass
    type AttrAllowedOps WidgetClassQueueDrawRegionFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint WidgetClassQueueDrawRegionFieldInfo = (~) (FunPtr Gtk.Callbacks.C_WidgetClassQueueDrawRegionFieldCallback)
    type AttrTransferTypeConstraint WidgetClassQueueDrawRegionFieldInfo = (~)Gtk.Callbacks.WidgetClassQueueDrawRegionFieldCallback
    type AttrTransferType WidgetClassQueueDrawRegionFieldInfo = (FunPtr Gtk.Callbacks.C_WidgetClassQueueDrawRegionFieldCallback)
    type AttrGetType WidgetClassQueueDrawRegionFieldInfo = Maybe Gtk.Callbacks.WidgetClassQueueDrawRegionFieldCallback
    type AttrLabel WidgetClassQueueDrawRegionFieldInfo = "queue_draw_region"
    type AttrOrigin WidgetClassQueueDrawRegionFieldInfo = WidgetClass
    attrGet = getWidgetClassQueueDrawRegion
    attrSet = setWidgetClassQueueDrawRegion
    attrConstruct = undefined
    attrClear = clearWidgetClassQueueDrawRegion
    attrTransfer _ v = do
        Gtk.Callbacks.mk_WidgetClassQueueDrawRegionFieldCallback (Gtk.Callbacks.wrap_WidgetClassQueueDrawRegionFieldCallback Nothing v)
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.queueDrawRegion"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#g:attr:queueDrawRegion"
        })

widgetClass_queueDrawRegion :: AttrLabelProxy "queueDrawRegion"
widgetClass_queueDrawRegion = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList WidgetClass
type instance O.AttributeList WidgetClass = WidgetClassAttributeList
type WidgetClassAttributeList = ('[ '("parentClass", WidgetClassParentClassFieldInfo), '("activateSignal", WidgetClassActivateSignalFieldInfo), '("dispatchChildPropertiesChanged", WidgetClassDispatchChildPropertiesChangedFieldInfo), '("destroy", WidgetClassDestroyFieldInfo), '("show", WidgetClassShowFieldInfo), '("showAll", WidgetClassShowAllFieldInfo), '("hide", WidgetClassHideFieldInfo), '("map", WidgetClassMapFieldInfo), '("unmap", WidgetClassUnmapFieldInfo), '("realize", WidgetClassRealizeFieldInfo), '("unrealize", WidgetClassUnrealizeFieldInfo), '("sizeAllocate", WidgetClassSizeAllocateFieldInfo), '("stateChanged", WidgetClassStateChangedFieldInfo), '("stateFlagsChanged", WidgetClassStateFlagsChangedFieldInfo), '("parentSet", WidgetClassParentSetFieldInfo), '("hierarchyChanged", WidgetClassHierarchyChangedFieldInfo), '("styleSet", WidgetClassStyleSetFieldInfo), '("directionChanged", WidgetClassDirectionChangedFieldInfo), '("grabNotify", WidgetClassGrabNotifyFieldInfo), '("childNotify", WidgetClassChildNotifyFieldInfo), '("draw", WidgetClassDrawFieldInfo), '("getRequestMode", WidgetClassGetRequestModeFieldInfo), '("getPreferredHeight", WidgetClassGetPreferredHeightFieldInfo), '("getPreferredWidthForHeight", WidgetClassGetPreferredWidthForHeightFieldInfo), '("getPreferredWidth", WidgetClassGetPreferredWidthFieldInfo), '("getPreferredHeightForWidth", WidgetClassGetPreferredHeightForWidthFieldInfo), '("mnemonicActivate", WidgetClassMnemonicActivateFieldInfo), '("grabFocus", WidgetClassGrabFocusFieldInfo), '("focus", WidgetClassFocusFieldInfo), '("moveFocus", WidgetClassMoveFocusFieldInfo), '("keynavFailed", WidgetClassKeynavFailedFieldInfo), '("event", WidgetClassEventFieldInfo), '("buttonPressEvent", WidgetClassButtonPressEventFieldInfo), '("buttonReleaseEvent", WidgetClassButtonReleaseEventFieldInfo), '("scrollEvent", WidgetClassScrollEventFieldInfo), '("motionNotifyEvent", WidgetClassMotionNotifyEventFieldInfo), '("deleteEvent", WidgetClassDeleteEventFieldInfo), '("destroyEvent", WidgetClassDestroyEventFieldInfo), '("keyPressEvent", WidgetClassKeyPressEventFieldInfo), '("keyReleaseEvent", WidgetClassKeyReleaseEventFieldInfo), '("enterNotifyEvent", WidgetClassEnterNotifyEventFieldInfo), '("leaveNotifyEvent", WidgetClassLeaveNotifyEventFieldInfo), '("configureEvent", WidgetClassConfigureEventFieldInfo), '("focusInEvent", WidgetClassFocusInEventFieldInfo), '("focusOutEvent", WidgetClassFocusOutEventFieldInfo), '("mapEvent", WidgetClassMapEventFieldInfo), '("unmapEvent", WidgetClassUnmapEventFieldInfo), '("propertyNotifyEvent", WidgetClassPropertyNotifyEventFieldInfo), '("selectionClearEvent", WidgetClassSelectionClearEventFieldInfo), '("selectionRequestEvent", WidgetClassSelectionRequestEventFieldInfo), '("selectionNotifyEvent", WidgetClassSelectionNotifyEventFieldInfo), '("proximityInEvent", WidgetClassProximityInEventFieldInfo), '("proximityOutEvent", WidgetClassProximityOutEventFieldInfo), '("visibilityNotifyEvent", WidgetClassVisibilityNotifyEventFieldInfo), '("windowStateEvent", WidgetClassWindowStateEventFieldInfo), '("damageEvent", WidgetClassDamageEventFieldInfo), '("grabBrokenEvent", WidgetClassGrabBrokenEventFieldInfo), '("selectionGet", WidgetClassSelectionGetFieldInfo), '("selectionReceived", WidgetClassSelectionReceivedFieldInfo), '("dragBegin", WidgetClassDragBeginFieldInfo), '("dragEnd", WidgetClassDragEndFieldInfo), '("dragDataGet", WidgetClassDragDataGetFieldInfo), '("dragDataDelete", WidgetClassDragDataDeleteFieldInfo), '("dragLeave", WidgetClassDragLeaveFieldInfo), '("dragMotion", WidgetClassDragMotionFieldInfo), '("dragDrop", WidgetClassDragDropFieldInfo), '("dragDataReceived", WidgetClassDragDataReceivedFieldInfo), '("dragFailed", WidgetClassDragFailedFieldInfo), '("popupMenu", WidgetClassPopupMenuFieldInfo), '("showHelp", WidgetClassShowHelpFieldInfo), '("getAccessible", WidgetClassGetAccessibleFieldInfo), '("screenChanged", WidgetClassScreenChangedFieldInfo), '("canActivateAccel", WidgetClassCanActivateAccelFieldInfo), '("compositedChanged", WidgetClassCompositedChangedFieldInfo), '("queryTooltip", WidgetClassQueryTooltipFieldInfo), '("computeExpand", WidgetClassComputeExpandFieldInfo), '("adjustSizeRequest", WidgetClassAdjustSizeRequestFieldInfo), '("adjustSizeAllocation", WidgetClassAdjustSizeAllocationFieldInfo), '("styleUpdated", WidgetClassStyleUpdatedFieldInfo), '("touchEvent", WidgetClassTouchEventFieldInfo), '("getPreferredHeightAndBaselineForWidth", WidgetClassGetPreferredHeightAndBaselineForWidthFieldInfo), '("adjustBaselineRequest", WidgetClassAdjustBaselineRequestFieldInfo), '("adjustBaselineAllocation", WidgetClassAdjustBaselineAllocationFieldInfo), '("queueDrawRegion", WidgetClassQueueDrawRegionFieldInfo)] :: [(Symbol, *)])
#endif

-- method WidgetClass::bind_template_callback_full
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "widget_class"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetClass" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkWidgetClass" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "callback_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The name of the callback as expected in the template XML"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "callback_symbol"
--           , argType =
--               TInterface Name { namespace = "GObject" , name = "Callback" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The callback symbol"
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

foreign import ccall "gtk_widget_class_bind_template_callback_full" gtk_widget_class_bind_template_callback_full :: 
    Ptr WidgetClass ->                      -- widget_class : TInterface (Name {namespace = "Gtk", name = "WidgetClass"})
    CString ->                              -- callback_name : TBasicType TUTF8
    FunPtr GObject.Callbacks.C_Callback ->  -- callback_symbol : TInterface (Name {namespace = "GObject", name = "Callback"})
    IO ()

-- | Declares a /@callbackSymbol@/ to handle /@callbackName@/ from the template XML
-- defined for /@widgetType@/. See 'GI.Gtk.Objects.Builder.builderAddCallbackSymbol'.
-- 
-- Note that this must be called from a composite widget classes class
-- initializer after calling 'GI.Gtk.Structs.WidgetClass.widgetClassSetTemplate'.
-- 
-- /Since: 3.10/
widgetClassBindTemplateCallbackFull ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetClass
    -- ^ /@widgetClass@/: A t'GI.Gtk.Structs.WidgetClass.WidgetClass'
    -> T.Text
    -- ^ /@callbackName@/: The name of the callback as expected in the template XML
    -> GObject.Callbacks.Callback
    -- ^ /@callbackSymbol@/: The callback symbol
    -> m ()
widgetClassBindTemplateCallbackFull widgetClass callbackName callbackSymbol = liftIO $ do
    widgetClass' <- unsafeManagedPtrGetPtr widgetClass
    callbackName' <- textToCString callbackName
    ptrcallbackSymbol <- callocMem :: IO (Ptr (FunPtr GObject.Callbacks.C_Callback))
    callbackSymbol' <- GObject.Callbacks.mk_Callback (GObject.Callbacks.wrap_Callback (Just ptrcallbackSymbol) callbackSymbol)
    poke ptrcallbackSymbol callbackSymbol'
    gtk_widget_class_bind_template_callback_full widgetClass' callbackName' callbackSymbol'
    touchManagedPtr widgetClass
    freeMem callbackName'
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetClassBindTemplateCallbackFullMethodInfo
instance (signature ~ (T.Text -> GObject.Callbacks.Callback -> m ()), MonadIO m) => O.OverloadedMethod WidgetClassBindTemplateCallbackFullMethodInfo WidgetClass signature where
    overloadedMethod = widgetClassBindTemplateCallbackFull

instance O.OverloadedMethodInfo WidgetClassBindTemplateCallbackFullMethodInfo WidgetClass where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.widgetClassBindTemplateCallbackFull",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#v:widgetClassBindTemplateCallbackFull"
        })


#endif

-- method WidgetClass::bind_template_child_full
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "widget_class"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetClass" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkWidgetClass" , sinceVersion = Nothing }
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
--                 { rawDocText =
--                     Just "The \8220id\8221 of the child defined in the template XML"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "internal_child"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Whether the child should be accessible as an \8220internal-child\8221\n                 when this class is used in GtkBuilder XML"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "struct_offset"
--           , argType = TBasicType TInt64
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "The structure offset into the composite widget\8217s instance public or private structure\n                where the automated child pointer should be set, or 0 to not assign the pointer."
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

foreign import ccall "gtk_widget_class_bind_template_child_full" gtk_widget_class_bind_template_child_full :: 
    Ptr WidgetClass ->                      -- widget_class : TInterface (Name {namespace = "Gtk", name = "WidgetClass"})
    CString ->                              -- name : TBasicType TUTF8
    CInt ->                                 -- internal_child : TBasicType TBoolean
    Int64 ->                                -- struct_offset : TBasicType TInt64
    IO ()

-- | Automatically assign an object declared in the class template XML to be set to a location
-- on a freshly built instance’s private data, or alternatively accessible via 'GI.Gtk.Objects.Widget.widgetGetTemplateChild'.
-- 
-- The struct can point either into the public instance, then you should use G_STRUCT_OFFSET(WidgetType, member)
-- for /@structOffset@/,  or in the private struct, then you should use G_PRIVATE_OFFSET(WidgetType, member).
-- 
-- An explicit strong reference will be held automatically for the duration of your
-- instance’s life cycle, it will be released automatically when t'GI.GObject.Structs.ObjectClass.ObjectClass'.@/dispose/@() runs
-- on your instance and if a /@structOffset@/ that is != 0 is specified, then the automatic location
-- in your instance public or private data will be set to 'P.Nothing'. You can however access an automated child
-- pointer the first time your classes t'GI.GObject.Structs.ObjectClass.ObjectClass'.@/dispose/@() runs, or alternatively in
-- t'GI.Gtk.Structs.WidgetClass.WidgetClass'.@/destroy/@().
-- 
-- If /@internalChild@/ is specified, t'GI.Gtk.Structs.BuildableIface.BuildableIface'.@/get_internal_child/@() will be automatically
-- implemented by the t'GI.Gtk.Objects.Widget.Widget' class so there is no need to implement it manually.
-- 
-- The wrapper macros @/gtk_widget_class_bind_template_child()/@, @/gtk_widget_class_bind_template_child_internal()/@,
-- @/gtk_widget_class_bind_template_child_private()/@ and @/gtk_widget_class_bind_template_child_internal_private()/@
-- might be more convenient to use.
-- 
-- Note that this must be called from a composite widget classes class
-- initializer after calling 'GI.Gtk.Structs.WidgetClass.widgetClassSetTemplate'.
-- 
-- /Since: 3.10/
widgetClassBindTemplateChildFull ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetClass
    -- ^ /@widgetClass@/: A t'GI.Gtk.Structs.WidgetClass.WidgetClass'
    -> T.Text
    -- ^ /@name@/: The “id” of the child defined in the template XML
    -> Bool
    -- ^ /@internalChild@/: Whether the child should be accessible as an “internal-child”
    --                  when this class is used in GtkBuilder XML
    -> Int64
    -- ^ /@structOffset@/: The structure offset into the composite widget’s instance public or private structure
    --                 where the automated child pointer should be set, or 0 to not assign the pointer.
    -> m ()
widgetClassBindTemplateChildFull widgetClass name internalChild structOffset = liftIO $ do
    widgetClass' <- unsafeManagedPtrGetPtr widgetClass
    name' <- textToCString name
    let internalChild' = (fromIntegral . fromEnum) internalChild
    gtk_widget_class_bind_template_child_full widgetClass' name' internalChild' structOffset
    touchManagedPtr widgetClass
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetClassBindTemplateChildFullMethodInfo
instance (signature ~ (T.Text -> Bool -> Int64 -> m ()), MonadIO m) => O.OverloadedMethod WidgetClassBindTemplateChildFullMethodInfo WidgetClass signature where
    overloadedMethod = widgetClassBindTemplateChildFull

instance O.OverloadedMethodInfo WidgetClassBindTemplateChildFullMethodInfo WidgetClass where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.widgetClassBindTemplateChildFull",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#v:widgetClassBindTemplateChildFull"
        })


#endif

-- method WidgetClass::find_style_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "klass"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetClass" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetClass" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "property_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the style property to find"
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
-- returnType: Just TParamSpec
-- throws : False
-- Skip return : False

foreign import ccall "gtk_widget_class_find_style_property" gtk_widget_class_find_style_property :: 
    Ptr WidgetClass ->                      -- klass : TInterface (Name {namespace = "Gtk", name = "WidgetClass"})
    CString ->                              -- property_name : TBasicType TUTF8
    IO (Ptr GParamSpec)

-- | Finds a style property of a widget class by name.
-- 
-- /Since: 2.2/
widgetClassFindStyleProperty ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetClass
    -- ^ /@klass@/: a t'GI.Gtk.Structs.WidgetClass.WidgetClass'
    -> T.Text
    -- ^ /@propertyName@/: the name of the style property to find
    -> m GParamSpec
    -- ^ __Returns:__ the t'GI.GObject.Objects.ParamSpec.ParamSpec' of the style property or
    --   'P.Nothing' if /@class@/ has no style property with that name.
widgetClassFindStyleProperty klass propertyName = liftIO $ do
    klass' <- unsafeManagedPtrGetPtr klass
    propertyName' <- textToCString propertyName
    result <- gtk_widget_class_find_style_property klass' propertyName'
    checkUnexpectedReturnNULL "widgetClassFindStyleProperty" result
    result' <- B.GParamSpec.newGParamSpecFromPtr result
    touchManagedPtr klass
    freeMem propertyName'
    return result'

#if defined(ENABLE_OVERLOADING)
data WidgetClassFindStylePropertyMethodInfo
instance (signature ~ (T.Text -> m GParamSpec), MonadIO m) => O.OverloadedMethod WidgetClassFindStylePropertyMethodInfo WidgetClass signature where
    overloadedMethod = widgetClassFindStyleProperty

instance O.OverloadedMethodInfo WidgetClassFindStylePropertyMethodInfo WidgetClass where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.widgetClassFindStyleProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#v:widgetClassFindStyleProperty"
        })


#endif

-- method WidgetClass::get_css_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "widget_class"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetClass" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "class to set the name on"
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

foreign import ccall "gtk_widget_class_get_css_name" gtk_widget_class_get_css_name :: 
    Ptr WidgetClass ->                      -- widget_class : TInterface (Name {namespace = "Gtk", name = "WidgetClass"})
    IO CString

-- | Gets the name used by this class for matching in CSS code. See
-- 'GI.Gtk.Structs.WidgetClass.widgetClassSetCssName' for details.
-- 
-- /Since: 3.20/
widgetClassGetCssName ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetClass
    -- ^ /@widgetClass@/: class to set the name on
    -> m T.Text
    -- ^ __Returns:__ the CSS name of the given class
widgetClassGetCssName widgetClass = liftIO $ do
    widgetClass' <- unsafeManagedPtrGetPtr widgetClass
    result <- gtk_widget_class_get_css_name widgetClass'
    checkUnexpectedReturnNULL "widgetClassGetCssName" result
    result' <- cstringToText result
    touchManagedPtr widgetClass
    return result'

#if defined(ENABLE_OVERLOADING)
data WidgetClassGetCssNameMethodInfo
instance (signature ~ (m T.Text), MonadIO m) => O.OverloadedMethod WidgetClassGetCssNameMethodInfo WidgetClass signature where
    overloadedMethod = widgetClassGetCssName

instance O.OverloadedMethodInfo WidgetClassGetCssNameMethodInfo WidgetClass where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.widgetClassGetCssName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#v:widgetClassGetCssName"
        })


#endif

-- method WidgetClass::install_style_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "klass"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetClass" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidgetClass" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pspec"
--           , argType = TParamSpec
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GParamSpec for the property"
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

foreign import ccall "gtk_widget_class_install_style_property" gtk_widget_class_install_style_property :: 
    Ptr WidgetClass ->                      -- klass : TInterface (Name {namespace = "Gtk", name = "WidgetClass"})
    Ptr GParamSpec ->                       -- pspec : TParamSpec
    IO ()

-- | Installs a style property on a widget class. The parser for the
-- style property is determined by the value type of /@pspec@/.
widgetClassInstallStyleProperty ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetClass
    -- ^ /@klass@/: a t'GI.Gtk.Structs.WidgetClass.WidgetClass'
    -> GParamSpec
    -- ^ /@pspec@/: the t'GI.GObject.Objects.ParamSpec.ParamSpec' for the property
    -> m ()
widgetClassInstallStyleProperty klass pspec = liftIO $ do
    klass' <- unsafeManagedPtrGetPtr klass
    pspec' <- unsafeManagedPtrGetPtr pspec
    gtk_widget_class_install_style_property klass' pspec'
    touchManagedPtr klass
    touchManagedPtr pspec
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetClassInstallStylePropertyMethodInfo
instance (signature ~ (GParamSpec -> m ()), MonadIO m) => O.OverloadedMethod WidgetClassInstallStylePropertyMethodInfo WidgetClass signature where
    overloadedMethod = widgetClassInstallStyleProperty

instance O.OverloadedMethodInfo WidgetClassInstallStylePropertyMethodInfo WidgetClass where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.widgetClassInstallStyleProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#v:widgetClassInstallStyleProperty"
        })


#endif

-- XXX Could not generate method WidgetClass::list_style_properties
-- Not implemented: unpackCArray : Don't know how to unpack C Array of type TParamSpec
-- method WidgetClass::set_accessible_role
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "widget_class"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetClass" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "class to set the accessible role for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "role"
--           , argType = TInterface Name { namespace = "Atk" , name = "Role" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The role to use for accessibles created for @widget_class"
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

foreign import ccall "gtk_widget_class_set_accessible_role" gtk_widget_class_set_accessible_role :: 
    Ptr WidgetClass ->                      -- widget_class : TInterface (Name {namespace = "Gtk", name = "WidgetClass"})
    CUInt ->                                -- role : TInterface (Name {namespace = "Atk", name = "Role"})
    IO ()

-- | Sets the default t'GI.Atk.Enums.Role' to be set on accessibles created for
-- widgets of /@widgetClass@/. Accessibles may decide to not honor this
-- setting if their role reporting is more refined. Calls to
-- 'GI.Gtk.Structs.WidgetClass.widgetClassSetAccessibleType' will reset this value.
-- 
-- In cases where you want more fine-grained control over the role of
-- accessibles created for /@widgetClass@/, you should provide your own
-- accessible type and use 'GI.Gtk.Structs.WidgetClass.widgetClassSetAccessibleType'
-- instead.
-- 
-- If /@role@/ is @/ATK_ROLE_INVALID/@, the default role will not be changed
-- and the accessible’s default role will be used instead.
-- 
-- This function should only be called from class init functions of widgets.
-- 
-- /Since: 3.2/
widgetClassSetAccessibleRole ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetClass
    -- ^ /@widgetClass@/: class to set the accessible role for
    -> Atk.Enums.Role
    -- ^ /@role@/: The role to use for accessibles created for /@widgetClass@/
    -> m ()
widgetClassSetAccessibleRole widgetClass role = liftIO $ do
    widgetClass' <- unsafeManagedPtrGetPtr widgetClass
    let role' = (fromIntegral . fromEnum) role
    gtk_widget_class_set_accessible_role widgetClass' role'
    touchManagedPtr widgetClass
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetClassSetAccessibleRoleMethodInfo
instance (signature ~ (Atk.Enums.Role -> m ()), MonadIO m) => O.OverloadedMethod WidgetClassSetAccessibleRoleMethodInfo WidgetClass signature where
    overloadedMethod = widgetClassSetAccessibleRole

instance O.OverloadedMethodInfo WidgetClassSetAccessibleRoleMethodInfo WidgetClass where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.widgetClassSetAccessibleRole",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#v:widgetClassSetAccessibleRole"
        })


#endif

-- method WidgetClass::set_accessible_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "widget_class"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetClass" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "class to set the accessible type for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "type"
--           , argType = TBasicType TGType
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "The object type that implements the accessible for @widget_class"
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

foreign import ccall "gtk_widget_class_set_accessible_type" gtk_widget_class_set_accessible_type :: 
    Ptr WidgetClass ->                      -- widget_class : TInterface (Name {namespace = "Gtk", name = "WidgetClass"})
    CGType ->                               -- type : TBasicType TGType
    IO ()

-- | Sets the type to be used for creating accessibles for widgets of
-- /@widgetClass@/. The given /@type@/ must be a subtype of the type used for
-- accessibles of the parent class.
-- 
-- This function should only be called from class init functions of widgets.
-- 
-- /Since: 3.2/
widgetClassSetAccessibleType ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetClass
    -- ^ /@widgetClass@/: class to set the accessible type for
    -> GType
    -- ^ /@type@/: The object type that implements the accessible for /@widgetClass@/
    -> m ()
widgetClassSetAccessibleType widgetClass type_ = liftIO $ do
    widgetClass' <- unsafeManagedPtrGetPtr widgetClass
    let type_' = gtypeToCGType type_
    gtk_widget_class_set_accessible_type widgetClass' type_'
    touchManagedPtr widgetClass
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetClassSetAccessibleTypeMethodInfo
instance (signature ~ (GType -> m ()), MonadIO m) => O.OverloadedMethod WidgetClassSetAccessibleTypeMethodInfo WidgetClass signature where
    overloadedMethod = widgetClassSetAccessibleType

instance O.OverloadedMethodInfo WidgetClassSetAccessibleTypeMethodInfo WidgetClass where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.widgetClassSetAccessibleType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#v:widgetClassSetAccessibleType"
        })


#endif

-- method WidgetClass::set_connect_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "widget_class"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetClass" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkWidgetClass" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "connect_func"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "BuilderConnectFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "The #GtkBuilderConnectFunc to use when connecting signals in the class template"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "connect_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The data to pass to @connect_func"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "connect_data_destroy"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "The #GDestroyNotify to free @connect_data, this will only be used at\n                       class finalization time, when no classes of type @widget_type are in use anymore."
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

foreign import ccall "gtk_widget_class_set_connect_func" gtk_widget_class_set_connect_func :: 
    Ptr WidgetClass ->                      -- widget_class : TInterface (Name {namespace = "Gtk", name = "WidgetClass"})
    FunPtr Gtk.Callbacks.C_BuilderConnectFunc -> -- connect_func : TInterface (Name {namespace = "Gtk", name = "BuilderConnectFunc"})
    Ptr () ->                               -- connect_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- connect_data_destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | For use in language bindings, this will override the default t'GI.Gtk.Callbacks.BuilderConnectFunc' to be
-- used when parsing GtkBuilder XML from this class’s template data.
-- 
-- Note that this must be called from a composite widget classes class
-- initializer after calling 'GI.Gtk.Structs.WidgetClass.widgetClassSetTemplate'.
-- 
-- /Since: 3.10/
widgetClassSetConnectFunc ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetClass
    -- ^ /@widgetClass@/: A t'GI.Gtk.Structs.WidgetClass.WidgetClass'
    -> Gtk.Callbacks.BuilderConnectFunc
    -- ^ /@connectFunc@/: The t'GI.Gtk.Callbacks.BuilderConnectFunc' to use when connecting signals in the class template
    -> m ()
widgetClassSetConnectFunc widgetClass connectFunc = liftIO $ do
    widgetClass' <- unsafeManagedPtrGetPtr widgetClass
    connectFunc' <- Gtk.Callbacks.mk_BuilderConnectFunc (Gtk.Callbacks.wrap_BuilderConnectFunc Nothing (Gtk.Callbacks.drop_closures_BuilderConnectFunc connectFunc))
    let connectData = castFunPtrToPtr connectFunc'
    let connectDataDestroy = SP.safeFreeFunPtrPtr
    gtk_widget_class_set_connect_func widgetClass' connectFunc' connectData connectDataDestroy
    touchManagedPtr widgetClass
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetClassSetConnectFuncMethodInfo
instance (signature ~ (Gtk.Callbacks.BuilderConnectFunc -> m ()), MonadIO m) => O.OverloadedMethod WidgetClassSetConnectFuncMethodInfo WidgetClass signature where
    overloadedMethod = widgetClassSetConnectFunc

instance O.OverloadedMethodInfo WidgetClassSetConnectFuncMethodInfo WidgetClass where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.widgetClassSetConnectFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#v:widgetClassSetConnectFunc"
        })


#endif

-- method WidgetClass::set_css_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "widget_class"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetClass" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "class to set the name on"
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
--                 { rawDocText = Just "name to use" , sinceVersion = Nothing }
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

foreign import ccall "gtk_widget_class_set_css_name" gtk_widget_class_set_css_name :: 
    Ptr WidgetClass ->                      -- widget_class : TInterface (Name {namespace = "Gtk", name = "WidgetClass"})
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Sets the name to be used for CSS matching of widgets.
-- 
-- If this function is not called for a given class, the name
-- of the parent class is used.
-- 
-- /Since: 3.20/
widgetClassSetCssName ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetClass
    -- ^ /@widgetClass@/: class to set the name on
    -> T.Text
    -- ^ /@name@/: name to use
    -> m ()
widgetClassSetCssName widgetClass name = liftIO $ do
    widgetClass' <- unsafeManagedPtrGetPtr widgetClass
    name' <- textToCString name
    gtk_widget_class_set_css_name widgetClass' name'
    touchManagedPtr widgetClass
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetClassSetCssNameMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m) => O.OverloadedMethod WidgetClassSetCssNameMethodInfo WidgetClass signature where
    overloadedMethod = widgetClassSetCssName

instance O.OverloadedMethodInfo WidgetClassSetCssNameMethodInfo WidgetClass where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.widgetClassSetCssName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#v:widgetClassSetCssName"
        })


#endif

-- method WidgetClass::set_template
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "widget_class"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetClass" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkWidgetClass" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "template_bytes"
--           , argType = TInterface Name { namespace = "GLib" , name = "Bytes" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GBytes holding the #GtkBuilder XML"
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

foreign import ccall "gtk_widget_class_set_template" gtk_widget_class_set_template :: 
    Ptr WidgetClass ->                      -- widget_class : TInterface (Name {namespace = "Gtk", name = "WidgetClass"})
    Ptr GLib.Bytes.Bytes ->                 -- template_bytes : TInterface (Name {namespace = "GLib", name = "Bytes"})
    IO ()

-- | This should be called at class initialization time to specify
-- the GtkBuilder XML to be used to extend a widget.
-- 
-- For convenience, 'GI.Gtk.Structs.WidgetClass.widgetClassSetTemplateFromResource' is also provided.
-- 
-- Note that any class that installs templates must call 'GI.Gtk.Objects.Widget.widgetInitTemplate'
-- in the widget’s instance initializer.
-- 
-- /Since: 3.10/
widgetClassSetTemplate ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetClass
    -- ^ /@widgetClass@/: A t'GI.Gtk.Structs.WidgetClass.WidgetClass'
    -> GLib.Bytes.Bytes
    -- ^ /@templateBytes@/: A t'GI.GLib.Structs.Bytes.Bytes' holding the t'GI.Gtk.Objects.Builder.Builder' XML
    -> m ()
widgetClassSetTemplate widgetClass templateBytes = liftIO $ do
    widgetClass' <- unsafeManagedPtrGetPtr widgetClass
    templateBytes' <- unsafeManagedPtrGetPtr templateBytes
    gtk_widget_class_set_template widgetClass' templateBytes'
    touchManagedPtr widgetClass
    touchManagedPtr templateBytes
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetClassSetTemplateMethodInfo
instance (signature ~ (GLib.Bytes.Bytes -> m ()), MonadIO m) => O.OverloadedMethod WidgetClassSetTemplateMethodInfo WidgetClass signature where
    overloadedMethod = widgetClassSetTemplate

instance O.OverloadedMethodInfo WidgetClassSetTemplateMethodInfo WidgetClass where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.widgetClassSetTemplate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#v:widgetClassSetTemplate"
        })


#endif

-- method WidgetClass::set_template_from_resource
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "widget_class"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetClass" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkWidgetClass" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "resource_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "The name of the resource to load the template from"
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

foreign import ccall "gtk_widget_class_set_template_from_resource" gtk_widget_class_set_template_from_resource :: 
    Ptr WidgetClass ->                      -- widget_class : TInterface (Name {namespace = "Gtk", name = "WidgetClass"})
    CString ->                              -- resource_name : TBasicType TUTF8
    IO ()

-- | A convenience function to call 'GI.Gtk.Structs.WidgetClass.widgetClassSetTemplate'.
-- 
-- Note that any class that installs templates must call 'GI.Gtk.Objects.Widget.widgetInitTemplate'
-- in the widget’s instance initializer.
-- 
-- /Since: 3.10/
widgetClassSetTemplateFromResource ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    WidgetClass
    -- ^ /@widgetClass@/: A t'GI.Gtk.Structs.WidgetClass.WidgetClass'
    -> T.Text
    -- ^ /@resourceName@/: The name of the resource to load the template from
    -> m ()
widgetClassSetTemplateFromResource widgetClass resourceName = liftIO $ do
    widgetClass' <- unsafeManagedPtrGetPtr widgetClass
    resourceName' <- textToCString resourceName
    gtk_widget_class_set_template_from_resource widgetClass' resourceName'
    touchManagedPtr widgetClass
    freeMem resourceName'
    return ()

#if defined(ENABLE_OVERLOADING)
data WidgetClassSetTemplateFromResourceMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m) => O.OverloadedMethod WidgetClassSetTemplateFromResourceMethodInfo WidgetClass signature where
    overloadedMethod = widgetClassSetTemplateFromResource

instance O.OverloadedMethodInfo WidgetClassSetTemplateFromResourceMethodInfo WidgetClass where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.WidgetClass.widgetClassSetTemplateFromResource",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-WidgetClass.html#v:widgetClassSetTemplateFromResource"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveWidgetClassMethod (t :: Symbol) (o :: *) :: * where
    ResolveWidgetClassMethod "bindTemplateCallbackFull" o = WidgetClassBindTemplateCallbackFullMethodInfo
    ResolveWidgetClassMethod "bindTemplateChildFull" o = WidgetClassBindTemplateChildFullMethodInfo
    ResolveWidgetClassMethod "findStyleProperty" o = WidgetClassFindStylePropertyMethodInfo
    ResolveWidgetClassMethod "installStyleProperty" o = WidgetClassInstallStylePropertyMethodInfo
    ResolveWidgetClassMethod "getCssName" o = WidgetClassGetCssNameMethodInfo
    ResolveWidgetClassMethod "setAccessibleRole" o = WidgetClassSetAccessibleRoleMethodInfo
    ResolveWidgetClassMethod "setAccessibleType" o = WidgetClassSetAccessibleTypeMethodInfo
    ResolveWidgetClassMethod "setConnectFunc" o = WidgetClassSetConnectFuncMethodInfo
    ResolveWidgetClassMethod "setCssName" o = WidgetClassSetCssNameMethodInfo
    ResolveWidgetClassMethod "setTemplate" o = WidgetClassSetTemplateMethodInfo
    ResolveWidgetClassMethod "setTemplateFromResource" o = WidgetClassSetTemplateFromResourceMethodInfo
    ResolveWidgetClassMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveWidgetClassMethod t WidgetClass, O.OverloadedMethod info WidgetClass p) => OL.IsLabel t (WidgetClass -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveWidgetClassMethod t WidgetClass, O.OverloadedMethod info WidgetClass p, R.HasField t WidgetClass p) => R.HasField t WidgetClass p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveWidgetClassMethod t WidgetClass, O.OverloadedMethodInfo info WidgetClass) => OL.IsLabel t (O.MethodProxy info WidgetClass) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


