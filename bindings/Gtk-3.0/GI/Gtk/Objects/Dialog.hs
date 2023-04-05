{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Dialog boxes are a convenient way to prompt the user for a small amount
-- of input, e.g. to display a message, ask a question, or anything else
-- that does not require extensive effort on the user’s part.
-- 
-- GTK+ treats a dialog as a window split vertically. The top section is a
-- t'GI.Gtk.Objects.VBox.VBox', and is where widgets such as a t'GI.Gtk.Objects.Label.Label' or a t'GI.Gtk.Objects.Entry.Entry' should
-- be packed. The bottom area is known as the
-- “action area”. This is generally used for
-- packing buttons into the dialog which may perform functions such as
-- cancel, ok, or apply.
-- 
-- t'GI.Gtk.Objects.Dialog.Dialog' boxes are created with a call to 'GI.Gtk.Objects.Dialog.dialogNew' or
-- @/gtk_dialog_new_with_buttons()/@. @/gtk_dialog_new_with_buttons()/@ is
-- recommended; it allows you to set the dialog title, some convenient
-- flags, and add simple buttons.
-- 
-- If “dialog” is a newly created dialog, the two primary areas of the
-- window can be accessed through 'GI.Gtk.Objects.Dialog.dialogGetContentArea' and
-- 'GI.Gtk.Objects.Dialog.dialogGetActionArea', as can be seen from the example below.
-- 
-- A “modal” dialog (that is, one which freezes the rest of the application
-- from user input), can be created by calling 'GI.Gtk.Objects.Window.windowSetModal' on the
-- dialog. Use the @/GTK_WINDOW()/@ macro to cast the widget returned from
-- 'GI.Gtk.Objects.Dialog.dialogNew' into a t'GI.Gtk.Objects.Window.Window'. When using @/gtk_dialog_new_with_buttons()/@
-- you can also pass the @/GTK_DIALOG_MODAL/@ flag to make a dialog modal.
-- 
-- If you add buttons to t'GI.Gtk.Objects.Dialog.Dialog' using @/gtk_dialog_new_with_buttons()/@,
-- 'GI.Gtk.Objects.Dialog.dialogAddButton', @/gtk_dialog_add_buttons()/@, or
-- 'GI.Gtk.Objects.Dialog.dialogAddActionWidget', clicking the button will emit a signal
-- called [Dialog::response]("GI.Gtk.Objects.Dialog#g:signal:response") with a response ID that you specified. GTK+
-- will never assign a meaning to positive response IDs; these are entirely
-- user-defined. But for convenience, you can use the response IDs in the
-- t'GI.Gtk.Enums.ResponseType' enumeration (these all have values less than zero). If
-- a dialog receives a delete event, the [Dialog::response]("GI.Gtk.Objects.Dialog#g:signal:response") signal will
-- be emitted with a response ID of @/GTK_RESPONSE_DELETE_EVENT/@.
-- 
-- If you want to block waiting for a dialog to return before returning
-- control flow to your code, you can call 'GI.Gtk.Objects.Dialog.dialogRun'. This function
-- enters a recursive main loop and waits for the user to respond to the
-- dialog, returning the response ID corresponding to the button the user
-- clicked.
-- 
-- For the simple dialog in the following example, in reality you’d probably
-- use t'GI.Gtk.Objects.MessageDialog.MessageDialog' to save yourself some effort. But you’d need to
-- create the dialog contents manually if you had more than a simple message
-- in the dialog.
-- 
-- An example for simple GtkDialog usage:
-- 
-- === /C code/
-- >
-- >// Function to open a dialog box with a message
-- >void
-- >quick_message (GtkWindow *parent, gchar *message)
-- >{
-- > GtkWidget *dialog, *label, *content_area;
-- > GtkDialogFlags flags;
-- >
-- > // Create the widgets
-- > flags = GTK_DIALOG_DESTROY_WITH_PARENT;
-- > dialog = gtk_dialog_new_with_buttons ("Message",
-- >                                       parent,
-- >                                       flags,
-- >                                       _("_OK"),
-- >                                       GTK_RESPONSE_NONE,
-- >                                       NULL);
-- > content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
-- > label = gtk_label_new (message);
-- >
-- > // Ensure that the dialog box is destroyed when the user responds
-- >
-- > g_signal_connect_swapped (dialog,
-- >                           "response",
-- >                           G_CALLBACK (gtk_widget_destroy),
-- >                           dialog);
-- >
-- > // Add the label, and show everything we’ve added
-- >
-- > gtk_container_add (GTK_CONTAINER (content_area), label);
-- > gtk_widget_show_all (dialog);
-- >}
-- 
-- 
-- = GtkDialog as GtkBuildable
-- 
-- The GtkDialog implementation of the t'GI.Gtk.Interfaces.Buildable.Buildable' interface exposes the
-- /@vbox@/ and /@actionArea@/ as internal children with the names “vbox” and
-- “action_area”.
-- 
-- GtkDialog supports a custom @\<action-widgets>@ element, which can contain
-- multiple @\<action-widget>@ elements. The “response” attribute specifies a
-- numeric response, and the content of the element is the id of widget
-- (which should be a child of the dialogs /@actionArea@/). To mark a response
-- as default, set the “default“ attribute of the @\<action-widget>@ element
-- to true.
-- 
-- GtkDialog supports adding action widgets by specifying “action“ as
-- the “type“ attribute of a @\<child>@ element. The widget will be added
-- either to the action area or the headerbar of the dialog, depending
-- on the “use-header-bar“ property. The response id has to be associated
-- with the action widget using the @\<action-widgets>@ element.
-- 
-- An example of a t'GI.Gtk.Objects.Dialog.Dialog' UI definition fragment:
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkDialog" id="dialog1">
-- >  <child type="action">
-- >    <object class="GtkButton" id="button_cancel"/>
-- >  </child>
-- >  <child type="action">
-- >    <object class="GtkButton" id="button_ok">
-- >      <property name="can-default">True</property>
-- >    </object>
-- >  </child>
-- >  <action-widgets>
-- >    <action-widget response="cancel">button_cancel</action-widget>
-- >    <action-widget response="ok" default="true">button_ok</action-widget>
-- >  </action-widgets>
-- ></object>
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Dialog
    ( 

-- * Exported types
    Dialog(..)                              ,
    IsDialog                                ,
    toDialog                                ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [activateDefault]("GI.Gtk.Objects.Window#g:method:activateDefault"), [activateFocus]("GI.Gtk.Objects.Window#g:method:activateFocus"), [activateKey]("GI.Gtk.Objects.Window#g:method:activateKey"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelGroup]("GI.Gtk.Objects.Window#g:method:addAccelGroup"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addActionWidget]("GI.Gtk.Objects.Dialog#g:method:addActionWidget"), [addButton]("GI.Gtk.Objects.Dialog#g:method:addButton"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonic]("GI.Gtk.Objects.Window#g:method:addMnemonic"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [beginMoveDrag]("GI.Gtk.Objects.Window#g:method:beginMoveDrag"), [beginResizeDrag]("GI.Gtk.Objects.Window#g:method:beginResizeDrag"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [close]("GI.Gtk.Objects.Window#g:method:close"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [deiconify]("GI.Gtk.Objects.Window#g:method:deiconify"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [fullscreen]("GI.Gtk.Objects.Window#g:method:fullscreen"), [fullscreenOnMonitor]("GI.Gtk.Objects.Window#g:method:fullscreenOnMonitor"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasGroup]("GI.Gtk.Objects.Window#g:method:hasGroup"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasToplevelFocus]("GI.Gtk.Objects.Window#g:method:hasToplevelFocus"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [iconify]("GI.Gtk.Objects.Window#g:method:iconify"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isActive]("GI.Gtk.Objects.Window#g:method:isActive"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isMaximized]("GI.Gtk.Objects.Window#g:method:isMaximized"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [maximize]("GI.Gtk.Objects.Window#g:method:maximize"), [mnemonicActivate]("GI.Gtk.Objects.Window#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [move]("GI.Gtk.Objects.Window#g:method:move"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parseGeometry]("GI.Gtk.Objects.Window#g:method:parseGeometry"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [present]("GI.Gtk.Objects.Window#g:method:present"), [presentWithTime]("GI.Gtk.Objects.Window#g:method:presentWithTime"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [propagateKeyEvent]("GI.Gtk.Objects.Window#g:method:propagateKeyEvent"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelGroup]("GI.Gtk.Objects.Window#g:method:removeAccelGroup"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonic]("GI.Gtk.Objects.Window#g:method:removeMnemonic"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [reshowWithInitialSize]("GI.Gtk.Objects.Window#g:method:reshowWithInitialSize"), [resize]("GI.Gtk.Objects.Window#g:method:resize"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [resizeGripIsVisible]("GI.Gtk.Objects.Window#g:method:resizeGripIsVisible"), [resizeToGeometry]("GI.Gtk.Objects.Window#g:method:resizeToGeometry"), [response]("GI.Gtk.Objects.Dialog#g:method:response"), [run]("GI.Gtk.Objects.Dialog#g:method:run"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [stick]("GI.Gtk.Objects.Window#g:method:stick"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unfullscreen]("GI.Gtk.Objects.Window#g:method:unfullscreen"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unmaximize]("GI.Gtk.Objects.Window#g:method:unmaximize"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [unstick]("GI.Gtk.Objects.Window#g:method:unstick"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAcceptFocus]("GI.Gtk.Objects.Window#g:method:getAcceptFocus"), [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionArea]("GI.Gtk.Objects.Dialog#g:method:getActionArea"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getApplication]("GI.Gtk.Objects.Window#g:method:getApplication"), [getAttachedTo]("GI.Gtk.Objects.Window#g:method:getAttachedTo"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChild]("GI.Gtk.Objects.Bin#g:method:getChild"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getContentArea]("GI.Gtk.Objects.Dialog#g:method:getContentArea"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDecorated]("GI.Gtk.Objects.Window#g:method:getDecorated"), [getDefaultSize]("GI.Gtk.Objects.Window#g:method:getDefaultSize"), [getDefaultWidget]("GI.Gtk.Objects.Window#g:method:getDefaultWidget"), [getDeletable]("GI.Gtk.Objects.Window#g:method:getDeletable"), [getDestroyWithParent]("GI.Gtk.Objects.Window#g:method:getDestroyWithParent"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocus]("GI.Gtk.Objects.Window#g:method:getFocus"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusOnMap]("GI.Gtk.Objects.Window#g:method:getFocusOnMap"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFocusVisible]("GI.Gtk.Objects.Window#g:method:getFocusVisible"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getGravity]("GI.Gtk.Objects.Window#g:method:getGravity"), [getGroup]("GI.Gtk.Objects.Window#g:method:getGroup"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasResizeGrip]("GI.Gtk.Objects.Window#g:method:getHasResizeGrip"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHeaderBar]("GI.Gtk.Objects.Dialog#g:method:getHeaderBar"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getHideTitlebarWhenMaximized]("GI.Gtk.Objects.Window#g:method:getHideTitlebarWhenMaximized"), [getIcon]("GI.Gtk.Objects.Window#g:method:getIcon"), [getIconList]("GI.Gtk.Objects.Window#g:method:getIconList"), [getIconName]("GI.Gtk.Objects.Window#g:method:getIconName"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMnemonicModifier]("GI.Gtk.Objects.Window#g:method:getMnemonicModifier"), [getMnemonicsVisible]("GI.Gtk.Objects.Window#g:method:getMnemonicsVisible"), [getModal]("GI.Gtk.Objects.Window#g:method:getModal"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Window#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPosition]("GI.Gtk.Objects.Window#g:method:getPosition"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizable]("GI.Gtk.Objects.Window#g:method:getResizable"), [getResizeGripArea]("GI.Gtk.Objects.Window#g:method:getResizeGripArea"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getResponseForWidget]("GI.Gtk.Objects.Dialog#g:method:getResponseForWidget"), [getRole]("GI.Gtk.Objects.Window#g:method:getRole"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Window#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSize]("GI.Gtk.Objects.Window#g:method:getSize"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getSkipPagerHint]("GI.Gtk.Objects.Window#g:method:getSkipPagerHint"), [getSkipTaskbarHint]("GI.Gtk.Objects.Window#g:method:getSkipTaskbarHint"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTitle]("GI.Gtk.Objects.Window#g:method:getTitle"), [getTitlebar]("GI.Gtk.Objects.Window#g:method:getTitlebar"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getTransientFor]("GI.Gtk.Objects.Window#g:method:getTransientFor"), [getTypeHint]("GI.Gtk.Objects.Window#g:method:getTypeHint"), [getUrgencyHint]("GI.Gtk.Objects.Window#g:method:getUrgencyHint"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWidgetForResponse]("GI.Gtk.Objects.Dialog#g:method:getWidgetForResponse"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow"), [getWindowType]("GI.Gtk.Objects.Window#g:method:getWindowType").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAcceptFocus]("GI.Gtk.Objects.Window#g:method:setAcceptFocus"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAlternativeButtonOrderFromArray]("GI.Gtk.Objects.Dialog#g:method:setAlternativeButtonOrderFromArray"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setApplication]("GI.Gtk.Objects.Window#g:method:setApplication"), [setAttachedTo]("GI.Gtk.Objects.Window#g:method:setAttachedTo"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDecorated]("GI.Gtk.Objects.Window#g:method:setDecorated"), [setDefault]("GI.Gtk.Objects.Window#g:method:setDefault"), [setDefaultGeometry]("GI.Gtk.Objects.Window#g:method:setDefaultGeometry"), [setDefaultResponse]("GI.Gtk.Objects.Dialog#g:method:setDefaultResponse"), [setDefaultSize]("GI.Gtk.Objects.Window#g:method:setDefaultSize"), [setDeletable]("GI.Gtk.Objects.Window#g:method:setDeletable"), [setDestroyWithParent]("GI.Gtk.Objects.Window#g:method:setDestroyWithParent"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocus]("GI.Gtk.Objects.Window#g:method:setFocus"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusOnMap]("GI.Gtk.Objects.Window#g:method:setFocusOnMap"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFocusVisible]("GI.Gtk.Objects.Window#g:method:setFocusVisible"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setGeometryHints]("GI.Gtk.Objects.Window#g:method:setGeometryHints"), [setGravity]("GI.Gtk.Objects.Window#g:method:setGravity"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasResizeGrip]("GI.Gtk.Objects.Window#g:method:setHasResizeGrip"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasUserRefCount]("GI.Gtk.Objects.Window#g:method:setHasUserRefCount"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setHideTitlebarWhenMaximized]("GI.Gtk.Objects.Window#g:method:setHideTitlebarWhenMaximized"), [setIcon]("GI.Gtk.Objects.Window#g:method:setIcon"), [setIconFromFile]("GI.Gtk.Objects.Window#g:method:setIconFromFile"), [setIconList]("GI.Gtk.Objects.Window#g:method:setIconList"), [setIconName]("GI.Gtk.Objects.Window#g:method:setIconName"), [setKeepAbove]("GI.Gtk.Objects.Window#g:method:setKeepAbove"), [setKeepBelow]("GI.Gtk.Objects.Window#g:method:setKeepBelow"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMnemonicModifier]("GI.Gtk.Objects.Window#g:method:setMnemonicModifier"), [setMnemonicsVisible]("GI.Gtk.Objects.Window#g:method:setMnemonicsVisible"), [setModal]("GI.Gtk.Objects.Window#g:method:setModal"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Window#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPosition]("GI.Gtk.Objects.Window#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizable]("GI.Gtk.Objects.Window#g:method:setResizable"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setResponseSensitive]("GI.Gtk.Objects.Dialog#g:method:setResponseSensitive"), [setRole]("GI.Gtk.Objects.Window#g:method:setRole"), [setScreen]("GI.Gtk.Objects.Window#g:method:setScreen"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setSkipPagerHint]("GI.Gtk.Objects.Window#g:method:setSkipPagerHint"), [setSkipTaskbarHint]("GI.Gtk.Objects.Window#g:method:setSkipTaskbarHint"), [setStartupId]("GI.Gtk.Objects.Window#g:method:setStartupId"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTitle]("GI.Gtk.Objects.Window#g:method:setTitle"), [setTitlebar]("GI.Gtk.Objects.Window#g:method:setTitlebar"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setTransientFor]("GI.Gtk.Objects.Window#g:method:setTransientFor"), [setTypeHint]("GI.Gtk.Objects.Window#g:method:setTypeHint"), [setUrgencyHint]("GI.Gtk.Objects.Window#g:method:setUrgencyHint"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow"), [setWmclass]("GI.Gtk.Objects.Window#g:method:setWmclass").

#if defined(ENABLE_OVERLOADING)
    ResolveDialogMethod                     ,
#endif

-- ** addActionWidget #method:addActionWidget#

#if defined(ENABLE_OVERLOADING)
    DialogAddActionWidgetMethodInfo         ,
#endif
    dialogAddActionWidget                   ,


-- ** addButton #method:addButton#

#if defined(ENABLE_OVERLOADING)
    DialogAddButtonMethodInfo               ,
#endif
    dialogAddButton                         ,


-- ** getActionArea #method:getActionArea#

#if defined(ENABLE_OVERLOADING)
    DialogGetActionAreaMethodInfo           ,
#endif
    dialogGetActionArea                     ,


-- ** getContentArea #method:getContentArea#

#if defined(ENABLE_OVERLOADING)
    DialogGetContentAreaMethodInfo          ,
#endif
    dialogGetContentArea                    ,


-- ** getHeaderBar #method:getHeaderBar#

#if defined(ENABLE_OVERLOADING)
    DialogGetHeaderBarMethodInfo            ,
#endif
    dialogGetHeaderBar                      ,


-- ** getResponseForWidget #method:getResponseForWidget#

#if defined(ENABLE_OVERLOADING)
    DialogGetResponseForWidgetMethodInfo    ,
#endif
    dialogGetResponseForWidget              ,


-- ** getWidgetForResponse #method:getWidgetForResponse#

#if defined(ENABLE_OVERLOADING)
    DialogGetWidgetForResponseMethodInfo    ,
#endif
    dialogGetWidgetForResponse              ,


-- ** new #method:new#

    dialogNew                               ,


-- ** response #method:response#

#if defined(ENABLE_OVERLOADING)
    DialogResponseMethodInfo                ,
#endif
    dialogResponse                          ,


-- ** run #method:run#

#if defined(ENABLE_OVERLOADING)
    DialogRunMethodInfo                     ,
#endif
    dialogRun                               ,


-- ** setAlternativeButtonOrderFromArray #method:setAlternativeButtonOrderFromArray#

#if defined(ENABLE_OVERLOADING)
    DialogSetAlternativeButtonOrderFromArrayMethodInfo,
#endif
    dialogSetAlternativeButtonOrderFromArray,


-- ** setDefaultResponse #method:setDefaultResponse#

#if defined(ENABLE_OVERLOADING)
    DialogSetDefaultResponseMethodInfo      ,
#endif
    dialogSetDefaultResponse                ,


-- ** setResponseSensitive #method:setResponseSensitive#

#if defined(ENABLE_OVERLOADING)
    DialogSetResponseSensitiveMethodInfo    ,
#endif
    dialogSetResponseSensitive              ,




 -- * Properties


-- ** useHeaderBar #attr:useHeaderBar#
-- | 'P.True' if the dialog uses a t'GI.Gtk.Objects.HeaderBar.HeaderBar' for action buttons
-- instead of the action-area.
-- 
-- For technical reasons, this property is declared as an integer
-- property, but you should only set it to 'P.True' or 'P.False'.
-- 
-- /Since: 3.12/

#if defined(ENABLE_OVERLOADING)
    DialogUseHeaderBarPropertyInfo          ,
#endif
    constructDialogUseHeaderBar             ,
#if defined(ENABLE_OVERLOADING)
    dialogUseHeaderBar                      ,
#endif
    getDialogUseHeaderBar                   ,




 -- * Signals


-- ** close #signal:close#

    DialogCloseCallback                     ,
#if defined(ENABLE_OVERLOADING)
    DialogCloseSignalInfo                   ,
#endif
    afterDialogClose                        ,
    onDialogClose                           ,


-- ** response #signal:response#

    DialogResponseCallback                  ,
#if defined(ENABLE_OVERLOADING)
    DialogResponseSignalInfo                ,
#endif
    afterDialogResponse                     ,
    onDialogResponse                        ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.Box as Gtk.Box
import {-# SOURCE #-} qualified GI.Gtk.Objects.Container as Gtk.Container
import {-# SOURCE #-} qualified GI.Gtk.Objects.HeaderBar as Gtk.HeaderBar
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Objects.Window as Gtk.Window

-- | Memory-managed wrapper type.
newtype Dialog = Dialog (SP.ManagedPtr Dialog)
    deriving (Eq)

instance SP.ManagedPtrNewtype Dialog where
    toManagedPtr (Dialog p) = p

foreign import ccall "gtk_dialog_get_type"
    c_gtk_dialog_get_type :: IO B.Types.GType

instance B.Types.TypedObject Dialog where
    glibType = c_gtk_dialog_get_type

instance B.Types.GObject Dialog

-- | Type class for types which can be safely cast to `Dialog`, for instance with `toDialog`.
class (SP.GObject o, O.IsDescendantOf Dialog o) => IsDialog o
instance (SP.GObject o, O.IsDescendantOf Dialog o) => IsDialog o

instance O.HasParentTypes Dialog
type instance O.ParentTypes Dialog = '[Gtk.Window.Window, Gtk.Bin.Bin, Gtk.Container.Container, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `Dialog`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toDialog :: (MIO.MonadIO m, IsDialog o) => o -> m Dialog
toDialog = MIO.liftIO . B.ManagedPtr.unsafeCastTo Dialog

-- | Convert 'Dialog' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Dialog) where
    gvalueGType_ = c_gtk_dialog_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Dialog)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Dialog)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Dialog ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveDialogMethod (t :: Symbol) (o :: *) :: * where
    ResolveDialogMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveDialogMethod "activateDefault" o = Gtk.Window.WindowActivateDefaultMethodInfo
    ResolveDialogMethod "activateFocus" o = Gtk.Window.WindowActivateFocusMethodInfo
    ResolveDialogMethod "activateKey" o = Gtk.Window.WindowActivateKeyMethodInfo
    ResolveDialogMethod "add" o = Gtk.Container.ContainerAddMethodInfo
    ResolveDialogMethod "addAccelGroup" o = Gtk.Window.WindowAddAccelGroupMethodInfo
    ResolveDialogMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveDialogMethod "addActionWidget" o = DialogAddActionWidgetMethodInfo
    ResolveDialogMethod "addButton" o = DialogAddButtonMethodInfo
    ResolveDialogMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveDialogMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveDialogMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveDialogMethod "addMnemonic" o = Gtk.Window.WindowAddMnemonicMethodInfo
    ResolveDialogMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveDialogMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveDialogMethod "beginMoveDrag" o = Gtk.Window.WindowBeginMoveDragMethodInfo
    ResolveDialogMethod "beginResizeDrag" o = Gtk.Window.WindowBeginResizeDragMethodInfo
    ResolveDialogMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveDialogMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveDialogMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveDialogMethod "checkResize" o = Gtk.Container.ContainerCheckResizeMethodInfo
    ResolveDialogMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveDialogMethod "childGetProperty" o = Gtk.Container.ContainerChildGetPropertyMethodInfo
    ResolveDialogMethod "childNotify" o = Gtk.Container.ContainerChildNotifyMethodInfo
    ResolveDialogMethod "childNotifyByPspec" o = Gtk.Container.ContainerChildNotifyByPspecMethodInfo
    ResolveDialogMethod "childSetProperty" o = Gtk.Container.ContainerChildSetPropertyMethodInfo
    ResolveDialogMethod "childType" o = Gtk.Container.ContainerChildTypeMethodInfo
    ResolveDialogMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveDialogMethod "close" o = Gtk.Window.WindowCloseMethodInfo
    ResolveDialogMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveDialogMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveDialogMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveDialogMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveDialogMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveDialogMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveDialogMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveDialogMethod "deiconify" o = Gtk.Window.WindowDeiconifyMethodInfo
    ResolveDialogMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveDialogMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveDialogMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveDialogMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveDialogMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveDialogMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveDialogMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveDialogMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveDialogMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveDialogMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveDialogMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveDialogMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveDialogMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveDialogMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveDialogMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveDialogMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveDialogMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveDialogMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveDialogMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveDialogMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveDialogMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveDialogMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveDialogMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveDialogMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveDialogMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveDialogMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveDialogMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveDialogMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveDialogMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveDialogMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveDialogMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveDialogMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveDialogMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveDialogMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveDialogMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveDialogMethod "forall" o = Gtk.Container.ContainerForallMethodInfo
    ResolveDialogMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveDialogMethod "foreach" o = Gtk.Container.ContainerForeachMethodInfo
    ResolveDialogMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveDialogMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveDialogMethod "fullscreen" o = Gtk.Window.WindowFullscreenMethodInfo
    ResolveDialogMethod "fullscreenOnMonitor" o = Gtk.Window.WindowFullscreenOnMonitorMethodInfo
    ResolveDialogMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveDialogMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveDialogMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveDialogMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveDialogMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveDialogMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveDialogMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveDialogMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveDialogMethod "hasGroup" o = Gtk.Window.WindowHasGroupMethodInfo
    ResolveDialogMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveDialogMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveDialogMethod "hasToplevelFocus" o = Gtk.Window.WindowHasToplevelFocusMethodInfo
    ResolveDialogMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveDialogMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveDialogMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveDialogMethod "iconify" o = Gtk.Window.WindowIconifyMethodInfo
    ResolveDialogMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveDialogMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveDialogMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveDialogMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveDialogMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveDialogMethod "isActive" o = Gtk.Window.WindowIsActiveMethodInfo
    ResolveDialogMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveDialogMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveDialogMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveDialogMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveDialogMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveDialogMethod "isMaximized" o = Gtk.Window.WindowIsMaximizedMethodInfo
    ResolveDialogMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveDialogMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveDialogMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveDialogMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveDialogMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveDialogMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveDialogMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveDialogMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveDialogMethod "maximize" o = Gtk.Window.WindowMaximizeMethodInfo
    ResolveDialogMethod "mnemonicActivate" o = Gtk.Window.WindowMnemonicActivateMethodInfo
    ResolveDialogMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveDialogMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveDialogMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveDialogMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveDialogMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveDialogMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveDialogMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveDialogMethod "move" o = Gtk.Window.WindowMoveMethodInfo
    ResolveDialogMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveDialogMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveDialogMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveDialogMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveDialogMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveDialogMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveDialogMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveDialogMethod "parseGeometry" o = Gtk.Window.WindowParseGeometryMethodInfo
    ResolveDialogMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveDialogMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveDialogMethod "present" o = Gtk.Window.WindowPresentMethodInfo
    ResolveDialogMethod "presentWithTime" o = Gtk.Window.WindowPresentWithTimeMethodInfo
    ResolveDialogMethod "propagateDraw" o = Gtk.Container.ContainerPropagateDrawMethodInfo
    ResolveDialogMethod "propagateKeyEvent" o = Gtk.Window.WindowPropagateKeyEventMethodInfo
    ResolveDialogMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveDialogMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveDialogMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveDialogMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveDialogMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveDialogMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveDialogMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveDialogMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveDialogMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveDialogMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveDialogMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveDialogMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveDialogMethod "remove" o = Gtk.Container.ContainerRemoveMethodInfo
    ResolveDialogMethod "removeAccelGroup" o = Gtk.Window.WindowRemoveAccelGroupMethodInfo
    ResolveDialogMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveDialogMethod "removeMnemonic" o = Gtk.Window.WindowRemoveMnemonicMethodInfo
    ResolveDialogMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveDialogMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveDialogMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveDialogMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveDialogMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveDialogMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveDialogMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveDialogMethod "reshowWithInitialSize" o = Gtk.Window.WindowReshowWithInitialSizeMethodInfo
    ResolveDialogMethod "resize" o = Gtk.Window.WindowResizeMethodInfo
    ResolveDialogMethod "resizeChildren" o = Gtk.Container.ContainerResizeChildrenMethodInfo
    ResolveDialogMethod "resizeGripIsVisible" o = Gtk.Window.WindowResizeGripIsVisibleMethodInfo
    ResolveDialogMethod "resizeToGeometry" o = Gtk.Window.WindowResizeToGeometryMethodInfo
    ResolveDialogMethod "response" o = DialogResponseMethodInfo
    ResolveDialogMethod "run" o = DialogRunMethodInfo
    ResolveDialogMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveDialogMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveDialogMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveDialogMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveDialogMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveDialogMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveDialogMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveDialogMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveDialogMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveDialogMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveDialogMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveDialogMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveDialogMethod "stick" o = Gtk.Window.WindowStickMethodInfo
    ResolveDialogMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveDialogMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveDialogMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveDialogMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveDialogMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveDialogMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveDialogMethod "unfullscreen" o = Gtk.Window.WindowUnfullscreenMethodInfo
    ResolveDialogMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveDialogMethod "unmaximize" o = Gtk.Window.WindowUnmaximizeMethodInfo
    ResolveDialogMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveDialogMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveDialogMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveDialogMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveDialogMethod "unsetFocusChain" o = Gtk.Container.ContainerUnsetFocusChainMethodInfo
    ResolveDialogMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveDialogMethod "unstick" o = Gtk.Window.WindowUnstickMethodInfo
    ResolveDialogMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveDialogMethod "getAcceptFocus" o = Gtk.Window.WindowGetAcceptFocusMethodInfo
    ResolveDialogMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveDialogMethod "getActionArea" o = DialogGetActionAreaMethodInfo
    ResolveDialogMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveDialogMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveDialogMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveDialogMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveDialogMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveDialogMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveDialogMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveDialogMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveDialogMethod "getApplication" o = Gtk.Window.WindowGetApplicationMethodInfo
    ResolveDialogMethod "getAttachedTo" o = Gtk.Window.WindowGetAttachedToMethodInfo
    ResolveDialogMethod "getBorderWidth" o = Gtk.Container.ContainerGetBorderWidthMethodInfo
    ResolveDialogMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveDialogMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveDialogMethod "getChild" o = Gtk.Bin.BinGetChildMethodInfo
    ResolveDialogMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveDialogMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveDialogMethod "getChildren" o = Gtk.Container.ContainerGetChildrenMethodInfo
    ResolveDialogMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveDialogMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveDialogMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveDialogMethod "getContentArea" o = DialogGetContentAreaMethodInfo
    ResolveDialogMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveDialogMethod "getDecorated" o = Gtk.Window.WindowGetDecoratedMethodInfo
    ResolveDialogMethod "getDefaultSize" o = Gtk.Window.WindowGetDefaultSizeMethodInfo
    ResolveDialogMethod "getDefaultWidget" o = Gtk.Window.WindowGetDefaultWidgetMethodInfo
    ResolveDialogMethod "getDeletable" o = Gtk.Window.WindowGetDeletableMethodInfo
    ResolveDialogMethod "getDestroyWithParent" o = Gtk.Window.WindowGetDestroyWithParentMethodInfo
    ResolveDialogMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveDialogMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveDialogMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveDialogMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveDialogMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveDialogMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveDialogMethod "getFocus" o = Gtk.Window.WindowGetFocusMethodInfo
    ResolveDialogMethod "getFocusChain" o = Gtk.Container.ContainerGetFocusChainMethodInfo
    ResolveDialogMethod "getFocusChild" o = Gtk.Container.ContainerGetFocusChildMethodInfo
    ResolveDialogMethod "getFocusHadjustment" o = Gtk.Container.ContainerGetFocusHadjustmentMethodInfo
    ResolveDialogMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveDialogMethod "getFocusOnMap" o = Gtk.Window.WindowGetFocusOnMapMethodInfo
    ResolveDialogMethod "getFocusVadjustment" o = Gtk.Container.ContainerGetFocusVadjustmentMethodInfo
    ResolveDialogMethod "getFocusVisible" o = Gtk.Window.WindowGetFocusVisibleMethodInfo
    ResolveDialogMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveDialogMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveDialogMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveDialogMethod "getGravity" o = Gtk.Window.WindowGetGravityMethodInfo
    ResolveDialogMethod "getGroup" o = Gtk.Window.WindowGetGroupMethodInfo
    ResolveDialogMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveDialogMethod "getHasResizeGrip" o = Gtk.Window.WindowGetHasResizeGripMethodInfo
    ResolveDialogMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveDialogMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveDialogMethod "getHeaderBar" o = DialogGetHeaderBarMethodInfo
    ResolveDialogMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveDialogMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveDialogMethod "getHideTitlebarWhenMaximized" o = Gtk.Window.WindowGetHideTitlebarWhenMaximizedMethodInfo
    ResolveDialogMethod "getIcon" o = Gtk.Window.WindowGetIconMethodInfo
    ResolveDialogMethod "getIconList" o = Gtk.Window.WindowGetIconListMethodInfo
    ResolveDialogMethod "getIconName" o = Gtk.Window.WindowGetIconNameMethodInfo
    ResolveDialogMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveDialogMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveDialogMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveDialogMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveDialogMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveDialogMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveDialogMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveDialogMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveDialogMethod "getMnemonicModifier" o = Gtk.Window.WindowGetMnemonicModifierMethodInfo
    ResolveDialogMethod "getMnemonicsVisible" o = Gtk.Window.WindowGetMnemonicsVisibleMethodInfo
    ResolveDialogMethod "getModal" o = Gtk.Window.WindowGetModalMethodInfo
    ResolveDialogMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveDialogMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveDialogMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveDialogMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveDialogMethod "getOpacity" o = Gtk.Window.WindowGetOpacityMethodInfo
    ResolveDialogMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveDialogMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveDialogMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveDialogMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveDialogMethod "getPathForChild" o = Gtk.Container.ContainerGetPathForChildMethodInfo
    ResolveDialogMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveDialogMethod "getPosition" o = Gtk.Window.WindowGetPositionMethodInfo
    ResolveDialogMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveDialogMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveDialogMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveDialogMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveDialogMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveDialogMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveDialogMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveDialogMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveDialogMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveDialogMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveDialogMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveDialogMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveDialogMethod "getResizable" o = Gtk.Window.WindowGetResizableMethodInfo
    ResolveDialogMethod "getResizeGripArea" o = Gtk.Window.WindowGetResizeGripAreaMethodInfo
    ResolveDialogMethod "getResizeMode" o = Gtk.Container.ContainerGetResizeModeMethodInfo
    ResolveDialogMethod "getResponseForWidget" o = DialogGetResponseForWidgetMethodInfo
    ResolveDialogMethod "getRole" o = Gtk.Window.WindowGetRoleMethodInfo
    ResolveDialogMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveDialogMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveDialogMethod "getScreen" o = Gtk.Window.WindowGetScreenMethodInfo
    ResolveDialogMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveDialogMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveDialogMethod "getSize" o = Gtk.Window.WindowGetSizeMethodInfo
    ResolveDialogMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveDialogMethod "getSkipPagerHint" o = Gtk.Window.WindowGetSkipPagerHintMethodInfo
    ResolveDialogMethod "getSkipTaskbarHint" o = Gtk.Window.WindowGetSkipTaskbarHintMethodInfo
    ResolveDialogMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveDialogMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveDialogMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveDialogMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveDialogMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveDialogMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveDialogMethod "getTitle" o = Gtk.Window.WindowGetTitleMethodInfo
    ResolveDialogMethod "getTitlebar" o = Gtk.Window.WindowGetTitlebarMethodInfo
    ResolveDialogMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveDialogMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveDialogMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveDialogMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveDialogMethod "getTransientFor" o = Gtk.Window.WindowGetTransientForMethodInfo
    ResolveDialogMethod "getTypeHint" o = Gtk.Window.WindowGetTypeHintMethodInfo
    ResolveDialogMethod "getUrgencyHint" o = Gtk.Window.WindowGetUrgencyHintMethodInfo
    ResolveDialogMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveDialogMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveDialogMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveDialogMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveDialogMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveDialogMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveDialogMethod "getWidgetForResponse" o = DialogGetWidgetForResponseMethodInfo
    ResolveDialogMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveDialogMethod "getWindowType" o = Gtk.Window.WindowGetWindowTypeMethodInfo
    ResolveDialogMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveDialogMethod "setAcceptFocus" o = Gtk.Window.WindowSetAcceptFocusMethodInfo
    ResolveDialogMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveDialogMethod "setAlternativeButtonOrderFromArray" o = DialogSetAlternativeButtonOrderFromArrayMethodInfo
    ResolveDialogMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveDialogMethod "setApplication" o = Gtk.Window.WindowSetApplicationMethodInfo
    ResolveDialogMethod "setAttachedTo" o = Gtk.Window.WindowSetAttachedToMethodInfo
    ResolveDialogMethod "setBorderWidth" o = Gtk.Container.ContainerSetBorderWidthMethodInfo
    ResolveDialogMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveDialogMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveDialogMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveDialogMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveDialogMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveDialogMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveDialogMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveDialogMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveDialogMethod "setDecorated" o = Gtk.Window.WindowSetDecoratedMethodInfo
    ResolveDialogMethod "setDefault" o = Gtk.Window.WindowSetDefaultMethodInfo
    ResolveDialogMethod "setDefaultGeometry" o = Gtk.Window.WindowSetDefaultGeometryMethodInfo
    ResolveDialogMethod "setDefaultResponse" o = DialogSetDefaultResponseMethodInfo
    ResolveDialogMethod "setDefaultSize" o = Gtk.Window.WindowSetDefaultSizeMethodInfo
    ResolveDialogMethod "setDeletable" o = Gtk.Window.WindowSetDeletableMethodInfo
    ResolveDialogMethod "setDestroyWithParent" o = Gtk.Window.WindowSetDestroyWithParentMethodInfo
    ResolveDialogMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveDialogMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveDialogMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveDialogMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveDialogMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveDialogMethod "setFocus" o = Gtk.Window.WindowSetFocusMethodInfo
    ResolveDialogMethod "setFocusChain" o = Gtk.Container.ContainerSetFocusChainMethodInfo
    ResolveDialogMethod "setFocusChild" o = Gtk.Container.ContainerSetFocusChildMethodInfo
    ResolveDialogMethod "setFocusHadjustment" o = Gtk.Container.ContainerSetFocusHadjustmentMethodInfo
    ResolveDialogMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveDialogMethod "setFocusOnMap" o = Gtk.Window.WindowSetFocusOnMapMethodInfo
    ResolveDialogMethod "setFocusVadjustment" o = Gtk.Container.ContainerSetFocusVadjustmentMethodInfo
    ResolveDialogMethod "setFocusVisible" o = Gtk.Window.WindowSetFocusVisibleMethodInfo
    ResolveDialogMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveDialogMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveDialogMethod "setGeometryHints" o = Gtk.Window.WindowSetGeometryHintsMethodInfo
    ResolveDialogMethod "setGravity" o = Gtk.Window.WindowSetGravityMethodInfo
    ResolveDialogMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveDialogMethod "setHasResizeGrip" o = Gtk.Window.WindowSetHasResizeGripMethodInfo
    ResolveDialogMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveDialogMethod "setHasUserRefCount" o = Gtk.Window.WindowSetHasUserRefCountMethodInfo
    ResolveDialogMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveDialogMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveDialogMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveDialogMethod "setHideTitlebarWhenMaximized" o = Gtk.Window.WindowSetHideTitlebarWhenMaximizedMethodInfo
    ResolveDialogMethod "setIcon" o = Gtk.Window.WindowSetIconMethodInfo
    ResolveDialogMethod "setIconFromFile" o = Gtk.Window.WindowSetIconFromFileMethodInfo
    ResolveDialogMethod "setIconList" o = Gtk.Window.WindowSetIconListMethodInfo
    ResolveDialogMethod "setIconName" o = Gtk.Window.WindowSetIconNameMethodInfo
    ResolveDialogMethod "setKeepAbove" o = Gtk.Window.WindowSetKeepAboveMethodInfo
    ResolveDialogMethod "setKeepBelow" o = Gtk.Window.WindowSetKeepBelowMethodInfo
    ResolveDialogMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveDialogMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveDialogMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveDialogMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveDialogMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveDialogMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveDialogMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveDialogMethod "setMnemonicModifier" o = Gtk.Window.WindowSetMnemonicModifierMethodInfo
    ResolveDialogMethod "setMnemonicsVisible" o = Gtk.Window.WindowSetMnemonicsVisibleMethodInfo
    ResolveDialogMethod "setModal" o = Gtk.Window.WindowSetModalMethodInfo
    ResolveDialogMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveDialogMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveDialogMethod "setOpacity" o = Gtk.Window.WindowSetOpacityMethodInfo
    ResolveDialogMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveDialogMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveDialogMethod "setPosition" o = Gtk.Window.WindowSetPositionMethodInfo
    ResolveDialogMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveDialogMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveDialogMethod "setReallocateRedraws" o = Gtk.Container.ContainerSetReallocateRedrawsMethodInfo
    ResolveDialogMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveDialogMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveDialogMethod "setResizable" o = Gtk.Window.WindowSetResizableMethodInfo
    ResolveDialogMethod "setResizeMode" o = Gtk.Container.ContainerSetResizeModeMethodInfo
    ResolveDialogMethod "setResponseSensitive" o = DialogSetResponseSensitiveMethodInfo
    ResolveDialogMethod "setRole" o = Gtk.Window.WindowSetRoleMethodInfo
    ResolveDialogMethod "setScreen" o = Gtk.Window.WindowSetScreenMethodInfo
    ResolveDialogMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveDialogMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveDialogMethod "setSkipPagerHint" o = Gtk.Window.WindowSetSkipPagerHintMethodInfo
    ResolveDialogMethod "setSkipTaskbarHint" o = Gtk.Window.WindowSetSkipTaskbarHintMethodInfo
    ResolveDialogMethod "setStartupId" o = Gtk.Window.WindowSetStartupIdMethodInfo
    ResolveDialogMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveDialogMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveDialogMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveDialogMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveDialogMethod "setTitle" o = Gtk.Window.WindowSetTitleMethodInfo
    ResolveDialogMethod "setTitlebar" o = Gtk.Window.WindowSetTitlebarMethodInfo
    ResolveDialogMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveDialogMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveDialogMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveDialogMethod "setTransientFor" o = Gtk.Window.WindowSetTransientForMethodInfo
    ResolveDialogMethod "setTypeHint" o = Gtk.Window.WindowSetTypeHintMethodInfo
    ResolveDialogMethod "setUrgencyHint" o = Gtk.Window.WindowSetUrgencyHintMethodInfo
    ResolveDialogMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveDialogMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveDialogMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveDialogMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveDialogMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveDialogMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveDialogMethod "setWmclass" o = Gtk.Window.WindowSetWmclassMethodInfo
    ResolveDialogMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveDialogMethod t Dialog, O.OverloadedMethod info Dialog p) => OL.IsLabel t (Dialog -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveDialogMethod t Dialog, O.OverloadedMethod info Dialog p, R.HasField t Dialog p) => R.HasField t Dialog p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveDialogMethod t Dialog, O.OverloadedMethodInfo info Dialog) => OL.IsLabel t (O.MethodProxy info Dialog) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Dialog::close
-- | The [close](#g:signal:close) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user uses a keybinding to close
-- the dialog.
-- 
-- The default binding for this signal is the Escape key.
type DialogCloseCallback =
    IO ()

type C_DialogCloseCallback =
    Ptr Dialog ->                           -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_DialogCloseCallback`.
foreign import ccall "wrapper"
    mk_DialogCloseCallback :: C_DialogCloseCallback -> IO (FunPtr C_DialogCloseCallback)

wrap_DialogCloseCallback :: 
    GObject a => (a -> DialogCloseCallback) ->
    C_DialogCloseCallback
wrap_DialogCloseCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [close](#signal:close) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' dialog #close callback
-- @
-- 
-- 
onDialogClose :: (IsDialog a, MonadIO m) => a -> ((?self :: a) => DialogCloseCallback) -> m SignalHandlerId
onDialogClose obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_DialogCloseCallback wrapped
    wrapped'' <- mk_DialogCloseCallback wrapped'
    connectSignalFunPtr obj "close" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [close](#signal:close) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' dialog #close callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterDialogClose :: (IsDialog a, MonadIO m) => a -> ((?self :: a) => DialogCloseCallback) -> m SignalHandlerId
afterDialogClose obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_DialogCloseCallback wrapped
    wrapped'' <- mk_DialogCloseCallback wrapped'
    connectSignalFunPtr obj "close" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data DialogCloseSignalInfo
instance SignalInfo DialogCloseSignalInfo where
    type HaskellCallbackType DialogCloseSignalInfo = DialogCloseCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_DialogCloseCallback cb
        cb'' <- mk_DialogCloseCallback cb'
        connectSignalFunPtr obj "close" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Dialog::close"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Dialog.html#g:signal:close"})

#endif

-- signal Dialog::response
-- | Emitted when an action widget is clicked, the dialog receives a
-- delete event, or the application programmer calls 'GI.Gtk.Objects.Dialog.dialogResponse'.
-- On a delete event, the response ID is @/GTK_RESPONSE_DELETE_EVENT/@.
-- Otherwise, it depends on which action widget was clicked.
type DialogResponseCallback =
    Int32
    -- ^ /@responseId@/: the response ID
    -> IO ()

type C_DialogResponseCallback =
    Ptr Dialog ->                           -- object
    Int32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_DialogResponseCallback`.
foreign import ccall "wrapper"
    mk_DialogResponseCallback :: C_DialogResponseCallback -> IO (FunPtr C_DialogResponseCallback)

wrap_DialogResponseCallback :: 
    GObject a => (a -> DialogResponseCallback) ->
    C_DialogResponseCallback
wrap_DialogResponseCallback gi'cb gi'selfPtr responseId _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  responseId


-- | Connect a signal handler for the [response](#signal:response) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' dialog #response callback
-- @
-- 
-- 
onDialogResponse :: (IsDialog a, MonadIO m) => a -> ((?self :: a) => DialogResponseCallback) -> m SignalHandlerId
onDialogResponse obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_DialogResponseCallback wrapped
    wrapped'' <- mk_DialogResponseCallback wrapped'
    connectSignalFunPtr obj "response" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [response](#signal:response) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' dialog #response callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterDialogResponse :: (IsDialog a, MonadIO m) => a -> ((?self :: a) => DialogResponseCallback) -> m SignalHandlerId
afterDialogResponse obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_DialogResponseCallback wrapped
    wrapped'' <- mk_DialogResponseCallback wrapped'
    connectSignalFunPtr obj "response" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data DialogResponseSignalInfo
instance SignalInfo DialogResponseSignalInfo where
    type HaskellCallbackType DialogResponseSignalInfo = DialogResponseCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_DialogResponseCallback cb
        cb'' <- mk_DialogResponseCallback cb'
        connectSignalFunPtr obj "response" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Dialog::response"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Dialog.html#g:signal:response"})

#endif

-- VVV Prop "use-header-bar"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@use-header-bar@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' dialog #useHeaderBar
-- @
getDialogUseHeaderBar :: (MonadIO m, IsDialog o) => o -> m Int32
getDialogUseHeaderBar obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "use-header-bar"

-- | Construct a `GValueConstruct` with valid value for the “@use-header-bar@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructDialogUseHeaderBar :: (IsDialog o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructDialogUseHeaderBar val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "use-header-bar" val

#if defined(ENABLE_OVERLOADING)
data DialogUseHeaderBarPropertyInfo
instance AttrInfo DialogUseHeaderBarPropertyInfo where
    type AttrAllowedOps DialogUseHeaderBarPropertyInfo = '[ 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint DialogUseHeaderBarPropertyInfo = IsDialog
    type AttrSetTypeConstraint DialogUseHeaderBarPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint DialogUseHeaderBarPropertyInfo = (~) Int32
    type AttrTransferType DialogUseHeaderBarPropertyInfo = Int32
    type AttrGetType DialogUseHeaderBarPropertyInfo = Int32
    type AttrLabel DialogUseHeaderBarPropertyInfo = "use-header-bar"
    type AttrOrigin DialogUseHeaderBarPropertyInfo = Dialog
    attrGet = getDialogUseHeaderBar
    attrSet = undefined
    attrTransfer _ v = do
        return v
    attrConstruct = constructDialogUseHeaderBar
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Dialog.useHeaderBar"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Dialog.html#g:attr:useHeaderBar"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Dialog
type instance O.AttributeList Dialog = DialogAttributeList
type DialogAttributeList = ('[ '("acceptFocus", Gtk.Window.WindowAcceptFocusPropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("application", Gtk.Window.WindowApplicationPropertyInfo), '("attachedTo", Gtk.Window.WindowAttachedToPropertyInfo), '("borderWidth", Gtk.Container.ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", Gtk.Container.ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("decorated", Gtk.Window.WindowDecoratedPropertyInfo), '("defaultHeight", Gtk.Window.WindowDefaultHeightPropertyInfo), '("defaultWidth", Gtk.Window.WindowDefaultWidthPropertyInfo), '("deletable", Gtk.Window.WindowDeletablePropertyInfo), '("destroyWithParent", Gtk.Window.WindowDestroyWithParentPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("focusOnMap", Gtk.Window.WindowFocusOnMapPropertyInfo), '("focusVisible", Gtk.Window.WindowFocusVisiblePropertyInfo), '("gravity", Gtk.Window.WindowGravityPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasResizeGrip", Gtk.Window.WindowHasResizeGripPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("hasToplevelFocus", Gtk.Window.WindowHasToplevelFocusPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("hideTitlebarWhenMaximized", Gtk.Window.WindowHideTitlebarWhenMaximizedPropertyInfo), '("icon", Gtk.Window.WindowIconPropertyInfo), '("iconName", Gtk.Window.WindowIconNamePropertyInfo), '("isActive", Gtk.Window.WindowIsActivePropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("isMaximized", Gtk.Window.WindowIsMaximizedPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("mnemonicsVisible", Gtk.Window.WindowMnemonicsVisiblePropertyInfo), '("modal", Gtk.Window.WindowModalPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizable", Gtk.Window.WindowResizablePropertyInfo), '("resizeGripVisible", Gtk.Window.WindowResizeGripVisiblePropertyInfo), '("resizeMode", Gtk.Container.ContainerResizeModePropertyInfo), '("role", Gtk.Window.WindowRolePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("screen", Gtk.Window.WindowScreenPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("skipPagerHint", Gtk.Window.WindowSkipPagerHintPropertyInfo), '("skipTaskbarHint", Gtk.Window.WindowSkipTaskbarHintPropertyInfo), '("startupId", Gtk.Window.WindowStartupIdPropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("title", Gtk.Window.WindowTitlePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("transientFor", Gtk.Window.WindowTransientForPropertyInfo), '("type", Gtk.Window.WindowTypePropertyInfo), '("typeHint", Gtk.Window.WindowTypeHintPropertyInfo), '("urgencyHint", Gtk.Window.WindowUrgencyHintPropertyInfo), '("useHeaderBar", DialogUseHeaderBarPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("windowPosition", Gtk.Window.WindowWindowPositionPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
dialogUseHeaderBar :: AttrLabelProxy "useHeaderBar"
dialogUseHeaderBar = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Dialog = DialogSignalList
type DialogSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activateDefault", Gtk.Window.WindowActivateDefaultSignalInfo), '("activateFocus", Gtk.Window.WindowActivateFocusSignalInfo), '("add", Gtk.Container.ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", Gtk.Container.ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("close", DialogCloseSignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enableDebugging", Gtk.Window.WindowEnableDebuggingSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("keysChanged", Gtk.Window.WindowKeysChangedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", Gtk.Container.ContainerRemoveSignalInfo), '("response", DialogResponseSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocus", Gtk.Window.WindowSetFocusSignalInfo), '("setFocusChild", Gtk.Container.ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Dialog::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Dialog" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_dialog_new" gtk_dialog_new :: 
    IO (Ptr Dialog)

-- | Creates a new dialog box.
-- 
-- Widgets should not be packed into this t'GI.Gtk.Objects.Window.Window'
-- directly, but into the /@vbox@/ and /@actionArea@/, as described above.
dialogNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Dialog
    -- ^ __Returns:__ the new dialog as a t'GI.Gtk.Objects.Widget.Widget'
dialogNew  = liftIO $ do
    result <- gtk_dialog_new
    checkUnexpectedReturnNULL "dialogNew" result
    result' <- (newObject Dialog) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Dialog::add_action_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "dialog"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Dialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkDialog" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "an activatable widget"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "response_id"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "response ID for @child"
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

foreign import ccall "gtk_dialog_add_action_widget" gtk_dialog_add_action_widget :: 
    Ptr Dialog ->                           -- dialog : TInterface (Name {namespace = "Gtk", name = "Dialog"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- response_id : TBasicType TInt
    IO ()

-- | Adds an activatable widget to the action area of a t'GI.Gtk.Objects.Dialog.Dialog',
-- connecting a signal handler that will emit the [Dialog::response]("GI.Gtk.Objects.Dialog#g:signal:response")
-- signal on the dialog when the widget is activated. The widget is
-- appended to the end of the dialog’s action area. If you want to add a
-- non-activatable widget, simply pack it into the /@actionArea@/ field
-- of the t'GI.Gtk.Objects.Dialog.Dialog' struct.
dialogAddActionWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsDialog a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@dialog@/: a t'GI.Gtk.Objects.Dialog.Dialog'
    -> b
    -- ^ /@child@/: an activatable widget
    -> Int32
    -- ^ /@responseId@/: response ID for /@child@/
    -> m ()
dialogAddActionWidget dialog child responseId = liftIO $ do
    dialog' <- unsafeManagedPtrCastPtr dialog
    child' <- unsafeManagedPtrCastPtr child
    gtk_dialog_add_action_widget dialog' child' responseId
    touchManagedPtr dialog
    touchManagedPtr child
    return ()

#if defined(ENABLE_OVERLOADING)
data DialogAddActionWidgetMethodInfo
instance (signature ~ (b -> Int32 -> m ()), MonadIO m, IsDialog a, Gtk.Widget.IsWidget b) => O.OverloadedMethod DialogAddActionWidgetMethodInfo a signature where
    overloadedMethod = dialogAddActionWidget

instance O.OverloadedMethodInfo DialogAddActionWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Dialog.dialogAddActionWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Dialog.html#v:dialogAddActionWidget"
        })


#endif

-- method Dialog::add_button
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "dialog"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Dialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkDialog" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "button_text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "text of button" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "response_id"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "response ID for the button"
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

foreign import ccall "gtk_dialog_add_button" gtk_dialog_add_button :: 
    Ptr Dialog ->                           -- dialog : TInterface (Name {namespace = "Gtk", name = "Dialog"})
    CString ->                              -- button_text : TBasicType TUTF8
    Int32 ->                                -- response_id : TBasicType TInt
    IO (Ptr Gtk.Widget.Widget)

-- | Adds a button with the given text and sets things up so that
-- clicking the button will emit the [Dialog::response]("GI.Gtk.Objects.Dialog#g:signal:response") signal with
-- the given /@responseId@/. The button is appended to the end of the
-- dialog’s action area. The button widget is returned, but usually
-- you don’t need it.
dialogAddButton ::
    (B.CallStack.HasCallStack, MonadIO m, IsDialog a) =>
    a
    -- ^ /@dialog@/: a t'GI.Gtk.Objects.Dialog.Dialog'
    -> T.Text
    -- ^ /@buttonText@/: text of button
    -> Int32
    -- ^ /@responseId@/: response ID for the button
    -> m Gtk.Widget.Widget
    -- ^ __Returns:__ the t'GI.Gtk.Objects.Button.Button' widget that was added
dialogAddButton dialog buttonText responseId = liftIO $ do
    dialog' <- unsafeManagedPtrCastPtr dialog
    buttonText' <- textToCString buttonText
    result <- gtk_dialog_add_button dialog' buttonText' responseId
    checkUnexpectedReturnNULL "dialogAddButton" result
    result' <- (newObject Gtk.Widget.Widget) result
    touchManagedPtr dialog
    freeMem buttonText'
    return result'

#if defined(ENABLE_OVERLOADING)
data DialogAddButtonMethodInfo
instance (signature ~ (T.Text -> Int32 -> m Gtk.Widget.Widget), MonadIO m, IsDialog a) => O.OverloadedMethod DialogAddButtonMethodInfo a signature where
    overloadedMethod = dialogAddButton

instance O.OverloadedMethodInfo DialogAddButtonMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Dialog.dialogAddButton",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Dialog.html#v:dialogAddButton"
        })


#endif

-- method Dialog::get_action_area
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "dialog"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Dialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkDialog" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Box" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_dialog_get_action_area" gtk_dialog_get_action_area :: 
    Ptr Dialog ->                           -- dialog : TInterface (Name {namespace = "Gtk", name = "Dialog"})
    IO (Ptr Gtk.Box.Box)

{-# DEPRECATED dialogGetActionArea ["(Since version 3.12)","Direct access to the action area","  is discouraged; use 'GI.Gtk.Objects.Dialog.dialogAddButton', etc."] #-}
-- | Returns the action area of /@dialog@/.
-- 
-- /Since: 2.14/
dialogGetActionArea ::
    (B.CallStack.HasCallStack, MonadIO m, IsDialog a) =>
    a
    -- ^ /@dialog@/: a t'GI.Gtk.Objects.Dialog.Dialog'
    -> m Gtk.Box.Box
    -- ^ __Returns:__ the action area
dialogGetActionArea dialog = liftIO $ do
    dialog' <- unsafeManagedPtrCastPtr dialog
    result <- gtk_dialog_get_action_area dialog'
    checkUnexpectedReturnNULL "dialogGetActionArea" result
    result' <- (newObject Gtk.Box.Box) result
    touchManagedPtr dialog
    return result'

#if defined(ENABLE_OVERLOADING)
data DialogGetActionAreaMethodInfo
instance (signature ~ (m Gtk.Box.Box), MonadIO m, IsDialog a) => O.OverloadedMethod DialogGetActionAreaMethodInfo a signature where
    overloadedMethod = dialogGetActionArea

instance O.OverloadedMethodInfo DialogGetActionAreaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Dialog.dialogGetActionArea",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Dialog.html#v:dialogGetActionArea"
        })


#endif

-- method Dialog::get_content_area
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "dialog"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Dialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkDialog" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Box" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_dialog_get_content_area" gtk_dialog_get_content_area :: 
    Ptr Dialog ->                           -- dialog : TInterface (Name {namespace = "Gtk", name = "Dialog"})
    IO (Ptr Gtk.Box.Box)

-- | Returns the content area of /@dialog@/.
-- 
-- /Since: 2.14/
dialogGetContentArea ::
    (B.CallStack.HasCallStack, MonadIO m, IsDialog a) =>
    a
    -- ^ /@dialog@/: a t'GI.Gtk.Objects.Dialog.Dialog'
    -> m Gtk.Box.Box
    -- ^ __Returns:__ the content area t'GI.Gtk.Objects.Box.Box'.
dialogGetContentArea dialog = liftIO $ do
    dialog' <- unsafeManagedPtrCastPtr dialog
    result <- gtk_dialog_get_content_area dialog'
    checkUnexpectedReturnNULL "dialogGetContentArea" result
    result' <- (newObject Gtk.Box.Box) result
    touchManagedPtr dialog
    return result'

#if defined(ENABLE_OVERLOADING)
data DialogGetContentAreaMethodInfo
instance (signature ~ (m Gtk.Box.Box), MonadIO m, IsDialog a) => O.OverloadedMethod DialogGetContentAreaMethodInfo a signature where
    overloadedMethod = dialogGetContentArea

instance O.OverloadedMethodInfo DialogGetContentAreaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Dialog.dialogGetContentArea",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Dialog.html#v:dialogGetContentArea"
        })


#endif

-- method Dialog::get_header_bar
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "dialog"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Dialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkDialog" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "HeaderBar" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_dialog_get_header_bar" gtk_dialog_get_header_bar :: 
    Ptr Dialog ->                           -- dialog : TInterface (Name {namespace = "Gtk", name = "Dialog"})
    IO (Ptr Gtk.HeaderBar.HeaderBar)

-- | Returns the header bar of /@dialog@/. Note that the
-- headerbar is only used by the dialog if the
-- [Dialog:useHeaderBar]("GI.Gtk.Objects.Dialog#g:attr:useHeaderBar") property is 'P.True'.
-- 
-- /Since: 3.12/
dialogGetHeaderBar ::
    (B.CallStack.HasCallStack, MonadIO m, IsDialog a) =>
    a
    -- ^ /@dialog@/: a t'GI.Gtk.Objects.Dialog.Dialog'
    -> m Gtk.HeaderBar.HeaderBar
    -- ^ __Returns:__ the header bar
dialogGetHeaderBar dialog = liftIO $ do
    dialog' <- unsafeManagedPtrCastPtr dialog
    result <- gtk_dialog_get_header_bar dialog'
    checkUnexpectedReturnNULL "dialogGetHeaderBar" result
    result' <- (newObject Gtk.HeaderBar.HeaderBar) result
    touchManagedPtr dialog
    return result'

#if defined(ENABLE_OVERLOADING)
data DialogGetHeaderBarMethodInfo
instance (signature ~ (m Gtk.HeaderBar.HeaderBar), MonadIO m, IsDialog a) => O.OverloadedMethod DialogGetHeaderBarMethodInfo a signature where
    overloadedMethod = dialogGetHeaderBar

instance O.OverloadedMethodInfo DialogGetHeaderBarMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Dialog.dialogGetHeaderBar",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Dialog.html#v:dialogGetHeaderBar"
        })


#endif

-- method Dialog::get_response_for_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "dialog"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Dialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkDialog" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a widget in the action area of @dialog"
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

foreign import ccall "gtk_dialog_get_response_for_widget" gtk_dialog_get_response_for_widget :: 
    Ptr Dialog ->                           -- dialog : TInterface (Name {namespace = "Gtk", name = "Dialog"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO Int32

-- | Gets the response id of a widget in the action area
-- of a dialog.
-- 
-- /Since: 2.8/
dialogGetResponseForWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsDialog a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@dialog@/: a t'GI.Gtk.Objects.Dialog.Dialog'
    -> b
    -- ^ /@widget@/: a widget in the action area of /@dialog@/
    -> m Int32
    -- ^ __Returns:__ the response id of /@widget@/, or 'GI.Gtk.Enums.ResponseTypeNone'
    --  if /@widget@/ doesn’t have a response id set.
dialogGetResponseForWidget dialog widget = liftIO $ do
    dialog' <- unsafeManagedPtrCastPtr dialog
    widget' <- unsafeManagedPtrCastPtr widget
    result <- gtk_dialog_get_response_for_widget dialog' widget'
    touchManagedPtr dialog
    touchManagedPtr widget
    return result

#if defined(ENABLE_OVERLOADING)
data DialogGetResponseForWidgetMethodInfo
instance (signature ~ (b -> m Int32), MonadIO m, IsDialog a, Gtk.Widget.IsWidget b) => O.OverloadedMethod DialogGetResponseForWidgetMethodInfo a signature where
    overloadedMethod = dialogGetResponseForWidget

instance O.OverloadedMethodInfo DialogGetResponseForWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Dialog.dialogGetResponseForWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Dialog.html#v:dialogGetResponseForWidget"
        })


#endif

-- method Dialog::get_widget_for_response
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "dialog"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Dialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkDialog" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "response_id"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the response ID used by the @dialog widget"
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

foreign import ccall "gtk_dialog_get_widget_for_response" gtk_dialog_get_widget_for_response :: 
    Ptr Dialog ->                           -- dialog : TInterface (Name {namespace = "Gtk", name = "Dialog"})
    Int32 ->                                -- response_id : TBasicType TInt
    IO (Ptr Gtk.Widget.Widget)

-- | Gets the widget button that uses the given response ID in the action area
-- of a dialog.
-- 
-- /Since: 2.20/
dialogGetWidgetForResponse ::
    (B.CallStack.HasCallStack, MonadIO m, IsDialog a) =>
    a
    -- ^ /@dialog@/: a t'GI.Gtk.Objects.Dialog.Dialog'
    -> Int32
    -- ^ /@responseId@/: the response ID used by the /@dialog@/ widget
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the /@widget@/ button that uses the given
    --     /@responseId@/, or 'P.Nothing'.
dialogGetWidgetForResponse dialog responseId = liftIO $ do
    dialog' <- unsafeManagedPtrCastPtr dialog
    result <- gtk_dialog_get_widget_for_response dialog' responseId
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr dialog
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data DialogGetWidgetForResponseMethodInfo
instance (signature ~ (Int32 -> m (Maybe Gtk.Widget.Widget)), MonadIO m, IsDialog a) => O.OverloadedMethod DialogGetWidgetForResponseMethodInfo a signature where
    overloadedMethod = dialogGetWidgetForResponse

instance O.OverloadedMethodInfo DialogGetWidgetForResponseMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Dialog.dialogGetWidgetForResponse",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Dialog.html#v:dialogGetWidgetForResponse"
        })


#endif

-- method Dialog::response
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "dialog"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Dialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkDialog" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "response_id"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "response ID" , sinceVersion = Nothing }
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

foreign import ccall "gtk_dialog_response" gtk_dialog_response :: 
    Ptr Dialog ->                           -- dialog : TInterface (Name {namespace = "Gtk", name = "Dialog"})
    Int32 ->                                -- response_id : TBasicType TInt
    IO ()

-- | Emits the [Dialog::response]("GI.Gtk.Objects.Dialog#g:signal:response") signal with the given response ID.
-- Used to indicate that the user has responded to the dialog in some way;
-- typically either you or 'GI.Gtk.Objects.Dialog.dialogRun' will be monitoring the
-- [response](#g:signal:response) signal and take appropriate action.
dialogResponse ::
    (B.CallStack.HasCallStack, MonadIO m, IsDialog a) =>
    a
    -- ^ /@dialog@/: a t'GI.Gtk.Objects.Dialog.Dialog'
    -> Int32
    -- ^ /@responseId@/: response ID
    -> m ()
dialogResponse dialog responseId = liftIO $ do
    dialog' <- unsafeManagedPtrCastPtr dialog
    gtk_dialog_response dialog' responseId
    touchManagedPtr dialog
    return ()

#if defined(ENABLE_OVERLOADING)
data DialogResponseMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsDialog a) => O.OverloadedMethod DialogResponseMethodInfo a signature where
    overloadedMethod = dialogResponse

instance O.OverloadedMethodInfo DialogResponseMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Dialog.dialogResponse",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Dialog.html#v:dialogResponse"
        })


#endif

-- method Dialog::run
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "dialog"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Dialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkDialog" , sinceVersion = Nothing }
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

foreign import ccall "gtk_dialog_run" gtk_dialog_run :: 
    Ptr Dialog ->                           -- dialog : TInterface (Name {namespace = "Gtk", name = "Dialog"})
    IO Int32

-- | Blocks in a recursive main loop until the /@dialog@/ either emits the
-- [Dialog::response]("GI.Gtk.Objects.Dialog#g:signal:response") signal, or is destroyed. If the dialog is
-- destroyed during the call to 'GI.Gtk.Objects.Dialog.dialogRun', 'GI.Gtk.Objects.Dialog.dialogRun' returns
-- @/GTK_RESPONSE_NONE/@. Otherwise, it returns the response ID from the
-- [response](#g:signal:response) signal emission.
-- 
-- Before entering the recursive main loop, 'GI.Gtk.Objects.Dialog.dialogRun' calls
-- 'GI.Gtk.Objects.Widget.widgetShow' on the dialog for you. Note that you still
-- need to show any children of the dialog yourself.
-- 
-- During 'GI.Gtk.Objects.Dialog.dialogRun', the default behavior of [Widget::deleteEvent]("GI.Gtk.Objects.Widget#g:signal:deleteEvent")
-- is disabled; if the dialog receives [delete_event](#g:signal:delete_event), it will not be
-- destroyed as windows usually are, and 'GI.Gtk.Objects.Dialog.dialogRun' will return
-- @/GTK_RESPONSE_DELETE_EVENT/@. Also, during 'GI.Gtk.Objects.Dialog.dialogRun' the dialog
-- will be modal. You can force 'GI.Gtk.Objects.Dialog.dialogRun' to return at any time by
-- calling 'GI.Gtk.Objects.Dialog.dialogResponse' to emit the [response](#g:signal:response) signal. Destroying
-- the dialog during 'GI.Gtk.Objects.Dialog.dialogRun' is a very bad idea, because your
-- post-run code won’t know whether the dialog was destroyed or not.
-- 
-- After 'GI.Gtk.Objects.Dialog.dialogRun' returns, you are responsible for hiding or
-- destroying the dialog if you wish to do so.
-- 
-- Typical usage of this function might be:
-- 
-- === /C code/
-- >
-- >  GtkWidget *dialog = gtk_dialog_new ();
-- >  // Set up dialog...
-- >
-- >  int result = gtk_dialog_run (GTK_DIALOG (dialog));
-- >  switch (result)
-- >    {
-- >      case GTK_RESPONSE_ACCEPT:
-- >         // do_application_specific_something ();
-- >         break;
-- >      default:
-- >         // do_nothing_since_dialog_was_cancelled ();
-- >         break;
-- >    }
-- >  gtk_widget_destroy (dialog);
-- 
-- 
-- Note that even though the recursive main loop gives the effect of a
-- modal dialog (it prevents the user from interacting with other
-- windows in the same window group while the dialog is run), callbacks
-- such as timeouts, IO channel watches, DND drops, etc, will
-- be triggered during a 'GI.Gtk.Objects.Dialog.dialogRun' call.
dialogRun ::
    (B.CallStack.HasCallStack, MonadIO m, IsDialog a) =>
    a
    -- ^ /@dialog@/: a t'GI.Gtk.Objects.Dialog.Dialog'
    -> m Int32
    -- ^ __Returns:__ response ID
dialogRun dialog = liftIO $ do
    dialog' <- unsafeManagedPtrCastPtr dialog
    result <- gtk_dialog_run dialog'
    touchManagedPtr dialog
    return result

#if defined(ENABLE_OVERLOADING)
data DialogRunMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsDialog a) => O.OverloadedMethod DialogRunMethodInfo a signature where
    overloadedMethod = dialogRun

instance O.OverloadedMethodInfo DialogRunMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Dialog.dialogRun",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Dialog.html#v:dialogRun"
        })


#endif

-- method Dialog::set_alternative_button_order_from_array
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "dialog"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Dialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkDialog" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_params"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the number of response ids in @new_order"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "new_order"
--           , argType = TCArray False (-1) 1 (TBasicType TInt)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "an array of response ids of\n    @dialog\8217s buttons"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "n_params"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "the number of response ids in @new_order"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_dialog_set_alternative_button_order_from_array" gtk_dialog_set_alternative_button_order_from_array :: 
    Ptr Dialog ->                           -- dialog : TInterface (Name {namespace = "Gtk", name = "Dialog"})
    Int32 ->                                -- n_params : TBasicType TInt
    Ptr Int32 ->                            -- new_order : TCArray False (-1) 1 (TBasicType TInt)
    IO ()

{-# DEPRECATED dialogSetAlternativeButtonOrderFromArray ["(Since version 3.10)","Deprecated"] #-}
-- | Sets an alternative button order. If the
-- [Settings:gtkAlternativeButtonOrder]("GI.Gtk.Objects.Settings#g:attr:gtkAlternativeButtonOrder") setting is set to 'P.True',
-- the dialog buttons are reordered according to the order of the
-- response ids in /@newOrder@/.
-- 
-- See @/gtk_dialog_set_alternative_button_order()/@ for more information.
-- 
-- This function is for use by language bindings.
-- 
-- /Since: 2.6/
dialogSetAlternativeButtonOrderFromArray ::
    (B.CallStack.HasCallStack, MonadIO m, IsDialog a) =>
    a
    -- ^ /@dialog@/: a t'GI.Gtk.Objects.Dialog.Dialog'
    -> [Int32]
    -- ^ /@newOrder@/: an array of response ids of
    --     /@dialog@/’s buttons
    -> m ()
dialogSetAlternativeButtonOrderFromArray dialog newOrder = liftIO $ do
    let nParams = fromIntegral $ P.length newOrder
    dialog' <- unsafeManagedPtrCastPtr dialog
    newOrder' <- packStorableArray newOrder
    gtk_dialog_set_alternative_button_order_from_array dialog' nParams newOrder'
    touchManagedPtr dialog
    freeMem newOrder'
    return ()

#if defined(ENABLE_OVERLOADING)
data DialogSetAlternativeButtonOrderFromArrayMethodInfo
instance (signature ~ ([Int32] -> m ()), MonadIO m, IsDialog a) => O.OverloadedMethod DialogSetAlternativeButtonOrderFromArrayMethodInfo a signature where
    overloadedMethod = dialogSetAlternativeButtonOrderFromArray

instance O.OverloadedMethodInfo DialogSetAlternativeButtonOrderFromArrayMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Dialog.dialogSetAlternativeButtonOrderFromArray",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Dialog.html#v:dialogSetAlternativeButtonOrderFromArray"
        })


#endif

-- method Dialog::set_default_response
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "dialog"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Dialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkDialog" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "response_id"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a response ID" , sinceVersion = Nothing }
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

foreign import ccall "gtk_dialog_set_default_response" gtk_dialog_set_default_response :: 
    Ptr Dialog ->                           -- dialog : TInterface (Name {namespace = "Gtk", name = "Dialog"})
    Int32 ->                                -- response_id : TBasicType TInt
    IO ()

-- | Sets the last widget in the dialog’s action area with the given /@responseId@/
-- as the default widget for the dialog. Pressing “Enter” normally activates
-- the default widget.
dialogSetDefaultResponse ::
    (B.CallStack.HasCallStack, MonadIO m, IsDialog a) =>
    a
    -- ^ /@dialog@/: a t'GI.Gtk.Objects.Dialog.Dialog'
    -> Int32
    -- ^ /@responseId@/: a response ID
    -> m ()
dialogSetDefaultResponse dialog responseId = liftIO $ do
    dialog' <- unsafeManagedPtrCastPtr dialog
    gtk_dialog_set_default_response dialog' responseId
    touchManagedPtr dialog
    return ()

#if defined(ENABLE_OVERLOADING)
data DialogSetDefaultResponseMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsDialog a) => O.OverloadedMethod DialogSetDefaultResponseMethodInfo a signature where
    overloadedMethod = dialogSetDefaultResponse

instance O.OverloadedMethodInfo DialogSetDefaultResponseMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Dialog.dialogSetDefaultResponse",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Dialog.html#v:dialogSetDefaultResponse"
        })


#endif

-- method Dialog::set_response_sensitive
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "dialog"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Dialog" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkDialog" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "response_id"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a response ID" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "%TRUE for sensitive"
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

foreign import ccall "gtk_dialog_set_response_sensitive" gtk_dialog_set_response_sensitive :: 
    Ptr Dialog ->                           -- dialog : TInterface (Name {namespace = "Gtk", name = "Dialog"})
    Int32 ->                                -- response_id : TBasicType TInt
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Calls @gtk_widget_set_sensitive (widget, \@setting)@
-- for each widget in the dialog’s action area with the given /@responseId@/.
-- A convenient way to sensitize\/desensitize dialog buttons.
dialogSetResponseSensitive ::
    (B.CallStack.HasCallStack, MonadIO m, IsDialog a) =>
    a
    -- ^ /@dialog@/: a t'GI.Gtk.Objects.Dialog.Dialog'
    -> Int32
    -- ^ /@responseId@/: a response ID
    -> Bool
    -- ^ /@setting@/: 'P.True' for sensitive
    -> m ()
dialogSetResponseSensitive dialog responseId setting = liftIO $ do
    dialog' <- unsafeManagedPtrCastPtr dialog
    let setting' = (fromIntegral . fromEnum) setting
    gtk_dialog_set_response_sensitive dialog' responseId setting'
    touchManagedPtr dialog
    return ()

#if defined(ENABLE_OVERLOADING)
data DialogSetResponseSensitiveMethodInfo
instance (signature ~ (Int32 -> Bool -> m ()), MonadIO m, IsDialog a) => O.OverloadedMethod DialogSetResponseSensitiveMethodInfo a signature where
    overloadedMethod = dialogSetResponseSensitive

instance O.OverloadedMethodInfo DialogSetResponseSensitiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Dialog.dialogSetResponseSensitive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Dialog.html#v:dialogSetResponseSensitive"
        })


#endif


