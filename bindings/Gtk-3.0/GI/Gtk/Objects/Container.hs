{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A GTK+ user interface is constructed by nesting widgets inside widgets.
-- Container widgets are the inner nodes in the resulting tree of widgets:
-- they contain other widgets. So, for example, you might have a t'GI.Gtk.Objects.Window.Window'
-- containing a t'GI.Gtk.Objects.Frame.Frame' containing a t'GI.Gtk.Objects.Label.Label'. If you wanted an image instead
-- of a textual label inside the frame, you might replace the t'GI.Gtk.Objects.Label.Label' widget
-- with a t'GI.Gtk.Objects.Image.Image' widget.
-- 
-- There are two major kinds of container widgets in GTK+. Both are subclasses
-- of the abstract GtkContainer base class.
-- 
-- The first type of container widget has a single child widget and derives
-- from t'GI.Gtk.Objects.Bin.Bin'. These containers are decorators, which
-- add some kind of functionality to the child. For example, a t'GI.Gtk.Objects.Button.Button' makes
-- its child into a clickable button; a t'GI.Gtk.Objects.Frame.Frame' draws a frame around its child
-- and a t'GI.Gtk.Objects.Window.Window' places its child widget inside a top-level window.
-- 
-- The second type of container can have more than one child; its purpose is to
-- manage layout. This means that these containers assign
-- sizes and positions to their children. For example, a t'GI.Gtk.Objects.HBox.HBox' arranges its
-- children in a horizontal row, and a t'GI.Gtk.Objects.Grid.Grid' arranges the widgets it contains
-- in a two-dimensional grid.
-- 
-- For implementations of t'GI.Gtk.Objects.Container.Container' the virtual method t'GI.Gtk.Structs.ContainerClass.ContainerClass'.@/forall/@()
-- is always required, since it\'s used for drawing and other internal operations
-- on the children.
-- If the t'GI.Gtk.Objects.Container.Container' implementation expect to have non internal children
-- it\'s needed to implement both t'GI.Gtk.Structs.ContainerClass.ContainerClass'.@/add/@() and t'GI.Gtk.Structs.ContainerClass.ContainerClass'.@/remove/@().
-- If the GtkContainer implementation has internal children, they should be added
-- with 'GI.Gtk.Objects.Widget.widgetSetParent' on @/init()/@ and removed with 'GI.Gtk.Objects.Widget.widgetUnparent'
-- in the t'GI.Gtk.Structs.WidgetClass.WidgetClass'.@/destroy/@() implementation.
-- See more about implementing custom widgets at https:\/\/wiki.gnome.org\/HowDoI\/CustomWidgets
-- 
-- = Height for width geometry management
-- 
-- GTK+ uses a height-for-width (and width-for-height) geometry management system.
-- Height-for-width means that a widget can change how much vertical space it needs,
-- depending on the amount of horizontal space that it is given (and similar for
-- width-for-height).
-- 
-- There are some things to keep in mind when implementing container widgets
-- that make use of GTK+’s height for width geometry management system. First,
-- it’s important to note that a container must prioritize one of its
-- dimensions, that is to say that a widget or container can only have a
-- t'GI.Gtk.Enums.SizeRequestMode' that is 'GI.Gtk.Enums.SizeRequestModeHeightForWidth' or
-- 'GI.Gtk.Enums.SizeRequestModeWidthForHeight'. However, every widget and container
-- must be able to respond to the APIs for both dimensions, i.e. even if a
-- widget has a request mode that is height-for-width, it is possible that
-- its parent will request its sizes using the width-for-height APIs.
-- 
-- To ensure that everything works properly, here are some guidelines to follow
-- when implementing height-for-width (or width-for-height) containers.
-- 
-- Each request mode involves 2 virtual methods. Height-for-width apis run
-- through 'GI.Gtk.Objects.Widget.widgetGetPreferredWidth' and then through 'GI.Gtk.Objects.Widget.widgetGetPreferredHeightForWidth'.
-- When handling requests in the opposite t'GI.Gtk.Enums.SizeRequestMode' it is important that
-- every widget request at least enough space to display all of its content at all times.
-- 
-- When 'GI.Gtk.Objects.Widget.widgetGetPreferredHeight' is called on a container that is height-for-width,
-- the container must return the height for its minimum width. This is easily achieved by
-- simply calling the reverse apis implemented for itself as follows:
-- 
-- 
-- === /C code/
-- >
-- >static void
-- >foo_container_get_preferred_height (GtkWidget *widget,
-- >                                    gint *min_height,
-- >                                    gint *nat_height)
-- >{
-- >   if (i_am_in_height_for_width_mode)
-- >     {
-- >       gint min_width;
-- >
-- >       GTK_WIDGET_GET_CLASS (widget)->get_preferred_width (widget,
-- >                                                           &min_width,
-- >                                                           NULL);
-- >       GTK_WIDGET_GET_CLASS (widget)->get_preferred_height_for_width
-- >                                                          (widget,
-- >                                                           min_width,
-- >                                                           min_height,
-- >                                                           nat_height);
-- >     }
-- >   else
-- >     {
-- >       ... many containers support both request modes, execute the
-- >       real width-for-height request here by returning the
-- >       collective heights of all widgets that are stacked
-- >       vertically (or whatever is appropriate for this container)
-- >       ...
-- >     }
-- >}
-- 
-- 
-- Similarly, when 'GI.Gtk.Objects.Widget.widgetGetPreferredWidthForHeight' is called for a container or widget
-- that is height-for-width, it then only needs to return the base minimum width like so:
-- 
-- 
-- === /C code/
-- >
-- >static void
-- >foo_container_get_preferred_width_for_height (GtkWidget *widget,
-- >                                              gint for_height,
-- >                                              gint *min_width,
-- >                                              gint *nat_width)
-- >{
-- >   if (i_am_in_height_for_width_mode)
-- >     {
-- >       GTK_WIDGET_GET_CLASS (widget)->get_preferred_width (widget,
-- >                                                           min_width,
-- >                                                           nat_width);
-- >     }
-- >   else
-- >     {
-- >       ... execute the real width-for-height request here based on
-- >       the required width of the children collectively if the
-- >       container were to be allocated the said height ...
-- >     }
-- >}
-- 
-- 
-- Height for width requests are generally implemented in terms of a virtual allocation
-- of widgets in the input orientation. Assuming an height-for-width request mode, a container
-- would implement the @/get_preferred_height_for_width()/@ virtual function by first calling
-- 'GI.Gtk.Objects.Widget.widgetGetPreferredWidth' for each of its children.
-- 
-- For each potential group of children that are lined up horizontally, the values returned by
-- 'GI.Gtk.Objects.Widget.widgetGetPreferredWidth' should be collected in an array of t'GI.Gtk.Structs.RequestedSize.RequestedSize' structures.
-- Any child spacing should be removed from the input /@forWidth@/ and then the collective size should be
-- allocated using the 'GI.Gtk.Functions.distributeNaturalAllocation' convenience function.
-- 
-- The container will then move on to request the preferred height for each child by using
-- 'GI.Gtk.Objects.Widget.widgetGetPreferredHeightForWidth' and using the sizes stored in the t'GI.Gtk.Structs.RequestedSize.RequestedSize' array.
-- 
-- To allocate a height-for-width container, it’s again important
-- to consider that a container must prioritize one dimension over the other. So if
-- a container is a height-for-width container it must first allocate all widgets horizontally
-- using a t'GI.Gtk.Structs.RequestedSize.RequestedSize' array and 'GI.Gtk.Functions.distributeNaturalAllocation' and then add any
-- extra space (if and where appropriate) for the widget to expand.
-- 
-- After adding all the expand space, the container assumes it was allocated sufficient
-- height to fit all of its content. At this time, the container must use the total horizontal sizes
-- of each widget to request the height-for-width of each of its children and store the requests in a
-- t'GI.Gtk.Structs.RequestedSize.RequestedSize' array for any widgets that stack vertically (for tabular containers this can
-- be generalized into the heights and widths of rows and columns).
-- The vertical space must then again be distributed using 'GI.Gtk.Functions.distributeNaturalAllocation'
-- while this time considering the allocated height of the widget minus any vertical spacing
-- that the container adds. Then vertical expand space should be added where appropriate and available
-- and the container should go on to actually allocating the child widgets.
-- 
-- See [GtkWidget’s geometry management section][geometry-management]
-- to learn more about implementing height-for-width geometry management for widgets.
-- 
-- = Child properties
-- 
-- GtkContainer introduces child properties.
-- These are object properties that are not specific
-- to either the container or the contained widget, but rather to their relation.
-- Typical examples of child properties are the position or pack-type of a widget
-- which is contained in a t'GI.Gtk.Objects.Box.Box'.
-- 
-- Use 'GI.Gtk.Structs.ContainerClass.containerClassInstallChildProperty' to install child properties
-- for a container class and 'GI.Gtk.Structs.ContainerClass.containerClassFindChildProperty' or
-- 'GI.Gtk.Structs.ContainerClass.containerClassListChildProperties' to get information about existing
-- child properties.
-- 
-- To set the value of a child property, use 'GI.Gtk.Objects.Container.containerChildSetProperty',
-- @/gtk_container_child_set()/@ or @/gtk_container_child_set_valist()/@.
-- To obtain the value of a child property, use
-- 'GI.Gtk.Objects.Container.containerChildGetProperty', @/gtk_container_child_get()/@ or
-- @/gtk_container_child_get_valist()/@. To emit notification about child property
-- changes, use 'GI.Gtk.Objects.Widget.widgetChildNotify'.
-- 
-- = GtkContainer as GtkBuildable
-- 
-- The GtkContainer implementation of the GtkBuildable interface supports
-- a @\<packing>@ element for children, which can contain multiple @\<property>@
-- elements that specify child properties for the child.
-- 
-- Since 2.16, child properties can also be marked as translatable using
-- the same “translatable”, “comments” and “context” attributes that are used
-- for regular properties.
-- 
-- Since 3.16, containers can have a @\<focus-chain>@ element containing multiple
-- @\<widget>@ elements, one for each child that should be added to the focus
-- chain. The ”name” attribute gives the id of the widget.
-- 
-- An example of these properties in UI definitions:
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkBox">
-- >  <child>
-- >    <object class="GtkEntry" id="entry1"/>
-- >    <packing>
-- >      <property name="pack-type">start</property>
-- >    </packing>
-- >  </child>
-- >  <child>
-- >    <object class="GtkEntry" id="entry2"/>
-- >  </child>
-- >  <focus-chain>
-- >    <widget name="entry1"/>
-- >    <widget name="entry2"/>
-- >  </focus-chain>
-- ></object>
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Container
    ( 

-- * Exported types
    Container(..)                           ,
    IsContainer                             ,
    toContainer                             ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [add]("GI.Gtk.Objects.Container#g:method:add"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [checkResize]("GI.Gtk.Objects.Container#g:method:checkResize"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childGetProperty]("GI.Gtk.Objects.Container#g:method:childGetProperty"), [childNotify]("GI.Gtk.Objects.Container#g:method:childNotify"), [childNotifyByPspec]("GI.Gtk.Objects.Container#g:method:childNotifyByPspec"), [childSetProperty]("GI.Gtk.Objects.Container#g:method:childSetProperty"), [childType]("GI.Gtk.Objects.Container#g:method:childType"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forall]("GI.Gtk.Objects.Container#g:method:forall"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Objects.Container#g:method:foreach"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [propagateDraw]("GI.Gtk.Objects.Container#g:method:propagateDraw"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [remove]("GI.Gtk.Objects.Container#g:method:remove"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [resizeChildren]("GI.Gtk.Objects.Container#g:method:resizeChildren"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetFocusChain]("GI.Gtk.Objects.Container#g:method:unsetFocusChain"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getBorderWidth]("GI.Gtk.Objects.Container#g:method:getBorderWidth"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getChildren]("GI.Gtk.Objects.Container#g:method:getChildren"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusChain]("GI.Gtk.Objects.Container#g:method:getFocusChain"), [getFocusChild]("GI.Gtk.Objects.Container#g:method:getFocusChild"), [getFocusHadjustment]("GI.Gtk.Objects.Container#g:method:getFocusHadjustment"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFocusVadjustment]("GI.Gtk.Objects.Container#g:method:getFocusVadjustment"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPathForChild]("GI.Gtk.Objects.Container#g:method:getPathForChild"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getResizeMode]("GI.Gtk.Objects.Container#g:method:getResizeMode"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setBorderWidth]("GI.Gtk.Objects.Container#g:method:setBorderWidth"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusChain]("GI.Gtk.Objects.Container#g:method:setFocusChain"), [setFocusChild]("GI.Gtk.Objects.Container#g:method:setFocusChild"), [setFocusHadjustment]("GI.Gtk.Objects.Container#g:method:setFocusHadjustment"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFocusVadjustment]("GI.Gtk.Objects.Container#g:method:setFocusVadjustment"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReallocateRedraws]("GI.Gtk.Objects.Container#g:method:setReallocateRedraws"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setResizeMode]("GI.Gtk.Objects.Container#g:method:setResizeMode"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow").

#if defined(ENABLE_OVERLOADING)
    ResolveContainerMethod                  ,
#endif

-- ** add #method:add#

#if defined(ENABLE_OVERLOADING)
    ContainerAddMethodInfo                  ,
#endif
    containerAdd                            ,


-- ** checkResize #method:checkResize#

#if defined(ENABLE_OVERLOADING)
    ContainerCheckResizeMethodInfo          ,
#endif
    containerCheckResize                    ,


-- ** childGetProperty #method:childGetProperty#

#if defined(ENABLE_OVERLOADING)
    ContainerChildGetPropertyMethodInfo     ,
#endif
    containerChildGetProperty               ,


-- ** childNotify #method:childNotify#

#if defined(ENABLE_OVERLOADING)
    ContainerChildNotifyMethodInfo          ,
#endif
    containerChildNotify                    ,


-- ** childNotifyByPspec #method:childNotifyByPspec#

#if defined(ENABLE_OVERLOADING)
    ContainerChildNotifyByPspecMethodInfo   ,
#endif
    containerChildNotifyByPspec             ,


-- ** childSetProperty #method:childSetProperty#

#if defined(ENABLE_OVERLOADING)
    ContainerChildSetPropertyMethodInfo     ,
#endif
    containerChildSetProperty               ,


-- ** childType #method:childType#

#if defined(ENABLE_OVERLOADING)
    ContainerChildTypeMethodInfo            ,
#endif
    containerChildType                      ,


-- ** forall #method:forall#

#if defined(ENABLE_OVERLOADING)
    ContainerForallMethodInfo               ,
#endif
    containerForall                         ,


-- ** foreach #method:foreach#

#if defined(ENABLE_OVERLOADING)
    ContainerForeachMethodInfo              ,
#endif
    containerForeach                        ,


-- ** getBorderWidth #method:getBorderWidth#

#if defined(ENABLE_OVERLOADING)
    ContainerGetBorderWidthMethodInfo       ,
#endif
    containerGetBorderWidth                 ,


-- ** getChildren #method:getChildren#

#if defined(ENABLE_OVERLOADING)
    ContainerGetChildrenMethodInfo          ,
#endif
    containerGetChildren                    ,


-- ** getFocusChain #method:getFocusChain#

#if defined(ENABLE_OVERLOADING)
    ContainerGetFocusChainMethodInfo        ,
#endif
    containerGetFocusChain                  ,


-- ** getFocusChild #method:getFocusChild#

#if defined(ENABLE_OVERLOADING)
    ContainerGetFocusChildMethodInfo        ,
#endif
    containerGetFocusChild                  ,


-- ** getFocusHadjustment #method:getFocusHadjustment#

#if defined(ENABLE_OVERLOADING)
    ContainerGetFocusHadjustmentMethodInfo  ,
#endif
    containerGetFocusHadjustment            ,


-- ** getFocusVadjustment #method:getFocusVadjustment#

#if defined(ENABLE_OVERLOADING)
    ContainerGetFocusVadjustmentMethodInfo  ,
#endif
    containerGetFocusVadjustment            ,


-- ** getPathForChild #method:getPathForChild#

#if defined(ENABLE_OVERLOADING)
    ContainerGetPathForChildMethodInfo      ,
#endif
    containerGetPathForChild                ,


-- ** getResizeMode #method:getResizeMode#

#if defined(ENABLE_OVERLOADING)
    ContainerGetResizeModeMethodInfo        ,
#endif
    containerGetResizeMode                  ,


-- ** propagateDraw #method:propagateDraw#

#if defined(ENABLE_OVERLOADING)
    ContainerPropagateDrawMethodInfo        ,
#endif
    containerPropagateDraw                  ,


-- ** remove #method:remove#

#if defined(ENABLE_OVERLOADING)
    ContainerRemoveMethodInfo               ,
#endif
    containerRemove                         ,


-- ** resizeChildren #method:resizeChildren#

#if defined(ENABLE_OVERLOADING)
    ContainerResizeChildrenMethodInfo       ,
#endif
    containerResizeChildren                 ,


-- ** setBorderWidth #method:setBorderWidth#

#if defined(ENABLE_OVERLOADING)
    ContainerSetBorderWidthMethodInfo       ,
#endif
    containerSetBorderWidth                 ,


-- ** setFocusChain #method:setFocusChain#

#if defined(ENABLE_OVERLOADING)
    ContainerSetFocusChainMethodInfo        ,
#endif
    containerSetFocusChain                  ,


-- ** setFocusChild #method:setFocusChild#

#if defined(ENABLE_OVERLOADING)
    ContainerSetFocusChildMethodInfo        ,
#endif
    containerSetFocusChild                  ,


-- ** setFocusHadjustment #method:setFocusHadjustment#

#if defined(ENABLE_OVERLOADING)
    ContainerSetFocusHadjustmentMethodInfo  ,
#endif
    containerSetFocusHadjustment            ,


-- ** setFocusVadjustment #method:setFocusVadjustment#

#if defined(ENABLE_OVERLOADING)
    ContainerSetFocusVadjustmentMethodInfo  ,
#endif
    containerSetFocusVadjustment            ,


-- ** setReallocateRedraws #method:setReallocateRedraws#

#if defined(ENABLE_OVERLOADING)
    ContainerSetReallocateRedrawsMethodInfo ,
#endif
    containerSetReallocateRedraws           ,


-- ** setResizeMode #method:setResizeMode#

#if defined(ENABLE_OVERLOADING)
    ContainerSetResizeModeMethodInfo        ,
#endif
    containerSetResizeMode                  ,


-- ** unsetFocusChain #method:unsetFocusChain#

#if defined(ENABLE_OVERLOADING)
    ContainerUnsetFocusChainMethodInfo      ,
#endif
    containerUnsetFocusChain                ,




 -- * Properties


-- ** borderWidth #attr:borderWidth#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ContainerBorderWidthPropertyInfo        ,
#endif
    constructContainerBorderWidth           ,
#if defined(ENABLE_OVERLOADING)
    containerBorderWidth                    ,
#endif
    getContainerBorderWidth                 ,
    setContainerBorderWidth                 ,


-- ** child #attr:child#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ContainerChildPropertyInfo              ,
#endif
    clearContainerChild                     ,
    constructContainerChild                 ,
#if defined(ENABLE_OVERLOADING)
    containerChild                          ,
#endif
    setContainerChild                       ,


-- ** resizeMode #attr:resizeMode#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    ContainerResizeModePropertyInfo         ,
#endif
    constructContainerResizeMode            ,
#if defined(ENABLE_OVERLOADING)
    containerResizeMode                     ,
#endif
    getContainerResizeMode                  ,
    setContainerResizeMode                  ,




 -- * Signals


-- ** add #signal:add#

    ContainerAddCallback                    ,
#if defined(ENABLE_OVERLOADING)
    ContainerAddSignalInfo                  ,
#endif
    afterContainerAdd                       ,
    onContainerAdd                          ,


-- ** checkResize #signal:checkResize#

    ContainerCheckResizeCallback            ,
#if defined(ENABLE_OVERLOADING)
    ContainerCheckResizeSignalInfo          ,
#endif
    afterContainerCheckResize               ,
    onContainerCheckResize                  ,


-- ** remove #signal:remove#

    ContainerRemoveCallback                 ,
#if defined(ENABLE_OVERLOADING)
    ContainerRemoveSignalInfo               ,
#endif
    afterContainerRemove                    ,
    onContainerRemove                       ,


-- ** setFocusChild #signal:setFocusChild#

    ContainerSetFocusChildCallback          ,
#if defined(ENABLE_OVERLOADING)
    ContainerSetFocusChildSignalInfo        ,
#endif
    afterContainerSetFocusChild             ,
    onContainerSetFocusChild                ,




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
import qualified GI.Cairo.Structs.Context as Cairo.Context
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Adjustment as Gtk.Adjustment
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Structs.WidgetPath as Gtk.WidgetPath

-- | Memory-managed wrapper type.
newtype Container = Container (SP.ManagedPtr Container)
    deriving (Eq)

instance SP.ManagedPtrNewtype Container where
    toManagedPtr (Container p) = p

foreign import ccall "gtk_container_get_type"
    c_gtk_container_get_type :: IO B.Types.GType

instance B.Types.TypedObject Container where
    glibType = c_gtk_container_get_type

instance B.Types.GObject Container

-- | Type class for types which can be safely cast to `Container`, for instance with `toContainer`.
class (SP.GObject o, O.IsDescendantOf Container o) => IsContainer o
instance (SP.GObject o, O.IsDescendantOf Container o) => IsContainer o

instance O.HasParentTypes Container
type instance O.ParentTypes Container = '[Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `Container`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toContainer :: (MIO.MonadIO m, IsContainer o) => o -> m Container
toContainer = MIO.liftIO . B.ManagedPtr.unsafeCastTo Container

-- | Convert 'Container' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Container) where
    gvalueGType_ = c_gtk_container_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Container)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Container)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Container ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveContainerMethod (t :: Symbol) (o :: *) :: * where
    ResolveContainerMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveContainerMethod "add" o = ContainerAddMethodInfo
    ResolveContainerMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveContainerMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveContainerMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveContainerMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveContainerMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveContainerMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveContainerMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveContainerMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveContainerMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveContainerMethod "checkResize" o = ContainerCheckResizeMethodInfo
    ResolveContainerMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveContainerMethod "childGetProperty" o = ContainerChildGetPropertyMethodInfo
    ResolveContainerMethod "childNotify" o = ContainerChildNotifyMethodInfo
    ResolveContainerMethod "childNotifyByPspec" o = ContainerChildNotifyByPspecMethodInfo
    ResolveContainerMethod "childSetProperty" o = ContainerChildSetPropertyMethodInfo
    ResolveContainerMethod "childType" o = ContainerChildTypeMethodInfo
    ResolveContainerMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveContainerMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveContainerMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveContainerMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveContainerMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveContainerMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveContainerMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveContainerMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveContainerMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveContainerMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveContainerMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveContainerMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveContainerMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveContainerMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveContainerMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveContainerMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveContainerMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveContainerMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveContainerMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveContainerMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveContainerMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveContainerMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveContainerMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveContainerMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveContainerMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveContainerMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveContainerMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveContainerMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveContainerMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveContainerMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveContainerMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveContainerMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveContainerMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveContainerMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveContainerMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveContainerMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveContainerMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveContainerMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveContainerMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveContainerMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveContainerMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveContainerMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveContainerMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveContainerMethod "forall" o = ContainerForallMethodInfo
    ResolveContainerMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveContainerMethod "foreach" o = ContainerForeachMethodInfo
    ResolveContainerMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveContainerMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveContainerMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveContainerMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveContainerMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveContainerMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveContainerMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveContainerMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveContainerMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveContainerMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveContainerMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveContainerMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveContainerMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveContainerMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveContainerMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveContainerMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveContainerMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveContainerMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveContainerMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveContainerMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveContainerMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveContainerMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveContainerMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveContainerMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveContainerMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveContainerMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveContainerMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveContainerMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveContainerMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveContainerMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveContainerMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveContainerMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveContainerMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveContainerMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveContainerMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveContainerMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveContainerMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveContainerMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveContainerMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveContainerMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveContainerMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveContainerMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveContainerMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveContainerMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveContainerMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveContainerMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveContainerMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveContainerMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveContainerMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveContainerMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveContainerMethod "propagateDraw" o = ContainerPropagateDrawMethodInfo
    ResolveContainerMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveContainerMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveContainerMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveContainerMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveContainerMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveContainerMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveContainerMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveContainerMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveContainerMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveContainerMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveContainerMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveContainerMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveContainerMethod "remove" o = ContainerRemoveMethodInfo
    ResolveContainerMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveContainerMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveContainerMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveContainerMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveContainerMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveContainerMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveContainerMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveContainerMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveContainerMethod "resizeChildren" o = ContainerResizeChildrenMethodInfo
    ResolveContainerMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveContainerMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveContainerMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveContainerMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveContainerMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveContainerMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveContainerMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveContainerMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveContainerMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveContainerMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveContainerMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveContainerMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveContainerMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveContainerMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveContainerMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveContainerMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveContainerMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveContainerMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveContainerMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveContainerMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveContainerMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveContainerMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveContainerMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveContainerMethod "unsetFocusChain" o = ContainerUnsetFocusChainMethodInfo
    ResolveContainerMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveContainerMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveContainerMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveContainerMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveContainerMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveContainerMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveContainerMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveContainerMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveContainerMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveContainerMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveContainerMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveContainerMethod "getBorderWidth" o = ContainerGetBorderWidthMethodInfo
    ResolveContainerMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveContainerMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveContainerMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveContainerMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveContainerMethod "getChildren" o = ContainerGetChildrenMethodInfo
    ResolveContainerMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveContainerMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveContainerMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveContainerMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveContainerMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveContainerMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveContainerMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveContainerMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveContainerMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveContainerMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveContainerMethod "getFocusChain" o = ContainerGetFocusChainMethodInfo
    ResolveContainerMethod "getFocusChild" o = ContainerGetFocusChildMethodInfo
    ResolveContainerMethod "getFocusHadjustment" o = ContainerGetFocusHadjustmentMethodInfo
    ResolveContainerMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveContainerMethod "getFocusVadjustment" o = ContainerGetFocusVadjustmentMethodInfo
    ResolveContainerMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveContainerMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveContainerMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveContainerMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveContainerMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveContainerMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveContainerMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveContainerMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveContainerMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveContainerMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveContainerMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveContainerMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveContainerMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveContainerMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveContainerMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveContainerMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveContainerMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveContainerMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveContainerMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveContainerMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveContainerMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveContainerMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveContainerMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveContainerMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveContainerMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveContainerMethod "getPathForChild" o = ContainerGetPathForChildMethodInfo
    ResolveContainerMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveContainerMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveContainerMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveContainerMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveContainerMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveContainerMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveContainerMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveContainerMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveContainerMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveContainerMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveContainerMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveContainerMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveContainerMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveContainerMethod "getResizeMode" o = ContainerGetResizeModeMethodInfo
    ResolveContainerMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveContainerMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveContainerMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveContainerMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveContainerMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveContainerMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveContainerMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveContainerMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveContainerMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveContainerMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveContainerMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveContainerMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveContainerMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveContainerMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveContainerMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveContainerMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveContainerMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveContainerMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveContainerMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveContainerMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveContainerMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveContainerMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveContainerMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveContainerMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveContainerMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveContainerMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveContainerMethod "setBorderWidth" o = ContainerSetBorderWidthMethodInfo
    ResolveContainerMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveContainerMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveContainerMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveContainerMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveContainerMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveContainerMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveContainerMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveContainerMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveContainerMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveContainerMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveContainerMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveContainerMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveContainerMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveContainerMethod "setFocusChain" o = ContainerSetFocusChainMethodInfo
    ResolveContainerMethod "setFocusChild" o = ContainerSetFocusChildMethodInfo
    ResolveContainerMethod "setFocusHadjustment" o = ContainerSetFocusHadjustmentMethodInfo
    ResolveContainerMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveContainerMethod "setFocusVadjustment" o = ContainerSetFocusVadjustmentMethodInfo
    ResolveContainerMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveContainerMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveContainerMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveContainerMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveContainerMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveContainerMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveContainerMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveContainerMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveContainerMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveContainerMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveContainerMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveContainerMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveContainerMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveContainerMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveContainerMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveContainerMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveContainerMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveContainerMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveContainerMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveContainerMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveContainerMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveContainerMethod "setReallocateRedraws" o = ContainerSetReallocateRedrawsMethodInfo
    ResolveContainerMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveContainerMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveContainerMethod "setResizeMode" o = ContainerSetResizeModeMethodInfo
    ResolveContainerMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveContainerMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveContainerMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveContainerMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveContainerMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveContainerMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveContainerMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveContainerMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveContainerMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveContainerMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveContainerMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveContainerMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveContainerMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveContainerMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveContainerMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveContainerMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveContainerMethod t Container, O.OverloadedMethod info Container p) => OL.IsLabel t (Container -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveContainerMethod t Container, O.OverloadedMethod info Container p, R.HasField t Container p) => R.HasField t Container p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveContainerMethod t Container, O.OverloadedMethodInfo info Container) => OL.IsLabel t (O.MethodProxy info Container) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Container::add
-- | /No description available in the introspection data./
type ContainerAddCallback =
    Gtk.Widget.Widget
    -> IO ()

type C_ContainerAddCallback =
    Ptr Container ->                        -- object
    Ptr Gtk.Widget.Widget ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ContainerAddCallback`.
foreign import ccall "wrapper"
    mk_ContainerAddCallback :: C_ContainerAddCallback -> IO (FunPtr C_ContainerAddCallback)

wrap_ContainerAddCallback :: 
    GObject a => (a -> ContainerAddCallback) ->
    C_ContainerAddCallback
wrap_ContainerAddCallback gi'cb gi'selfPtr object _ = do
    object' <- (newObject Gtk.Widget.Widget) object
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  object'


-- | Connect a signal handler for the [add](#signal:add) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' container #add callback
-- @
-- 
-- 
onContainerAdd :: (IsContainer a, MonadIO m) => a -> ((?self :: a) => ContainerAddCallback) -> m SignalHandlerId
onContainerAdd obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ContainerAddCallback wrapped
    wrapped'' <- mk_ContainerAddCallback wrapped'
    connectSignalFunPtr obj "add" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [add](#signal:add) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' container #add callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterContainerAdd :: (IsContainer a, MonadIO m) => a -> ((?self :: a) => ContainerAddCallback) -> m SignalHandlerId
afterContainerAdd obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ContainerAddCallback wrapped
    wrapped'' <- mk_ContainerAddCallback wrapped'
    connectSignalFunPtr obj "add" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ContainerAddSignalInfo
instance SignalInfo ContainerAddSignalInfo where
    type HaskellCallbackType ContainerAddSignalInfo = ContainerAddCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ContainerAddCallback cb
        cb'' <- mk_ContainerAddCallback cb'
        connectSignalFunPtr obj "add" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container::add"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#g:signal:add"})

#endif

-- signal Container::check-resize
-- | /No description available in the introspection data./
type ContainerCheckResizeCallback =
    IO ()

type C_ContainerCheckResizeCallback =
    Ptr Container ->                        -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ContainerCheckResizeCallback`.
foreign import ccall "wrapper"
    mk_ContainerCheckResizeCallback :: C_ContainerCheckResizeCallback -> IO (FunPtr C_ContainerCheckResizeCallback)

wrap_ContainerCheckResizeCallback :: 
    GObject a => (a -> ContainerCheckResizeCallback) ->
    C_ContainerCheckResizeCallback
wrap_ContainerCheckResizeCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [checkResize](#signal:checkResize) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' container #checkResize callback
-- @
-- 
-- 
onContainerCheckResize :: (IsContainer a, MonadIO m) => a -> ((?self :: a) => ContainerCheckResizeCallback) -> m SignalHandlerId
onContainerCheckResize obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ContainerCheckResizeCallback wrapped
    wrapped'' <- mk_ContainerCheckResizeCallback wrapped'
    connectSignalFunPtr obj "check-resize" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [checkResize](#signal:checkResize) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' container #checkResize callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterContainerCheckResize :: (IsContainer a, MonadIO m) => a -> ((?self :: a) => ContainerCheckResizeCallback) -> m SignalHandlerId
afterContainerCheckResize obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ContainerCheckResizeCallback wrapped
    wrapped'' <- mk_ContainerCheckResizeCallback wrapped'
    connectSignalFunPtr obj "check-resize" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ContainerCheckResizeSignalInfo
instance SignalInfo ContainerCheckResizeSignalInfo where
    type HaskellCallbackType ContainerCheckResizeSignalInfo = ContainerCheckResizeCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ContainerCheckResizeCallback cb
        cb'' <- mk_ContainerCheckResizeCallback cb'
        connectSignalFunPtr obj "check-resize" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container::check-resize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#g:signal:checkResize"})

#endif

-- signal Container::remove
-- | /No description available in the introspection data./
type ContainerRemoveCallback =
    Gtk.Widget.Widget
    -> IO ()

type C_ContainerRemoveCallback =
    Ptr Container ->                        -- object
    Ptr Gtk.Widget.Widget ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ContainerRemoveCallback`.
foreign import ccall "wrapper"
    mk_ContainerRemoveCallback :: C_ContainerRemoveCallback -> IO (FunPtr C_ContainerRemoveCallback)

wrap_ContainerRemoveCallback :: 
    GObject a => (a -> ContainerRemoveCallback) ->
    C_ContainerRemoveCallback
wrap_ContainerRemoveCallback gi'cb gi'selfPtr object _ = do
    object' <- (newObject Gtk.Widget.Widget) object
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  object'


-- | Connect a signal handler for the [remove](#signal:remove) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' container #remove callback
-- @
-- 
-- 
onContainerRemove :: (IsContainer a, MonadIO m) => a -> ((?self :: a) => ContainerRemoveCallback) -> m SignalHandlerId
onContainerRemove obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ContainerRemoveCallback wrapped
    wrapped'' <- mk_ContainerRemoveCallback wrapped'
    connectSignalFunPtr obj "remove" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [remove](#signal:remove) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' container #remove callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterContainerRemove :: (IsContainer a, MonadIO m) => a -> ((?self :: a) => ContainerRemoveCallback) -> m SignalHandlerId
afterContainerRemove obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ContainerRemoveCallback wrapped
    wrapped'' <- mk_ContainerRemoveCallback wrapped'
    connectSignalFunPtr obj "remove" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ContainerRemoveSignalInfo
instance SignalInfo ContainerRemoveSignalInfo where
    type HaskellCallbackType ContainerRemoveSignalInfo = ContainerRemoveCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ContainerRemoveCallback cb
        cb'' <- mk_ContainerRemoveCallback cb'
        connectSignalFunPtr obj "remove" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container::remove"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#g:signal:remove"})

#endif

-- signal Container::set-focus-child
-- | /No description available in the introspection data./
type ContainerSetFocusChildCallback =
    Gtk.Widget.Widget
    -> IO ()

type C_ContainerSetFocusChildCallback =
    Ptr Container ->                        -- object
    Ptr Gtk.Widget.Widget ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_ContainerSetFocusChildCallback`.
foreign import ccall "wrapper"
    mk_ContainerSetFocusChildCallback :: C_ContainerSetFocusChildCallback -> IO (FunPtr C_ContainerSetFocusChildCallback)

wrap_ContainerSetFocusChildCallback :: 
    GObject a => (a -> ContainerSetFocusChildCallback) ->
    C_ContainerSetFocusChildCallback
wrap_ContainerSetFocusChildCallback gi'cb gi'selfPtr object _ = do
    object' <- (newObject Gtk.Widget.Widget) object
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  object'


-- | Connect a signal handler for the [setFocusChild](#signal:setFocusChild) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' container #setFocusChild callback
-- @
-- 
-- 
onContainerSetFocusChild :: (IsContainer a, MonadIO m) => a -> ((?self :: a) => ContainerSetFocusChildCallback) -> m SignalHandlerId
onContainerSetFocusChild obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ContainerSetFocusChildCallback wrapped
    wrapped'' <- mk_ContainerSetFocusChildCallback wrapped'
    connectSignalFunPtr obj "set-focus-child" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [setFocusChild](#signal:setFocusChild) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' container #setFocusChild callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterContainerSetFocusChild :: (IsContainer a, MonadIO m) => a -> ((?self :: a) => ContainerSetFocusChildCallback) -> m SignalHandlerId
afterContainerSetFocusChild obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_ContainerSetFocusChildCallback wrapped
    wrapped'' <- mk_ContainerSetFocusChildCallback wrapped'
    connectSignalFunPtr obj "set-focus-child" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data ContainerSetFocusChildSignalInfo
instance SignalInfo ContainerSetFocusChildSignalInfo where
    type HaskellCallbackType ContainerSetFocusChildSignalInfo = ContainerSetFocusChildCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_ContainerSetFocusChildCallback cb
        cb'' <- mk_ContainerSetFocusChildCallback cb'
        connectSignalFunPtr obj "set-focus-child" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container::set-focus-child"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#g:signal:setFocusChild"})

#endif

-- VVV Prop "border-width"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@border-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' container #borderWidth
-- @
getContainerBorderWidth :: (MonadIO m, IsContainer o) => o -> m Word32
getContainerBorderWidth obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "border-width"

-- | Set the value of the “@border-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' container [ #borderWidth 'Data.GI.Base.Attributes.:=' value ]
-- @
setContainerBorderWidth :: (MonadIO m, IsContainer o) => o -> Word32 -> m ()
setContainerBorderWidth obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "border-width" val

-- | Construct a `GValueConstruct` with valid value for the “@border-width@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructContainerBorderWidth :: (IsContainer o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructContainerBorderWidth val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "border-width" val

#if defined(ENABLE_OVERLOADING)
data ContainerBorderWidthPropertyInfo
instance AttrInfo ContainerBorderWidthPropertyInfo where
    type AttrAllowedOps ContainerBorderWidthPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ContainerBorderWidthPropertyInfo = IsContainer
    type AttrSetTypeConstraint ContainerBorderWidthPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint ContainerBorderWidthPropertyInfo = (~) Word32
    type AttrTransferType ContainerBorderWidthPropertyInfo = Word32
    type AttrGetType ContainerBorderWidthPropertyInfo = Word32
    type AttrLabel ContainerBorderWidthPropertyInfo = "border-width"
    type AttrOrigin ContainerBorderWidthPropertyInfo = Container
    attrGet = getContainerBorderWidth
    attrSet = setContainerBorderWidth
    attrTransfer _ v = do
        return v
    attrConstruct = constructContainerBorderWidth
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.borderWidth"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#g:attr:borderWidth"
        })
#endif

-- VVV Prop "child"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Widget"})
   -- Flags: [PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Set the value of the “@child@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' container [ #child 'Data.GI.Base.Attributes.:=' value ]
-- @
setContainerChild :: (MonadIO m, IsContainer o, Gtk.Widget.IsWidget a) => o -> a -> m ()
setContainerChild obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "child" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@child@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructContainerChild :: (IsContainer o, MIO.MonadIO m, Gtk.Widget.IsWidget a) => a -> m (GValueConstruct o)
constructContainerChild val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "child" (P.Just val)

-- | Set the value of the “@child@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #child
-- @
clearContainerChild :: (MonadIO m, IsContainer o) => o -> m ()
clearContainerChild obj = liftIO $ B.Properties.setObjectPropertyObject obj "child" (Nothing :: Maybe Gtk.Widget.Widget)

#if defined(ENABLE_OVERLOADING)
data ContainerChildPropertyInfo
instance AttrInfo ContainerChildPropertyInfo where
    type AttrAllowedOps ContainerChildPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrClear]
    type AttrBaseTypeConstraint ContainerChildPropertyInfo = IsContainer
    type AttrSetTypeConstraint ContainerChildPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferTypeConstraint ContainerChildPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferType ContainerChildPropertyInfo = Gtk.Widget.Widget
    type AttrGetType ContainerChildPropertyInfo = ()
    type AttrLabel ContainerChildPropertyInfo = "child"
    type AttrOrigin ContainerChildPropertyInfo = Container
    attrGet = undefined
    attrSet = setContainerChild
    attrTransfer _ v = do
        unsafeCastTo Gtk.Widget.Widget v
    attrConstruct = constructContainerChild
    attrClear = clearContainerChild
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.child"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#g:attr:child"
        })
#endif

-- VVV Prop "resize-mode"
   -- Type: TInterface (Name {namespace = "Gtk", name = "ResizeMode"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@resize-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' container #resizeMode
-- @
getContainerResizeMode :: (MonadIO m, IsContainer o) => o -> m Gtk.Enums.ResizeMode
getContainerResizeMode obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "resize-mode"

-- | Set the value of the “@resize-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' container [ #resizeMode 'Data.GI.Base.Attributes.:=' value ]
-- @
setContainerResizeMode :: (MonadIO m, IsContainer o) => o -> Gtk.Enums.ResizeMode -> m ()
setContainerResizeMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "resize-mode" val

-- | Construct a `GValueConstruct` with valid value for the “@resize-mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructContainerResizeMode :: (IsContainer o, MIO.MonadIO m) => Gtk.Enums.ResizeMode -> m (GValueConstruct o)
constructContainerResizeMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "resize-mode" val

#if defined(ENABLE_OVERLOADING)
data ContainerResizeModePropertyInfo
instance AttrInfo ContainerResizeModePropertyInfo where
    type AttrAllowedOps ContainerResizeModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint ContainerResizeModePropertyInfo = IsContainer
    type AttrSetTypeConstraint ContainerResizeModePropertyInfo = (~) Gtk.Enums.ResizeMode
    type AttrTransferTypeConstraint ContainerResizeModePropertyInfo = (~) Gtk.Enums.ResizeMode
    type AttrTransferType ContainerResizeModePropertyInfo = Gtk.Enums.ResizeMode
    type AttrGetType ContainerResizeModePropertyInfo = Gtk.Enums.ResizeMode
    type AttrLabel ContainerResizeModePropertyInfo = "resize-mode"
    type AttrOrigin ContainerResizeModePropertyInfo = Container
    attrGet = getContainerResizeMode
    attrSet = setContainerResizeMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructContainerResizeMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.resizeMode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#g:attr:resizeMode"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Container
type instance O.AttributeList Container = ContainerAttributeList
type ContainerAttributeList = ('[ '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("borderWidth", ContainerBorderWidthPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("child", ContainerChildPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("resizeMode", ContainerResizeModePropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
containerBorderWidth :: AttrLabelProxy "borderWidth"
containerBorderWidth = AttrLabelProxy

containerChild :: AttrLabelProxy "child"
containerChild = AttrLabelProxy

containerResizeMode :: AttrLabelProxy "resizeMode"
containerResizeMode = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Container = ContainerSignalList
type ContainerSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("add", ContainerAddSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("checkResize", ContainerCheckResizeSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("remove", ContainerRemoveSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("setFocusChild", ContainerSetFocusChildSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Container::add
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a widget to be placed inside @container"
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

foreign import ccall "gtk_container_add" gtk_container_add :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Adds /@widget@/ to /@container@/. Typically used for simple containers
-- such as t'GI.Gtk.Objects.Window.Window', t'GI.Gtk.Objects.Frame.Frame', or t'GI.Gtk.Objects.Button.Button'; for more complicated
-- layout containers such as t'GI.Gtk.Objects.Box.Box' or t'GI.Gtk.Objects.Grid.Grid', this function will
-- pick default packing parameters that may not be correct.  So
-- consider functions such as 'GI.Gtk.Objects.Box.boxPackStart' and
-- 'GI.Gtk.Objects.Grid.gridAttach' as an alternative to 'GI.Gtk.Objects.Container.containerAdd' in
-- those cases. A widget may be added to only one container at a time;
-- you can’t place the same widget inside two different containers.
-- 
-- Note that some containers, such as t'GI.Gtk.Objects.ScrolledWindow.ScrolledWindow' or t'GI.Gtk.Objects.ListBox.ListBox',
-- may add intermediate children between the added widget and the
-- container.
containerAdd ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> b
    -- ^ /@widget@/: a widget to be placed inside /@container@/
    -> m ()
containerAdd container widget = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    widget' <- unsafeManagedPtrCastPtr widget
    gtk_container_add container' widget'
    touchManagedPtr container
    touchManagedPtr widget
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerAddMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod ContainerAddMethodInfo a signature where
    overloadedMethod = containerAdd

instance O.OverloadedMethodInfo ContainerAddMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerAdd",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerAdd"
        })


#endif

-- method Container::check_resize
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
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

foreign import ccall "gtk_container_check_resize" gtk_container_check_resize :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    IO ()

-- | /No description available in the introspection data./
containerCheckResize ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a) =>
    a
    -> m ()
containerCheckResize container = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    gtk_container_check_resize container'
    touchManagedPtr container
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerCheckResizeMethodInfo
instance (signature ~ (m ()), MonadIO m, IsContainer a) => O.OverloadedMethod ContainerCheckResizeMethodInfo a signature where
    overloadedMethod = containerCheckResize

instance O.OverloadedMethodInfo ContainerCheckResizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerCheckResize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerCheckResize"
        })


#endif

-- method Container::child_get_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a widget which is a child of @container"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the name of the property to get"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TGValue
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a location to return the value"
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

foreign import ccall "gtk_container_child_get_property" gtk_container_child_get_property :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- property_name : TBasicType TUTF8
    Ptr GValue ->                           -- value : TGValue
    IO ()

-- | Gets the value of a child property for /@child@/ and /@container@/.
containerChildGetProperty ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> b
    -- ^ /@child@/: a widget which is a child of /@container@/
    -> T.Text
    -- ^ /@propertyName@/: the name of the property to get
    -> GValue
    -- ^ /@value@/: a location to return the value
    -> m ()
containerChildGetProperty container child propertyName value = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    child' <- unsafeManagedPtrCastPtr child
    propertyName' <- textToCString propertyName
    value' <- unsafeManagedPtrGetPtr value
    gtk_container_child_get_property container' child' propertyName' value'
    touchManagedPtr container
    touchManagedPtr child
    touchManagedPtr value
    freeMem propertyName'
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerChildGetPropertyMethodInfo
instance (signature ~ (b -> T.Text -> GValue -> m ()), MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod ContainerChildGetPropertyMethodInfo a signature where
    overloadedMethod = containerChildGetProperty

instance O.OverloadedMethodInfo ContainerChildGetPropertyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerChildGetProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerChildGetProperty"
        })


#endif

-- method Container::child_notify
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkContainer" , sinceVersion = Nothing }
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
--           { argCName = "child_property"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the name of a child property installed on\n    the class of @container"
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

foreign import ccall "gtk_container_child_notify" gtk_container_child_notify :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- child_property : TBasicType TUTF8
    IO ()

-- | Emits a [Widget::childNotify]("GI.Gtk.Objects.Widget#g:signal:childNotify") signal for the
-- [child property][child-properties]
-- /@childProperty@/ on the child.
-- 
-- This is an analogue of 'GI.GObject.Objects.Object.objectNotify' for child properties.
-- 
-- Also see 'GI.Gtk.Objects.Widget.widgetChildNotify'.
-- 
-- /Since: 3.2/
containerChildNotify ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@container@/: the t'GI.Gtk.Objects.Container.Container'
    -> b
    -- ^ /@child@/: the child widget
    -> T.Text
    -- ^ /@childProperty@/: the name of a child property installed on
    --     the class of /@container@/
    -> m ()
containerChildNotify container child childProperty = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    child' <- unsafeManagedPtrCastPtr child
    childProperty' <- textToCString childProperty
    gtk_container_child_notify container' child' childProperty'
    touchManagedPtr container
    touchManagedPtr child
    freeMem childProperty'
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerChildNotifyMethodInfo
instance (signature ~ (b -> T.Text -> m ()), MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod ContainerChildNotifyMethodInfo a signature where
    overloadedMethod = containerChildNotify

instance O.OverloadedMethodInfo ContainerChildNotifyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerChildNotify",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerChildNotify"
        })


#endif

-- method Container::child_notify_by_pspec
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkContainer" , sinceVersion = Nothing }
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
--           { argCName = "pspec"
--           , argType = TParamSpec
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the #GParamSpec of a child property instealled on\n    the class of @container"
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

foreign import ccall "gtk_container_child_notify_by_pspec" gtk_container_child_notify_by_pspec :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr GParamSpec ->                       -- pspec : TParamSpec
    IO ()

-- | Emits a [Widget::childNotify]("GI.Gtk.Objects.Widget#g:signal:childNotify") signal for the
-- [child property][child-properties] specified by
-- /@pspec@/ on the child.
-- 
-- This is an analogue of 'GI.GObject.Objects.Object.objectNotifyByPspec' for child properties.
-- 
-- /Since: 3.18/
containerChildNotifyByPspec ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@container@/: the t'GI.Gtk.Objects.Container.Container'
    -> b
    -- ^ /@child@/: the child widget
    -> GParamSpec
    -- ^ /@pspec@/: the t'GI.GObject.Objects.ParamSpec.ParamSpec' of a child property instealled on
    --     the class of /@container@/
    -> m ()
containerChildNotifyByPspec container child pspec = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    child' <- unsafeManagedPtrCastPtr child
    pspec' <- unsafeManagedPtrGetPtr pspec
    gtk_container_child_notify_by_pspec container' child' pspec'
    touchManagedPtr container
    touchManagedPtr child
    touchManagedPtr pspec
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerChildNotifyByPspecMethodInfo
instance (signature ~ (b -> GParamSpec -> m ()), MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod ContainerChildNotifyByPspecMethodInfo a signature where
    overloadedMethod = containerChildNotifyByPspec

instance O.OverloadedMethodInfo ContainerChildNotifyByPspecMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerChildNotifyByPspec",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerChildNotifyByPspec"
        })


#endif

-- method Container::child_set_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a widget which is a child of @container"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the name of the property to set"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TGValue
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the value to set the property to"
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

foreign import ccall "gtk_container_child_set_property" gtk_container_child_set_property :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- property_name : TBasicType TUTF8
    Ptr GValue ->                           -- value : TGValue
    IO ()

-- | Sets a child property for /@child@/ and /@container@/.
containerChildSetProperty ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> b
    -- ^ /@child@/: a widget which is a child of /@container@/
    -> T.Text
    -- ^ /@propertyName@/: the name of the property to set
    -> GValue
    -- ^ /@value@/: the value to set the property to
    -> m ()
containerChildSetProperty container child propertyName value = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    child' <- unsafeManagedPtrCastPtr child
    propertyName' <- textToCString propertyName
    value' <- unsafeManagedPtrGetPtr value
    gtk_container_child_set_property container' child' propertyName' value'
    touchManagedPtr container
    touchManagedPtr child
    touchManagedPtr value
    freeMem propertyName'
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerChildSetPropertyMethodInfo
instance (signature ~ (b -> T.Text -> GValue -> m ()), MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod ContainerChildSetPropertyMethodInfo a signature where
    overloadedMethod = containerChildSetProperty

instance O.OverloadedMethodInfo ContainerChildSetPropertyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerChildSetProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerChildSetProperty"
        })


#endif

-- method Container::child_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TGType)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_container_child_type" gtk_container_child_type :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    IO CGType

-- | Returns the type of the children supported by the container.
-- 
-- Note that this may return @/G_TYPE_NONE/@ to indicate that no more
-- children can be added, e.g. for a t'GI.Gtk.Objects.Paned.Paned' which already has two
-- children.
containerChildType ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> m GType
    -- ^ __Returns:__ a t'GType'.
containerChildType container = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    result <- gtk_container_child_type container'
    let result' = GType result
    touchManagedPtr container
    return result'

#if defined(ENABLE_OVERLOADING)
data ContainerChildTypeMethodInfo
instance (signature ~ (m GType), MonadIO m, IsContainer a) => O.OverloadedMethod ContainerChildTypeMethodInfo a signature where
    overloadedMethod = containerChildType

instance O.OverloadedMethodInfo ContainerChildTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerChildType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerChildType"
        })


#endif

-- method Container::forall
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "callback"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Callback" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a callback" , sinceVersion = Nothing }
--           , argScope = ScopeTypeCall
--           , argClosure = 2
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "callback_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "callback user data" , sinceVersion = Nothing }
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

foreign import ccall "gtk_container_forall" gtk_container_forall :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    FunPtr Gtk.Callbacks.C_Callback ->      -- callback : TInterface (Name {namespace = "Gtk", name = "Callback"})
    Ptr () ->                               -- callback_data : TBasicType TPtr
    IO ()

-- | Invokes /@callback@/ on each direct child of /@container@/, including
-- children that are considered “internal” (implementation details
-- of the container). “Internal” children generally weren’t added
-- by the user of the container, but were added by the container
-- implementation itself.
-- 
-- Most applications should use 'GI.Gtk.Objects.Container.containerForeach', rather
-- than 'GI.Gtk.Objects.Container.containerForall'.
containerForall ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> Gtk.Callbacks.Callback
    -- ^ /@callback@/: a callback
    -> m ()
containerForall container callback = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    callback' <- Gtk.Callbacks.mk_Callback (Gtk.Callbacks.wrap_Callback Nothing (Gtk.Callbacks.drop_closures_Callback callback))
    let callbackData = nullPtr
    gtk_container_forall container' callback' callbackData
    safeFreeFunPtr $ castFunPtrToPtr callback'
    touchManagedPtr container
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerForallMethodInfo
instance (signature ~ (Gtk.Callbacks.Callback -> m ()), MonadIO m, IsContainer a) => O.OverloadedMethod ContainerForallMethodInfo a signature where
    overloadedMethod = containerForall

instance O.OverloadedMethodInfo ContainerForallMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerForall",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerForall"
        })


#endif

-- method Container::foreach
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "callback"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Callback" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a callback" , sinceVersion = Nothing }
--           , argScope = ScopeTypeCall
--           , argClosure = 2
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "callback_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "callback user data" , sinceVersion = Nothing }
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

foreign import ccall "gtk_container_foreach" gtk_container_foreach :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    FunPtr Gtk.Callbacks.C_Callback ->      -- callback : TInterface (Name {namespace = "Gtk", name = "Callback"})
    Ptr () ->                               -- callback_data : TBasicType TPtr
    IO ()

-- | Invokes /@callback@/ on each non-internal child of /@container@/.
-- See 'GI.Gtk.Objects.Container.containerForall' for details on what constitutes
-- an “internal” child. For all practical purposes, this function
-- should iterate over precisely those child widgets that were
-- added to the container by the application with explicit @/add()/@
-- calls.
-- 
-- It is permissible to remove the child from the /@callback@/ handler.
-- 
-- Most applications should use 'GI.Gtk.Objects.Container.containerForeach',
-- rather than 'GI.Gtk.Objects.Container.containerForall'.
containerForeach ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> Gtk.Callbacks.Callback
    -- ^ /@callback@/: a callback
    -> m ()
containerForeach container callback = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    callback' <- Gtk.Callbacks.mk_Callback (Gtk.Callbacks.wrap_Callback Nothing (Gtk.Callbacks.drop_closures_Callback callback))
    let callbackData = nullPtr
    gtk_container_foreach container' callback' callbackData
    safeFreeFunPtr $ castFunPtrToPtr callback'
    touchManagedPtr container
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerForeachMethodInfo
instance (signature ~ (Gtk.Callbacks.Callback -> m ()), MonadIO m, IsContainer a) => O.OverloadedMethod ContainerForeachMethodInfo a signature where
    overloadedMethod = containerForeach

instance O.OverloadedMethodInfo ContainerForeachMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerForeach",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerForeach"
        })


#endif

-- method Container::get_border_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_container_get_border_width" gtk_container_get_border_width :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    IO Word32

-- | Retrieves the border width of the container. See
-- 'GI.Gtk.Objects.Container.containerSetBorderWidth'.
containerGetBorderWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> m Word32
    -- ^ __Returns:__ the current border width
containerGetBorderWidth container = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    result <- gtk_container_get_border_width container'
    touchManagedPtr container
    return result

#if defined(ENABLE_OVERLOADING)
data ContainerGetBorderWidthMethodInfo
instance (signature ~ (m Word32), MonadIO m, IsContainer a) => O.OverloadedMethod ContainerGetBorderWidthMethodInfo a signature where
    overloadedMethod = containerGetBorderWidth

instance O.OverloadedMethodInfo ContainerGetBorderWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerGetBorderWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerGetBorderWidth"
        })


#endif

-- method Container::get_children
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TGList (TInterface Name { namespace = "Gtk" , name = "Widget" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_container_get_children" gtk_container_get_children :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    IO (Ptr (GList (Ptr Gtk.Widget.Widget)))

-- | Returns the container’s non-internal children. See
-- 'GI.Gtk.Objects.Container.containerForall' for details on what constitutes an \"internal\" child.
containerGetChildren ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> m [Gtk.Widget.Widget]
    -- ^ __Returns:__ a newly-allocated list of the container’s non-internal children.
containerGetChildren container = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    result <- gtk_container_get_children container'
    result' <- unpackGList result
    result'' <- mapM (newObject Gtk.Widget.Widget) result'
    g_list_free result
    touchManagedPtr container
    return result''

#if defined(ENABLE_OVERLOADING)
data ContainerGetChildrenMethodInfo
instance (signature ~ (m [Gtk.Widget.Widget]), MonadIO m, IsContainer a) => O.OverloadedMethod ContainerGetChildrenMethodInfo a signature where
    overloadedMethod = containerGetChildren

instance O.OverloadedMethodInfo ContainerGetChildrenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerGetChildren",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerGetChildren"
        })


#endif

-- method Container::get_focus_chain
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "focusable_widgets"
--           , argType =
--               TGList (TInterface Name { namespace = "Gtk" , name = "Widget" })
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location\n                    to store the focus chain of the\n                    container, or %NULL. You should free this list\n                    using g_list_free() when you are done with it, however\n                    no additional reference count is added to the\n                    individual widgets in the focus chain."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferContainer
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_container_get_focus_chain" gtk_container_get_focus_chain :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    Ptr (Ptr (GList (Ptr Gtk.Widget.Widget))) -> -- focusable_widgets : TGList (TInterface (Name {namespace = "Gtk", name = "Widget"}))
    IO CInt

{-# DEPRECATED containerGetFocusChain ["(Since version 3.24)","For overriding focus behavior, use the","    GtkWidgetClass[focus](#g:signal:focus) signal."] #-}
-- | Retrieves the focus chain of the container, if one has been
-- set explicitly. If no focus chain has been explicitly
-- set, GTK+ computes the focus chain based on the positions
-- of the children. In that case, GTK+ stores 'P.Nothing' in
-- /@focusableWidgets@/ and returns 'P.False'.
containerGetFocusChain ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> m ((Bool, [Gtk.Widget.Widget]))
    -- ^ __Returns:__ 'P.True' if the focus chain of the container
    -- has been set explicitly.
containerGetFocusChain container = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    focusableWidgets <- callocMem :: IO (Ptr (Ptr (GList (Ptr Gtk.Widget.Widget))))
    result <- gtk_container_get_focus_chain container' focusableWidgets
    let result' = (/= 0) result
    focusableWidgets' <- peek focusableWidgets
    focusableWidgets'' <- unpackGList focusableWidgets'
    focusableWidgets''' <- mapM (newObject Gtk.Widget.Widget) focusableWidgets''
    g_list_free focusableWidgets'
    touchManagedPtr container
    freeMem focusableWidgets
    return (result', focusableWidgets''')

#if defined(ENABLE_OVERLOADING)
data ContainerGetFocusChainMethodInfo
instance (signature ~ (m ((Bool, [Gtk.Widget.Widget]))), MonadIO m, IsContainer a) => O.OverloadedMethod ContainerGetFocusChainMethodInfo a signature where
    overloadedMethod = containerGetFocusChain

instance O.OverloadedMethodInfo ContainerGetFocusChainMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerGetFocusChain",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerGetFocusChain"
        })


#endif

-- method Container::get_focus_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_container_get_focus_child" gtk_container_get_focus_child :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    IO (Ptr Gtk.Widget.Widget)

-- | Returns the current focus child widget inside /@container@/. This is not the
-- currently focused widget. That can be obtained by calling
-- 'GI.Gtk.Objects.Window.windowGetFocus'.
-- 
-- /Since: 2.14/
containerGetFocusChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ The child widget which will receive the
    --          focus inside /@container@/ when the /@container@/ is focused,
    --          or 'P.Nothing' if none is set.
containerGetFocusChild container = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    result <- gtk_container_get_focus_child container'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr container
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ContainerGetFocusChildMethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsContainer a) => O.OverloadedMethod ContainerGetFocusChildMethodInfo a signature where
    overloadedMethod = containerGetFocusChild

instance O.OverloadedMethodInfo ContainerGetFocusChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerGetFocusChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerGetFocusChild"
        })


#endif

-- method Container::get_focus_hadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_container_get_focus_hadjustment" gtk_container_get_focus_hadjustment :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    IO (Ptr Gtk.Adjustment.Adjustment)

-- | Retrieves the horizontal focus adjustment for the container. See
-- gtk_container_set_focus_hadjustment ().
containerGetFocusHadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> m (Maybe Gtk.Adjustment.Adjustment)
    -- ^ __Returns:__ the horizontal focus adjustment, or 'P.Nothing' if
    --   none has been set.
containerGetFocusHadjustment container = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    result <- gtk_container_get_focus_hadjustment container'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Adjustment.Adjustment) result'
        return result''
    touchManagedPtr container
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ContainerGetFocusHadjustmentMethodInfo
instance (signature ~ (m (Maybe Gtk.Adjustment.Adjustment)), MonadIO m, IsContainer a) => O.OverloadedMethod ContainerGetFocusHadjustmentMethodInfo a signature where
    overloadedMethod = containerGetFocusHadjustment

instance O.OverloadedMethodInfo ContainerGetFocusHadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerGetFocusHadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerGetFocusHadjustment"
        })


#endif

-- method Container::get_focus_vadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_container_get_focus_vadjustment" gtk_container_get_focus_vadjustment :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    IO (Ptr Gtk.Adjustment.Adjustment)

-- | Retrieves the vertical focus adjustment for the container. See
-- 'GI.Gtk.Objects.Container.containerSetFocusVadjustment'.
containerGetFocusVadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> m (Maybe Gtk.Adjustment.Adjustment)
    -- ^ __Returns:__ the vertical focus adjustment, or
    --   'P.Nothing' if none has been set.
containerGetFocusVadjustment container = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    result <- gtk_container_get_focus_vadjustment container'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Adjustment.Adjustment) result'
        return result''
    touchManagedPtr container
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ContainerGetFocusVadjustmentMethodInfo
instance (signature ~ (m (Maybe Gtk.Adjustment.Adjustment)), MonadIO m, IsContainer a) => O.OverloadedMethod ContainerGetFocusVadjustmentMethodInfo a signature where
    overloadedMethod = containerGetFocusVadjustment

instance O.OverloadedMethodInfo ContainerGetFocusVadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerGetFocusVadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerGetFocusVadjustment"
        })


#endif

-- method Container::get_path_for_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a child of @container"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "WidgetPath" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_container_get_path_for_child" gtk_container_get_path_for_child :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO (Ptr Gtk.WidgetPath.WidgetPath)

-- | Returns a newly created widget path representing all the widget hierarchy
-- from the toplevel down to and including /@child@/.
containerGetPathForChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> b
    -- ^ /@child@/: a child of /@container@/
    -> m Gtk.WidgetPath.WidgetPath
    -- ^ __Returns:__ A newly created t'GI.Gtk.Structs.WidgetPath.WidgetPath'
containerGetPathForChild container child = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    child' <- unsafeManagedPtrCastPtr child
    result <- gtk_container_get_path_for_child container' child'
    checkUnexpectedReturnNULL "containerGetPathForChild" result
    result' <- (wrapBoxed Gtk.WidgetPath.WidgetPath) result
    touchManagedPtr container
    touchManagedPtr child
    return result'

#if defined(ENABLE_OVERLOADING)
data ContainerGetPathForChildMethodInfo
instance (signature ~ (b -> m Gtk.WidgetPath.WidgetPath), MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod ContainerGetPathForChildMethodInfo a signature where
    overloadedMethod = containerGetPathForChild

instance O.OverloadedMethodInfo ContainerGetPathForChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerGetPathForChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerGetPathForChild"
        })


#endif

-- method Container::get_resize_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ResizeMode" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_container_get_resize_mode" gtk_container_get_resize_mode :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    IO CUInt

{-# DEPRECATED containerGetResizeMode ["(Since version 3.12)","Resize modes are deprecated. They aren\8217t necessary","    anymore since frame clocks and might introduce obscure bugs if","    used."] #-}
-- | Returns the resize mode for the container. See
-- gtk_container_set_resize_mode ().
containerGetResizeMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> m Gtk.Enums.ResizeMode
    -- ^ __Returns:__ the current resize mode
containerGetResizeMode container = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    result <- gtk_container_get_resize_mode container'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr container
    return result'

#if defined(ENABLE_OVERLOADING)
data ContainerGetResizeModeMethodInfo
instance (signature ~ (m Gtk.Enums.ResizeMode), MonadIO m, IsContainer a) => O.OverloadedMethod ContainerGetResizeModeMethodInfo a signature where
    overloadedMethod = containerGetResizeMode

instance O.OverloadedMethodInfo ContainerGetResizeModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerGetResizeMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerGetResizeMode"
        })


#endif

-- method Container::propagate_draw
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a child of @container"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "Cairo context as passed to the container. If you want to use @cr\n  in container\8217s draw function, consider using cairo_save() and\n  cairo_restore() before calling this function."
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

foreign import ccall "gtk_container_propagate_draw" gtk_container_propagate_draw :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    IO ()

-- | When a container receives a call to the draw function, it must send
-- synthetic [Widget::draw]("GI.Gtk.Objects.Widget#g:signal:draw") calls to all children that don’t have their
-- own @/GdkWindows/@. This function provides a convenient way of doing this.
-- A container, when it receives a call to its [Widget::draw]("GI.Gtk.Objects.Widget#g:signal:draw") function,
-- calls 'GI.Gtk.Objects.Container.containerPropagateDraw' once for each child, passing in
-- the /@cr@/ the container received.
-- 
-- 'GI.Gtk.Objects.Container.containerPropagateDraw' takes care of translating the origin of /@cr@/,
-- and deciding whether the draw needs to be sent to the child. It is a
-- convenient and optimized way of getting the same effect as calling
-- 'GI.Gtk.Objects.Widget.widgetDraw' on the child directly.
-- 
-- In most cases, a container can simply either inherit the
-- [Widget::draw]("GI.Gtk.Objects.Widget#g:signal:draw") implementation from t'GI.Gtk.Objects.Container.Container', or do some drawing
-- and then chain to the [draw](#g:signal:draw) implementation from t'GI.Gtk.Objects.Container.Container'.
containerPropagateDraw ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> b
    -- ^ /@child@/: a child of /@container@/
    -> Cairo.Context.Context
    -- ^ /@cr@/: Cairo context as passed to the container. If you want to use /@cr@/
    --   in container’s draw function, consider using @/cairo_save()/@ and
    --   @/cairo_restore()/@ before calling this function.
    -> m ()
containerPropagateDraw container child cr = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    child' <- unsafeManagedPtrCastPtr child
    cr' <- unsafeManagedPtrGetPtr cr
    gtk_container_propagate_draw container' child' cr'
    touchManagedPtr container
    touchManagedPtr child
    touchManagedPtr cr
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerPropagateDrawMethodInfo
instance (signature ~ (b -> Cairo.Context.Context -> m ()), MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod ContainerPropagateDrawMethodInfo a signature where
    overloadedMethod = containerPropagateDraw

instance O.OverloadedMethodInfo ContainerPropagateDrawMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerPropagateDraw",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerPropagateDraw"
        })


#endif

-- method Container::remove
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a current child of @container"
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

foreign import ccall "gtk_container_remove" gtk_container_remove :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Removes /@widget@/ from /@container@/. /@widget@/ must be inside /@container@/.
-- Note that /@container@/ will own a reference to /@widget@/, and that this
-- may be the last reference held; so removing a widget from its
-- container can destroy that widget. If you want to use /@widget@/
-- again, you need to add a reference to it before removing it from
-- a container, using 'GI.GObject.Objects.Object.objectRef'. If you don’t want to use /@widget@/
-- again it’s usually more efficient to simply destroy it directly
-- using 'GI.Gtk.Objects.Widget.widgetDestroy' since this will remove it from the
-- container and help break any circular reference count cycles.
containerRemove ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> b
    -- ^ /@widget@/: a current child of /@container@/
    -> m ()
containerRemove container widget = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    widget' <- unsafeManagedPtrCastPtr widget
    gtk_container_remove container' widget'
    touchManagedPtr container
    touchManagedPtr widget
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerRemoveMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod ContainerRemoveMethodInfo a signature where
    overloadedMethod = containerRemove

instance O.OverloadedMethodInfo ContainerRemoveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerRemove",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerRemove"
        })


#endif

-- method Container::resize_children
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_container_resize_children" gtk_container_resize_children :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    IO ()

{-# DEPRECATED containerResizeChildren ["(Since version 3.10)"] #-}
-- | /No description available in the introspection data./
containerResizeChildren ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> m ()
containerResizeChildren container = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    gtk_container_resize_children container'
    touchManagedPtr container
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerResizeChildrenMethodInfo
instance (signature ~ (m ()), MonadIO m, IsContainer a) => O.OverloadedMethod ContainerResizeChildrenMethodInfo a signature where
    overloadedMethod = containerResizeChildren

instance O.OverloadedMethodInfo ContainerResizeChildrenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerResizeChildren",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerResizeChildren"
        })


#endif

-- method Container::set_border_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "border_width"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "amount of blank space to leave outside\n  the container. Valid values are in the range 0-65535 pixels."
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

foreign import ccall "gtk_container_set_border_width" gtk_container_set_border_width :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    Word32 ->                               -- border_width : TBasicType TUInt
    IO ()

-- | Sets the border width of the container.
-- 
-- The border width of a container is the amount of space to leave
-- around the outside of the container. The only exception to this is
-- t'GI.Gtk.Objects.Window.Window'; because toplevel windows can’t leave space outside,
-- they leave the space inside. The border is added on all sides of
-- the container. To add space to only one side, use a specific
-- [Widget:margin]("GI.Gtk.Objects.Widget#g:attr:margin") property on the child widget, for example
-- [Widget:marginTop]("GI.Gtk.Objects.Widget#g:attr:marginTop").
containerSetBorderWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> Word32
    -- ^ /@borderWidth@/: amount of blank space to leave outside
    --   the container. Valid values are in the range 0-65535 pixels.
    -> m ()
containerSetBorderWidth container borderWidth = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    gtk_container_set_border_width container' borderWidth
    touchManagedPtr container
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerSetBorderWidthMethodInfo
instance (signature ~ (Word32 -> m ()), MonadIO m, IsContainer a) => O.OverloadedMethod ContainerSetBorderWidthMethodInfo a signature where
    overloadedMethod = containerSetBorderWidth

instance O.OverloadedMethodInfo ContainerSetBorderWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerSetBorderWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerSetBorderWidth"
        })


#endif

-- method Container::set_focus_chain
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "focusable_widgets"
--           , argType =
--               TGList (TInterface Name { namespace = "Gtk" , name = "Widget" })
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "\n    the new focus chain"
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

foreign import ccall "gtk_container_set_focus_chain" gtk_container_set_focus_chain :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    Ptr (GList (Ptr Gtk.Widget.Widget)) ->  -- focusable_widgets : TGList (TInterface (Name {namespace = "Gtk", name = "Widget"}))
    IO ()

{-# DEPRECATED containerSetFocusChain ["(Since version 3.24)","For overriding focus behavior, use the","    GtkWidgetClass[focus](#g:signal:focus) signal."] #-}
-- | Sets a focus chain, overriding the one computed automatically by GTK+.
-- 
-- In principle each widget in the chain should be a descendant of the
-- container, but this is not enforced by this method, since it’s allowed
-- to set the focus chain before you pack the widgets, or have a widget
-- in the chain that isn’t always packed. The necessary checks are done
-- when the focus chain is actually traversed.
containerSetFocusChain ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> [b]
    -- ^ /@focusableWidgets@/: 
    --     the new focus chain
    -> m ()
containerSetFocusChain container focusableWidgets = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    focusableWidgets' <- mapM unsafeManagedPtrCastPtr focusableWidgets
    focusableWidgets'' <- packGList focusableWidgets'
    gtk_container_set_focus_chain container' focusableWidgets''
    touchManagedPtr container
    mapM_ touchManagedPtr focusableWidgets
    g_list_free focusableWidgets''
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerSetFocusChainMethodInfo
instance (signature ~ ([b] -> m ()), MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod ContainerSetFocusChainMethodInfo a signature where
    overloadedMethod = containerSetFocusChain

instance O.OverloadedMethodInfo ContainerSetFocusChainMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerSetFocusChain",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerSetFocusChain"
        })


#endif

-- method Container::set_focus_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidget, or %NULL"
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

foreign import ccall "gtk_container_set_focus_child" gtk_container_set_focus_child :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    Ptr Gtk.Widget.Widget ->                -- child : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Sets, or unsets if /@child@/ is 'P.Nothing', the focused child of /@container@/.
-- 
-- This function emits the GtkContainer[set_focus_child](#g:signal:set_focus_child) signal of
-- /@container@/. Implementations of t'GI.Gtk.Objects.Container.Container' can override the
-- default behaviour by overriding the class closure of this signal.
-- 
-- This is function is mostly meant to be used by widgets. Applications can use
-- 'GI.Gtk.Objects.Widget.widgetGrabFocus' to manually set the focus to a specific widget.
containerSetFocusChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> Maybe (b)
    -- ^ /@child@/: a t'GI.Gtk.Objects.Widget.Widget', or 'P.Nothing'
    -> m ()
containerSetFocusChild container child = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    maybeChild <- case child of
        Nothing -> return nullPtr
        Just jChild -> do
            jChild' <- unsafeManagedPtrCastPtr jChild
            return jChild'
    gtk_container_set_focus_child container' maybeChild
    touchManagedPtr container
    whenJust child touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerSetFocusChildMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsContainer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod ContainerSetFocusChildMethodInfo a signature where
    overloadedMethod = containerSetFocusChild

instance O.OverloadedMethodInfo ContainerSetFocusChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerSetFocusChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerSetFocusChild"
        })


#endif

-- method Container::set_focus_hadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
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
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "an adjustment which should be adjusted when the focus is\n  moved among the descendents of @container"
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

foreign import ccall "gtk_container_set_focus_hadjustment" gtk_container_set_focus_hadjustment :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    Ptr Gtk.Adjustment.Adjustment ->        -- adjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    IO ()

-- | Hooks up an adjustment to focus handling in a container, so when a child
-- of the container is focused, the adjustment is scrolled to show that
-- widget. This function sets the horizontal alignment.
-- See 'GI.Gtk.Objects.ScrolledWindow.scrolledWindowGetHadjustment' for a typical way of obtaining
-- the adjustment and 'GI.Gtk.Objects.Container.containerSetFocusVadjustment' for setting
-- the vertical adjustment.
-- 
-- The adjustments have to be in pixel units and in the same coordinate
-- system as the allocation for immediate children of the container.
containerSetFocusHadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a, Gtk.Adjustment.IsAdjustment b) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> b
    -- ^ /@adjustment@/: an adjustment which should be adjusted when the focus is
    --   moved among the descendents of /@container@/
    -> m ()
containerSetFocusHadjustment container adjustment = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    adjustment' <- unsafeManagedPtrCastPtr adjustment
    gtk_container_set_focus_hadjustment container' adjustment'
    touchManagedPtr container
    touchManagedPtr adjustment
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerSetFocusHadjustmentMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsContainer a, Gtk.Adjustment.IsAdjustment b) => O.OverloadedMethod ContainerSetFocusHadjustmentMethodInfo a signature where
    overloadedMethod = containerSetFocusHadjustment

instance O.OverloadedMethodInfo ContainerSetFocusHadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerSetFocusHadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerSetFocusHadjustment"
        })


#endif

-- method Container::set_focus_vadjustment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
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
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "an adjustment which should be adjusted when the focus\n  is moved among the descendents of @container"
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

foreign import ccall "gtk_container_set_focus_vadjustment" gtk_container_set_focus_vadjustment :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    Ptr Gtk.Adjustment.Adjustment ->        -- adjustment : TInterface (Name {namespace = "Gtk", name = "Adjustment"})
    IO ()

-- | Hooks up an adjustment to focus handling in a container, so when a
-- child of the container is focused, the adjustment is scrolled to
-- show that widget. This function sets the vertical alignment. See
-- 'GI.Gtk.Objects.ScrolledWindow.scrolledWindowGetVadjustment' for a typical way of obtaining
-- the adjustment and 'GI.Gtk.Objects.Container.containerSetFocusHadjustment' for setting
-- the horizontal adjustment.
-- 
-- The adjustments have to be in pixel units and in the same coordinate
-- system as the allocation for immediate children of the container.
containerSetFocusVadjustment ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a, Gtk.Adjustment.IsAdjustment b) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> b
    -- ^ /@adjustment@/: an adjustment which should be adjusted when the focus
    --   is moved among the descendents of /@container@/
    -> m ()
containerSetFocusVadjustment container adjustment = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    adjustment' <- unsafeManagedPtrCastPtr adjustment
    gtk_container_set_focus_vadjustment container' adjustment'
    touchManagedPtr container
    touchManagedPtr adjustment
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerSetFocusVadjustmentMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsContainer a, Gtk.Adjustment.IsAdjustment b) => O.OverloadedMethod ContainerSetFocusVadjustmentMethodInfo a signature where
    overloadedMethod = containerSetFocusVadjustment

instance O.OverloadedMethodInfo ContainerSetFocusVadjustmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerSetFocusVadjustment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerSetFocusVadjustment"
        })


#endif

-- method Container::set_reallocate_redraws
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "needs_redraws"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the new value for the container\8217s @reallocate_redraws flag"
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

foreign import ccall "gtk_container_set_reallocate_redraws" gtk_container_set_reallocate_redraws :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    CInt ->                                 -- needs_redraws : TBasicType TBoolean
    IO ()

{-# DEPRECATED containerSetReallocateRedraws ["(Since version 3.14)","Call 'GI.Gtk.Objects.Widget.widgetQueueDraw' in your size_allocate handler."] #-}
-- | Sets the /@reallocateRedraws@/ flag of the container to the given value.
-- 
-- Containers requesting reallocation redraws get automatically
-- redrawn if any of their children changed allocation.
containerSetReallocateRedraws ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> Bool
    -- ^ /@needsRedraws@/: the new value for the container’s /@reallocateRedraws@/ flag
    -> m ()
containerSetReallocateRedraws container needsRedraws = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    let needsRedraws' = (fromIntegral . fromEnum) needsRedraws
    gtk_container_set_reallocate_redraws container' needsRedraws'
    touchManagedPtr container
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerSetReallocateRedrawsMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsContainer a) => O.OverloadedMethod ContainerSetReallocateRedrawsMethodInfo a signature where
    overloadedMethod = containerSetReallocateRedraws

instance O.OverloadedMethodInfo ContainerSetReallocateRedrawsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerSetReallocateRedraws",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerSetReallocateRedraws"
        })


#endif

-- method Container::set_resize_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "resize_mode"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ResizeMode" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new resize mode"
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

foreign import ccall "gtk_container_set_resize_mode" gtk_container_set_resize_mode :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    CUInt ->                                -- resize_mode : TInterface (Name {namespace = "Gtk", name = "ResizeMode"})
    IO ()

{-# DEPRECATED containerSetResizeMode ["(Since version 3.12)","Resize modes are deprecated. They aren\8217t necessary","    anymore since frame clocks and might introduce obscure bugs if","    used."] #-}
-- | Sets the resize mode for the container.
-- 
-- The resize mode of a container determines whether a resize request
-- will be passed to the container’s parent, queued for later execution
-- or executed immediately.
containerSetResizeMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> Gtk.Enums.ResizeMode
    -- ^ /@resizeMode@/: the new resize mode
    -> m ()
containerSetResizeMode container resizeMode = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    let resizeMode' = (fromIntegral . fromEnum) resizeMode
    gtk_container_set_resize_mode container' resizeMode'
    touchManagedPtr container
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerSetResizeModeMethodInfo
instance (signature ~ (Gtk.Enums.ResizeMode -> m ()), MonadIO m, IsContainer a) => O.OverloadedMethod ContainerSetResizeModeMethodInfo a signature where
    overloadedMethod = containerSetResizeMode

instance O.OverloadedMethodInfo ContainerSetResizeModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerSetResizeMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerSetResizeMode"
        })


#endif

-- method Container::unset_focus_chain
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "container"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Container" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkContainer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_container_unset_focus_chain" gtk_container_unset_focus_chain :: 
    Ptr Container ->                        -- container : TInterface (Name {namespace = "Gtk", name = "Container"})
    IO ()

{-# DEPRECATED containerUnsetFocusChain ["(Since version 3.24)","For overriding focus behavior, use the","    GtkWidgetClass[focus](#g:signal:focus) signal."] #-}
-- | Removes a focus chain explicitly set with 'GI.Gtk.Objects.Container.containerSetFocusChain'.
containerUnsetFocusChain ::
    (B.CallStack.HasCallStack, MonadIO m, IsContainer a) =>
    a
    -- ^ /@container@/: a t'GI.Gtk.Objects.Container.Container'
    -> m ()
containerUnsetFocusChain container = liftIO $ do
    container' <- unsafeManagedPtrCastPtr container
    gtk_container_unset_focus_chain container'
    touchManagedPtr container
    return ()

#if defined(ENABLE_OVERLOADING)
data ContainerUnsetFocusChainMethodInfo
instance (signature ~ (m ()), MonadIO m, IsContainer a) => O.OverloadedMethod ContainerUnsetFocusChainMethodInfo a signature where
    overloadedMethod = containerUnsetFocusChain

instance O.OverloadedMethodInfo ContainerUnsetFocusChainMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Container.containerUnsetFocusChain",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Container.html#v:containerUnsetFocusChain"
        })


#endif


