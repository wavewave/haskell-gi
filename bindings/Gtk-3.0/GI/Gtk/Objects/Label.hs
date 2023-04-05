{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.Label.Label' widget displays a small amount of text. As the name
-- implies, most labels are used to label another widget such as a
-- t'GI.Gtk.Objects.Button.Button', a t'GI.Gtk.Objects.MenuItem.MenuItem', or a t'GI.Gtk.Objects.ComboBox.ComboBox'.
-- 
-- = CSS nodes
-- 
-- 
-- === /plain code/
-- >
-- >label
-- >├── [selection]
-- >├── [link]
-- >┊
-- >╰── [link]
-- 
-- 
-- GtkLabel has a single CSS node with the name label. A wide variety
-- of style classes may be applied to labels, such as .title, .subtitle,
-- .dim-label, etc. In the t'GI.Gtk.Objects.ShortcutsWindow.ShortcutsWindow', labels are used wth the
-- .keycap style class.
-- 
-- If the label has a selection, it gets a subnode with name selection.
-- 
-- If the label has links, there is one subnode per link. These subnodes
-- carry the link or visited state depending on whether they have been
-- visited.
-- 
-- = GtkLabel as GtkBuildable
-- 
-- The GtkLabel implementation of the GtkBuildable interface supports a
-- custom @\<attributes>@ element, which supports any number of @\<attribute>@
-- elements. The @\<attribute>@ element has attributes named “name“, “value“,
-- “start“ and “end“ and allows you to specify t'GI.Pango.Structs.Attribute.Attribute' values for
-- this label.
-- 
-- An example of a UI definition fragment specifying Pango attributes:
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkLabel">
-- >  <attributes>
-- >    <attribute name="weight" value="PANGO_WEIGHT_BOLD"/>
-- >    <attribute name="background" value="red" start="5" end="10"/>
-- >  </attributes>
-- ></object>
-- 
-- 
-- The start and end attributes specify the range of characters to which the
-- Pango attribute applies. If start and end are not specified, the attribute is
-- applied to the whole text. Note that specifying ranges does not make much
-- sense with translatable attributes. Use markup embedded in the translatable
-- content instead.
-- 
-- = Mnemonics
-- 
-- Labels may contain “mnemonics”. Mnemonics are
-- underlined characters in the label, used for keyboard navigation.
-- Mnemonics are created by providing a string with an underscore before
-- the mnemonic character, such as @\"_File\"@, to the
-- functions 'GI.Gtk.Objects.Label.labelNewWithMnemonic' or
-- 'GI.Gtk.Objects.Label.labelSetTextWithMnemonic'.
-- 
-- Mnemonics automatically activate any activatable widget the label is
-- inside, such as a t'GI.Gtk.Objects.Button.Button'; if the label is not inside the
-- mnemonic’s target widget, you have to tell the label about the target
-- using 'GI.Gtk.Objects.Label.labelSetMnemonicWidget'. Here’s a simple example where
-- the label is inside a button:
-- 
-- 
-- === /C code/
-- >
-- >  // Pressing Alt+H will activate this button
-- >  GtkWidget *button = gtk_button_new ();
-- >  GtkWidget *label = gtk_label_new_with_mnemonic ("_Hello");
-- >  gtk_container_add (GTK_CONTAINER (button), label);
-- 
-- 
-- There’s a convenience function to create buttons with a mnemonic label
-- already inside:
-- 
-- 
-- === /C code/
-- >
-- >  // Pressing Alt+H will activate this button
-- >  GtkWidget *button = gtk_button_new_with_mnemonic ("_Hello");
-- 
-- 
-- To create a mnemonic for a widget alongside the label, such as a
-- t'GI.Gtk.Objects.Entry.Entry', you have to point the label at the entry with
-- 'GI.Gtk.Objects.Label.labelSetMnemonicWidget':
-- 
-- 
-- === /C code/
-- >
-- >  // Pressing Alt+H will focus the entry
-- >  GtkWidget *entry = gtk_entry_new ();
-- >  GtkWidget *label = gtk_label_new_with_mnemonic ("_Hello");
-- >  gtk_label_set_mnemonic_widget (GTK_LABEL (label), entry);
-- 
-- 
-- = Markup (styled text)
-- 
-- To make it easy to format text in a label (changing colors,
-- fonts, etc.), label text can be provided in a simple
-- [markup format][PangoMarkupFormat].
-- 
-- Here’s how to create a label with a small font:
-- 
-- === /C code/
-- >
-- >  GtkWidget *label = gtk_label_new (NULL);
-- >  gtk_label_set_markup (GTK_LABEL (label), "<small>Small text</small>");
-- 
-- 
-- (See [complete documentation][PangoMarkupFormat] of available
-- tags in the Pango manual.)
-- 
-- The markup passed to 'GI.Gtk.Objects.Label.labelSetMarkup' must be valid; for example,
-- literal \<, > and & characters must be escaped as &lt;, &gt;, and &amp;.
-- If you pass text obtained from the user, file, or a network to
-- 'GI.Gtk.Objects.Label.labelSetMarkup', you’ll want to escape it with
-- 'GI.GLib.Functions.markupEscapeText' or @/g_markup_printf_escaped()/@.
-- 
-- Markup strings are just a convenient way to set the t'GI.Pango.Structs.AttrList.AttrList' on
-- a label; 'GI.Gtk.Objects.Label.labelSetAttributes' may be a simpler way to set
-- attributes in some cases. Be careful though; t'GI.Pango.Structs.AttrList.AttrList' tends to
-- cause internationalization problems, unless you’re applying attributes
-- to the entire string (i.e. unless you set the range of each attribute
-- to [0, @/G_MAXINT/@)). The reason is that specifying the start_index and
-- end_index for a t'GI.Pango.Structs.Attribute.Attribute' requires knowledge of the exact string
-- being displayed, so translations will cause problems.
-- 
-- = Selectable labels
-- 
-- Labels can be made selectable with 'GI.Gtk.Objects.Label.labelSetSelectable'.
-- Selectable labels allow the user to copy the label contents to
-- the clipboard. Only labels that contain useful-to-copy information
-- — such as error messages — should be made selectable.
-- 
-- # Text layout # {@/label/@-text-layout}
-- 
-- A label can contain any number of paragraphs, but will have
-- performance problems if it contains more than a small number.
-- Paragraphs are separated by newlines or other paragraph separators
-- understood by Pango.
-- 
-- Labels can automatically wrap text if you call
-- 'GI.Gtk.Objects.Label.labelSetLineWrap'.
-- 
-- 'GI.Gtk.Objects.Label.labelSetJustify' sets how the lines in a label align
-- with one another. If you want to set how the label as a whole
-- aligns in its available space, see the [Widget:halign]("GI.Gtk.Objects.Widget#g:attr:halign") and
-- [Widget:valign]("GI.Gtk.Objects.Widget#g:attr:valign") properties.
-- 
-- The [Label:widthChars]("GI.Gtk.Objects.Label#g:attr:widthChars") and [Label:maxWidthChars]("GI.Gtk.Objects.Label#g:attr:maxWidthChars") properties
-- can be used to control the size allocation of ellipsized or wrapped
-- labels. For ellipsizing labels, if either is specified (and less
-- than the actual text size), it is used as the minimum width, and the actual
-- text size is used as the natural width of the label. For wrapping labels,
-- width-chars is used as the minimum width, if specified, and max-width-chars
-- is used as the natural width. Even if max-width-chars specified, wrapping
-- labels will be rewrapped to use all of the available width.
-- 
-- Note that the interpretation of [Label:widthChars]("GI.Gtk.Objects.Label#g:attr:widthChars") and
-- [Label:maxWidthChars]("GI.Gtk.Objects.Label#g:attr:maxWidthChars") has changed a bit with the introduction of
-- [width-for-height geometry management.][geometry-management]
-- 
-- = Links
-- 
-- Since 2.18, GTK+ supports markup for clickable hyperlinks in addition
-- to regular Pango markup. The markup for links is borrowed from HTML,
-- using the @\<a>@ with “href“ and “title“ attributes. GTK+ renders links
-- similar to the way they appear in web browsers, with colored, underlined
-- text. The “title“ attribute is displayed as a tooltip on the link.
-- 
-- An example looks like this:
-- 
-- 
-- === /C code/
-- >
-- >const gchar *text =
-- >"Go to the"
-- >"<a href=\"http://www.gtk.org title=\"&lt;i&gt;Our&lt;/i&gt; website\">"
-- >"GTK+ website</a> for more...";
-- >GtkWidget *label = gtk_label_new (NULL);
-- >gtk_label_set_markup (GTK_LABEL (label), text);
-- 
-- 
-- It is possible to implement custom handling for links and their tooltips with
-- the [Label::activateLink]("GI.Gtk.Objects.Label#g:signal:activateLink") signal and the 'GI.Gtk.Objects.Label.labelGetCurrentUri' function.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Label
    ( 

-- * Exported types
    Label(..)                               ,
    IsLabel                                 ,
    toLabel                                 ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.Widget#g:method:activate"), [addAccelerator]("GI.Gtk.Objects.Widget#g:method:addAccelerator"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDeviceEvents]("GI.Gtk.Objects.Widget#g:method:addDeviceEvents"), [addEvents]("GI.Gtk.Objects.Widget#g:method:addEvents"), [addMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:addMnemonicLabel"), [addTickCallback]("GI.Gtk.Objects.Widget#g:method:addTickCallback"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [canActivateAccel]("GI.Gtk.Objects.Widget#g:method:canActivateAccel"), [childFocus]("GI.Gtk.Objects.Widget#g:method:childFocus"), [childNotify]("GI.Gtk.Objects.Widget#g:method:childNotify"), [classPath]("GI.Gtk.Objects.Widget#g:method:classPath"), [computeExpand]("GI.Gtk.Objects.Widget#g:method:computeExpand"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [createPangoContext]("GI.Gtk.Objects.Widget#g:method:createPangoContext"), [createPangoLayout]("GI.Gtk.Objects.Widget#g:method:createPangoLayout"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [destroy]("GI.Gtk.Objects.Widget#g:method:destroy"), [destroyed]("GI.Gtk.Objects.Widget#g:method:destroyed"), [deviceIsShadowed]("GI.Gtk.Objects.Widget#g:method:deviceIsShadowed"), [dragBegin]("GI.Gtk.Objects.Widget#g:method:dragBegin"), [dragBeginWithCoordinates]("GI.Gtk.Objects.Widget#g:method:dragBeginWithCoordinates"), [dragCheckThreshold]("GI.Gtk.Objects.Widget#g:method:dragCheckThreshold"), [dragDestAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddImageTargets"), [dragDestAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddTextTargets"), [dragDestAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragDestAddUriTargets"), [dragDestFindTarget]("GI.Gtk.Objects.Widget#g:method:dragDestFindTarget"), [dragDestGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestGetTargetList"), [dragDestGetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestGetTrackMotion"), [dragDestSet]("GI.Gtk.Objects.Widget#g:method:dragDestSet"), [dragDestSetProxy]("GI.Gtk.Objects.Widget#g:method:dragDestSetProxy"), [dragDestSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragDestSetTargetList"), [dragDestSetTrackMotion]("GI.Gtk.Objects.Widget#g:method:dragDestSetTrackMotion"), [dragDestUnset]("GI.Gtk.Objects.Widget#g:method:dragDestUnset"), [dragGetData]("GI.Gtk.Objects.Widget#g:method:dragGetData"), [dragHighlight]("GI.Gtk.Objects.Widget#g:method:dragHighlight"), [dragSourceAddImageTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddImageTargets"), [dragSourceAddTextTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddTextTargets"), [dragSourceAddUriTargets]("GI.Gtk.Objects.Widget#g:method:dragSourceAddUriTargets"), [dragSourceGetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceGetTargetList"), [dragSourceSet]("GI.Gtk.Objects.Widget#g:method:dragSourceSet"), [dragSourceSetIconGicon]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconGicon"), [dragSourceSetIconName]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconName"), [dragSourceSetIconPixbuf]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconPixbuf"), [dragSourceSetIconStock]("GI.Gtk.Objects.Widget#g:method:dragSourceSetIconStock"), [dragSourceSetTargetList]("GI.Gtk.Objects.Widget#g:method:dragSourceSetTargetList"), [dragSourceUnset]("GI.Gtk.Objects.Widget#g:method:dragSourceUnset"), [dragUnhighlight]("GI.Gtk.Objects.Widget#g:method:dragUnhighlight"), [draw]("GI.Gtk.Objects.Widget#g:method:draw"), [ensureStyle]("GI.Gtk.Objects.Widget#g:method:ensureStyle"), [errorBell]("GI.Gtk.Objects.Widget#g:method:errorBell"), [event]("GI.Gtk.Objects.Widget#g:method:event"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeChildNotify]("GI.Gtk.Objects.Widget#g:method:freezeChildNotify"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [grabAdd]("GI.Gtk.Objects.Widget#g:method:grabAdd"), [grabDefault]("GI.Gtk.Objects.Widget#g:method:grabDefault"), [grabFocus]("GI.Gtk.Objects.Widget#g:method:grabFocus"), [grabRemove]("GI.Gtk.Objects.Widget#g:method:grabRemove"), [hasDefault]("GI.Gtk.Objects.Widget#g:method:hasDefault"), [hasFocus]("GI.Gtk.Objects.Widget#g:method:hasFocus"), [hasGrab]("GI.Gtk.Objects.Widget#g:method:hasGrab"), [hasRcStyle]("GI.Gtk.Objects.Widget#g:method:hasRcStyle"), [hasScreen]("GI.Gtk.Objects.Widget#g:method:hasScreen"), [hasVisibleFocus]("GI.Gtk.Objects.Widget#g:method:hasVisibleFocus"), [hide]("GI.Gtk.Objects.Widget#g:method:hide"), [hideOnDelete]("GI.Gtk.Objects.Widget#g:method:hideOnDelete"), [inDestruction]("GI.Gtk.Objects.Widget#g:method:inDestruction"), [initTemplate]("GI.Gtk.Objects.Widget#g:method:initTemplate"), [inputShapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:inputShapeCombineRegion"), [insertActionGroup]("GI.Gtk.Objects.Widget#g:method:insertActionGroup"), [intersect]("GI.Gtk.Objects.Widget#g:method:intersect"), [isAncestor]("GI.Gtk.Objects.Widget#g:method:isAncestor"), [isComposited]("GI.Gtk.Objects.Widget#g:method:isComposited"), [isDrawable]("GI.Gtk.Objects.Widget#g:method:isDrawable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isFocus]("GI.Gtk.Objects.Widget#g:method:isFocus"), [isSensitive]("GI.Gtk.Objects.Widget#g:method:isSensitive"), [isToplevel]("GI.Gtk.Objects.Widget#g:method:isToplevel"), [isVisible]("GI.Gtk.Objects.Widget#g:method:isVisible"), [keynavFailed]("GI.Gtk.Objects.Widget#g:method:keynavFailed"), [listAccelClosures]("GI.Gtk.Objects.Widget#g:method:listAccelClosures"), [listActionPrefixes]("GI.Gtk.Objects.Widget#g:method:listActionPrefixes"), [listMnemonicLabels]("GI.Gtk.Objects.Widget#g:method:listMnemonicLabels"), [map]("GI.Gtk.Objects.Widget#g:method:map"), [mnemonicActivate]("GI.Gtk.Objects.Widget#g:method:mnemonicActivate"), [modifyBase]("GI.Gtk.Objects.Widget#g:method:modifyBase"), [modifyBg]("GI.Gtk.Objects.Widget#g:method:modifyBg"), [modifyCursor]("GI.Gtk.Objects.Widget#g:method:modifyCursor"), [modifyFg]("GI.Gtk.Objects.Widget#g:method:modifyFg"), [modifyFont]("GI.Gtk.Objects.Widget#g:method:modifyFont"), [modifyStyle]("GI.Gtk.Objects.Widget#g:method:modifyStyle"), [modifyText]("GI.Gtk.Objects.Widget#g:method:modifyText"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [overrideBackgroundColor]("GI.Gtk.Objects.Widget#g:method:overrideBackgroundColor"), [overrideColor]("GI.Gtk.Objects.Widget#g:method:overrideColor"), [overrideCursor]("GI.Gtk.Objects.Widget#g:method:overrideCursor"), [overrideFont]("GI.Gtk.Objects.Widget#g:method:overrideFont"), [overrideSymbolicColor]("GI.Gtk.Objects.Widget#g:method:overrideSymbolicColor"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [path]("GI.Gtk.Objects.Widget#g:method:path"), [queueAllocate]("GI.Gtk.Objects.Widget#g:method:queueAllocate"), [queueComputeExpand]("GI.Gtk.Objects.Widget#g:method:queueComputeExpand"), [queueDraw]("GI.Gtk.Objects.Widget#g:method:queueDraw"), [queueDrawArea]("GI.Gtk.Objects.Widget#g:method:queueDrawArea"), [queueDrawRegion]("GI.Gtk.Objects.Widget#g:method:queueDrawRegion"), [queueResize]("GI.Gtk.Objects.Widget#g:method:queueResize"), [queueResizeNoRedraw]("GI.Gtk.Objects.Widget#g:method:queueResizeNoRedraw"), [realize]("GI.Gtk.Objects.Widget#g:method:realize"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [regionIntersect]("GI.Gtk.Objects.Widget#g:method:regionIntersect"), [registerWindow]("GI.Gtk.Objects.Widget#g:method:registerWindow"), [removeAccelerator]("GI.Gtk.Objects.Widget#g:method:removeAccelerator"), [removeMnemonicLabel]("GI.Gtk.Objects.Widget#g:method:removeMnemonicLabel"), [removeTickCallback]("GI.Gtk.Objects.Widget#g:method:removeTickCallback"), [renderIcon]("GI.Gtk.Objects.Widget#g:method:renderIcon"), [renderIconPixbuf]("GI.Gtk.Objects.Widget#g:method:renderIconPixbuf"), [reparent]("GI.Gtk.Objects.Widget#g:method:reparent"), [resetRcStyles]("GI.Gtk.Objects.Widget#g:method:resetRcStyles"), [resetStyle]("GI.Gtk.Objects.Widget#g:method:resetStyle"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [selectRegion]("GI.Gtk.Objects.Label#g:method:selectRegion"), [sendExpose]("GI.Gtk.Objects.Widget#g:method:sendExpose"), [sendFocusChange]("GI.Gtk.Objects.Widget#g:method:sendFocusChange"), [shapeCombineRegion]("GI.Gtk.Objects.Widget#g:method:shapeCombineRegion"), [show]("GI.Gtk.Objects.Widget#g:method:show"), [showAll]("GI.Gtk.Objects.Widget#g:method:showAll"), [showNow]("GI.Gtk.Objects.Widget#g:method:showNow"), [sizeAllocate]("GI.Gtk.Objects.Widget#g:method:sizeAllocate"), [sizeAllocateWithBaseline]("GI.Gtk.Objects.Widget#g:method:sizeAllocateWithBaseline"), [sizeRequest]("GI.Gtk.Objects.Widget#g:method:sizeRequest"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [styleAttach]("GI.Gtk.Objects.Widget#g:method:styleAttach"), [styleGetProperty]("GI.Gtk.Objects.Widget#g:method:styleGetProperty"), [thawChildNotify]("GI.Gtk.Objects.Widget#g:method:thawChildNotify"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [translateCoordinates]("GI.Gtk.Objects.Widget#g:method:translateCoordinates"), [triggerTooltipQuery]("GI.Gtk.Objects.Widget#g:method:triggerTooltipQuery"), [unmap]("GI.Gtk.Objects.Widget#g:method:unmap"), [unparent]("GI.Gtk.Objects.Widget#g:method:unparent"), [unrealize]("GI.Gtk.Objects.Widget#g:method:unrealize"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unregisterWindow]("GI.Gtk.Objects.Widget#g:method:unregisterWindow"), [unsetStateFlags]("GI.Gtk.Objects.Widget#g:method:unsetStateFlags"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAccessible]("GI.Gtk.Objects.Widget#g:method:getAccessible"), [getActionGroup]("GI.Gtk.Objects.Widget#g:method:getActionGroup"), [getAlignment]("GI.Gtk.Objects.Misc#g:method:getAlignment"), [getAllocatedBaseline]("GI.Gtk.Objects.Widget#g:method:getAllocatedBaseline"), [getAllocatedHeight]("GI.Gtk.Objects.Widget#g:method:getAllocatedHeight"), [getAllocatedSize]("GI.Gtk.Objects.Widget#g:method:getAllocatedSize"), [getAllocatedWidth]("GI.Gtk.Objects.Widget#g:method:getAllocatedWidth"), [getAllocation]("GI.Gtk.Objects.Widget#g:method:getAllocation"), [getAncestor]("GI.Gtk.Objects.Widget#g:method:getAncestor"), [getAngle]("GI.Gtk.Objects.Label#g:method:getAngle"), [getAppPaintable]("GI.Gtk.Objects.Widget#g:method:getAppPaintable"), [getAttributes]("GI.Gtk.Objects.Label#g:method:getAttributes"), [getCanDefault]("GI.Gtk.Objects.Widget#g:method:getCanDefault"), [getCanFocus]("GI.Gtk.Objects.Widget#g:method:getCanFocus"), [getChildRequisition]("GI.Gtk.Objects.Widget#g:method:getChildRequisition"), [getChildVisible]("GI.Gtk.Objects.Widget#g:method:getChildVisible"), [getClip]("GI.Gtk.Objects.Widget#g:method:getClip"), [getClipboard]("GI.Gtk.Objects.Widget#g:method:getClipboard"), [getCompositeName]("GI.Gtk.Objects.Widget#g:method:getCompositeName"), [getCurrentUri]("GI.Gtk.Objects.Label#g:method:getCurrentUri"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:getDeviceEnabled"), [getDeviceEvents]("GI.Gtk.Objects.Widget#g:method:getDeviceEvents"), [getDirection]("GI.Gtk.Objects.Widget#g:method:getDirection"), [getDisplay]("GI.Gtk.Objects.Widget#g:method:getDisplay"), [getDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:getDoubleBuffered"), [getEllipsize]("GI.Gtk.Objects.Label#g:method:getEllipsize"), [getEvents]("GI.Gtk.Objects.Widget#g:method:getEvents"), [getFocusOnClick]("GI.Gtk.Objects.Widget#g:method:getFocusOnClick"), [getFontMap]("GI.Gtk.Objects.Widget#g:method:getFontMap"), [getFontOptions]("GI.Gtk.Objects.Widget#g:method:getFontOptions"), [getFrameClock]("GI.Gtk.Objects.Widget#g:method:getFrameClock"), [getHalign]("GI.Gtk.Objects.Widget#g:method:getHalign"), [getHasTooltip]("GI.Gtk.Objects.Widget#g:method:getHasTooltip"), [getHasWindow]("GI.Gtk.Objects.Widget#g:method:getHasWindow"), [getHexpand]("GI.Gtk.Objects.Widget#g:method:getHexpand"), [getHexpandSet]("GI.Gtk.Objects.Widget#g:method:getHexpandSet"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getJustify]("GI.Gtk.Objects.Label#g:method:getJustify"), [getLabel]("GI.Gtk.Objects.Label#g:method:getLabel"), [getLayout]("GI.Gtk.Objects.Label#g:method:getLayout"), [getLayoutOffsets]("GI.Gtk.Objects.Label#g:method:getLayoutOffsets"), [getLineWrap]("GI.Gtk.Objects.Label#g:method:getLineWrap"), [getLineWrapMode]("GI.Gtk.Objects.Label#g:method:getLineWrapMode"), [getLines]("GI.Gtk.Objects.Label#g:method:getLines"), [getMapped]("GI.Gtk.Objects.Widget#g:method:getMapped"), [getMarginBottom]("GI.Gtk.Objects.Widget#g:method:getMarginBottom"), [getMarginEnd]("GI.Gtk.Objects.Widget#g:method:getMarginEnd"), [getMarginLeft]("GI.Gtk.Objects.Widget#g:method:getMarginLeft"), [getMarginRight]("GI.Gtk.Objects.Widget#g:method:getMarginRight"), [getMarginStart]("GI.Gtk.Objects.Widget#g:method:getMarginStart"), [getMarginTop]("GI.Gtk.Objects.Widget#g:method:getMarginTop"), [getMaxWidthChars]("GI.Gtk.Objects.Label#g:method:getMaxWidthChars"), [getMnemonicKeyval]("GI.Gtk.Objects.Label#g:method:getMnemonicKeyval"), [getMnemonicWidget]("GI.Gtk.Objects.Label#g:method:getMnemonicWidget"), [getModifierMask]("GI.Gtk.Objects.Widget#g:method:getModifierMask"), [getModifierStyle]("GI.Gtk.Objects.Widget#g:method:getModifierStyle"), [getName]("GI.Gtk.Objects.Widget#g:method:getName"), [getNoShowAll]("GI.Gtk.Objects.Widget#g:method:getNoShowAll"), [getOpacity]("GI.Gtk.Objects.Widget#g:method:getOpacity"), [getPadding]("GI.Gtk.Objects.Misc#g:method:getPadding"), [getPangoContext]("GI.Gtk.Objects.Widget#g:method:getPangoContext"), [getParent]("GI.Gtk.Objects.Widget#g:method:getParent"), [getParentWindow]("GI.Gtk.Objects.Widget#g:method:getParentWindow"), [getPath]("GI.Gtk.Objects.Widget#g:method:getPath"), [getPointer]("GI.Gtk.Objects.Widget#g:method:getPointer"), [getPreferredHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredHeight"), [getPreferredHeightAndBaselineForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightAndBaselineForWidth"), [getPreferredHeightForWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.Widget#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.Widget#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.Widget#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRealized]("GI.Gtk.Objects.Widget#g:method:getRealized"), [getReceivesDefault]("GI.Gtk.Objects.Widget#g:method:getReceivesDefault"), [getRequestMode]("GI.Gtk.Objects.Widget#g:method:getRequestMode"), [getRequisition]("GI.Gtk.Objects.Widget#g:method:getRequisition"), [getRootWindow]("GI.Gtk.Objects.Widget#g:method:getRootWindow"), [getScaleFactor]("GI.Gtk.Objects.Widget#g:method:getScaleFactor"), [getScreen]("GI.Gtk.Objects.Widget#g:method:getScreen"), [getSelectable]("GI.Gtk.Objects.Label#g:method:getSelectable"), [getSelectionBounds]("GI.Gtk.Objects.Label#g:method:getSelectionBounds"), [getSensitive]("GI.Gtk.Objects.Widget#g:method:getSensitive"), [getSettings]("GI.Gtk.Objects.Widget#g:method:getSettings"), [getSingleLineMode]("GI.Gtk.Objects.Label#g:method:getSingleLineMode"), [getSizeRequest]("GI.Gtk.Objects.Widget#g:method:getSizeRequest"), [getState]("GI.Gtk.Objects.Widget#g:method:getState"), [getStateFlags]("GI.Gtk.Objects.Widget#g:method:getStateFlags"), [getStyle]("GI.Gtk.Objects.Widget#g:method:getStyle"), [getStyleContext]("GI.Gtk.Objects.Widget#g:method:getStyleContext"), [getSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:getSupportMultidevice"), [getTemplateChild]("GI.Gtk.Objects.Widget#g:method:getTemplateChild"), [getText]("GI.Gtk.Objects.Label#g:method:getText"), [getTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:getTooltipMarkup"), [getTooltipText]("GI.Gtk.Objects.Widget#g:method:getTooltipText"), [getTooltipWindow]("GI.Gtk.Objects.Widget#g:method:getTooltipWindow"), [getToplevel]("GI.Gtk.Objects.Widget#g:method:getToplevel"), [getTrackVisitedLinks]("GI.Gtk.Objects.Label#g:method:getTrackVisitedLinks"), [getUseMarkup]("GI.Gtk.Objects.Label#g:method:getUseMarkup"), [getUseUnderline]("GI.Gtk.Objects.Label#g:method:getUseUnderline"), [getValign]("GI.Gtk.Objects.Widget#g:method:getValign"), [getValignWithBaseline]("GI.Gtk.Objects.Widget#g:method:getValignWithBaseline"), [getVexpand]("GI.Gtk.Objects.Widget#g:method:getVexpand"), [getVexpandSet]("GI.Gtk.Objects.Widget#g:method:getVexpandSet"), [getVisible]("GI.Gtk.Objects.Widget#g:method:getVisible"), [getVisual]("GI.Gtk.Objects.Widget#g:method:getVisual"), [getWidthChars]("GI.Gtk.Objects.Label#g:method:getWidthChars"), [getWindow]("GI.Gtk.Objects.Widget#g:method:getWindow"), [getXalign]("GI.Gtk.Objects.Label#g:method:getXalign"), [getYalign]("GI.Gtk.Objects.Label#g:method:getYalign").
-- 
-- ==== Setters
-- [setAccelPath]("GI.Gtk.Objects.Widget#g:method:setAccelPath"), [setAlignment]("GI.Gtk.Objects.Misc#g:method:setAlignment"), [setAllocation]("GI.Gtk.Objects.Widget#g:method:setAllocation"), [setAngle]("GI.Gtk.Objects.Label#g:method:setAngle"), [setAppPaintable]("GI.Gtk.Objects.Widget#g:method:setAppPaintable"), [setAttributes]("GI.Gtk.Objects.Label#g:method:setAttributes"), [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCanDefault]("GI.Gtk.Objects.Widget#g:method:setCanDefault"), [setCanFocus]("GI.Gtk.Objects.Widget#g:method:setCanFocus"), [setChildVisible]("GI.Gtk.Objects.Widget#g:method:setChildVisible"), [setClip]("GI.Gtk.Objects.Widget#g:method:setClip"), [setCompositeName]("GI.Gtk.Objects.Widget#g:method:setCompositeName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDeviceEnabled]("GI.Gtk.Objects.Widget#g:method:setDeviceEnabled"), [setDeviceEvents]("GI.Gtk.Objects.Widget#g:method:setDeviceEvents"), [setDirection]("GI.Gtk.Objects.Widget#g:method:setDirection"), [setDoubleBuffered]("GI.Gtk.Objects.Widget#g:method:setDoubleBuffered"), [setEllipsize]("GI.Gtk.Objects.Label#g:method:setEllipsize"), [setEvents]("GI.Gtk.Objects.Widget#g:method:setEvents"), [setFocusOnClick]("GI.Gtk.Objects.Widget#g:method:setFocusOnClick"), [setFontMap]("GI.Gtk.Objects.Widget#g:method:setFontMap"), [setFontOptions]("GI.Gtk.Objects.Widget#g:method:setFontOptions"), [setHalign]("GI.Gtk.Objects.Widget#g:method:setHalign"), [setHasTooltip]("GI.Gtk.Objects.Widget#g:method:setHasTooltip"), [setHasWindow]("GI.Gtk.Objects.Widget#g:method:setHasWindow"), [setHexpand]("GI.Gtk.Objects.Widget#g:method:setHexpand"), [setHexpandSet]("GI.Gtk.Objects.Widget#g:method:setHexpandSet"), [setJustify]("GI.Gtk.Objects.Label#g:method:setJustify"), [setLabel]("GI.Gtk.Objects.Label#g:method:setLabel"), [setLineWrap]("GI.Gtk.Objects.Label#g:method:setLineWrap"), [setLineWrapMode]("GI.Gtk.Objects.Label#g:method:setLineWrapMode"), [setLines]("GI.Gtk.Objects.Label#g:method:setLines"), [setMapped]("GI.Gtk.Objects.Widget#g:method:setMapped"), [setMarginBottom]("GI.Gtk.Objects.Widget#g:method:setMarginBottom"), [setMarginEnd]("GI.Gtk.Objects.Widget#g:method:setMarginEnd"), [setMarginLeft]("GI.Gtk.Objects.Widget#g:method:setMarginLeft"), [setMarginRight]("GI.Gtk.Objects.Widget#g:method:setMarginRight"), [setMarginStart]("GI.Gtk.Objects.Widget#g:method:setMarginStart"), [setMarginTop]("GI.Gtk.Objects.Widget#g:method:setMarginTop"), [setMarkup]("GI.Gtk.Objects.Label#g:method:setMarkup"), [setMarkupWithMnemonic]("GI.Gtk.Objects.Label#g:method:setMarkupWithMnemonic"), [setMaxWidthChars]("GI.Gtk.Objects.Label#g:method:setMaxWidthChars"), [setMnemonicWidget]("GI.Gtk.Objects.Label#g:method:setMnemonicWidget"), [setName]("GI.Gtk.Objects.Widget#g:method:setName"), [setNoShowAll]("GI.Gtk.Objects.Widget#g:method:setNoShowAll"), [setOpacity]("GI.Gtk.Objects.Widget#g:method:setOpacity"), [setPadding]("GI.Gtk.Objects.Misc#g:method:setPadding"), [setParent]("GI.Gtk.Objects.Widget#g:method:setParent"), [setParentWindow]("GI.Gtk.Objects.Widget#g:method:setParentWindow"), [setPattern]("GI.Gtk.Objects.Label#g:method:setPattern"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRealized]("GI.Gtk.Objects.Widget#g:method:setRealized"), [setReceivesDefault]("GI.Gtk.Objects.Widget#g:method:setReceivesDefault"), [setRedrawOnAllocate]("GI.Gtk.Objects.Widget#g:method:setRedrawOnAllocate"), [setSelectable]("GI.Gtk.Objects.Label#g:method:setSelectable"), [setSensitive]("GI.Gtk.Objects.Widget#g:method:setSensitive"), [setSingleLineMode]("GI.Gtk.Objects.Label#g:method:setSingleLineMode"), [setSizeRequest]("GI.Gtk.Objects.Widget#g:method:setSizeRequest"), [setState]("GI.Gtk.Objects.Widget#g:method:setState"), [setStateFlags]("GI.Gtk.Objects.Widget#g:method:setStateFlags"), [setStyle]("GI.Gtk.Objects.Widget#g:method:setStyle"), [setSupportMultidevice]("GI.Gtk.Objects.Widget#g:method:setSupportMultidevice"), [setText]("GI.Gtk.Objects.Label#g:method:setText"), [setTextWithMnemonic]("GI.Gtk.Objects.Label#g:method:setTextWithMnemonic"), [setTooltipMarkup]("GI.Gtk.Objects.Widget#g:method:setTooltipMarkup"), [setTooltipText]("GI.Gtk.Objects.Widget#g:method:setTooltipText"), [setTooltipWindow]("GI.Gtk.Objects.Widget#g:method:setTooltipWindow"), [setTrackVisitedLinks]("GI.Gtk.Objects.Label#g:method:setTrackVisitedLinks"), [setUseMarkup]("GI.Gtk.Objects.Label#g:method:setUseMarkup"), [setUseUnderline]("GI.Gtk.Objects.Label#g:method:setUseUnderline"), [setValign]("GI.Gtk.Objects.Widget#g:method:setValign"), [setVexpand]("GI.Gtk.Objects.Widget#g:method:setVexpand"), [setVexpandSet]("GI.Gtk.Objects.Widget#g:method:setVexpandSet"), [setVisible]("GI.Gtk.Objects.Widget#g:method:setVisible"), [setVisual]("GI.Gtk.Objects.Widget#g:method:setVisual"), [setWidthChars]("GI.Gtk.Objects.Label#g:method:setWidthChars"), [setWindow]("GI.Gtk.Objects.Widget#g:method:setWindow"), [setXalign]("GI.Gtk.Objects.Label#g:method:setXalign"), [setYalign]("GI.Gtk.Objects.Label#g:method:setYalign").

#if defined(ENABLE_OVERLOADING)
    ResolveLabelMethod                      ,
#endif

-- ** getAngle #method:getAngle#

#if defined(ENABLE_OVERLOADING)
    LabelGetAngleMethodInfo                 ,
#endif
    labelGetAngle                           ,


-- ** getAttributes #method:getAttributes#

#if defined(ENABLE_OVERLOADING)
    LabelGetAttributesMethodInfo            ,
#endif
    labelGetAttributes                      ,


-- ** getCurrentUri #method:getCurrentUri#

#if defined(ENABLE_OVERLOADING)
    LabelGetCurrentUriMethodInfo            ,
#endif
    labelGetCurrentUri                      ,


-- ** getEllipsize #method:getEllipsize#

#if defined(ENABLE_OVERLOADING)
    LabelGetEllipsizeMethodInfo             ,
#endif
    labelGetEllipsize                       ,


-- ** getJustify #method:getJustify#

#if defined(ENABLE_OVERLOADING)
    LabelGetJustifyMethodInfo               ,
#endif
    labelGetJustify                         ,


-- ** getLabel #method:getLabel#

#if defined(ENABLE_OVERLOADING)
    LabelGetLabelMethodInfo                 ,
#endif
    labelGetLabel                           ,


-- ** getLayout #method:getLayout#

#if defined(ENABLE_OVERLOADING)
    LabelGetLayoutMethodInfo                ,
#endif
    labelGetLayout                          ,


-- ** getLayoutOffsets #method:getLayoutOffsets#

#if defined(ENABLE_OVERLOADING)
    LabelGetLayoutOffsetsMethodInfo         ,
#endif
    labelGetLayoutOffsets                   ,


-- ** getLineWrap #method:getLineWrap#

#if defined(ENABLE_OVERLOADING)
    LabelGetLineWrapMethodInfo              ,
#endif
    labelGetLineWrap                        ,


-- ** getLineWrapMode #method:getLineWrapMode#

#if defined(ENABLE_OVERLOADING)
    LabelGetLineWrapModeMethodInfo          ,
#endif
    labelGetLineWrapMode                    ,


-- ** getLines #method:getLines#

#if defined(ENABLE_OVERLOADING)
    LabelGetLinesMethodInfo                 ,
#endif
    labelGetLines                           ,


-- ** getMaxWidthChars #method:getMaxWidthChars#

#if defined(ENABLE_OVERLOADING)
    LabelGetMaxWidthCharsMethodInfo         ,
#endif
    labelGetMaxWidthChars                   ,


-- ** getMnemonicKeyval #method:getMnemonicKeyval#

#if defined(ENABLE_OVERLOADING)
    LabelGetMnemonicKeyvalMethodInfo        ,
#endif
    labelGetMnemonicKeyval                  ,


-- ** getMnemonicWidget #method:getMnemonicWidget#

#if defined(ENABLE_OVERLOADING)
    LabelGetMnemonicWidgetMethodInfo        ,
#endif
    labelGetMnemonicWidget                  ,


-- ** getSelectable #method:getSelectable#

#if defined(ENABLE_OVERLOADING)
    LabelGetSelectableMethodInfo            ,
#endif
    labelGetSelectable                      ,


-- ** getSelectionBounds #method:getSelectionBounds#

#if defined(ENABLE_OVERLOADING)
    LabelGetSelectionBoundsMethodInfo       ,
#endif
    labelGetSelectionBounds                 ,


-- ** getSingleLineMode #method:getSingleLineMode#

#if defined(ENABLE_OVERLOADING)
    LabelGetSingleLineModeMethodInfo        ,
#endif
    labelGetSingleLineMode                  ,


-- ** getText #method:getText#

#if defined(ENABLE_OVERLOADING)
    LabelGetTextMethodInfo                  ,
#endif
    labelGetText                            ,


-- ** getTrackVisitedLinks #method:getTrackVisitedLinks#

#if defined(ENABLE_OVERLOADING)
    LabelGetTrackVisitedLinksMethodInfo     ,
#endif
    labelGetTrackVisitedLinks               ,


-- ** getUseMarkup #method:getUseMarkup#

#if defined(ENABLE_OVERLOADING)
    LabelGetUseMarkupMethodInfo             ,
#endif
    labelGetUseMarkup                       ,


-- ** getUseUnderline #method:getUseUnderline#

#if defined(ENABLE_OVERLOADING)
    LabelGetUseUnderlineMethodInfo          ,
#endif
    labelGetUseUnderline                    ,


-- ** getWidthChars #method:getWidthChars#

#if defined(ENABLE_OVERLOADING)
    LabelGetWidthCharsMethodInfo            ,
#endif
    labelGetWidthChars                      ,


-- ** getXalign #method:getXalign#

#if defined(ENABLE_OVERLOADING)
    LabelGetXalignMethodInfo                ,
#endif
    labelGetXalign                          ,


-- ** getYalign #method:getYalign#

#if defined(ENABLE_OVERLOADING)
    LabelGetYalignMethodInfo                ,
#endif
    labelGetYalign                          ,


-- ** new #method:new#

    labelNew                                ,


-- ** newWithMnemonic #method:newWithMnemonic#

    labelNewWithMnemonic                    ,


-- ** selectRegion #method:selectRegion#

#if defined(ENABLE_OVERLOADING)
    LabelSelectRegionMethodInfo             ,
#endif
    labelSelectRegion                       ,


-- ** setAngle #method:setAngle#

#if defined(ENABLE_OVERLOADING)
    LabelSetAngleMethodInfo                 ,
#endif
    labelSetAngle                           ,


-- ** setAttributes #method:setAttributes#

#if defined(ENABLE_OVERLOADING)
    LabelSetAttributesMethodInfo            ,
#endif
    labelSetAttributes                      ,


-- ** setEllipsize #method:setEllipsize#

#if defined(ENABLE_OVERLOADING)
    LabelSetEllipsizeMethodInfo             ,
#endif
    labelSetEllipsize                       ,


-- ** setJustify #method:setJustify#

#if defined(ENABLE_OVERLOADING)
    LabelSetJustifyMethodInfo               ,
#endif
    labelSetJustify                         ,


-- ** setLabel #method:setLabel#

#if defined(ENABLE_OVERLOADING)
    LabelSetLabelMethodInfo                 ,
#endif
    labelSetLabel                           ,


-- ** setLineWrap #method:setLineWrap#

#if defined(ENABLE_OVERLOADING)
    LabelSetLineWrapMethodInfo              ,
#endif
    labelSetLineWrap                        ,


-- ** setLineWrapMode #method:setLineWrapMode#

#if defined(ENABLE_OVERLOADING)
    LabelSetLineWrapModeMethodInfo          ,
#endif
    labelSetLineWrapMode                    ,


-- ** setLines #method:setLines#

#if defined(ENABLE_OVERLOADING)
    LabelSetLinesMethodInfo                 ,
#endif
    labelSetLines                           ,


-- ** setMarkup #method:setMarkup#

#if defined(ENABLE_OVERLOADING)
    LabelSetMarkupMethodInfo                ,
#endif
    labelSetMarkup                          ,


-- ** setMarkupWithMnemonic #method:setMarkupWithMnemonic#

#if defined(ENABLE_OVERLOADING)
    LabelSetMarkupWithMnemonicMethodInfo    ,
#endif
    labelSetMarkupWithMnemonic              ,


-- ** setMaxWidthChars #method:setMaxWidthChars#

#if defined(ENABLE_OVERLOADING)
    LabelSetMaxWidthCharsMethodInfo         ,
#endif
    labelSetMaxWidthChars                   ,


-- ** setMnemonicWidget #method:setMnemonicWidget#

#if defined(ENABLE_OVERLOADING)
    LabelSetMnemonicWidgetMethodInfo        ,
#endif
    labelSetMnemonicWidget                  ,


-- ** setPattern #method:setPattern#

#if defined(ENABLE_OVERLOADING)
    LabelSetPatternMethodInfo               ,
#endif
    labelSetPattern                         ,


-- ** setSelectable #method:setSelectable#

#if defined(ENABLE_OVERLOADING)
    LabelSetSelectableMethodInfo            ,
#endif
    labelSetSelectable                      ,


-- ** setSingleLineMode #method:setSingleLineMode#

#if defined(ENABLE_OVERLOADING)
    LabelSetSingleLineModeMethodInfo        ,
#endif
    labelSetSingleLineMode                  ,


-- ** setText #method:setText#

#if defined(ENABLE_OVERLOADING)
    LabelSetTextMethodInfo                  ,
#endif
    labelSetText                            ,


-- ** setTextWithMnemonic #method:setTextWithMnemonic#

#if defined(ENABLE_OVERLOADING)
    LabelSetTextWithMnemonicMethodInfo      ,
#endif
    labelSetTextWithMnemonic                ,


-- ** setTrackVisitedLinks #method:setTrackVisitedLinks#

#if defined(ENABLE_OVERLOADING)
    LabelSetTrackVisitedLinksMethodInfo     ,
#endif
    labelSetTrackVisitedLinks               ,


-- ** setUseMarkup #method:setUseMarkup#

#if defined(ENABLE_OVERLOADING)
    LabelSetUseMarkupMethodInfo             ,
#endif
    labelSetUseMarkup                       ,


-- ** setUseUnderline #method:setUseUnderline#

#if defined(ENABLE_OVERLOADING)
    LabelSetUseUnderlineMethodInfo          ,
#endif
    labelSetUseUnderline                    ,


-- ** setWidthChars #method:setWidthChars#

#if defined(ENABLE_OVERLOADING)
    LabelSetWidthCharsMethodInfo            ,
#endif
    labelSetWidthChars                      ,


-- ** setXalign #method:setXalign#

#if defined(ENABLE_OVERLOADING)
    LabelSetXalignMethodInfo                ,
#endif
    labelSetXalign                          ,


-- ** setYalign #method:setYalign#

#if defined(ENABLE_OVERLOADING)
    LabelSetYalignMethodInfo                ,
#endif
    labelSetYalign                          ,




 -- * Properties


-- ** angle #attr:angle#
-- | The angle that the baseline of the label makes with the horizontal,
-- in degrees, measured counterclockwise. An angle of 90 reads from
-- from bottom to top, an angle of 270, from top to bottom. Ignored
-- if the label is selectable.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    LabelAnglePropertyInfo                  ,
#endif
    constructLabelAngle                     ,
    getLabelAngle                           ,
#if defined(ENABLE_OVERLOADING)
    labelAngle                              ,
#endif
    setLabelAngle                           ,


-- ** attributes #attr:attributes#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LabelAttributesPropertyInfo             ,
#endif
    clearLabelAttributes                    ,
    constructLabelAttributes                ,
    getLabelAttributes                      ,
#if defined(ENABLE_OVERLOADING)
    labelAttributes                         ,
#endif
    setLabelAttributes                      ,


-- ** cursorPosition #attr:cursorPosition#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LabelCursorPositionPropertyInfo         ,
#endif
    getLabelCursorPosition                  ,
#if defined(ENABLE_OVERLOADING)
    labelCursorPosition                     ,
#endif


-- ** ellipsize #attr:ellipsize#
-- | The preferred place to ellipsize the string, if the label does
-- not have enough room to display the entire string, specified as a
-- t'GI.Pango.Enums.EllipsizeMode'.
-- 
-- Note that setting this property to a value other than
-- 'GI.Pango.Enums.EllipsizeModeNone' has the side-effect that the label requests
-- only enough space to display the ellipsis \"...\". In particular, this
-- means that ellipsizing labels do not work well in notebook tabs, unless
-- the t'GI.Gtk.Objects.Notebook.Notebook' tab-expand child property is set to 'P.True'. Other ways
-- to set a label\'s width are 'GI.Gtk.Objects.Widget.widgetSetSizeRequest' and
-- 'GI.Gtk.Objects.Label.labelSetWidthChars'.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    LabelEllipsizePropertyInfo              ,
#endif
    constructLabelEllipsize                 ,
    getLabelEllipsize                       ,
#if defined(ENABLE_OVERLOADING)
    labelEllipsize                          ,
#endif
    setLabelEllipsize                       ,


-- ** justify #attr:justify#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LabelJustifyPropertyInfo                ,
#endif
    constructLabelJustify                   ,
    getLabelJustify                         ,
#if defined(ENABLE_OVERLOADING)
    labelJustify                            ,
#endif
    setLabelJustify                         ,


-- ** label #attr:label#
-- | The contents of the label.
-- 
-- If the string contains [Pango XML markup][PangoMarkupFormat], you will
-- have to set the [Label:useMarkup]("GI.Gtk.Objects.Label#g:attr:useMarkup") property to 'P.True' in order for the
-- label to display the markup attributes. See also 'GI.Gtk.Objects.Label.labelSetMarkup'
-- for a convenience function that sets both this property and the
-- [Label:useMarkup]("GI.Gtk.Objects.Label#g:attr:useMarkup") property at the same time.
-- 
-- If the string contains underlines acting as mnemonics, you will have to
-- set the [Label:useUnderline]("GI.Gtk.Objects.Label#g:attr:useUnderline") property to 'P.True' in order for the label
-- to display them.

#if defined(ENABLE_OVERLOADING)
    LabelLabelPropertyInfo                  ,
#endif
    constructLabelLabel                     ,
    getLabelLabel                           ,
#if defined(ENABLE_OVERLOADING)
    labelLabel                              ,
#endif
    setLabelLabel                           ,


-- ** lines #attr:lines#
-- | The number of lines to which an ellipsized, wrapping label
-- should be limited. This property has no effect if the
-- label is not wrapping or ellipsized. Set this property to
-- -1 if you don\'t want to limit the number of lines.
-- 
-- /Since: 3.10/

#if defined(ENABLE_OVERLOADING)
    LabelLinesPropertyInfo                  ,
#endif
    constructLabelLines                     ,
    getLabelLines                           ,
#if defined(ENABLE_OVERLOADING)
    labelLines                              ,
#endif
    setLabelLines                           ,


-- ** maxWidthChars #attr:maxWidthChars#
-- | The desired maximum width of the label, in characters. If this property
-- is set to -1, the width will be calculated automatically.
-- 
-- See the section on [text layout][label-text-layout]
-- for details of how [Label:widthChars]("GI.Gtk.Objects.Label#g:attr:widthChars") and [Label:maxWidthChars]("GI.Gtk.Objects.Label#g:attr:maxWidthChars")
-- determine the width of ellipsized and wrapped labels.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    LabelMaxWidthCharsPropertyInfo          ,
#endif
    constructLabelMaxWidthChars             ,
    getLabelMaxWidthChars                   ,
#if defined(ENABLE_OVERLOADING)
    labelMaxWidthChars                      ,
#endif
    setLabelMaxWidthChars                   ,


-- ** mnemonicKeyval #attr:mnemonicKeyval#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LabelMnemonicKeyvalPropertyInfo         ,
#endif
    getLabelMnemonicKeyval                  ,
#if defined(ENABLE_OVERLOADING)
    labelMnemonicKeyval                     ,
#endif


-- ** mnemonicWidget #attr:mnemonicWidget#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LabelMnemonicWidgetPropertyInfo         ,
#endif
    clearLabelMnemonicWidget                ,
    constructLabelMnemonicWidget            ,
    getLabelMnemonicWidget                  ,
#if defined(ENABLE_OVERLOADING)
    labelMnemonicWidget                     ,
#endif
    setLabelMnemonicWidget                  ,


-- ** pattern #attr:pattern#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LabelPatternPropertyInfo                ,
#endif
    constructLabelPattern                   ,
#if defined(ENABLE_OVERLOADING)
    labelPattern                            ,
#endif
    setLabelPattern                         ,


-- ** selectable #attr:selectable#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LabelSelectablePropertyInfo             ,
#endif
    constructLabelSelectable                ,
    getLabelSelectable                      ,
#if defined(ENABLE_OVERLOADING)
    labelSelectable                         ,
#endif
    setLabelSelectable                      ,


-- ** selectionBound #attr:selectionBound#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LabelSelectionBoundPropertyInfo         ,
#endif
    getLabelSelectionBound                  ,
#if defined(ENABLE_OVERLOADING)
    labelSelectionBound                     ,
#endif


-- ** singleLineMode #attr:singleLineMode#
-- | Whether the label is in single line mode. In single line mode,
-- the height of the label does not depend on the actual text, it
-- is always set to ascent + descent of the font. This can be an
-- advantage in situations where resizing the label because of text
-- changes would be distracting, e.g. in a statusbar.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    LabelSingleLineModePropertyInfo         ,
#endif
    constructLabelSingleLineMode            ,
    getLabelSingleLineMode                  ,
#if defined(ENABLE_OVERLOADING)
    labelSingleLineMode                     ,
#endif
    setLabelSingleLineMode                  ,


-- ** trackVisitedLinks #attr:trackVisitedLinks#
-- | Set this property to 'P.True' to make the label track which links
-- have been visited. It will then apply the @/GTK_STATE_FLAG_VISITED/@
-- when rendering this link, in addition to @/GTK_STATE_FLAG_LINK/@.
-- 
-- /Since: 2.18/

#if defined(ENABLE_OVERLOADING)
    LabelTrackVisitedLinksPropertyInfo      ,
#endif
    constructLabelTrackVisitedLinks         ,
    getLabelTrackVisitedLinks               ,
#if defined(ENABLE_OVERLOADING)
    labelTrackVisitedLinks                  ,
#endif
    setLabelTrackVisitedLinks               ,


-- ** useMarkup #attr:useMarkup#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LabelUseMarkupPropertyInfo              ,
#endif
    constructLabelUseMarkup                 ,
    getLabelUseMarkup                       ,
#if defined(ENABLE_OVERLOADING)
    labelUseMarkup                          ,
#endif
    setLabelUseMarkup                       ,


-- ** useUnderline #attr:useUnderline#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LabelUseUnderlinePropertyInfo           ,
#endif
    constructLabelUseUnderline              ,
    getLabelUseUnderline                    ,
#if defined(ENABLE_OVERLOADING)
    labelUseUnderline                       ,
#endif
    setLabelUseUnderline                    ,


-- ** widthChars #attr:widthChars#
-- | The desired width of the label, in characters. If this property is set to
-- -1, the width will be calculated automatically.
-- 
-- See the section on [text layout][label-text-layout]
-- for details of how [Label:widthChars]("GI.Gtk.Objects.Label#g:attr:widthChars") and [Label:maxWidthChars]("GI.Gtk.Objects.Label#g:attr:maxWidthChars")
-- determine the width of ellipsized and wrapped labels.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    LabelWidthCharsPropertyInfo             ,
#endif
    constructLabelWidthChars                ,
    getLabelWidthChars                      ,
#if defined(ENABLE_OVERLOADING)
    labelWidthChars                         ,
#endif
    setLabelWidthChars                      ,


-- ** wrap #attr:wrap#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    LabelWrapPropertyInfo                   ,
#endif
    constructLabelWrap                      ,
    getLabelWrap                            ,
#if defined(ENABLE_OVERLOADING)
    labelWrap                               ,
#endif
    setLabelWrap                            ,


-- ** wrapMode #attr:wrapMode#
-- | If line wrapping is on (see the [Label:wrap]("GI.Gtk.Objects.Label#g:attr:wrap") property) this controls
-- how the line wrapping is done. The default is 'GI.Pango.Enums.WrapModeWord', which
-- means wrap on word boundaries.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    LabelWrapModePropertyInfo               ,
#endif
    constructLabelWrapMode                  ,
    getLabelWrapMode                        ,
#if defined(ENABLE_OVERLOADING)
    labelWrapMode                           ,
#endif
    setLabelWrapMode                        ,


-- ** xalign #attr:xalign#
-- | The xalign property determines the horizontal aligment of the label text
-- inside the labels size allocation. Compare this to [Widget:halign]("GI.Gtk.Objects.Widget#g:attr:halign"),
-- which determines how the labels size allocation is positioned in the
-- space available for the label.
-- 
-- /Since: 3.16/

#if defined(ENABLE_OVERLOADING)
    LabelXalignPropertyInfo                 ,
#endif
    constructLabelXalign                    ,
    getLabelXalign                          ,
#if defined(ENABLE_OVERLOADING)
    labelXalign                             ,
#endif
    setLabelXalign                          ,


-- ** yalign #attr:yalign#
-- | The yalign property determines the vertical aligment of the label text
-- inside the labels size allocation. Compare this to [Widget:valign]("GI.Gtk.Objects.Widget#g:attr:valign"),
-- which determines how the labels size allocation is positioned in the
-- space available for the label.
-- 
-- /Since: 3.16/

#if defined(ENABLE_OVERLOADING)
    LabelYalignPropertyInfo                 ,
#endif
    constructLabelYalign                    ,
    getLabelYalign                          ,
#if defined(ENABLE_OVERLOADING)
    labelYalign                             ,
#endif
    setLabelYalign                          ,




 -- * Signals


-- ** activateCurrentLink #signal:activateCurrentLink#

    LabelActivateCurrentLinkCallback        ,
#if defined(ENABLE_OVERLOADING)
    LabelActivateCurrentLinkSignalInfo      ,
#endif
    afterLabelActivateCurrentLink           ,
    onLabelActivateCurrentLink              ,


-- ** activateLink #signal:activateLink#

    LabelActivateLinkCallback               ,
#if defined(ENABLE_OVERLOADING)
    LabelActivateLinkSignalInfo             ,
#endif
    afterLabelActivateLink                  ,
    onLabelActivateLink                     ,


-- ** copyClipboard #signal:copyClipboard#

    LabelCopyClipboardCallback              ,
#if defined(ENABLE_OVERLOADING)
    LabelCopyClipboardSignalInfo            ,
#endif
    afterLabelCopyClipboard                 ,
    onLabelCopyClipboard                    ,


-- ** moveCursor #signal:moveCursor#

    LabelMoveCursorCallback                 ,
#if defined(ENABLE_OVERLOADING)
    LabelMoveCursorSignalInfo               ,
#endif
    afterLabelMoveCursor                    ,
    onLabelMoveCursor                       ,


-- ** populatePopup #signal:populatePopup#

    LabelPopulatePopupCallback              ,
#if defined(ENABLE_OVERLOADING)
    LabelPopulatePopupSignalInfo            ,
#endif
    afterLabelPopulatePopup                 ,
    onLabelPopulatePopup                    ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.Menu as Gtk.Menu
import {-# SOURCE #-} qualified GI.Gtk.Objects.Misc as Gtk.Misc
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import qualified GI.Pango.Enums as Pango.Enums
import qualified GI.Pango.Objects.Layout as Pango.Layout
import qualified GI.Pango.Structs.AttrList as Pango.AttrList

-- | Memory-managed wrapper type.
newtype Label = Label (SP.ManagedPtr Label)
    deriving (Eq)

instance SP.ManagedPtrNewtype Label where
    toManagedPtr (Label p) = p

foreign import ccall "gtk_label_get_type"
    c_gtk_label_get_type :: IO B.Types.GType

instance B.Types.TypedObject Label where
    glibType = c_gtk_label_get_type

instance B.Types.GObject Label

-- | Type class for types which can be safely cast to `Label`, for instance with `toLabel`.
class (SP.GObject o, O.IsDescendantOf Label o) => IsLabel o
instance (SP.GObject o, O.IsDescendantOf Label o) => IsLabel o

instance O.HasParentTypes Label
type instance O.ParentTypes Label = '[Gtk.Misc.Misc, Gtk.Widget.Widget, GObject.Object.Object, Atk.ImplementorIface.ImplementorIface, Gtk.Buildable.Buildable]

-- | Cast to `Label`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toLabel :: (MIO.MonadIO m, IsLabel o) => o -> m Label
toLabel = MIO.liftIO . B.ManagedPtr.unsafeCastTo Label

-- | Convert 'Label' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Label) where
    gvalueGType_ = c_gtk_label_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Label)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Label)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Label ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveLabelMethod (t :: Symbol) (o :: *) :: * where
    ResolveLabelMethod "activate" o = Gtk.Widget.WidgetActivateMethodInfo
    ResolveLabelMethod "addAccelerator" o = Gtk.Widget.WidgetAddAcceleratorMethodInfo
    ResolveLabelMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveLabelMethod "addDeviceEvents" o = Gtk.Widget.WidgetAddDeviceEventsMethodInfo
    ResolveLabelMethod "addEvents" o = Gtk.Widget.WidgetAddEventsMethodInfo
    ResolveLabelMethod "addMnemonicLabel" o = Gtk.Widget.WidgetAddMnemonicLabelMethodInfo
    ResolveLabelMethod "addTickCallback" o = Gtk.Widget.WidgetAddTickCallbackMethodInfo
    ResolveLabelMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveLabelMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveLabelMethod "canActivateAccel" o = Gtk.Widget.WidgetCanActivateAccelMethodInfo
    ResolveLabelMethod "childFocus" o = Gtk.Widget.WidgetChildFocusMethodInfo
    ResolveLabelMethod "childNotify" o = Gtk.Widget.WidgetChildNotifyMethodInfo
    ResolveLabelMethod "classPath" o = Gtk.Widget.WidgetClassPathMethodInfo
    ResolveLabelMethod "computeExpand" o = Gtk.Widget.WidgetComputeExpandMethodInfo
    ResolveLabelMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveLabelMethod "createPangoContext" o = Gtk.Widget.WidgetCreatePangoContextMethodInfo
    ResolveLabelMethod "createPangoLayout" o = Gtk.Widget.WidgetCreatePangoLayoutMethodInfo
    ResolveLabelMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveLabelMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveLabelMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveLabelMethod "destroy" o = Gtk.Widget.WidgetDestroyMethodInfo
    ResolveLabelMethod "destroyed" o = Gtk.Widget.WidgetDestroyedMethodInfo
    ResolveLabelMethod "deviceIsShadowed" o = Gtk.Widget.WidgetDeviceIsShadowedMethodInfo
    ResolveLabelMethod "dragBegin" o = Gtk.Widget.WidgetDragBeginMethodInfo
    ResolveLabelMethod "dragBeginWithCoordinates" o = Gtk.Widget.WidgetDragBeginWithCoordinatesMethodInfo
    ResolveLabelMethod "dragCheckThreshold" o = Gtk.Widget.WidgetDragCheckThresholdMethodInfo
    ResolveLabelMethod "dragDestAddImageTargets" o = Gtk.Widget.WidgetDragDestAddImageTargetsMethodInfo
    ResolveLabelMethod "dragDestAddTextTargets" o = Gtk.Widget.WidgetDragDestAddTextTargetsMethodInfo
    ResolveLabelMethod "dragDestAddUriTargets" o = Gtk.Widget.WidgetDragDestAddUriTargetsMethodInfo
    ResolveLabelMethod "dragDestFindTarget" o = Gtk.Widget.WidgetDragDestFindTargetMethodInfo
    ResolveLabelMethod "dragDestGetTargetList" o = Gtk.Widget.WidgetDragDestGetTargetListMethodInfo
    ResolveLabelMethod "dragDestGetTrackMotion" o = Gtk.Widget.WidgetDragDestGetTrackMotionMethodInfo
    ResolveLabelMethod "dragDestSet" o = Gtk.Widget.WidgetDragDestSetMethodInfo
    ResolveLabelMethod "dragDestSetProxy" o = Gtk.Widget.WidgetDragDestSetProxyMethodInfo
    ResolveLabelMethod "dragDestSetTargetList" o = Gtk.Widget.WidgetDragDestSetTargetListMethodInfo
    ResolveLabelMethod "dragDestSetTrackMotion" o = Gtk.Widget.WidgetDragDestSetTrackMotionMethodInfo
    ResolveLabelMethod "dragDestUnset" o = Gtk.Widget.WidgetDragDestUnsetMethodInfo
    ResolveLabelMethod "dragGetData" o = Gtk.Widget.WidgetDragGetDataMethodInfo
    ResolveLabelMethod "dragHighlight" o = Gtk.Widget.WidgetDragHighlightMethodInfo
    ResolveLabelMethod "dragSourceAddImageTargets" o = Gtk.Widget.WidgetDragSourceAddImageTargetsMethodInfo
    ResolveLabelMethod "dragSourceAddTextTargets" o = Gtk.Widget.WidgetDragSourceAddTextTargetsMethodInfo
    ResolveLabelMethod "dragSourceAddUriTargets" o = Gtk.Widget.WidgetDragSourceAddUriTargetsMethodInfo
    ResolveLabelMethod "dragSourceGetTargetList" o = Gtk.Widget.WidgetDragSourceGetTargetListMethodInfo
    ResolveLabelMethod "dragSourceSet" o = Gtk.Widget.WidgetDragSourceSetMethodInfo
    ResolveLabelMethod "dragSourceSetIconGicon" o = Gtk.Widget.WidgetDragSourceSetIconGiconMethodInfo
    ResolveLabelMethod "dragSourceSetIconName" o = Gtk.Widget.WidgetDragSourceSetIconNameMethodInfo
    ResolveLabelMethod "dragSourceSetIconPixbuf" o = Gtk.Widget.WidgetDragSourceSetIconPixbufMethodInfo
    ResolveLabelMethod "dragSourceSetIconStock" o = Gtk.Widget.WidgetDragSourceSetIconStockMethodInfo
    ResolveLabelMethod "dragSourceSetTargetList" o = Gtk.Widget.WidgetDragSourceSetTargetListMethodInfo
    ResolveLabelMethod "dragSourceUnset" o = Gtk.Widget.WidgetDragSourceUnsetMethodInfo
    ResolveLabelMethod "dragUnhighlight" o = Gtk.Widget.WidgetDragUnhighlightMethodInfo
    ResolveLabelMethod "draw" o = Gtk.Widget.WidgetDrawMethodInfo
    ResolveLabelMethod "ensureStyle" o = Gtk.Widget.WidgetEnsureStyleMethodInfo
    ResolveLabelMethod "errorBell" o = Gtk.Widget.WidgetErrorBellMethodInfo
    ResolveLabelMethod "event" o = Gtk.Widget.WidgetEventMethodInfo
    ResolveLabelMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveLabelMethod "freezeChildNotify" o = Gtk.Widget.WidgetFreezeChildNotifyMethodInfo
    ResolveLabelMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveLabelMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveLabelMethod "grabAdd" o = Gtk.Widget.WidgetGrabAddMethodInfo
    ResolveLabelMethod "grabDefault" o = Gtk.Widget.WidgetGrabDefaultMethodInfo
    ResolveLabelMethod "grabFocus" o = Gtk.Widget.WidgetGrabFocusMethodInfo
    ResolveLabelMethod "grabRemove" o = Gtk.Widget.WidgetGrabRemoveMethodInfo
    ResolveLabelMethod "hasDefault" o = Gtk.Widget.WidgetHasDefaultMethodInfo
    ResolveLabelMethod "hasFocus" o = Gtk.Widget.WidgetHasFocusMethodInfo
    ResolveLabelMethod "hasGrab" o = Gtk.Widget.WidgetHasGrabMethodInfo
    ResolveLabelMethod "hasRcStyle" o = Gtk.Widget.WidgetHasRcStyleMethodInfo
    ResolveLabelMethod "hasScreen" o = Gtk.Widget.WidgetHasScreenMethodInfo
    ResolveLabelMethod "hasVisibleFocus" o = Gtk.Widget.WidgetHasVisibleFocusMethodInfo
    ResolveLabelMethod "hide" o = Gtk.Widget.WidgetHideMethodInfo
    ResolveLabelMethod "hideOnDelete" o = Gtk.Widget.WidgetHideOnDeleteMethodInfo
    ResolveLabelMethod "inDestruction" o = Gtk.Widget.WidgetInDestructionMethodInfo
    ResolveLabelMethod "initTemplate" o = Gtk.Widget.WidgetInitTemplateMethodInfo
    ResolveLabelMethod "inputShapeCombineRegion" o = Gtk.Widget.WidgetInputShapeCombineRegionMethodInfo
    ResolveLabelMethod "insertActionGroup" o = Gtk.Widget.WidgetInsertActionGroupMethodInfo
    ResolveLabelMethod "intersect" o = Gtk.Widget.WidgetIntersectMethodInfo
    ResolveLabelMethod "isAncestor" o = Gtk.Widget.WidgetIsAncestorMethodInfo
    ResolveLabelMethod "isComposited" o = Gtk.Widget.WidgetIsCompositedMethodInfo
    ResolveLabelMethod "isDrawable" o = Gtk.Widget.WidgetIsDrawableMethodInfo
    ResolveLabelMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveLabelMethod "isFocus" o = Gtk.Widget.WidgetIsFocusMethodInfo
    ResolveLabelMethod "isSensitive" o = Gtk.Widget.WidgetIsSensitiveMethodInfo
    ResolveLabelMethod "isToplevel" o = Gtk.Widget.WidgetIsToplevelMethodInfo
    ResolveLabelMethod "isVisible" o = Gtk.Widget.WidgetIsVisibleMethodInfo
    ResolveLabelMethod "keynavFailed" o = Gtk.Widget.WidgetKeynavFailedMethodInfo
    ResolveLabelMethod "listAccelClosures" o = Gtk.Widget.WidgetListAccelClosuresMethodInfo
    ResolveLabelMethod "listActionPrefixes" o = Gtk.Widget.WidgetListActionPrefixesMethodInfo
    ResolveLabelMethod "listMnemonicLabels" o = Gtk.Widget.WidgetListMnemonicLabelsMethodInfo
    ResolveLabelMethod "map" o = Gtk.Widget.WidgetMapMethodInfo
    ResolveLabelMethod "mnemonicActivate" o = Gtk.Widget.WidgetMnemonicActivateMethodInfo
    ResolveLabelMethod "modifyBase" o = Gtk.Widget.WidgetModifyBaseMethodInfo
    ResolveLabelMethod "modifyBg" o = Gtk.Widget.WidgetModifyBgMethodInfo
    ResolveLabelMethod "modifyCursor" o = Gtk.Widget.WidgetModifyCursorMethodInfo
    ResolveLabelMethod "modifyFg" o = Gtk.Widget.WidgetModifyFgMethodInfo
    ResolveLabelMethod "modifyFont" o = Gtk.Widget.WidgetModifyFontMethodInfo
    ResolveLabelMethod "modifyStyle" o = Gtk.Widget.WidgetModifyStyleMethodInfo
    ResolveLabelMethod "modifyText" o = Gtk.Widget.WidgetModifyTextMethodInfo
    ResolveLabelMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveLabelMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveLabelMethod "overrideBackgroundColor" o = Gtk.Widget.WidgetOverrideBackgroundColorMethodInfo
    ResolveLabelMethod "overrideColor" o = Gtk.Widget.WidgetOverrideColorMethodInfo
    ResolveLabelMethod "overrideCursor" o = Gtk.Widget.WidgetOverrideCursorMethodInfo
    ResolveLabelMethod "overrideFont" o = Gtk.Widget.WidgetOverrideFontMethodInfo
    ResolveLabelMethod "overrideSymbolicColor" o = Gtk.Widget.WidgetOverrideSymbolicColorMethodInfo
    ResolveLabelMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveLabelMethod "path" o = Gtk.Widget.WidgetPathMethodInfo
    ResolveLabelMethod "queueAllocate" o = Gtk.Widget.WidgetQueueAllocateMethodInfo
    ResolveLabelMethod "queueComputeExpand" o = Gtk.Widget.WidgetQueueComputeExpandMethodInfo
    ResolveLabelMethod "queueDraw" o = Gtk.Widget.WidgetQueueDrawMethodInfo
    ResolveLabelMethod "queueDrawArea" o = Gtk.Widget.WidgetQueueDrawAreaMethodInfo
    ResolveLabelMethod "queueDrawRegion" o = Gtk.Widget.WidgetQueueDrawRegionMethodInfo
    ResolveLabelMethod "queueResize" o = Gtk.Widget.WidgetQueueResizeMethodInfo
    ResolveLabelMethod "queueResizeNoRedraw" o = Gtk.Widget.WidgetQueueResizeNoRedrawMethodInfo
    ResolveLabelMethod "realize" o = Gtk.Widget.WidgetRealizeMethodInfo
    ResolveLabelMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveLabelMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveLabelMethod "regionIntersect" o = Gtk.Widget.WidgetRegionIntersectMethodInfo
    ResolveLabelMethod "registerWindow" o = Gtk.Widget.WidgetRegisterWindowMethodInfo
    ResolveLabelMethod "removeAccelerator" o = Gtk.Widget.WidgetRemoveAcceleratorMethodInfo
    ResolveLabelMethod "removeMnemonicLabel" o = Gtk.Widget.WidgetRemoveMnemonicLabelMethodInfo
    ResolveLabelMethod "removeTickCallback" o = Gtk.Widget.WidgetRemoveTickCallbackMethodInfo
    ResolveLabelMethod "renderIcon" o = Gtk.Widget.WidgetRenderIconMethodInfo
    ResolveLabelMethod "renderIconPixbuf" o = Gtk.Widget.WidgetRenderIconPixbufMethodInfo
    ResolveLabelMethod "reparent" o = Gtk.Widget.WidgetReparentMethodInfo
    ResolveLabelMethod "resetRcStyles" o = Gtk.Widget.WidgetResetRcStylesMethodInfo
    ResolveLabelMethod "resetStyle" o = Gtk.Widget.WidgetResetStyleMethodInfo
    ResolveLabelMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveLabelMethod "selectRegion" o = LabelSelectRegionMethodInfo
    ResolveLabelMethod "sendExpose" o = Gtk.Widget.WidgetSendExposeMethodInfo
    ResolveLabelMethod "sendFocusChange" o = Gtk.Widget.WidgetSendFocusChangeMethodInfo
    ResolveLabelMethod "shapeCombineRegion" o = Gtk.Widget.WidgetShapeCombineRegionMethodInfo
    ResolveLabelMethod "show" o = Gtk.Widget.WidgetShowMethodInfo
    ResolveLabelMethod "showAll" o = Gtk.Widget.WidgetShowAllMethodInfo
    ResolveLabelMethod "showNow" o = Gtk.Widget.WidgetShowNowMethodInfo
    ResolveLabelMethod "sizeAllocate" o = Gtk.Widget.WidgetSizeAllocateMethodInfo
    ResolveLabelMethod "sizeAllocateWithBaseline" o = Gtk.Widget.WidgetSizeAllocateWithBaselineMethodInfo
    ResolveLabelMethod "sizeRequest" o = Gtk.Widget.WidgetSizeRequestMethodInfo
    ResolveLabelMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveLabelMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveLabelMethod "styleAttach" o = Gtk.Widget.WidgetStyleAttachMethodInfo
    ResolveLabelMethod "styleGetProperty" o = Gtk.Widget.WidgetStyleGetPropertyMethodInfo
    ResolveLabelMethod "thawChildNotify" o = Gtk.Widget.WidgetThawChildNotifyMethodInfo
    ResolveLabelMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveLabelMethod "translateCoordinates" o = Gtk.Widget.WidgetTranslateCoordinatesMethodInfo
    ResolveLabelMethod "triggerTooltipQuery" o = Gtk.Widget.WidgetTriggerTooltipQueryMethodInfo
    ResolveLabelMethod "unmap" o = Gtk.Widget.WidgetUnmapMethodInfo
    ResolveLabelMethod "unparent" o = Gtk.Widget.WidgetUnparentMethodInfo
    ResolveLabelMethod "unrealize" o = Gtk.Widget.WidgetUnrealizeMethodInfo
    ResolveLabelMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveLabelMethod "unregisterWindow" o = Gtk.Widget.WidgetUnregisterWindowMethodInfo
    ResolveLabelMethod "unsetStateFlags" o = Gtk.Widget.WidgetUnsetStateFlagsMethodInfo
    ResolveLabelMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveLabelMethod "getAccessible" o = Gtk.Widget.WidgetGetAccessibleMethodInfo
    ResolveLabelMethod "getActionGroup" o = Gtk.Widget.WidgetGetActionGroupMethodInfo
    ResolveLabelMethod "getAlignment" o = Gtk.Misc.MiscGetAlignmentMethodInfo
    ResolveLabelMethod "getAllocatedBaseline" o = Gtk.Widget.WidgetGetAllocatedBaselineMethodInfo
    ResolveLabelMethod "getAllocatedHeight" o = Gtk.Widget.WidgetGetAllocatedHeightMethodInfo
    ResolveLabelMethod "getAllocatedSize" o = Gtk.Widget.WidgetGetAllocatedSizeMethodInfo
    ResolveLabelMethod "getAllocatedWidth" o = Gtk.Widget.WidgetGetAllocatedWidthMethodInfo
    ResolveLabelMethod "getAllocation" o = Gtk.Widget.WidgetGetAllocationMethodInfo
    ResolveLabelMethod "getAncestor" o = Gtk.Widget.WidgetGetAncestorMethodInfo
    ResolveLabelMethod "getAngle" o = LabelGetAngleMethodInfo
    ResolveLabelMethod "getAppPaintable" o = Gtk.Widget.WidgetGetAppPaintableMethodInfo
    ResolveLabelMethod "getAttributes" o = LabelGetAttributesMethodInfo
    ResolveLabelMethod "getCanDefault" o = Gtk.Widget.WidgetGetCanDefaultMethodInfo
    ResolveLabelMethod "getCanFocus" o = Gtk.Widget.WidgetGetCanFocusMethodInfo
    ResolveLabelMethod "getChildRequisition" o = Gtk.Widget.WidgetGetChildRequisitionMethodInfo
    ResolveLabelMethod "getChildVisible" o = Gtk.Widget.WidgetGetChildVisibleMethodInfo
    ResolveLabelMethod "getClip" o = Gtk.Widget.WidgetGetClipMethodInfo
    ResolveLabelMethod "getClipboard" o = Gtk.Widget.WidgetGetClipboardMethodInfo
    ResolveLabelMethod "getCompositeName" o = Gtk.Widget.WidgetGetCompositeNameMethodInfo
    ResolveLabelMethod "getCurrentUri" o = LabelGetCurrentUriMethodInfo
    ResolveLabelMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveLabelMethod "getDeviceEnabled" o = Gtk.Widget.WidgetGetDeviceEnabledMethodInfo
    ResolveLabelMethod "getDeviceEvents" o = Gtk.Widget.WidgetGetDeviceEventsMethodInfo
    ResolveLabelMethod "getDirection" o = Gtk.Widget.WidgetGetDirectionMethodInfo
    ResolveLabelMethod "getDisplay" o = Gtk.Widget.WidgetGetDisplayMethodInfo
    ResolveLabelMethod "getDoubleBuffered" o = Gtk.Widget.WidgetGetDoubleBufferedMethodInfo
    ResolveLabelMethod "getEllipsize" o = LabelGetEllipsizeMethodInfo
    ResolveLabelMethod "getEvents" o = Gtk.Widget.WidgetGetEventsMethodInfo
    ResolveLabelMethod "getFocusOnClick" o = Gtk.Widget.WidgetGetFocusOnClickMethodInfo
    ResolveLabelMethod "getFontMap" o = Gtk.Widget.WidgetGetFontMapMethodInfo
    ResolveLabelMethod "getFontOptions" o = Gtk.Widget.WidgetGetFontOptionsMethodInfo
    ResolveLabelMethod "getFrameClock" o = Gtk.Widget.WidgetGetFrameClockMethodInfo
    ResolveLabelMethod "getHalign" o = Gtk.Widget.WidgetGetHalignMethodInfo
    ResolveLabelMethod "getHasTooltip" o = Gtk.Widget.WidgetGetHasTooltipMethodInfo
    ResolveLabelMethod "getHasWindow" o = Gtk.Widget.WidgetGetHasWindowMethodInfo
    ResolveLabelMethod "getHexpand" o = Gtk.Widget.WidgetGetHexpandMethodInfo
    ResolveLabelMethod "getHexpandSet" o = Gtk.Widget.WidgetGetHexpandSetMethodInfo
    ResolveLabelMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveLabelMethod "getJustify" o = LabelGetJustifyMethodInfo
    ResolveLabelMethod "getLabel" o = LabelGetLabelMethodInfo
    ResolveLabelMethod "getLayout" o = LabelGetLayoutMethodInfo
    ResolveLabelMethod "getLayoutOffsets" o = LabelGetLayoutOffsetsMethodInfo
    ResolveLabelMethod "getLineWrap" o = LabelGetLineWrapMethodInfo
    ResolveLabelMethod "getLineWrapMode" o = LabelGetLineWrapModeMethodInfo
    ResolveLabelMethod "getLines" o = LabelGetLinesMethodInfo
    ResolveLabelMethod "getMapped" o = Gtk.Widget.WidgetGetMappedMethodInfo
    ResolveLabelMethod "getMarginBottom" o = Gtk.Widget.WidgetGetMarginBottomMethodInfo
    ResolveLabelMethod "getMarginEnd" o = Gtk.Widget.WidgetGetMarginEndMethodInfo
    ResolveLabelMethod "getMarginLeft" o = Gtk.Widget.WidgetGetMarginLeftMethodInfo
    ResolveLabelMethod "getMarginRight" o = Gtk.Widget.WidgetGetMarginRightMethodInfo
    ResolveLabelMethod "getMarginStart" o = Gtk.Widget.WidgetGetMarginStartMethodInfo
    ResolveLabelMethod "getMarginTop" o = Gtk.Widget.WidgetGetMarginTopMethodInfo
    ResolveLabelMethod "getMaxWidthChars" o = LabelGetMaxWidthCharsMethodInfo
    ResolveLabelMethod "getMnemonicKeyval" o = LabelGetMnemonicKeyvalMethodInfo
    ResolveLabelMethod "getMnemonicWidget" o = LabelGetMnemonicWidgetMethodInfo
    ResolveLabelMethod "getModifierMask" o = Gtk.Widget.WidgetGetModifierMaskMethodInfo
    ResolveLabelMethod "getModifierStyle" o = Gtk.Widget.WidgetGetModifierStyleMethodInfo
    ResolveLabelMethod "getName" o = Gtk.Widget.WidgetGetNameMethodInfo
    ResolveLabelMethod "getNoShowAll" o = Gtk.Widget.WidgetGetNoShowAllMethodInfo
    ResolveLabelMethod "getOpacity" o = Gtk.Widget.WidgetGetOpacityMethodInfo
    ResolveLabelMethod "getPadding" o = Gtk.Misc.MiscGetPaddingMethodInfo
    ResolveLabelMethod "getPangoContext" o = Gtk.Widget.WidgetGetPangoContextMethodInfo
    ResolveLabelMethod "getParent" o = Gtk.Widget.WidgetGetParentMethodInfo
    ResolveLabelMethod "getParentWindow" o = Gtk.Widget.WidgetGetParentWindowMethodInfo
    ResolveLabelMethod "getPath" o = Gtk.Widget.WidgetGetPathMethodInfo
    ResolveLabelMethod "getPointer" o = Gtk.Widget.WidgetGetPointerMethodInfo
    ResolveLabelMethod "getPreferredHeight" o = Gtk.Widget.WidgetGetPreferredHeightMethodInfo
    ResolveLabelMethod "getPreferredHeightAndBaselineForWidth" o = Gtk.Widget.WidgetGetPreferredHeightAndBaselineForWidthMethodInfo
    ResolveLabelMethod "getPreferredHeightForWidth" o = Gtk.Widget.WidgetGetPreferredHeightForWidthMethodInfo
    ResolveLabelMethod "getPreferredSize" o = Gtk.Widget.WidgetGetPreferredSizeMethodInfo
    ResolveLabelMethod "getPreferredWidth" o = Gtk.Widget.WidgetGetPreferredWidthMethodInfo
    ResolveLabelMethod "getPreferredWidthForHeight" o = Gtk.Widget.WidgetGetPreferredWidthForHeightMethodInfo
    ResolveLabelMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveLabelMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveLabelMethod "getRealized" o = Gtk.Widget.WidgetGetRealizedMethodInfo
    ResolveLabelMethod "getReceivesDefault" o = Gtk.Widget.WidgetGetReceivesDefaultMethodInfo
    ResolveLabelMethod "getRequestMode" o = Gtk.Widget.WidgetGetRequestModeMethodInfo
    ResolveLabelMethod "getRequisition" o = Gtk.Widget.WidgetGetRequisitionMethodInfo
    ResolveLabelMethod "getRootWindow" o = Gtk.Widget.WidgetGetRootWindowMethodInfo
    ResolveLabelMethod "getScaleFactor" o = Gtk.Widget.WidgetGetScaleFactorMethodInfo
    ResolveLabelMethod "getScreen" o = Gtk.Widget.WidgetGetScreenMethodInfo
    ResolveLabelMethod "getSelectable" o = LabelGetSelectableMethodInfo
    ResolveLabelMethod "getSelectionBounds" o = LabelGetSelectionBoundsMethodInfo
    ResolveLabelMethod "getSensitive" o = Gtk.Widget.WidgetGetSensitiveMethodInfo
    ResolveLabelMethod "getSettings" o = Gtk.Widget.WidgetGetSettingsMethodInfo
    ResolveLabelMethod "getSingleLineMode" o = LabelGetSingleLineModeMethodInfo
    ResolveLabelMethod "getSizeRequest" o = Gtk.Widget.WidgetGetSizeRequestMethodInfo
    ResolveLabelMethod "getState" o = Gtk.Widget.WidgetGetStateMethodInfo
    ResolveLabelMethod "getStateFlags" o = Gtk.Widget.WidgetGetStateFlagsMethodInfo
    ResolveLabelMethod "getStyle" o = Gtk.Widget.WidgetGetStyleMethodInfo
    ResolveLabelMethod "getStyleContext" o = Gtk.Widget.WidgetGetStyleContextMethodInfo
    ResolveLabelMethod "getSupportMultidevice" o = Gtk.Widget.WidgetGetSupportMultideviceMethodInfo
    ResolveLabelMethod "getTemplateChild" o = Gtk.Widget.WidgetGetTemplateChildMethodInfo
    ResolveLabelMethod "getText" o = LabelGetTextMethodInfo
    ResolveLabelMethod "getTooltipMarkup" o = Gtk.Widget.WidgetGetTooltipMarkupMethodInfo
    ResolveLabelMethod "getTooltipText" o = Gtk.Widget.WidgetGetTooltipTextMethodInfo
    ResolveLabelMethod "getTooltipWindow" o = Gtk.Widget.WidgetGetTooltipWindowMethodInfo
    ResolveLabelMethod "getToplevel" o = Gtk.Widget.WidgetGetToplevelMethodInfo
    ResolveLabelMethod "getTrackVisitedLinks" o = LabelGetTrackVisitedLinksMethodInfo
    ResolveLabelMethod "getUseMarkup" o = LabelGetUseMarkupMethodInfo
    ResolveLabelMethod "getUseUnderline" o = LabelGetUseUnderlineMethodInfo
    ResolveLabelMethod "getValign" o = Gtk.Widget.WidgetGetValignMethodInfo
    ResolveLabelMethod "getValignWithBaseline" o = Gtk.Widget.WidgetGetValignWithBaselineMethodInfo
    ResolveLabelMethod "getVexpand" o = Gtk.Widget.WidgetGetVexpandMethodInfo
    ResolveLabelMethod "getVexpandSet" o = Gtk.Widget.WidgetGetVexpandSetMethodInfo
    ResolveLabelMethod "getVisible" o = Gtk.Widget.WidgetGetVisibleMethodInfo
    ResolveLabelMethod "getVisual" o = Gtk.Widget.WidgetGetVisualMethodInfo
    ResolveLabelMethod "getWidthChars" o = LabelGetWidthCharsMethodInfo
    ResolveLabelMethod "getWindow" o = Gtk.Widget.WidgetGetWindowMethodInfo
    ResolveLabelMethod "getXalign" o = LabelGetXalignMethodInfo
    ResolveLabelMethod "getYalign" o = LabelGetYalignMethodInfo
    ResolveLabelMethod "setAccelPath" o = Gtk.Widget.WidgetSetAccelPathMethodInfo
    ResolveLabelMethod "setAlignment" o = Gtk.Misc.MiscSetAlignmentMethodInfo
    ResolveLabelMethod "setAllocation" o = Gtk.Widget.WidgetSetAllocationMethodInfo
    ResolveLabelMethod "setAngle" o = LabelSetAngleMethodInfo
    ResolveLabelMethod "setAppPaintable" o = Gtk.Widget.WidgetSetAppPaintableMethodInfo
    ResolveLabelMethod "setAttributes" o = LabelSetAttributesMethodInfo
    ResolveLabelMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveLabelMethod "setCanDefault" o = Gtk.Widget.WidgetSetCanDefaultMethodInfo
    ResolveLabelMethod "setCanFocus" o = Gtk.Widget.WidgetSetCanFocusMethodInfo
    ResolveLabelMethod "setChildVisible" o = Gtk.Widget.WidgetSetChildVisibleMethodInfo
    ResolveLabelMethod "setClip" o = Gtk.Widget.WidgetSetClipMethodInfo
    ResolveLabelMethod "setCompositeName" o = Gtk.Widget.WidgetSetCompositeNameMethodInfo
    ResolveLabelMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveLabelMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveLabelMethod "setDeviceEnabled" o = Gtk.Widget.WidgetSetDeviceEnabledMethodInfo
    ResolveLabelMethod "setDeviceEvents" o = Gtk.Widget.WidgetSetDeviceEventsMethodInfo
    ResolveLabelMethod "setDirection" o = Gtk.Widget.WidgetSetDirectionMethodInfo
    ResolveLabelMethod "setDoubleBuffered" o = Gtk.Widget.WidgetSetDoubleBufferedMethodInfo
    ResolveLabelMethod "setEllipsize" o = LabelSetEllipsizeMethodInfo
    ResolveLabelMethod "setEvents" o = Gtk.Widget.WidgetSetEventsMethodInfo
    ResolveLabelMethod "setFocusOnClick" o = Gtk.Widget.WidgetSetFocusOnClickMethodInfo
    ResolveLabelMethod "setFontMap" o = Gtk.Widget.WidgetSetFontMapMethodInfo
    ResolveLabelMethod "setFontOptions" o = Gtk.Widget.WidgetSetFontOptionsMethodInfo
    ResolveLabelMethod "setHalign" o = Gtk.Widget.WidgetSetHalignMethodInfo
    ResolveLabelMethod "setHasTooltip" o = Gtk.Widget.WidgetSetHasTooltipMethodInfo
    ResolveLabelMethod "setHasWindow" o = Gtk.Widget.WidgetSetHasWindowMethodInfo
    ResolveLabelMethod "setHexpand" o = Gtk.Widget.WidgetSetHexpandMethodInfo
    ResolveLabelMethod "setHexpandSet" o = Gtk.Widget.WidgetSetHexpandSetMethodInfo
    ResolveLabelMethod "setJustify" o = LabelSetJustifyMethodInfo
    ResolveLabelMethod "setLabel" o = LabelSetLabelMethodInfo
    ResolveLabelMethod "setLineWrap" o = LabelSetLineWrapMethodInfo
    ResolveLabelMethod "setLineWrapMode" o = LabelSetLineWrapModeMethodInfo
    ResolveLabelMethod "setLines" o = LabelSetLinesMethodInfo
    ResolveLabelMethod "setMapped" o = Gtk.Widget.WidgetSetMappedMethodInfo
    ResolveLabelMethod "setMarginBottom" o = Gtk.Widget.WidgetSetMarginBottomMethodInfo
    ResolveLabelMethod "setMarginEnd" o = Gtk.Widget.WidgetSetMarginEndMethodInfo
    ResolveLabelMethod "setMarginLeft" o = Gtk.Widget.WidgetSetMarginLeftMethodInfo
    ResolveLabelMethod "setMarginRight" o = Gtk.Widget.WidgetSetMarginRightMethodInfo
    ResolveLabelMethod "setMarginStart" o = Gtk.Widget.WidgetSetMarginStartMethodInfo
    ResolveLabelMethod "setMarginTop" o = Gtk.Widget.WidgetSetMarginTopMethodInfo
    ResolveLabelMethod "setMarkup" o = LabelSetMarkupMethodInfo
    ResolveLabelMethod "setMarkupWithMnemonic" o = LabelSetMarkupWithMnemonicMethodInfo
    ResolveLabelMethod "setMaxWidthChars" o = LabelSetMaxWidthCharsMethodInfo
    ResolveLabelMethod "setMnemonicWidget" o = LabelSetMnemonicWidgetMethodInfo
    ResolveLabelMethod "setName" o = Gtk.Widget.WidgetSetNameMethodInfo
    ResolveLabelMethod "setNoShowAll" o = Gtk.Widget.WidgetSetNoShowAllMethodInfo
    ResolveLabelMethod "setOpacity" o = Gtk.Widget.WidgetSetOpacityMethodInfo
    ResolveLabelMethod "setPadding" o = Gtk.Misc.MiscSetPaddingMethodInfo
    ResolveLabelMethod "setParent" o = Gtk.Widget.WidgetSetParentMethodInfo
    ResolveLabelMethod "setParentWindow" o = Gtk.Widget.WidgetSetParentWindowMethodInfo
    ResolveLabelMethod "setPattern" o = LabelSetPatternMethodInfo
    ResolveLabelMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveLabelMethod "setRealized" o = Gtk.Widget.WidgetSetRealizedMethodInfo
    ResolveLabelMethod "setReceivesDefault" o = Gtk.Widget.WidgetSetReceivesDefaultMethodInfo
    ResolveLabelMethod "setRedrawOnAllocate" o = Gtk.Widget.WidgetSetRedrawOnAllocateMethodInfo
    ResolveLabelMethod "setSelectable" o = LabelSetSelectableMethodInfo
    ResolveLabelMethod "setSensitive" o = Gtk.Widget.WidgetSetSensitiveMethodInfo
    ResolveLabelMethod "setSingleLineMode" o = LabelSetSingleLineModeMethodInfo
    ResolveLabelMethod "setSizeRequest" o = Gtk.Widget.WidgetSetSizeRequestMethodInfo
    ResolveLabelMethod "setState" o = Gtk.Widget.WidgetSetStateMethodInfo
    ResolveLabelMethod "setStateFlags" o = Gtk.Widget.WidgetSetStateFlagsMethodInfo
    ResolveLabelMethod "setStyle" o = Gtk.Widget.WidgetSetStyleMethodInfo
    ResolveLabelMethod "setSupportMultidevice" o = Gtk.Widget.WidgetSetSupportMultideviceMethodInfo
    ResolveLabelMethod "setText" o = LabelSetTextMethodInfo
    ResolveLabelMethod "setTextWithMnemonic" o = LabelSetTextWithMnemonicMethodInfo
    ResolveLabelMethod "setTooltipMarkup" o = Gtk.Widget.WidgetSetTooltipMarkupMethodInfo
    ResolveLabelMethod "setTooltipText" o = Gtk.Widget.WidgetSetTooltipTextMethodInfo
    ResolveLabelMethod "setTooltipWindow" o = Gtk.Widget.WidgetSetTooltipWindowMethodInfo
    ResolveLabelMethod "setTrackVisitedLinks" o = LabelSetTrackVisitedLinksMethodInfo
    ResolveLabelMethod "setUseMarkup" o = LabelSetUseMarkupMethodInfo
    ResolveLabelMethod "setUseUnderline" o = LabelSetUseUnderlineMethodInfo
    ResolveLabelMethod "setValign" o = Gtk.Widget.WidgetSetValignMethodInfo
    ResolveLabelMethod "setVexpand" o = Gtk.Widget.WidgetSetVexpandMethodInfo
    ResolveLabelMethod "setVexpandSet" o = Gtk.Widget.WidgetSetVexpandSetMethodInfo
    ResolveLabelMethod "setVisible" o = Gtk.Widget.WidgetSetVisibleMethodInfo
    ResolveLabelMethod "setVisual" o = Gtk.Widget.WidgetSetVisualMethodInfo
    ResolveLabelMethod "setWidthChars" o = LabelSetWidthCharsMethodInfo
    ResolveLabelMethod "setWindow" o = Gtk.Widget.WidgetSetWindowMethodInfo
    ResolveLabelMethod "setXalign" o = LabelSetXalignMethodInfo
    ResolveLabelMethod "setYalign" o = LabelSetYalignMethodInfo
    ResolveLabelMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveLabelMethod t Label, O.OverloadedMethod info Label p) => OL.IsLabel t (Label -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveLabelMethod t Label, O.OverloadedMethod info Label p, R.HasField t Label p) => R.HasField t Label p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveLabelMethod t Label, O.OverloadedMethodInfo info Label) => OL.IsLabel t (O.MethodProxy info Label) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Label::activate-current-link
-- | A [keybinding signal][GtkBindingSignal]
-- which gets emitted when the user activates a link in the label.
-- 
-- Applications may also emit the signal with @/g_signal_emit_by_name()/@
-- if they need to control activation of URIs programmatically.
-- 
-- The default bindings for this signal are all forms of the Enter key.
-- 
-- /Since: 2.18/
type LabelActivateCurrentLinkCallback =
    IO ()

type C_LabelActivateCurrentLinkCallback =
    Ptr Label ->                            -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_LabelActivateCurrentLinkCallback`.
foreign import ccall "wrapper"
    mk_LabelActivateCurrentLinkCallback :: C_LabelActivateCurrentLinkCallback -> IO (FunPtr C_LabelActivateCurrentLinkCallback)

wrap_LabelActivateCurrentLinkCallback :: 
    GObject a => (a -> LabelActivateCurrentLinkCallback) ->
    C_LabelActivateCurrentLinkCallback
wrap_LabelActivateCurrentLinkCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [activateCurrentLink](#signal:activateCurrentLink) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' label #activateCurrentLink callback
-- @
-- 
-- 
onLabelActivateCurrentLink :: (IsLabel a, MonadIO m) => a -> ((?self :: a) => LabelActivateCurrentLinkCallback) -> m SignalHandlerId
onLabelActivateCurrentLink obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_LabelActivateCurrentLinkCallback wrapped
    wrapped'' <- mk_LabelActivateCurrentLinkCallback wrapped'
    connectSignalFunPtr obj "activate-current-link" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [activateCurrentLink](#signal:activateCurrentLink) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' label #activateCurrentLink callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterLabelActivateCurrentLink :: (IsLabel a, MonadIO m) => a -> ((?self :: a) => LabelActivateCurrentLinkCallback) -> m SignalHandlerId
afterLabelActivateCurrentLink obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_LabelActivateCurrentLinkCallback wrapped
    wrapped'' <- mk_LabelActivateCurrentLinkCallback wrapped'
    connectSignalFunPtr obj "activate-current-link" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data LabelActivateCurrentLinkSignalInfo
instance SignalInfo LabelActivateCurrentLinkSignalInfo where
    type HaskellCallbackType LabelActivateCurrentLinkSignalInfo = LabelActivateCurrentLinkCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_LabelActivateCurrentLinkCallback cb
        cb'' <- mk_LabelActivateCurrentLinkCallback cb'
        connectSignalFunPtr obj "activate-current-link" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label::activate-current-link"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:signal:activateCurrentLink"})

#endif

-- signal Label::activate-link
-- | The signal which gets emitted to activate a URI.
-- Applications may connect to it to override the default behaviour,
-- which is to call 'GI.Gtk.Functions.showUriOnWindow'.
-- 
-- /Since: 2.18/
type LabelActivateLinkCallback =
    T.Text
    -- ^ /@uri@/: the URI that is activated
    -> IO Bool
    -- ^ __Returns:__ 'P.True' if the link has been activated

type C_LabelActivateLinkCallback =
    Ptr Label ->                            -- object
    CString ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_LabelActivateLinkCallback`.
foreign import ccall "wrapper"
    mk_LabelActivateLinkCallback :: C_LabelActivateLinkCallback -> IO (FunPtr C_LabelActivateLinkCallback)

wrap_LabelActivateLinkCallback :: 
    GObject a => (a -> LabelActivateLinkCallback) ->
    C_LabelActivateLinkCallback
wrap_LabelActivateLinkCallback gi'cb gi'selfPtr uri _ = do
    uri' <- cstringToText uri
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  uri'
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [activateLink](#signal:activateLink) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' label #activateLink callback
-- @
-- 
-- 
onLabelActivateLink :: (IsLabel a, MonadIO m) => a -> ((?self :: a) => LabelActivateLinkCallback) -> m SignalHandlerId
onLabelActivateLink obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_LabelActivateLinkCallback wrapped
    wrapped'' <- mk_LabelActivateLinkCallback wrapped'
    connectSignalFunPtr obj "activate-link" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [activateLink](#signal:activateLink) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' label #activateLink callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterLabelActivateLink :: (IsLabel a, MonadIO m) => a -> ((?self :: a) => LabelActivateLinkCallback) -> m SignalHandlerId
afterLabelActivateLink obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_LabelActivateLinkCallback wrapped
    wrapped'' <- mk_LabelActivateLinkCallback wrapped'
    connectSignalFunPtr obj "activate-link" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data LabelActivateLinkSignalInfo
instance SignalInfo LabelActivateLinkSignalInfo where
    type HaskellCallbackType LabelActivateLinkSignalInfo = LabelActivateLinkCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_LabelActivateLinkCallback cb
        cb'' <- mk_LabelActivateLinkCallback cb'
        connectSignalFunPtr obj "activate-link" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label::activate-link"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:signal:activateLink"})

#endif

-- signal Label::copy-clipboard
-- | The [copyClipboard](#g:signal:copyClipboard) signal is a
-- [keybinding signal][GtkBindingSignal]
-- which gets emitted to copy the selection to the clipboard.
-- 
-- The default binding for this signal is Ctrl-c.
type LabelCopyClipboardCallback =
    IO ()

type C_LabelCopyClipboardCallback =
    Ptr Label ->                            -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_LabelCopyClipboardCallback`.
foreign import ccall "wrapper"
    mk_LabelCopyClipboardCallback :: C_LabelCopyClipboardCallback -> IO (FunPtr C_LabelCopyClipboardCallback)

wrap_LabelCopyClipboardCallback :: 
    GObject a => (a -> LabelCopyClipboardCallback) ->
    C_LabelCopyClipboardCallback
wrap_LabelCopyClipboardCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [copyClipboard](#signal:copyClipboard) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' label #copyClipboard callback
-- @
-- 
-- 
onLabelCopyClipboard :: (IsLabel a, MonadIO m) => a -> ((?self :: a) => LabelCopyClipboardCallback) -> m SignalHandlerId
onLabelCopyClipboard obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_LabelCopyClipboardCallback wrapped
    wrapped'' <- mk_LabelCopyClipboardCallback wrapped'
    connectSignalFunPtr obj "copy-clipboard" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [copyClipboard](#signal:copyClipboard) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' label #copyClipboard callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterLabelCopyClipboard :: (IsLabel a, MonadIO m) => a -> ((?self :: a) => LabelCopyClipboardCallback) -> m SignalHandlerId
afterLabelCopyClipboard obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_LabelCopyClipboardCallback wrapped
    wrapped'' <- mk_LabelCopyClipboardCallback wrapped'
    connectSignalFunPtr obj "copy-clipboard" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data LabelCopyClipboardSignalInfo
instance SignalInfo LabelCopyClipboardSignalInfo where
    type HaskellCallbackType LabelCopyClipboardSignalInfo = LabelCopyClipboardCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_LabelCopyClipboardCallback cb
        cb'' <- mk_LabelCopyClipboardCallback cb'
        connectSignalFunPtr obj "copy-clipboard" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label::copy-clipboard"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:signal:copyClipboard"})

#endif

-- signal Label::move-cursor
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
type LabelMoveCursorCallback =
    Gtk.Enums.MovementStep
    -- ^ /@step@/: the granularity of the move, as a t'GI.Gtk.Enums.MovementStep'
    -> Int32
    -- ^ /@count@/: the number of /@step@/ units to move
    -> Bool
    -- ^ /@extendSelection@/: 'P.True' if the move should extend the selection
    -> IO ()

type C_LabelMoveCursorCallback =
    Ptr Label ->                            -- object
    CUInt ->
    Int32 ->
    CInt ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_LabelMoveCursorCallback`.
foreign import ccall "wrapper"
    mk_LabelMoveCursorCallback :: C_LabelMoveCursorCallback -> IO (FunPtr C_LabelMoveCursorCallback)

wrap_LabelMoveCursorCallback :: 
    GObject a => (a -> LabelMoveCursorCallback) ->
    C_LabelMoveCursorCallback
wrap_LabelMoveCursorCallback gi'cb gi'selfPtr step count extendSelection _ = do
    let step' = (toEnum . fromIntegral) step
    let extendSelection' = (/= 0) extendSelection
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  step' count extendSelection'


-- | Connect a signal handler for the [moveCursor](#signal:moveCursor) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' label #moveCursor callback
-- @
-- 
-- 
onLabelMoveCursor :: (IsLabel a, MonadIO m) => a -> ((?self :: a) => LabelMoveCursorCallback) -> m SignalHandlerId
onLabelMoveCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_LabelMoveCursorCallback wrapped
    wrapped'' <- mk_LabelMoveCursorCallback wrapped'
    connectSignalFunPtr obj "move-cursor" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [moveCursor](#signal:moveCursor) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' label #moveCursor callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterLabelMoveCursor :: (IsLabel a, MonadIO m) => a -> ((?self :: a) => LabelMoveCursorCallback) -> m SignalHandlerId
afterLabelMoveCursor obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_LabelMoveCursorCallback wrapped
    wrapped'' <- mk_LabelMoveCursorCallback wrapped'
    connectSignalFunPtr obj "move-cursor" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data LabelMoveCursorSignalInfo
instance SignalInfo LabelMoveCursorSignalInfo where
    type HaskellCallbackType LabelMoveCursorSignalInfo = LabelMoveCursorCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_LabelMoveCursorCallback cb
        cb'' <- mk_LabelMoveCursorCallback cb'
        connectSignalFunPtr obj "move-cursor" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label::move-cursor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:signal:moveCursor"})

#endif

-- signal Label::populate-popup
-- | The [populatePopup](#g:signal:populatePopup) signal gets emitted before showing the
-- context menu of the label. Note that only selectable labels
-- have context menus.
-- 
-- If you need to add items to the context menu, connect
-- to this signal and append your menuitems to the /@menu@/.
type LabelPopulatePopupCallback =
    Gtk.Menu.Menu
    -- ^ /@menu@/: the menu that is being populated
    -> IO ()

type C_LabelPopulatePopupCallback =
    Ptr Label ->                            -- object
    Ptr Gtk.Menu.Menu ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_LabelPopulatePopupCallback`.
foreign import ccall "wrapper"
    mk_LabelPopulatePopupCallback :: C_LabelPopulatePopupCallback -> IO (FunPtr C_LabelPopulatePopupCallback)

wrap_LabelPopulatePopupCallback :: 
    GObject a => (a -> LabelPopulatePopupCallback) ->
    C_LabelPopulatePopupCallback
wrap_LabelPopulatePopupCallback gi'cb gi'selfPtr menu _ = do
    menu' <- (newObject Gtk.Menu.Menu) menu
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  menu'


-- | Connect a signal handler for the [populatePopup](#signal:populatePopup) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' label #populatePopup callback
-- @
-- 
-- 
onLabelPopulatePopup :: (IsLabel a, MonadIO m) => a -> ((?self :: a) => LabelPopulatePopupCallback) -> m SignalHandlerId
onLabelPopulatePopup obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_LabelPopulatePopupCallback wrapped
    wrapped'' <- mk_LabelPopulatePopupCallback wrapped'
    connectSignalFunPtr obj "populate-popup" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [populatePopup](#signal:populatePopup) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' label #populatePopup callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterLabelPopulatePopup :: (IsLabel a, MonadIO m) => a -> ((?self :: a) => LabelPopulatePopupCallback) -> m SignalHandlerId
afterLabelPopulatePopup obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_LabelPopulatePopupCallback wrapped
    wrapped'' <- mk_LabelPopulatePopupCallback wrapped'
    connectSignalFunPtr obj "populate-popup" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data LabelPopulatePopupSignalInfo
instance SignalInfo LabelPopulatePopupSignalInfo where
    type HaskellCallbackType LabelPopulatePopupSignalInfo = LabelPopulatePopupCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_LabelPopulatePopupCallback cb
        cb'' <- mk_LabelPopulatePopupCallback cb'
        connectSignalFunPtr obj "populate-popup" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label::populate-popup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:signal:populatePopup"})

#endif

--- XXX Duplicated object with different types:
  --- Name {namespace = "Gtk", name = "Label"} -> Property {propName = "xalign", propType = TBasicType TFloat, propFlags = [PropertyReadable,PropertyWritable], propReadNullable = Just False, propWriteNullable = Just False, propTransfer = TransferNothing, propDoc = Documentation {rawDocText = Just "The xalign property determines the horizontal aligment of the label text\ninside the labels size allocation. Compare this to #GtkWidget:halign,\nwhich determines how the labels size allocation is positioned in the\nspace available for the label.", sinceVersion = Just "3.16"}, propDeprecated = Nothing}
  --- Name {namespace = "Gtk", name = "Misc"} -> Property {propName = "xalign", propType = TBasicType TFloat, propFlags = [PropertyReadable,PropertyWritable], propReadNullable = Nothing, propWriteNullable = Nothing, propTransfer = TransferNothing, propDoc = Documentation {rawDocText = Just "The horizontal alignment. A value of 0.0 means left alignment (or right\non RTL locales); a value of 1.0 means right alignment (or left on RTL\nlocales).", sinceVersion = Nothing}, propDeprecated = Just (DeprecationInfo {deprecatedSinceVersion = Just "3.14", deprecationMessage = Just "Use gtk_widget_set_halign() instead. If you are using\n  #GtkLabel, use #GtkLabel:xalign instead."})}
--- XXX Duplicated object with different types:
  --- Name {namespace = "Gtk", name = "Label"} -> Property {propName = "yalign", propType = TBasicType TFloat, propFlags = [PropertyReadable,PropertyWritable], propReadNullable = Just False, propWriteNullable = Just False, propTransfer = TransferNothing, propDoc = Documentation {rawDocText = Just "The yalign property determines the vertical aligment of the label text\ninside the labels size allocation. Compare this to #GtkWidget:valign,\nwhich determines how the labels size allocation is positioned in the\nspace available for the label.", sinceVersion = Just "3.16"}, propDeprecated = Nothing}
  --- Name {namespace = "Gtk", name = "Misc"} -> Property {propName = "yalign", propType = TBasicType TFloat, propFlags = [PropertyReadable,PropertyWritable], propReadNullable = Nothing, propWriteNullable = Nothing, propTransfer = TransferNothing, propDoc = Documentation {rawDocText = Just "The vertical alignment. A value of 0.0 means top alignment;\na value of 1.0 means bottom alignment.", sinceVersion = Nothing}, propDeprecated = Just (DeprecationInfo {deprecatedSinceVersion = Just "3.14", deprecationMessage = Just "Use gtk_widget_set_valign() instead. If you are using\n  #GtkLabel, use #GtkLabel:yalign instead."})}
-- VVV Prop "angle"
   -- Type: TBasicType TDouble
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@angle@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #angle
-- @
getLabelAngle :: (MonadIO m, IsLabel o) => o -> m Double
getLabelAngle obj = MIO.liftIO $ B.Properties.getObjectPropertyDouble obj "angle"

-- | Set the value of the “@angle@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #angle 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelAngle :: (MonadIO m, IsLabel o) => o -> Double -> m ()
setLabelAngle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyDouble obj "angle" val

-- | Construct a `GValueConstruct` with valid value for the “@angle@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelAngle :: (IsLabel o, MIO.MonadIO m) => Double -> m (GValueConstruct o)
constructLabelAngle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyDouble "angle" val

#if defined(ENABLE_OVERLOADING)
data LabelAnglePropertyInfo
instance AttrInfo LabelAnglePropertyInfo where
    type AttrAllowedOps LabelAnglePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LabelAnglePropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelAnglePropertyInfo = (~) Double
    type AttrTransferTypeConstraint LabelAnglePropertyInfo = (~) Double
    type AttrTransferType LabelAnglePropertyInfo = Double
    type AttrGetType LabelAnglePropertyInfo = Double
    type AttrLabel LabelAnglePropertyInfo = "angle"
    type AttrOrigin LabelAnglePropertyInfo = Label
    attrGet = getLabelAngle
    attrSet = setLabelAngle
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelAngle
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.angle"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:angle"
        })
#endif

-- VVV Prop "attributes"
   -- Type: TInterface (Name {namespace = "Pango", name = "AttrList"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@attributes@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #attributes
-- @
getLabelAttributes :: (MonadIO m, IsLabel o) => o -> m (Maybe Pango.AttrList.AttrList)
getLabelAttributes obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "attributes" Pango.AttrList.AttrList

-- | Set the value of the “@attributes@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #attributes 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelAttributes :: (MonadIO m, IsLabel o) => o -> Pango.AttrList.AttrList -> m ()
setLabelAttributes obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "attributes" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@attributes@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelAttributes :: (IsLabel o, MIO.MonadIO m) => Pango.AttrList.AttrList -> m (GValueConstruct o)
constructLabelAttributes val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "attributes" (P.Just val)

-- | Set the value of the “@attributes@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #attributes
-- @
clearLabelAttributes :: (MonadIO m, IsLabel o) => o -> m ()
clearLabelAttributes obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "attributes" (Nothing :: Maybe Pango.AttrList.AttrList)

#if defined(ENABLE_OVERLOADING)
data LabelAttributesPropertyInfo
instance AttrInfo LabelAttributesPropertyInfo where
    type AttrAllowedOps LabelAttributesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint LabelAttributesPropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelAttributesPropertyInfo = (~) Pango.AttrList.AttrList
    type AttrTransferTypeConstraint LabelAttributesPropertyInfo = (~) Pango.AttrList.AttrList
    type AttrTransferType LabelAttributesPropertyInfo = Pango.AttrList.AttrList
    type AttrGetType LabelAttributesPropertyInfo = (Maybe Pango.AttrList.AttrList)
    type AttrLabel LabelAttributesPropertyInfo = "attributes"
    type AttrOrigin LabelAttributesPropertyInfo = Label
    attrGet = getLabelAttributes
    attrSet = setLabelAttributes
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelAttributes
    attrClear = clearLabelAttributes
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.attributes"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:attributes"
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
-- 'Data.GI.Base.Attributes.get' label #cursorPosition
-- @
getLabelCursorPosition :: (MonadIO m, IsLabel o) => o -> m Int32
getLabelCursorPosition obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "cursor-position"

#if defined(ENABLE_OVERLOADING)
data LabelCursorPositionPropertyInfo
instance AttrInfo LabelCursorPositionPropertyInfo where
    type AttrAllowedOps LabelCursorPositionPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint LabelCursorPositionPropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelCursorPositionPropertyInfo = (~) ()
    type AttrTransferTypeConstraint LabelCursorPositionPropertyInfo = (~) ()
    type AttrTransferType LabelCursorPositionPropertyInfo = ()
    type AttrGetType LabelCursorPositionPropertyInfo = Int32
    type AttrLabel LabelCursorPositionPropertyInfo = "cursor-position"
    type AttrOrigin LabelCursorPositionPropertyInfo = Label
    attrGet = getLabelCursorPosition
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.cursorPosition"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:cursorPosition"
        })
#endif

-- VVV Prop "ellipsize"
   -- Type: TInterface (Name {namespace = "Pango", name = "EllipsizeMode"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@ellipsize@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #ellipsize
-- @
getLabelEllipsize :: (MonadIO m, IsLabel o) => o -> m Pango.Enums.EllipsizeMode
getLabelEllipsize obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "ellipsize"

-- | Set the value of the “@ellipsize@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #ellipsize 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelEllipsize :: (MonadIO m, IsLabel o) => o -> Pango.Enums.EllipsizeMode -> m ()
setLabelEllipsize obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "ellipsize" val

-- | Construct a `GValueConstruct` with valid value for the “@ellipsize@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelEllipsize :: (IsLabel o, MIO.MonadIO m) => Pango.Enums.EllipsizeMode -> m (GValueConstruct o)
constructLabelEllipsize val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "ellipsize" val

#if defined(ENABLE_OVERLOADING)
data LabelEllipsizePropertyInfo
instance AttrInfo LabelEllipsizePropertyInfo where
    type AttrAllowedOps LabelEllipsizePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LabelEllipsizePropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelEllipsizePropertyInfo = (~) Pango.Enums.EllipsizeMode
    type AttrTransferTypeConstraint LabelEllipsizePropertyInfo = (~) Pango.Enums.EllipsizeMode
    type AttrTransferType LabelEllipsizePropertyInfo = Pango.Enums.EllipsizeMode
    type AttrGetType LabelEllipsizePropertyInfo = Pango.Enums.EllipsizeMode
    type AttrLabel LabelEllipsizePropertyInfo = "ellipsize"
    type AttrOrigin LabelEllipsizePropertyInfo = Label
    attrGet = getLabelEllipsize
    attrSet = setLabelEllipsize
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelEllipsize
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.ellipsize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:ellipsize"
        })
#endif

-- VVV Prop "justify"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Justification"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@justify@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #justify
-- @
getLabelJustify :: (MonadIO m, IsLabel o) => o -> m Gtk.Enums.Justification
getLabelJustify obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "justify"

-- | Set the value of the “@justify@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #justify 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelJustify :: (MonadIO m, IsLabel o) => o -> Gtk.Enums.Justification -> m ()
setLabelJustify obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "justify" val

-- | Construct a `GValueConstruct` with valid value for the “@justify@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelJustify :: (IsLabel o, MIO.MonadIO m) => Gtk.Enums.Justification -> m (GValueConstruct o)
constructLabelJustify val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "justify" val

#if defined(ENABLE_OVERLOADING)
data LabelJustifyPropertyInfo
instance AttrInfo LabelJustifyPropertyInfo where
    type AttrAllowedOps LabelJustifyPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LabelJustifyPropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelJustifyPropertyInfo = (~) Gtk.Enums.Justification
    type AttrTransferTypeConstraint LabelJustifyPropertyInfo = (~) Gtk.Enums.Justification
    type AttrTransferType LabelJustifyPropertyInfo = Gtk.Enums.Justification
    type AttrGetType LabelJustifyPropertyInfo = Gtk.Enums.Justification
    type AttrLabel LabelJustifyPropertyInfo = "justify"
    type AttrOrigin LabelJustifyPropertyInfo = Label
    attrGet = getLabelJustify
    attrSet = setLabelJustify
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelJustify
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.justify"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:justify"
        })
#endif

-- VVV Prop "label"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@label@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #label
-- @
getLabelLabel :: (MonadIO m, IsLabel o) => o -> m T.Text
getLabelLabel obj = MIO.liftIO $ checkUnexpectedNothing "getLabelLabel" $ B.Properties.getObjectPropertyString obj "label"

-- | Set the value of the “@label@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #label 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelLabel :: (MonadIO m, IsLabel o) => o -> T.Text -> m ()
setLabelLabel obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "label" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@label@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelLabel :: (IsLabel o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructLabelLabel val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "label" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data LabelLabelPropertyInfo
instance AttrInfo LabelLabelPropertyInfo where
    type AttrAllowedOps LabelLabelPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LabelLabelPropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelLabelPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint LabelLabelPropertyInfo = (~) T.Text
    type AttrTransferType LabelLabelPropertyInfo = T.Text
    type AttrGetType LabelLabelPropertyInfo = T.Text
    type AttrLabel LabelLabelPropertyInfo = "label"
    type AttrOrigin LabelLabelPropertyInfo = Label
    attrGet = getLabelLabel
    attrSet = setLabelLabel
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelLabel
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.label"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:label"
        })
#endif

-- VVV Prop "lines"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@lines@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #lines
-- @
getLabelLines :: (MonadIO m, IsLabel o) => o -> m Int32
getLabelLines obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "lines"

-- | Set the value of the “@lines@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #lines 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelLines :: (MonadIO m, IsLabel o) => o -> Int32 -> m ()
setLabelLines obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "lines" val

-- | Construct a `GValueConstruct` with valid value for the “@lines@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelLines :: (IsLabel o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructLabelLines val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "lines" val

#if defined(ENABLE_OVERLOADING)
data LabelLinesPropertyInfo
instance AttrInfo LabelLinesPropertyInfo where
    type AttrAllowedOps LabelLinesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LabelLinesPropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelLinesPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint LabelLinesPropertyInfo = (~) Int32
    type AttrTransferType LabelLinesPropertyInfo = Int32
    type AttrGetType LabelLinesPropertyInfo = Int32
    type AttrLabel LabelLinesPropertyInfo = "lines"
    type AttrOrigin LabelLinesPropertyInfo = Label
    attrGet = getLabelLines
    attrSet = setLabelLines
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelLines
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.lines"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:lines"
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
-- 'Data.GI.Base.Attributes.get' label #maxWidthChars
-- @
getLabelMaxWidthChars :: (MonadIO m, IsLabel o) => o -> m Int32
getLabelMaxWidthChars obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "max-width-chars"

-- | Set the value of the “@max-width-chars@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #maxWidthChars 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelMaxWidthChars :: (MonadIO m, IsLabel o) => o -> Int32 -> m ()
setLabelMaxWidthChars obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "max-width-chars" val

-- | Construct a `GValueConstruct` with valid value for the “@max-width-chars@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelMaxWidthChars :: (IsLabel o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructLabelMaxWidthChars val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "max-width-chars" val

#if defined(ENABLE_OVERLOADING)
data LabelMaxWidthCharsPropertyInfo
instance AttrInfo LabelMaxWidthCharsPropertyInfo where
    type AttrAllowedOps LabelMaxWidthCharsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LabelMaxWidthCharsPropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelMaxWidthCharsPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint LabelMaxWidthCharsPropertyInfo = (~) Int32
    type AttrTransferType LabelMaxWidthCharsPropertyInfo = Int32
    type AttrGetType LabelMaxWidthCharsPropertyInfo = Int32
    type AttrLabel LabelMaxWidthCharsPropertyInfo = "max-width-chars"
    type AttrOrigin LabelMaxWidthCharsPropertyInfo = Label
    attrGet = getLabelMaxWidthChars
    attrSet = setLabelMaxWidthChars
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelMaxWidthChars
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.maxWidthChars"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:maxWidthChars"
        })
#endif

-- VVV Prop "mnemonic-keyval"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@mnemonic-keyval@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #mnemonicKeyval
-- @
getLabelMnemonicKeyval :: (MonadIO m, IsLabel o) => o -> m Word32
getLabelMnemonicKeyval obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "mnemonic-keyval"

#if defined(ENABLE_OVERLOADING)
data LabelMnemonicKeyvalPropertyInfo
instance AttrInfo LabelMnemonicKeyvalPropertyInfo where
    type AttrAllowedOps LabelMnemonicKeyvalPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint LabelMnemonicKeyvalPropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelMnemonicKeyvalPropertyInfo = (~) ()
    type AttrTransferTypeConstraint LabelMnemonicKeyvalPropertyInfo = (~) ()
    type AttrTransferType LabelMnemonicKeyvalPropertyInfo = ()
    type AttrGetType LabelMnemonicKeyvalPropertyInfo = Word32
    type AttrLabel LabelMnemonicKeyvalPropertyInfo = "mnemonic-keyval"
    type AttrOrigin LabelMnemonicKeyvalPropertyInfo = Label
    attrGet = getLabelMnemonicKeyval
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.mnemonicKeyval"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:mnemonicKeyval"
        })
#endif

-- VVV Prop "mnemonic-widget"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Widget"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@mnemonic-widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #mnemonicWidget
-- @
getLabelMnemonicWidget :: (MonadIO m, IsLabel o) => o -> m (Maybe Gtk.Widget.Widget)
getLabelMnemonicWidget obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "mnemonic-widget" Gtk.Widget.Widget

-- | Set the value of the “@mnemonic-widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #mnemonicWidget 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelMnemonicWidget :: (MonadIO m, IsLabel o, Gtk.Widget.IsWidget a) => o -> a -> m ()
setLabelMnemonicWidget obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "mnemonic-widget" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@mnemonic-widget@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelMnemonicWidget :: (IsLabel o, MIO.MonadIO m, Gtk.Widget.IsWidget a) => a -> m (GValueConstruct o)
constructLabelMnemonicWidget val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "mnemonic-widget" (P.Just val)

-- | Set the value of the “@mnemonic-widget@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #mnemonicWidget
-- @
clearLabelMnemonicWidget :: (MonadIO m, IsLabel o) => o -> m ()
clearLabelMnemonicWidget obj = liftIO $ B.Properties.setObjectPropertyObject obj "mnemonic-widget" (Nothing :: Maybe Gtk.Widget.Widget)

#if defined(ENABLE_OVERLOADING)
data LabelMnemonicWidgetPropertyInfo
instance AttrInfo LabelMnemonicWidgetPropertyInfo where
    type AttrAllowedOps LabelMnemonicWidgetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint LabelMnemonicWidgetPropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelMnemonicWidgetPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferTypeConstraint LabelMnemonicWidgetPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferType LabelMnemonicWidgetPropertyInfo = Gtk.Widget.Widget
    type AttrGetType LabelMnemonicWidgetPropertyInfo = (Maybe Gtk.Widget.Widget)
    type AttrLabel LabelMnemonicWidgetPropertyInfo = "mnemonic-widget"
    type AttrOrigin LabelMnemonicWidgetPropertyInfo = Label
    attrGet = getLabelMnemonicWidget
    attrSet = setLabelMnemonicWidget
    attrTransfer _ v = do
        unsafeCastTo Gtk.Widget.Widget v
    attrConstruct = constructLabelMnemonicWidget
    attrClear = clearLabelMnemonicWidget
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.mnemonicWidget"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:mnemonicWidget"
        })
#endif

-- VVV Prop "pattern"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyWritable]
   -- Nullable: (Nothing,Just False)

-- | Set the value of the “@pattern@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #pattern 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelPattern :: (MonadIO m, IsLabel o) => o -> T.Text -> m ()
setLabelPattern obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "pattern" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@pattern@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelPattern :: (IsLabel o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructLabelPattern val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "pattern" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data LabelPatternPropertyInfo
instance AttrInfo LabelPatternPropertyInfo where
    type AttrAllowedOps LabelPatternPropertyInfo = '[ 'AttrSet, 'AttrConstruct]
    type AttrBaseTypeConstraint LabelPatternPropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelPatternPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint LabelPatternPropertyInfo = (~) T.Text
    type AttrTransferType LabelPatternPropertyInfo = T.Text
    type AttrGetType LabelPatternPropertyInfo = ()
    type AttrLabel LabelPatternPropertyInfo = "pattern"
    type AttrOrigin LabelPatternPropertyInfo = Label
    attrGet = undefined
    attrSet = setLabelPattern
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelPattern
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.pattern"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:pattern"
        })
#endif

-- VVV Prop "selectable"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@selectable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #selectable
-- @
getLabelSelectable :: (MonadIO m, IsLabel o) => o -> m Bool
getLabelSelectable obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "selectable"

-- | Set the value of the “@selectable@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #selectable 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelSelectable :: (MonadIO m, IsLabel o) => o -> Bool -> m ()
setLabelSelectable obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "selectable" val

-- | Construct a `GValueConstruct` with valid value for the “@selectable@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelSelectable :: (IsLabel o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructLabelSelectable val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "selectable" val

#if defined(ENABLE_OVERLOADING)
data LabelSelectablePropertyInfo
instance AttrInfo LabelSelectablePropertyInfo where
    type AttrAllowedOps LabelSelectablePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LabelSelectablePropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelSelectablePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint LabelSelectablePropertyInfo = (~) Bool
    type AttrTransferType LabelSelectablePropertyInfo = Bool
    type AttrGetType LabelSelectablePropertyInfo = Bool
    type AttrLabel LabelSelectablePropertyInfo = "selectable"
    type AttrOrigin LabelSelectablePropertyInfo = Label
    attrGet = getLabelSelectable
    attrSet = setLabelSelectable
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelSelectable
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.selectable"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:selectable"
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
-- 'Data.GI.Base.Attributes.get' label #selectionBound
-- @
getLabelSelectionBound :: (MonadIO m, IsLabel o) => o -> m Int32
getLabelSelectionBound obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "selection-bound"

#if defined(ENABLE_OVERLOADING)
data LabelSelectionBoundPropertyInfo
instance AttrInfo LabelSelectionBoundPropertyInfo where
    type AttrAllowedOps LabelSelectionBoundPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint LabelSelectionBoundPropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelSelectionBoundPropertyInfo = (~) ()
    type AttrTransferTypeConstraint LabelSelectionBoundPropertyInfo = (~) ()
    type AttrTransferType LabelSelectionBoundPropertyInfo = ()
    type AttrGetType LabelSelectionBoundPropertyInfo = Int32
    type AttrLabel LabelSelectionBoundPropertyInfo = "selection-bound"
    type AttrOrigin LabelSelectionBoundPropertyInfo = Label
    attrGet = getLabelSelectionBound
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.selectionBound"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:selectionBound"
        })
#endif

-- VVV Prop "single-line-mode"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@single-line-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #singleLineMode
-- @
getLabelSingleLineMode :: (MonadIO m, IsLabel o) => o -> m Bool
getLabelSingleLineMode obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "single-line-mode"

-- | Set the value of the “@single-line-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #singleLineMode 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelSingleLineMode :: (MonadIO m, IsLabel o) => o -> Bool -> m ()
setLabelSingleLineMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "single-line-mode" val

-- | Construct a `GValueConstruct` with valid value for the “@single-line-mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelSingleLineMode :: (IsLabel o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructLabelSingleLineMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "single-line-mode" val

#if defined(ENABLE_OVERLOADING)
data LabelSingleLineModePropertyInfo
instance AttrInfo LabelSingleLineModePropertyInfo where
    type AttrAllowedOps LabelSingleLineModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LabelSingleLineModePropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelSingleLineModePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint LabelSingleLineModePropertyInfo = (~) Bool
    type AttrTransferType LabelSingleLineModePropertyInfo = Bool
    type AttrGetType LabelSingleLineModePropertyInfo = Bool
    type AttrLabel LabelSingleLineModePropertyInfo = "single-line-mode"
    type AttrOrigin LabelSingleLineModePropertyInfo = Label
    attrGet = getLabelSingleLineMode
    attrSet = setLabelSingleLineMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelSingleLineMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.singleLineMode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:singleLineMode"
        })
#endif

-- VVV Prop "track-visited-links"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@track-visited-links@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #trackVisitedLinks
-- @
getLabelTrackVisitedLinks :: (MonadIO m, IsLabel o) => o -> m Bool
getLabelTrackVisitedLinks obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "track-visited-links"

-- | Set the value of the “@track-visited-links@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #trackVisitedLinks 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelTrackVisitedLinks :: (MonadIO m, IsLabel o) => o -> Bool -> m ()
setLabelTrackVisitedLinks obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "track-visited-links" val

-- | Construct a `GValueConstruct` with valid value for the “@track-visited-links@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelTrackVisitedLinks :: (IsLabel o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructLabelTrackVisitedLinks val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "track-visited-links" val

#if defined(ENABLE_OVERLOADING)
data LabelTrackVisitedLinksPropertyInfo
instance AttrInfo LabelTrackVisitedLinksPropertyInfo where
    type AttrAllowedOps LabelTrackVisitedLinksPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LabelTrackVisitedLinksPropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelTrackVisitedLinksPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint LabelTrackVisitedLinksPropertyInfo = (~) Bool
    type AttrTransferType LabelTrackVisitedLinksPropertyInfo = Bool
    type AttrGetType LabelTrackVisitedLinksPropertyInfo = Bool
    type AttrLabel LabelTrackVisitedLinksPropertyInfo = "track-visited-links"
    type AttrOrigin LabelTrackVisitedLinksPropertyInfo = Label
    attrGet = getLabelTrackVisitedLinks
    attrSet = setLabelTrackVisitedLinks
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelTrackVisitedLinks
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.trackVisitedLinks"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:trackVisitedLinks"
        })
#endif

-- VVV Prop "use-markup"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@use-markup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #useMarkup
-- @
getLabelUseMarkup :: (MonadIO m, IsLabel o) => o -> m Bool
getLabelUseMarkup obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "use-markup"

-- | Set the value of the “@use-markup@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #useMarkup 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelUseMarkup :: (MonadIO m, IsLabel o) => o -> Bool -> m ()
setLabelUseMarkup obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "use-markup" val

-- | Construct a `GValueConstruct` with valid value for the “@use-markup@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelUseMarkup :: (IsLabel o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructLabelUseMarkup val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "use-markup" val

#if defined(ENABLE_OVERLOADING)
data LabelUseMarkupPropertyInfo
instance AttrInfo LabelUseMarkupPropertyInfo where
    type AttrAllowedOps LabelUseMarkupPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LabelUseMarkupPropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelUseMarkupPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint LabelUseMarkupPropertyInfo = (~) Bool
    type AttrTransferType LabelUseMarkupPropertyInfo = Bool
    type AttrGetType LabelUseMarkupPropertyInfo = Bool
    type AttrLabel LabelUseMarkupPropertyInfo = "use-markup"
    type AttrOrigin LabelUseMarkupPropertyInfo = Label
    attrGet = getLabelUseMarkup
    attrSet = setLabelUseMarkup
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelUseMarkup
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.useMarkup"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:useMarkup"
        })
#endif

-- VVV Prop "use-underline"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@use-underline@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #useUnderline
-- @
getLabelUseUnderline :: (MonadIO m, IsLabel o) => o -> m Bool
getLabelUseUnderline obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "use-underline"

-- | Set the value of the “@use-underline@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #useUnderline 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelUseUnderline :: (MonadIO m, IsLabel o) => o -> Bool -> m ()
setLabelUseUnderline obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "use-underline" val

-- | Construct a `GValueConstruct` with valid value for the “@use-underline@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelUseUnderline :: (IsLabel o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructLabelUseUnderline val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "use-underline" val

#if defined(ENABLE_OVERLOADING)
data LabelUseUnderlinePropertyInfo
instance AttrInfo LabelUseUnderlinePropertyInfo where
    type AttrAllowedOps LabelUseUnderlinePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LabelUseUnderlinePropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelUseUnderlinePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint LabelUseUnderlinePropertyInfo = (~) Bool
    type AttrTransferType LabelUseUnderlinePropertyInfo = Bool
    type AttrGetType LabelUseUnderlinePropertyInfo = Bool
    type AttrLabel LabelUseUnderlinePropertyInfo = "use-underline"
    type AttrOrigin LabelUseUnderlinePropertyInfo = Label
    attrGet = getLabelUseUnderline
    attrSet = setLabelUseUnderline
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelUseUnderline
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.useUnderline"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:useUnderline"
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
-- 'Data.GI.Base.Attributes.get' label #widthChars
-- @
getLabelWidthChars :: (MonadIO m, IsLabel o) => o -> m Int32
getLabelWidthChars obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "width-chars"

-- | Set the value of the “@width-chars@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #widthChars 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelWidthChars :: (MonadIO m, IsLabel o) => o -> Int32 -> m ()
setLabelWidthChars obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "width-chars" val

-- | Construct a `GValueConstruct` with valid value for the “@width-chars@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelWidthChars :: (IsLabel o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructLabelWidthChars val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "width-chars" val

#if defined(ENABLE_OVERLOADING)
data LabelWidthCharsPropertyInfo
instance AttrInfo LabelWidthCharsPropertyInfo where
    type AttrAllowedOps LabelWidthCharsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LabelWidthCharsPropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelWidthCharsPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint LabelWidthCharsPropertyInfo = (~) Int32
    type AttrTransferType LabelWidthCharsPropertyInfo = Int32
    type AttrGetType LabelWidthCharsPropertyInfo = Int32
    type AttrLabel LabelWidthCharsPropertyInfo = "width-chars"
    type AttrOrigin LabelWidthCharsPropertyInfo = Label
    attrGet = getLabelWidthChars
    attrSet = setLabelWidthChars
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelWidthChars
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.widthChars"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:widthChars"
        })
#endif

-- VVV Prop "wrap"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@wrap@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #wrap
-- @
getLabelWrap :: (MonadIO m, IsLabel o) => o -> m Bool
getLabelWrap obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "wrap"

-- | Set the value of the “@wrap@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #wrap 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelWrap :: (MonadIO m, IsLabel o) => o -> Bool -> m ()
setLabelWrap obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "wrap" val

-- | Construct a `GValueConstruct` with valid value for the “@wrap@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelWrap :: (IsLabel o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructLabelWrap val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "wrap" val

#if defined(ENABLE_OVERLOADING)
data LabelWrapPropertyInfo
instance AttrInfo LabelWrapPropertyInfo where
    type AttrAllowedOps LabelWrapPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LabelWrapPropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelWrapPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint LabelWrapPropertyInfo = (~) Bool
    type AttrTransferType LabelWrapPropertyInfo = Bool
    type AttrGetType LabelWrapPropertyInfo = Bool
    type AttrLabel LabelWrapPropertyInfo = "wrap"
    type AttrOrigin LabelWrapPropertyInfo = Label
    attrGet = getLabelWrap
    attrSet = setLabelWrap
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelWrap
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.wrap"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:wrap"
        })
#endif

-- VVV Prop "wrap-mode"
   -- Type: TInterface (Name {namespace = "Pango", name = "WrapMode"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@wrap-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #wrapMode
-- @
getLabelWrapMode :: (MonadIO m, IsLabel o) => o -> m Pango.Enums.WrapMode
getLabelWrapMode obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "wrap-mode"

-- | Set the value of the “@wrap-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #wrapMode 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelWrapMode :: (MonadIO m, IsLabel o) => o -> Pango.Enums.WrapMode -> m ()
setLabelWrapMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "wrap-mode" val

-- | Construct a `GValueConstruct` with valid value for the “@wrap-mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelWrapMode :: (IsLabel o, MIO.MonadIO m) => Pango.Enums.WrapMode -> m (GValueConstruct o)
constructLabelWrapMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "wrap-mode" val

#if defined(ENABLE_OVERLOADING)
data LabelWrapModePropertyInfo
instance AttrInfo LabelWrapModePropertyInfo where
    type AttrAllowedOps LabelWrapModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LabelWrapModePropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelWrapModePropertyInfo = (~) Pango.Enums.WrapMode
    type AttrTransferTypeConstraint LabelWrapModePropertyInfo = (~) Pango.Enums.WrapMode
    type AttrTransferType LabelWrapModePropertyInfo = Pango.Enums.WrapMode
    type AttrGetType LabelWrapModePropertyInfo = Pango.Enums.WrapMode
    type AttrLabel LabelWrapModePropertyInfo = "wrap-mode"
    type AttrOrigin LabelWrapModePropertyInfo = Label
    attrGet = getLabelWrapMode
    attrSet = setLabelWrapMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelWrapMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.wrapMode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:wrapMode"
        })
#endif

-- VVV Prop "xalign"
   -- Type: TBasicType TFloat
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@xalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #xalign
-- @
getLabelXalign :: (MonadIO m, IsLabel o) => o -> m Float
getLabelXalign obj = MIO.liftIO $ B.Properties.getObjectPropertyFloat obj "xalign"

-- | Set the value of the “@xalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #xalign 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelXalign :: (MonadIO m, IsLabel o) => o -> Float -> m ()
setLabelXalign obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFloat obj "xalign" val

-- | Construct a `GValueConstruct` with valid value for the “@xalign@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelXalign :: (IsLabel o, MIO.MonadIO m) => Float -> m (GValueConstruct o)
constructLabelXalign val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFloat "xalign" val

#if defined(ENABLE_OVERLOADING)
data LabelXalignPropertyInfo
instance AttrInfo LabelXalignPropertyInfo where
    type AttrAllowedOps LabelXalignPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LabelXalignPropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelXalignPropertyInfo = (~) Float
    type AttrTransferTypeConstraint LabelXalignPropertyInfo = (~) Float
    type AttrTransferType LabelXalignPropertyInfo = Float
    type AttrGetType LabelXalignPropertyInfo = Float
    type AttrLabel LabelXalignPropertyInfo = "xalign"
    type AttrOrigin LabelXalignPropertyInfo = Label
    attrGet = getLabelXalign
    attrSet = setLabelXalign
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelXalign
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.xalign"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:xalign"
        })
#endif

-- VVV Prop "yalign"
   -- Type: TBasicType TFloat
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@yalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' label #yalign
-- @
getLabelYalign :: (MonadIO m, IsLabel o) => o -> m Float
getLabelYalign obj = MIO.liftIO $ B.Properties.getObjectPropertyFloat obj "yalign"

-- | Set the value of the “@yalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' label [ #yalign 'Data.GI.Base.Attributes.:=' value ]
-- @
setLabelYalign :: (MonadIO m, IsLabel o) => o -> Float -> m ()
setLabelYalign obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFloat obj "yalign" val

-- | Construct a `GValueConstruct` with valid value for the “@yalign@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructLabelYalign :: (IsLabel o, MIO.MonadIO m) => Float -> m (GValueConstruct o)
constructLabelYalign val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFloat "yalign" val

#if defined(ENABLE_OVERLOADING)
data LabelYalignPropertyInfo
instance AttrInfo LabelYalignPropertyInfo where
    type AttrAllowedOps LabelYalignPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint LabelYalignPropertyInfo = IsLabel
    type AttrSetTypeConstraint LabelYalignPropertyInfo = (~) Float
    type AttrTransferTypeConstraint LabelYalignPropertyInfo = (~) Float
    type AttrTransferType LabelYalignPropertyInfo = Float
    type AttrGetType LabelYalignPropertyInfo = Float
    type AttrLabel LabelYalignPropertyInfo = "yalign"
    type AttrOrigin LabelYalignPropertyInfo = Label
    attrGet = getLabelYalign
    attrSet = setLabelYalign
    attrTransfer _ v = do
        return v
    attrConstruct = constructLabelYalign
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.yalign"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#g:attr:yalign"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Label
type instance O.AttributeList Label = LabelAttributeList
type LabelAttributeList = ('[ '("angle", LabelAnglePropertyInfo), '("appPaintable", Gtk.Widget.WidgetAppPaintablePropertyInfo), '("attributes", LabelAttributesPropertyInfo), '("canDefault", Gtk.Widget.WidgetCanDefaultPropertyInfo), '("canFocus", Gtk.Widget.WidgetCanFocusPropertyInfo), '("compositeChild", Gtk.Widget.WidgetCompositeChildPropertyInfo), '("cursorPosition", LabelCursorPositionPropertyInfo), '("doubleBuffered", Gtk.Widget.WidgetDoubleBufferedPropertyInfo), '("ellipsize", LabelEllipsizePropertyInfo), '("events", Gtk.Widget.WidgetEventsPropertyInfo), '("expand", Gtk.Widget.WidgetExpandPropertyInfo), '("focusOnClick", Gtk.Widget.WidgetFocusOnClickPropertyInfo), '("halign", Gtk.Widget.WidgetHalignPropertyInfo), '("hasDefault", Gtk.Widget.WidgetHasDefaultPropertyInfo), '("hasFocus", Gtk.Widget.WidgetHasFocusPropertyInfo), '("hasTooltip", Gtk.Widget.WidgetHasTooltipPropertyInfo), '("heightRequest", Gtk.Widget.WidgetHeightRequestPropertyInfo), '("hexpand", Gtk.Widget.WidgetHexpandPropertyInfo), '("hexpandSet", Gtk.Widget.WidgetHexpandSetPropertyInfo), '("isFocus", Gtk.Widget.WidgetIsFocusPropertyInfo), '("justify", LabelJustifyPropertyInfo), '("label", LabelLabelPropertyInfo), '("lines", LabelLinesPropertyInfo), '("margin", Gtk.Widget.WidgetMarginPropertyInfo), '("marginBottom", Gtk.Widget.WidgetMarginBottomPropertyInfo), '("marginEnd", Gtk.Widget.WidgetMarginEndPropertyInfo), '("marginLeft", Gtk.Widget.WidgetMarginLeftPropertyInfo), '("marginRight", Gtk.Widget.WidgetMarginRightPropertyInfo), '("marginStart", Gtk.Widget.WidgetMarginStartPropertyInfo), '("marginTop", Gtk.Widget.WidgetMarginTopPropertyInfo), '("maxWidthChars", LabelMaxWidthCharsPropertyInfo), '("mnemonicKeyval", LabelMnemonicKeyvalPropertyInfo), '("mnemonicWidget", LabelMnemonicWidgetPropertyInfo), '("name", Gtk.Widget.WidgetNamePropertyInfo), '("noShowAll", Gtk.Widget.WidgetNoShowAllPropertyInfo), '("opacity", Gtk.Widget.WidgetOpacityPropertyInfo), '("parent", Gtk.Widget.WidgetParentPropertyInfo), '("pattern", LabelPatternPropertyInfo), '("receivesDefault", Gtk.Widget.WidgetReceivesDefaultPropertyInfo), '("scaleFactor", Gtk.Widget.WidgetScaleFactorPropertyInfo), '("selectable", LabelSelectablePropertyInfo), '("selectionBound", LabelSelectionBoundPropertyInfo), '("sensitive", Gtk.Widget.WidgetSensitivePropertyInfo), '("singleLineMode", LabelSingleLineModePropertyInfo), '("style", Gtk.Widget.WidgetStylePropertyInfo), '("tooltipMarkup", Gtk.Widget.WidgetTooltipMarkupPropertyInfo), '("tooltipText", Gtk.Widget.WidgetTooltipTextPropertyInfo), '("trackVisitedLinks", LabelTrackVisitedLinksPropertyInfo), '("useMarkup", LabelUseMarkupPropertyInfo), '("useUnderline", LabelUseUnderlinePropertyInfo), '("valign", Gtk.Widget.WidgetValignPropertyInfo), '("vexpand", Gtk.Widget.WidgetVexpandPropertyInfo), '("vexpandSet", Gtk.Widget.WidgetVexpandSetPropertyInfo), '("visible", Gtk.Widget.WidgetVisiblePropertyInfo), '("widthChars", LabelWidthCharsPropertyInfo), '("widthRequest", Gtk.Widget.WidgetWidthRequestPropertyInfo), '("window", Gtk.Widget.WidgetWindowPropertyInfo), '("wrap", LabelWrapPropertyInfo), '("wrapMode", LabelWrapModePropertyInfo), '("xalign", LabelXalignPropertyInfo), '("xpad", Gtk.Misc.MiscXpadPropertyInfo), '("yalign", LabelYalignPropertyInfo), '("ypad", Gtk.Misc.MiscYpadPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
labelAngle :: AttrLabelProxy "angle"
labelAngle = AttrLabelProxy

labelAttributes :: AttrLabelProxy "attributes"
labelAttributes = AttrLabelProxy

labelCursorPosition :: AttrLabelProxy "cursorPosition"
labelCursorPosition = AttrLabelProxy

labelEllipsize :: AttrLabelProxy "ellipsize"
labelEllipsize = AttrLabelProxy

labelJustify :: AttrLabelProxy "justify"
labelJustify = AttrLabelProxy

labelLabel :: AttrLabelProxy "label"
labelLabel = AttrLabelProxy

labelLines :: AttrLabelProxy "lines"
labelLines = AttrLabelProxy

labelMaxWidthChars :: AttrLabelProxy "maxWidthChars"
labelMaxWidthChars = AttrLabelProxy

labelMnemonicKeyval :: AttrLabelProxy "mnemonicKeyval"
labelMnemonicKeyval = AttrLabelProxy

labelMnemonicWidget :: AttrLabelProxy "mnemonicWidget"
labelMnemonicWidget = AttrLabelProxy

labelPattern :: AttrLabelProxy "pattern"
labelPattern = AttrLabelProxy

labelSelectable :: AttrLabelProxy "selectable"
labelSelectable = AttrLabelProxy

labelSelectionBound :: AttrLabelProxy "selectionBound"
labelSelectionBound = AttrLabelProxy

labelSingleLineMode :: AttrLabelProxy "singleLineMode"
labelSingleLineMode = AttrLabelProxy

labelTrackVisitedLinks :: AttrLabelProxy "trackVisitedLinks"
labelTrackVisitedLinks = AttrLabelProxy

labelUseMarkup :: AttrLabelProxy "useMarkup"
labelUseMarkup = AttrLabelProxy

labelUseUnderline :: AttrLabelProxy "useUnderline"
labelUseUnderline = AttrLabelProxy

labelWidthChars :: AttrLabelProxy "widthChars"
labelWidthChars = AttrLabelProxy

labelWrap :: AttrLabelProxy "wrap"
labelWrap = AttrLabelProxy

labelWrapMode :: AttrLabelProxy "wrapMode"
labelWrapMode = AttrLabelProxy

labelXalign :: AttrLabelProxy "xalign"
labelXalign = AttrLabelProxy

labelYalign :: AttrLabelProxy "yalign"
labelYalign = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Label = LabelSignalList
type LabelSignalList = ('[ '("accelClosuresChanged", Gtk.Widget.WidgetAccelClosuresChangedSignalInfo), '("activateCurrentLink", LabelActivateCurrentLinkSignalInfo), '("activateLink", LabelActivateLinkSignalInfo), '("buttonPressEvent", Gtk.Widget.WidgetButtonPressEventSignalInfo), '("buttonReleaseEvent", Gtk.Widget.WidgetButtonReleaseEventSignalInfo), '("canActivateAccel", Gtk.Widget.WidgetCanActivateAccelSignalInfo), '("childNotify", Gtk.Widget.WidgetChildNotifySignalInfo), '("compositedChanged", Gtk.Widget.WidgetCompositedChangedSignalInfo), '("configureEvent", Gtk.Widget.WidgetConfigureEventSignalInfo), '("copyClipboard", LabelCopyClipboardSignalInfo), '("damageEvent", Gtk.Widget.WidgetDamageEventSignalInfo), '("deleteEvent", Gtk.Widget.WidgetDeleteEventSignalInfo), '("destroy", Gtk.Widget.WidgetDestroySignalInfo), '("destroyEvent", Gtk.Widget.WidgetDestroyEventSignalInfo), '("directionChanged", Gtk.Widget.WidgetDirectionChangedSignalInfo), '("dragBegin", Gtk.Widget.WidgetDragBeginSignalInfo), '("dragDataDelete", Gtk.Widget.WidgetDragDataDeleteSignalInfo), '("dragDataGet", Gtk.Widget.WidgetDragDataGetSignalInfo), '("dragDataReceived", Gtk.Widget.WidgetDragDataReceivedSignalInfo), '("dragDrop", Gtk.Widget.WidgetDragDropSignalInfo), '("dragEnd", Gtk.Widget.WidgetDragEndSignalInfo), '("dragFailed", Gtk.Widget.WidgetDragFailedSignalInfo), '("dragLeave", Gtk.Widget.WidgetDragLeaveSignalInfo), '("dragMotion", Gtk.Widget.WidgetDragMotionSignalInfo), '("draw", Gtk.Widget.WidgetDrawSignalInfo), '("enterNotifyEvent", Gtk.Widget.WidgetEnterNotifyEventSignalInfo), '("event", Gtk.Widget.WidgetEventSignalInfo), '("eventAfter", Gtk.Widget.WidgetEventAfterSignalInfo), '("focus", Gtk.Widget.WidgetFocusSignalInfo), '("focusInEvent", Gtk.Widget.WidgetFocusInEventSignalInfo), '("focusOutEvent", Gtk.Widget.WidgetFocusOutEventSignalInfo), '("grabBrokenEvent", Gtk.Widget.WidgetGrabBrokenEventSignalInfo), '("grabFocus", Gtk.Widget.WidgetGrabFocusSignalInfo), '("grabNotify", Gtk.Widget.WidgetGrabNotifySignalInfo), '("hide", Gtk.Widget.WidgetHideSignalInfo), '("hierarchyChanged", Gtk.Widget.WidgetHierarchyChangedSignalInfo), '("keyPressEvent", Gtk.Widget.WidgetKeyPressEventSignalInfo), '("keyReleaseEvent", Gtk.Widget.WidgetKeyReleaseEventSignalInfo), '("keynavFailed", Gtk.Widget.WidgetKeynavFailedSignalInfo), '("leaveNotifyEvent", Gtk.Widget.WidgetLeaveNotifyEventSignalInfo), '("map", Gtk.Widget.WidgetMapSignalInfo), '("mapEvent", Gtk.Widget.WidgetMapEventSignalInfo), '("mnemonicActivate", Gtk.Widget.WidgetMnemonicActivateSignalInfo), '("motionNotifyEvent", Gtk.Widget.WidgetMotionNotifyEventSignalInfo), '("moveCursor", LabelMoveCursorSignalInfo), '("moveFocus", Gtk.Widget.WidgetMoveFocusSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("parentSet", Gtk.Widget.WidgetParentSetSignalInfo), '("populatePopup", LabelPopulatePopupSignalInfo), '("popupMenu", Gtk.Widget.WidgetPopupMenuSignalInfo), '("propertyNotifyEvent", Gtk.Widget.WidgetPropertyNotifyEventSignalInfo), '("proximityInEvent", Gtk.Widget.WidgetProximityInEventSignalInfo), '("proximityOutEvent", Gtk.Widget.WidgetProximityOutEventSignalInfo), '("queryTooltip", Gtk.Widget.WidgetQueryTooltipSignalInfo), '("realize", Gtk.Widget.WidgetRealizeSignalInfo), '("screenChanged", Gtk.Widget.WidgetScreenChangedSignalInfo), '("scrollEvent", Gtk.Widget.WidgetScrollEventSignalInfo), '("selectionClearEvent", Gtk.Widget.WidgetSelectionClearEventSignalInfo), '("selectionGet", Gtk.Widget.WidgetSelectionGetSignalInfo), '("selectionNotifyEvent", Gtk.Widget.WidgetSelectionNotifyEventSignalInfo), '("selectionReceived", Gtk.Widget.WidgetSelectionReceivedSignalInfo), '("selectionRequestEvent", Gtk.Widget.WidgetSelectionRequestEventSignalInfo), '("show", Gtk.Widget.WidgetShowSignalInfo), '("showHelp", Gtk.Widget.WidgetShowHelpSignalInfo), '("sizeAllocate", Gtk.Widget.WidgetSizeAllocateSignalInfo), '("stateChanged", Gtk.Widget.WidgetStateChangedSignalInfo), '("stateFlagsChanged", Gtk.Widget.WidgetStateFlagsChangedSignalInfo), '("styleSet", Gtk.Widget.WidgetStyleSetSignalInfo), '("styleUpdated", Gtk.Widget.WidgetStyleUpdatedSignalInfo), '("touchEvent", Gtk.Widget.WidgetTouchEventSignalInfo), '("unmap", Gtk.Widget.WidgetUnmapSignalInfo), '("unmapEvent", Gtk.Widget.WidgetUnmapEventSignalInfo), '("unrealize", Gtk.Widget.WidgetUnrealizeSignalInfo), '("visibilityNotifyEvent", Gtk.Widget.WidgetVisibilityNotifyEventSignalInfo), '("windowStateEvent", Gtk.Widget.WidgetWindowStateEventSignalInfo)] :: [(Symbol, *)])

#endif

-- method Label::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "str"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The text of the label"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Label" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_label_new" gtk_label_new :: 
    CString ->                              -- str : TBasicType TUTF8
    IO (Ptr Label)

-- | Creates a new label with the given text inside it. You can
-- pass 'P.Nothing' to get an empty label widget.
labelNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Maybe (T.Text)
    -- ^ /@str@/: The text of the label
    -> m Label
    -- ^ __Returns:__ the new t'GI.Gtk.Objects.Label.Label'
labelNew str = liftIO $ do
    maybeStr <- case str of
        Nothing -> return nullPtr
        Just jStr -> do
            jStr' <- textToCString jStr
            return jStr'
    result <- gtk_label_new maybeStr
    checkUnexpectedReturnNULL "labelNew" result
    result' <- (newObject Label) result
    freeMem maybeStr
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Label::new_with_mnemonic
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "str"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "The text of the label, with an underscore in front of the\n      mnemonic character"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Label" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_label_new_with_mnemonic" gtk_label_new_with_mnemonic :: 
    CString ->                              -- str : TBasicType TUTF8
    IO (Ptr Label)

-- | Creates a new t'GI.Gtk.Objects.Label.Label', containing the text in /@str@/.
-- 
-- If characters in /@str@/ are preceded by an underscore, they are
-- underlined. If you need a literal underscore character in a label, use
-- \'__\' (two underscores). The first underlined character represents a
-- keyboard accelerator called a mnemonic. The mnemonic key can be used
-- to activate another widget, chosen automatically, or explicitly using
-- 'GI.Gtk.Objects.Label.labelSetMnemonicWidget'.
-- 
-- If 'GI.Gtk.Objects.Label.labelSetMnemonicWidget' is not called, then the first
-- activatable ancestor of the t'GI.Gtk.Objects.Label.Label' will be chosen as the mnemonic
-- widget. For instance, if the label is inside a button or menu item,
-- the button or menu item will automatically become the mnemonic widget
-- and be activated by the mnemonic.
labelNewWithMnemonic ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Maybe (T.Text)
    -- ^ /@str@/: The text of the label, with an underscore in front of the
    --       mnemonic character
    -> m Label
    -- ^ __Returns:__ the new t'GI.Gtk.Objects.Label.Label'
labelNewWithMnemonic str = liftIO $ do
    maybeStr <- case str of
        Nothing -> return nullPtr
        Just jStr -> do
            jStr' <- textToCString jStr
            return jStr'
    result <- gtk_label_new_with_mnemonic maybeStr
    checkUnexpectedReturnNULL "labelNewWithMnemonic" result
    result' <- (newObject Label) result
    freeMem maybeStr
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Label::get_angle
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_angle" gtk_label_get_angle :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO CDouble

-- | Gets the angle of rotation for the label. See
-- 'GI.Gtk.Objects.Label.labelSetAngle'.
-- 
-- /Since: 2.6/
labelGetAngle ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Double
    -- ^ __Returns:__ the angle of rotation for the label
labelGetAngle label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_angle label'
    let result' = realToFrac result
    touchManagedPtr label
    return result'

#if defined(ENABLE_OVERLOADING)
data LabelGetAngleMethodInfo
instance (signature ~ (m Double), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetAngleMethodInfo a signature where
    overloadedMethod = labelGetAngle

instance O.OverloadedMethodInfo LabelGetAngleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetAngle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetAngle"
        })


#endif

-- method Label::get_attributes
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_attributes" gtk_label_get_attributes :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO (Ptr Pango.AttrList.AttrList)

-- | Gets the attribute list that was set on the label using
-- 'GI.Gtk.Objects.Label.labelSetAttributes', if any. This function does
-- not reflect attributes that come from the labels markup
-- (see 'GI.Gtk.Objects.Label.labelSetMarkup'). If you want to get the
-- effective attributes for the label, use
-- pango_layout_get_attribute (gtk_label_get_layout (label)).
labelGetAttributes ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m (Maybe Pango.AttrList.AttrList)
    -- ^ __Returns:__ the attribute list, or 'P.Nothing'
    --     if none was set.
labelGetAttributes label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_attributes label'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newBoxed Pango.AttrList.AttrList) result'
        return result''
    touchManagedPtr label
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data LabelGetAttributesMethodInfo
instance (signature ~ (m (Maybe Pango.AttrList.AttrList)), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetAttributesMethodInfo a signature where
    overloadedMethod = labelGetAttributes

instance O.OverloadedMethodInfo LabelGetAttributesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetAttributes",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetAttributes"
        })


#endif

-- method Label::get_current_uri
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_current_uri" gtk_label_get_current_uri :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO CString

-- | Returns the URI for the currently active link in the label.
-- The active link is the one under the mouse pointer or, in a
-- selectable label, the link in which the text cursor is currently
-- positioned.
-- 
-- This function is intended for use in a [Label::activateLink]("GI.Gtk.Objects.Label#g:signal:activateLink") handler
-- or for use in a [Widget::queryTooltip]("GI.Gtk.Objects.Widget#g:signal:queryTooltip") handler.
-- 
-- /Since: 2.18/
labelGetCurrentUri ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m T.Text
    -- ^ __Returns:__ the currently active URI. The string is owned by GTK+ and must
    --   not be freed or modified.
labelGetCurrentUri label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_current_uri label'
    checkUnexpectedReturnNULL "labelGetCurrentUri" result
    result' <- cstringToText result
    touchManagedPtr label
    return result'

#if defined(ENABLE_OVERLOADING)
data LabelGetCurrentUriMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetCurrentUriMethodInfo a signature where
    overloadedMethod = labelGetCurrentUri

instance O.OverloadedMethodInfo LabelGetCurrentUriMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetCurrentUri",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetCurrentUri"
        })


#endif

-- method Label::get_ellipsize
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Pango" , name = "EllipsizeMode" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_label_get_ellipsize" gtk_label_get_ellipsize :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO CUInt

-- | Returns the ellipsizing position of the label. See 'GI.Gtk.Objects.Label.labelSetEllipsize'.
-- 
-- /Since: 2.6/
labelGetEllipsize ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Pango.Enums.EllipsizeMode
    -- ^ __Returns:__ t'GI.Pango.Enums.EllipsizeMode'
labelGetEllipsize label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_ellipsize label'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr label
    return result'

#if defined(ENABLE_OVERLOADING)
data LabelGetEllipsizeMethodInfo
instance (signature ~ (m Pango.Enums.EllipsizeMode), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetEllipsizeMethodInfo a signature where
    overloadedMethod = labelGetEllipsize

instance O.OverloadedMethodInfo LabelGetEllipsizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetEllipsize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetEllipsize"
        })


#endif

-- method Label::get_justify
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "Justification" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_label_get_justify" gtk_label_get_justify :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO CUInt

-- | Returns the justification of the label. See 'GI.Gtk.Objects.Label.labelSetJustify'.
labelGetJustify ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Gtk.Enums.Justification
    -- ^ __Returns:__ t'GI.Gtk.Enums.Justification'
labelGetJustify label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_justify label'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr label
    return result'

#if defined(ENABLE_OVERLOADING)
data LabelGetJustifyMethodInfo
instance (signature ~ (m Gtk.Enums.Justification), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetJustifyMethodInfo a signature where
    overloadedMethod = labelGetJustify

instance O.OverloadedMethodInfo LabelGetJustifyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetJustify",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetJustify"
        })


#endif

-- method Label::get_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_label" gtk_label_get_label :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO CString

-- | Fetches the text from a label widget including any embedded
-- underlines indicating mnemonics and Pango markup. (See
-- 'GI.Gtk.Objects.Label.labelGetText').
labelGetLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m T.Text
    -- ^ __Returns:__ the text of the label widget. This string is
    --   owned by the widget and must not be modified or freed.
labelGetLabel label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_label label'
    checkUnexpectedReturnNULL "labelGetLabel" result
    result' <- cstringToText result
    touchManagedPtr label
    return result'

#if defined(ENABLE_OVERLOADING)
data LabelGetLabelMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetLabelMethodInfo a signature where
    overloadedMethod = labelGetLabel

instance O.OverloadedMethodInfo LabelGetLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetLabel"
        })


#endif

-- method Label::get_layout
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_layout" gtk_label_get_layout :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO (Ptr Pango.Layout.Layout)

-- | Gets the t'GI.Pango.Objects.Layout.Layout' used to display the label.
-- The layout is useful to e.g. convert text positions to
-- pixel positions, in combination with 'GI.Gtk.Objects.Label.labelGetLayoutOffsets'.
-- The returned layout is owned by the /@label@/ so need not be
-- freed by the caller. The /@label@/ is free to recreate its layout at
-- any time, so it should be considered read-only.
labelGetLayout ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Pango.Layout.Layout
    -- ^ __Returns:__ the t'GI.Pango.Objects.Layout.Layout' for this label
labelGetLayout label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_layout label'
    checkUnexpectedReturnNULL "labelGetLayout" result
    result' <- (newObject Pango.Layout.Layout) result
    touchManagedPtr label
    return result'

#if defined(ENABLE_OVERLOADING)
data LabelGetLayoutMethodInfo
instance (signature ~ (m Pango.Layout.Layout), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetLayoutMethodInfo a signature where
    overloadedMethod = labelGetLayout

instance O.OverloadedMethodInfo LabelGetLayoutMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetLayout",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetLayout"
        })


#endif

-- method Label::get_layout_offsets
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_layout_offsets" gtk_label_get_layout_offsets :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    Ptr Int32 ->                            -- x : TBasicType TInt
    Ptr Int32 ->                            -- y : TBasicType TInt
    IO ()

-- | Obtains the coordinates where the label will draw the t'GI.Pango.Objects.Layout.Layout'
-- representing the text in the label; useful to convert mouse events
-- into coordinates inside the t'GI.Pango.Objects.Layout.Layout', e.g. to take some action
-- if some part of the label is clicked. Of course you will need to
-- create a t'GI.Gtk.Objects.EventBox.EventBox' to receive the events, and pack the label
-- inside it, since labels are windowless (they return 'P.False' from
-- 'GI.Gtk.Objects.Widget.widgetGetHasWindow'). Remember
-- when using the t'GI.Pango.Objects.Layout.Layout' functions you need to convert to
-- and from pixels using @/PANGO_PIXELS()/@ or 'GI.Pango.Constants.SCALE'.
labelGetLayoutOffsets ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m ((Int32, Int32))
labelGetLayoutOffsets label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    x <- allocMem :: IO (Ptr Int32)
    y <- allocMem :: IO (Ptr Int32)
    gtk_label_get_layout_offsets label' x y
    x' <- peek x
    y' <- peek y
    touchManagedPtr label
    freeMem x
    freeMem y
    return (x', y')

#if defined(ENABLE_OVERLOADING)
data LabelGetLayoutOffsetsMethodInfo
instance (signature ~ (m ((Int32, Int32))), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetLayoutOffsetsMethodInfo a signature where
    overloadedMethod = labelGetLayoutOffsets

instance O.OverloadedMethodInfo LabelGetLayoutOffsetsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetLayoutOffsets",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetLayoutOffsets"
        })


#endif

-- method Label::get_line_wrap
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_line_wrap" gtk_label_get_line_wrap :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO CInt

-- | Returns whether lines in the label are automatically wrapped.
-- See 'GI.Gtk.Objects.Label.labelSetLineWrap'.
labelGetLineWrap ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the lines of the label are automatically wrapped.
labelGetLineWrap label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_line_wrap label'
    let result' = (/= 0) result
    touchManagedPtr label
    return result'

#if defined(ENABLE_OVERLOADING)
data LabelGetLineWrapMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetLineWrapMethodInfo a signature where
    overloadedMethod = labelGetLineWrap

instance O.OverloadedMethodInfo LabelGetLineWrapMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetLineWrap",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetLineWrap"
        })


#endif

-- method Label::get_line_wrap_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Pango" , name = "WrapMode" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_label_get_line_wrap_mode" gtk_label_get_line_wrap_mode :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO CUInt

-- | Returns line wrap mode used by the label. See 'GI.Gtk.Objects.Label.labelSetLineWrapMode'.
-- 
-- /Since: 2.10/
labelGetLineWrapMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Pango.Enums.WrapMode
    -- ^ __Returns:__ 'P.True' if the lines of the label are automatically wrapped.
labelGetLineWrapMode label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_line_wrap_mode label'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr label
    return result'

#if defined(ENABLE_OVERLOADING)
data LabelGetLineWrapModeMethodInfo
instance (signature ~ (m Pango.Enums.WrapMode), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetLineWrapModeMethodInfo a signature where
    overloadedMethod = labelGetLineWrapMode

instance O.OverloadedMethodInfo LabelGetLineWrapModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetLineWrapMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetLineWrapMode"
        })


#endif

-- method Label::get_lines
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_lines" gtk_label_get_lines :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO Int32

-- | Gets the number of lines to which an ellipsized, wrapping
-- label should be limited. See 'GI.Gtk.Objects.Label.labelSetLines'.
-- 
-- /Since: 3.10/
labelGetLines ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Int32
    -- ^ __Returns:__ The number of lines
labelGetLines label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_lines label'
    touchManagedPtr label
    return result

#if defined(ENABLE_OVERLOADING)
data LabelGetLinesMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetLinesMethodInfo a signature where
    overloadedMethod = labelGetLines

instance O.OverloadedMethodInfo LabelGetLinesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetLines",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetLines"
        })


#endif

-- method Label::get_max_width_chars
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_max_width_chars" gtk_label_get_max_width_chars :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO Int32

-- | Retrieves the desired maximum width of /@label@/, in characters. See
-- 'GI.Gtk.Objects.Label.labelSetWidthChars'.
-- 
-- /Since: 2.6/
labelGetMaxWidthChars ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Int32
    -- ^ __Returns:__ the maximum width of the label in characters.
labelGetMaxWidthChars label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_max_width_chars label'
    touchManagedPtr label
    return result

#if defined(ENABLE_OVERLOADING)
data LabelGetMaxWidthCharsMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetMaxWidthCharsMethodInfo a signature where
    overloadedMethod = labelGetMaxWidthChars

instance O.OverloadedMethodInfo LabelGetMaxWidthCharsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetMaxWidthChars",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetMaxWidthChars"
        })


#endif

-- method Label::get_mnemonic_keyval
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_mnemonic_keyval" gtk_label_get_mnemonic_keyval :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO Word32

-- | If the label has been set so that it has an mnemonic key this function
-- returns the keyval used for the mnemonic accelerator. If there is no
-- mnemonic set up it returns 'GI.Gdk.Constants.KEY_VoidSymbol'.
labelGetMnemonicKeyval ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Word32
    -- ^ __Returns:__ GDK keyval usable for accelerators, or 'GI.Gdk.Constants.KEY_VoidSymbol'
labelGetMnemonicKeyval label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_mnemonic_keyval label'
    touchManagedPtr label
    return result

#if defined(ENABLE_OVERLOADING)
data LabelGetMnemonicKeyvalMethodInfo
instance (signature ~ (m Word32), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetMnemonicKeyvalMethodInfo a signature where
    overloadedMethod = labelGetMnemonicKeyval

instance O.OverloadedMethodInfo LabelGetMnemonicKeyvalMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetMnemonicKeyval",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetMnemonicKeyval"
        })


#endif

-- method Label::get_mnemonic_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_mnemonic_widget" gtk_label_get_mnemonic_widget :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO (Ptr Gtk.Widget.Widget)

-- | Retrieves the target of the mnemonic (keyboard shortcut) of this
-- label. See 'GI.Gtk.Objects.Label.labelSetMnemonicWidget'.
labelGetMnemonicWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the target of the label’s mnemonic,
    --     or 'P.Nothing' if none has been set and the default algorithm will be used.
labelGetMnemonicWidget label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_mnemonic_widget label'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr label
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data LabelGetMnemonicWidgetMethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetMnemonicWidgetMethodInfo a signature where
    overloadedMethod = labelGetMnemonicWidget

instance O.OverloadedMethodInfo LabelGetMnemonicWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetMnemonicWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetMnemonicWidget"
        })


#endif

-- method Label::get_selectable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_selectable" gtk_label_get_selectable :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO CInt

-- | Gets the value set by 'GI.Gtk.Objects.Label.labelSetSelectable'.
labelGetSelectable ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the user can copy text from the label
labelGetSelectable label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_selectable label'
    let result' = (/= 0) result
    touchManagedPtr label
    return result'

#if defined(ENABLE_OVERLOADING)
data LabelGetSelectableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetSelectableMethodInfo a signature where
    overloadedMethod = labelGetSelectable

instance O.OverloadedMethodInfo LabelGetSelectableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetSelectable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetSelectable"
        })


#endif

-- method Label::get_selection_bounds
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "return location for start of selection, as a character offset"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "end"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for end of selection, as a character offset"
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
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_label_get_selection_bounds" gtk_label_get_selection_bounds :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    Ptr Int32 ->                            -- start : TBasicType TInt
    Ptr Int32 ->                            -- end : TBasicType TInt
    IO CInt

-- | Gets the selected range of characters in the label, returning 'P.True'
-- if there’s a selection.
labelGetSelectionBounds ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m ((Bool, Int32, Int32))
    -- ^ __Returns:__ 'P.True' if selection is non-empty
labelGetSelectionBounds label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    start <- allocMem :: IO (Ptr Int32)
    end <- allocMem :: IO (Ptr Int32)
    result <- gtk_label_get_selection_bounds label' start end
    let result' = (/= 0) result
    start' <- peek start
    end' <- peek end
    touchManagedPtr label
    freeMem start
    freeMem end
    return (result', start', end')

#if defined(ENABLE_OVERLOADING)
data LabelGetSelectionBoundsMethodInfo
instance (signature ~ (m ((Bool, Int32, Int32))), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetSelectionBoundsMethodInfo a signature where
    overloadedMethod = labelGetSelectionBounds

instance O.OverloadedMethodInfo LabelGetSelectionBoundsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetSelectionBounds",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetSelectionBounds"
        })


#endif

-- method Label::get_single_line_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_single_line_mode" gtk_label_get_single_line_mode :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO CInt

-- | Returns whether the label is in single line mode.
-- 
-- /Since: 2.6/
labelGetSingleLineMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Bool
    -- ^ __Returns:__ 'P.True' when the label is in single line mode.
labelGetSingleLineMode label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_single_line_mode label'
    let result' = (/= 0) result
    touchManagedPtr label
    return result'

#if defined(ENABLE_OVERLOADING)
data LabelGetSingleLineModeMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetSingleLineModeMethodInfo a signature where
    overloadedMethod = labelGetSingleLineMode

instance O.OverloadedMethodInfo LabelGetSingleLineModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetSingleLineMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetSingleLineMode"
        })


#endif

-- method Label::get_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_text" gtk_label_get_text :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO CString

-- | Fetches the text from a label widget, as displayed on the
-- screen. This does not include any embedded underlines
-- indicating mnemonics or Pango markup. (See 'GI.Gtk.Objects.Label.labelGetLabel')
labelGetText ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m T.Text
    -- ^ __Returns:__ the text in the label widget. This is the internal
    --   string used by the label, and must not be modified.
labelGetText label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_text label'
    checkUnexpectedReturnNULL "labelGetText" result
    result' <- cstringToText result
    touchManagedPtr label
    return result'

#if defined(ENABLE_OVERLOADING)
data LabelGetTextMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetTextMethodInfo a signature where
    overloadedMethod = labelGetText

instance O.OverloadedMethodInfo LabelGetTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetText"
        })


#endif

-- method Label::get_track_visited_links
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_track_visited_links" gtk_label_get_track_visited_links :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO CInt

-- | Returns whether the label is currently keeping track
-- of clicked links.
-- 
-- /Since: 2.18/
labelGetTrackVisitedLinks ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if clicked links are remembered
labelGetTrackVisitedLinks label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_track_visited_links label'
    let result' = (/= 0) result
    touchManagedPtr label
    return result'

#if defined(ENABLE_OVERLOADING)
data LabelGetTrackVisitedLinksMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetTrackVisitedLinksMethodInfo a signature where
    overloadedMethod = labelGetTrackVisitedLinks

instance O.OverloadedMethodInfo LabelGetTrackVisitedLinksMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetTrackVisitedLinks",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetTrackVisitedLinks"
        })


#endif

-- method Label::get_use_markup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_use_markup" gtk_label_get_use_markup :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO CInt

-- | Returns whether the label’s text is interpreted as marked up with
-- the [Pango text markup language][PangoMarkupFormat].
-- See gtk_label_set_use_markup ().
labelGetUseMarkup ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the label’s text will be parsed for markup.
labelGetUseMarkup label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_use_markup label'
    let result' = (/= 0) result
    touchManagedPtr label
    return result'

#if defined(ENABLE_OVERLOADING)
data LabelGetUseMarkupMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetUseMarkupMethodInfo a signature where
    overloadedMethod = labelGetUseMarkup

instance O.OverloadedMethodInfo LabelGetUseMarkupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetUseMarkup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetUseMarkup"
        })


#endif

-- method Label::get_use_underline
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_use_underline" gtk_label_get_use_underline :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO CInt

-- | Returns whether an embedded underline in the label indicates a
-- mnemonic. See 'GI.Gtk.Objects.Label.labelSetUseUnderline'.
labelGetUseUnderline ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Bool
    -- ^ __Returns:__ 'P.True' whether an embedded underline in the label indicates
    --               the mnemonic accelerator keys.
labelGetUseUnderline label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_use_underline label'
    let result' = (/= 0) result
    touchManagedPtr label
    return result'

#if defined(ENABLE_OVERLOADING)
data LabelGetUseUnderlineMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetUseUnderlineMethodInfo a signature where
    overloadedMethod = labelGetUseUnderline

instance O.OverloadedMethodInfo LabelGetUseUnderlineMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetUseUnderline",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetUseUnderline"
        })


#endif

-- method Label::get_width_chars
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_width_chars" gtk_label_get_width_chars :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO Int32

-- | Retrieves the desired width of /@label@/, in characters. See
-- 'GI.Gtk.Objects.Label.labelSetWidthChars'.
-- 
-- /Since: 2.6/
labelGetWidthChars ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Int32
    -- ^ __Returns:__ the width of the label in characters.
labelGetWidthChars label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_width_chars label'
    touchManagedPtr label
    return result

#if defined(ENABLE_OVERLOADING)
data LabelGetWidthCharsMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetWidthCharsMethodInfo a signature where
    overloadedMethod = labelGetWidthChars

instance O.OverloadedMethodInfo LabelGetWidthCharsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetWidthChars",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetWidthChars"
        })


#endif

-- method Label::get_xalign
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_xalign" gtk_label_get_xalign :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO CFloat

-- | Gets the [Label:xalign]("GI.Gtk.Objects.Label#g:attr:xalign") property for /@label@/.
-- 
-- /Since: 3.16/
labelGetXalign ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Float
    -- ^ __Returns:__ the xalign property
labelGetXalign label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_xalign label'
    let result' = realToFrac result
    touchManagedPtr label
    return result'

#if defined(ENABLE_OVERLOADING)
data LabelGetXalignMethodInfo
instance (signature ~ (m Float), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetXalignMethodInfo a signature where
    overloadedMethod = labelGetXalign

instance O.OverloadedMethodInfo LabelGetXalignMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetXalign",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetXalign"
        })


#endif

-- method Label::get_yalign
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_get_yalign" gtk_label_get_yalign :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    IO CFloat

-- | Gets the [Label:yalign]("GI.Gtk.Objects.Label#g:attr:yalign") property for /@label@/.
-- 
-- /Since: 3.16/
labelGetYalign ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> m Float
    -- ^ __Returns:__ the yalign property
labelGetYalign label = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    result <- gtk_label_get_yalign label'
    let result' = realToFrac result
    touchManagedPtr label
    return result'

#if defined(ENABLE_OVERLOADING)
data LabelGetYalignMethodInfo
instance (signature ~ (m Float), MonadIO m, IsLabel a) => O.OverloadedMethod LabelGetYalignMethodInfo a signature where
    overloadedMethod = labelGetYalign

instance O.OverloadedMethodInfo LabelGetYalignMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelGetYalign",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelGetYalign"
        })


#endif

-- method Label::select_region
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start_offset"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "start offset (in characters not bytes)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end_offset"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "end offset (in characters not bytes)"
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

foreign import ccall "gtk_label_select_region" gtk_label_select_region :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    Int32 ->                                -- start_offset : TBasicType TInt
    Int32 ->                                -- end_offset : TBasicType TInt
    IO ()

-- | Selects a range of characters in the label, if the label is selectable.
-- See 'GI.Gtk.Objects.Label.labelSetSelectable'. If the label is not selectable,
-- this function has no effect. If /@startOffset@/ or
-- /@endOffset@/ are -1, then the end of the label will be substituted.
labelSelectRegion ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Int32
    -- ^ /@startOffset@/: start offset (in characters not bytes)
    -> Int32
    -- ^ /@endOffset@/: end offset (in characters not bytes)
    -> m ()
labelSelectRegion label startOffset endOffset = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    gtk_label_select_region label' startOffset endOffset
    touchManagedPtr label
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSelectRegionMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSelectRegionMethodInfo a signature where
    overloadedMethod = labelSelectRegion

instance O.OverloadedMethodInfo LabelSelectRegionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSelectRegion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSelectRegion"
        })


#endif

-- method Label::set_angle
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "angle"
--           , argType = TBasicType TDouble
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the angle that the baseline of the label makes with\n  the horizontal, in degrees, measured counterclockwise"
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

foreign import ccall "gtk_label_set_angle" gtk_label_set_angle :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CDouble ->                              -- angle : TBasicType TDouble
    IO ()

-- | Sets the angle of rotation for the label. An angle of 90 reads from
-- from bottom to top, an angle of 270, from top to bottom. The angle
-- setting for the label is ignored if the label is selectable,
-- wrapped, or ellipsized.
-- 
-- /Since: 2.6/
labelSetAngle ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Double
    -- ^ /@angle@/: the angle that the baseline of the label makes with
    --   the horizontal, in degrees, measured counterclockwise
    -> m ()
labelSetAngle label angle = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    let angle' = realToFrac angle
    gtk_label_set_angle label' angle'
    touchManagedPtr label
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetAngleMethodInfo
instance (signature ~ (Double -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetAngleMethodInfo a signature where
    overloadedMethod = labelSetAngle

instance O.OverloadedMethodInfo LabelSetAngleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetAngle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetAngle"
        })


#endif

-- method Label::set_attributes
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #PangoAttrList, or %NULL"
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

foreign import ccall "gtk_label_set_attributes" gtk_label_set_attributes :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    Ptr Pango.AttrList.AttrList ->          -- attrs : TInterface (Name {namespace = "Pango", name = "AttrList"})
    IO ()

-- | Sets a t'GI.Pango.Structs.AttrList.AttrList'; the attributes in the list are applied to the
-- label text.
-- 
-- The attributes set with this function will be applied
-- and merged with any other attributes previously effected by way
-- of the [Label:useUnderline]("GI.Gtk.Objects.Label#g:attr:useUnderline") or [Label:useMarkup]("GI.Gtk.Objects.Label#g:attr:useMarkup") properties.
-- While it is not recommended to mix markup strings with manually set
-- attributes, if you must; know that the attributes will be applied
-- to the label after the markup string is parsed.
labelSetAttributes ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Maybe (Pango.AttrList.AttrList)
    -- ^ /@attrs@/: a t'GI.Pango.Structs.AttrList.AttrList', or 'P.Nothing'
    -> m ()
labelSetAttributes label attrs = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    maybeAttrs <- case attrs of
        Nothing -> return nullPtr
        Just jAttrs -> do
            jAttrs' <- unsafeManagedPtrGetPtr jAttrs
            return jAttrs'
    gtk_label_set_attributes label' maybeAttrs
    touchManagedPtr label
    whenJust attrs touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetAttributesMethodInfo
instance (signature ~ (Maybe (Pango.AttrList.AttrList) -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetAttributesMethodInfo a signature where
    overloadedMethod = labelSetAttributes

instance O.OverloadedMethodInfo LabelSetAttributesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetAttributes",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetAttributes"
        })


#endif

-- method Label::set_ellipsize
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "mode"
--           , argType =
--               TInterface Name { namespace = "Pango" , name = "EllipsizeMode" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #PangoEllipsizeMode"
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

foreign import ccall "gtk_label_set_ellipsize" gtk_label_set_ellipsize :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CUInt ->                                -- mode : TInterface (Name {namespace = "Pango", name = "EllipsizeMode"})
    IO ()

-- | Sets the mode used to ellipsize (add an ellipsis: \"...\") to the text
-- if there is not enough space to render the entire string.
-- 
-- /Since: 2.6/
labelSetEllipsize ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Pango.Enums.EllipsizeMode
    -- ^ /@mode@/: a t'GI.Pango.Enums.EllipsizeMode'
    -> m ()
labelSetEllipsize label mode = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    let mode' = (fromIntegral . fromEnum) mode
    gtk_label_set_ellipsize label' mode'
    touchManagedPtr label
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetEllipsizeMethodInfo
instance (signature ~ (Pango.Enums.EllipsizeMode -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetEllipsizeMethodInfo a signature where
    overloadedMethod = labelSetEllipsize

instance O.OverloadedMethodInfo LabelSetEllipsizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetEllipsize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetEllipsize"
        })


#endif

-- method Label::set_justify
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "jtype"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Justification" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkJustification"
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

foreign import ccall "gtk_label_set_justify" gtk_label_set_justify :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CUInt ->                                -- jtype : TInterface (Name {namespace = "Gtk", name = "Justification"})
    IO ()

-- | Sets the alignment of the lines in the text of the label relative to
-- each other. 'GI.Gtk.Enums.JustificationLeft' is the default value when the widget is
-- first created with 'GI.Gtk.Objects.Label.labelNew'. If you instead want to set the
-- alignment of the label as a whole, use 'GI.Gtk.Objects.Widget.widgetSetHalign' instead.
-- 'GI.Gtk.Objects.Label.labelSetJustify' has no effect on labels containing only a
-- single line.
labelSetJustify ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Gtk.Enums.Justification
    -- ^ /@jtype@/: a t'GI.Gtk.Enums.Justification'
    -> m ()
labelSetJustify label jtype = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    let jtype' = (fromIntegral . fromEnum) jtype
    gtk_label_set_justify label' jtype'
    touchManagedPtr label
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetJustifyMethodInfo
instance (signature ~ (Gtk.Enums.Justification -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetJustifyMethodInfo a signature where
    overloadedMethod = labelSetJustify

instance O.OverloadedMethodInfo LabelSetJustifyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetJustify",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetJustify"
        })


#endif

-- method Label::set_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "str"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new text to set for the label"
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

foreign import ccall "gtk_label_set_label" gtk_label_set_label :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CString ->                              -- str : TBasicType TUTF8
    IO ()

-- | Sets the text of the label. The label is interpreted as
-- including embedded underlines and\/or Pango markup depending
-- on the values of the [Label:useUnderline]("GI.Gtk.Objects.Label#g:attr:useUnderline") and
-- [Label:useMarkup]("GI.Gtk.Objects.Label#g:attr:useMarkup") properties.
labelSetLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> T.Text
    -- ^ /@str@/: the new text to set for the label
    -> m ()
labelSetLabel label str = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    str' <- textToCString str
    gtk_label_set_label label' str'
    touchManagedPtr label
    freeMem str'
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetLabelMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetLabelMethodInfo a signature where
    overloadedMethod = labelSetLabel

instance O.OverloadedMethodInfo LabelSetLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetLabel"
        })


#endif

-- method Label::set_line_wrap
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "wrap"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the setting" , sinceVersion = Nothing }
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

foreign import ccall "gtk_label_set_line_wrap" gtk_label_set_line_wrap :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CInt ->                                 -- wrap : TBasicType TBoolean
    IO ()

-- | Toggles line wrapping within the t'GI.Gtk.Objects.Label.Label' widget. 'P.True' makes it break
-- lines if text exceeds the widget’s size. 'P.False' lets the text get cut off
-- by the edge of the widget if it exceeds the widget size.
-- 
-- Note that setting line wrapping to 'P.True' does not make the label
-- wrap at its parent container’s width, because GTK+ widgets
-- conceptually can’t make their requisition depend on the parent
-- container’s size. For a label that wraps at a specific position,
-- set the label’s width using 'GI.Gtk.Objects.Widget.widgetSetSizeRequest'.
labelSetLineWrap ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Bool
    -- ^ /@wrap@/: the setting
    -> m ()
labelSetLineWrap label wrap = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    let wrap' = (fromIntegral . fromEnum) wrap
    gtk_label_set_line_wrap label' wrap'
    touchManagedPtr label
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetLineWrapMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetLineWrapMethodInfo a signature where
    overloadedMethod = labelSetLineWrap

instance O.OverloadedMethodInfo LabelSetLineWrapMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetLineWrap",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetLineWrap"
        })


#endif

-- method Label::set_line_wrap_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "wrap_mode"
--           , argType =
--               TInterface Name { namespace = "Pango" , name = "WrapMode" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the line wrapping mode"
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

foreign import ccall "gtk_label_set_line_wrap_mode" gtk_label_set_line_wrap_mode :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CUInt ->                                -- wrap_mode : TInterface (Name {namespace = "Pango", name = "WrapMode"})
    IO ()

-- | If line wrapping is on (see 'GI.Gtk.Objects.Label.labelSetLineWrap') this controls how
-- the line wrapping is done. The default is 'GI.Pango.Enums.WrapModeWord' which means
-- wrap on word boundaries.
-- 
-- /Since: 2.10/
labelSetLineWrapMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Pango.Enums.WrapMode
    -- ^ /@wrapMode@/: the line wrapping mode
    -> m ()
labelSetLineWrapMode label wrapMode = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    let wrapMode' = (fromIntegral . fromEnum) wrapMode
    gtk_label_set_line_wrap_mode label' wrapMode'
    touchManagedPtr label
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetLineWrapModeMethodInfo
instance (signature ~ (Pango.Enums.WrapMode -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetLineWrapModeMethodInfo a signature where
    overloadedMethod = labelSetLineWrapMode

instance O.OverloadedMethodInfo LabelSetLineWrapModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetLineWrapMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetLineWrapMode"
        })


#endif

-- method Label::set_lines
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "lines"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the desired number of lines, or -1"
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

foreign import ccall "gtk_label_set_lines" gtk_label_set_lines :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    Int32 ->                                -- lines : TBasicType TInt
    IO ()

-- | Sets the number of lines to which an ellipsized, wrapping label
-- should be limited. This has no effect if the label is not wrapping
-- or ellipsized. Set this to -1 if you don’t want to limit the
-- number of lines.
-- 
-- /Since: 3.10/
labelSetLines ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Int32
    -- ^ /@lines@/: the desired number of lines, or -1
    -> m ()
labelSetLines label lines = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    gtk_label_set_lines label' lines
    touchManagedPtr label
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetLinesMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetLinesMethodInfo a signature where
    overloadedMethod = labelSetLines

instance O.OverloadedMethodInfo LabelSetLinesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetLines",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetLines"
        })


#endif

-- method Label::set_markup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "str"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a markup string (see [Pango markup format][PangoMarkupFormat])"
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

foreign import ccall "gtk_label_set_markup" gtk_label_set_markup :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CString ->                              -- str : TBasicType TUTF8
    IO ()

-- | Parses /@str@/ which is marked up with the
-- [Pango text markup language][PangoMarkupFormat], setting the
-- label’s text and attribute list based on the parse results.
-- 
-- If the /@str@/ is external data, you may need to escape it with
-- 'GI.GLib.Functions.markupEscapeText' or @/g_markup_printf_escaped()/@:
-- 
-- 
-- === /C code/
-- >
-- >GtkWidget *label = gtk_label_new (NULL);
-- >const char *str = "some text";
-- >const char *format = "<span style=\"italic\">\%s</span>";
-- >char *markup;
-- >
-- >markup = g_markup_printf_escaped (format, str);
-- >gtk_label_set_markup (GTK_LABEL (label), markup);
-- >g_free (markup);
-- 
-- 
-- This function will set the [Label:useMarkup]("GI.Gtk.Objects.Label#g:attr:useMarkup") property to 'P.True' as
-- a side effect.
-- 
-- If you set the label contents using the [Label:label]("GI.Gtk.Objects.Label#g:attr:label") property you
-- should also ensure that you set the [Label:useMarkup]("GI.Gtk.Objects.Label#g:attr:useMarkup") property
-- accordingly.
-- 
-- See also: 'GI.Gtk.Objects.Label.labelSetText'
labelSetMarkup ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> T.Text
    -- ^ /@str@/: a markup string (see [Pango markup format][PangoMarkupFormat])
    -> m ()
labelSetMarkup label str = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    str' <- textToCString str
    gtk_label_set_markup label' str'
    touchManagedPtr label
    freeMem str'
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetMarkupMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetMarkupMethodInfo a signature where
    overloadedMethod = labelSetMarkup

instance O.OverloadedMethodInfo LabelSetMarkupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetMarkup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetMarkup"
        })


#endif

-- method Label::set_markup_with_mnemonic
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "str"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a markup string (see\n    [Pango markup format][PangoMarkupFormat])"
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

foreign import ccall "gtk_label_set_markup_with_mnemonic" gtk_label_set_markup_with_mnemonic :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CString ->                              -- str : TBasicType TUTF8
    IO ()

-- | Parses /@str@/ which is marked up with the
-- [Pango text markup language][PangoMarkupFormat],
-- setting the label’s text and attribute list based on the parse results.
-- If characters in /@str@/ are preceded by an underscore, they are underlined
-- indicating that they represent a keyboard accelerator called a mnemonic.
-- 
-- The mnemonic key can be used to activate another widget, chosen
-- automatically, or explicitly using 'GI.Gtk.Objects.Label.labelSetMnemonicWidget'.
labelSetMarkupWithMnemonic ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> T.Text
    -- ^ /@str@/: a markup string (see
    --     [Pango markup format][PangoMarkupFormat])
    -> m ()
labelSetMarkupWithMnemonic label str = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    str' <- textToCString str
    gtk_label_set_markup_with_mnemonic label' str'
    touchManagedPtr label
    freeMem str'
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetMarkupWithMnemonicMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetMarkupWithMnemonicMethodInfo a signature where
    overloadedMethod = labelSetMarkupWithMnemonic

instance O.OverloadedMethodInfo LabelSetMarkupWithMnemonicMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetMarkupWithMnemonic",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetMarkupWithMnemonic"
        })


#endif

-- method Label::set_max_width_chars
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the new desired maximum width, in characters."
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

foreign import ccall "gtk_label_set_max_width_chars" gtk_label_set_max_width_chars :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    Int32 ->                                -- n_chars : TBasicType TInt
    IO ()

-- | Sets the desired maximum width in characters of /@label@/ to /@nChars@/.
-- 
-- /Since: 2.6/
labelSetMaxWidthChars ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Int32
    -- ^ /@nChars@/: the new desired maximum width, in characters.
    -> m ()
labelSetMaxWidthChars label nChars = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    gtk_label_set_max_width_chars label' nChars
    touchManagedPtr label
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetMaxWidthCharsMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetMaxWidthCharsMethodInfo a signature where
    overloadedMethod = labelSetMaxWidthChars

instance O.OverloadedMethodInfo LabelSetMaxWidthCharsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetMaxWidthChars",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetMaxWidthChars"
        })


#endif

-- method Label::set_mnemonic_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the target #GtkWidget, or %NULL to unset"
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

foreign import ccall "gtk_label_set_mnemonic_widget" gtk_label_set_mnemonic_widget :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | If the label has been set so that it has an mnemonic key (using
-- i.e. 'GI.Gtk.Objects.Label.labelSetMarkupWithMnemonic',
-- 'GI.Gtk.Objects.Label.labelSetTextWithMnemonic', 'GI.Gtk.Objects.Label.labelNewWithMnemonic'
-- or the “use_underline” property) the label can be associated with a
-- widget that is the target of the mnemonic. When the label is inside
-- a widget (like a t'GI.Gtk.Objects.Button.Button' or a t'GI.Gtk.Objects.Notebook.Notebook' tab) it is
-- automatically associated with the correct widget, but sometimes
-- (i.e. when the target is a t'GI.Gtk.Objects.Entry.Entry' next to the label) you need to
-- set it explicitly using this function.
-- 
-- The target widget will be accelerated by emitting the
-- GtkWidget[mnemonicActivate](#g:signal:mnemonicActivate) signal on it. The default handler for
-- this signal will activate the widget if there are no mnemonic collisions
-- and toggle focus between the colliding widgets otherwise.
labelSetMnemonicWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Maybe (b)
    -- ^ /@widget@/: the target t'GI.Gtk.Objects.Widget.Widget', or 'P.Nothing' to unset
    -> m ()
labelSetMnemonicWidget label widget = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    gtk_label_set_mnemonic_widget label' maybeWidget
    touchManagedPtr label
    whenJust widget touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetMnemonicWidgetMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsLabel a, Gtk.Widget.IsWidget b) => O.OverloadedMethod LabelSetMnemonicWidgetMethodInfo a signature where
    overloadedMethod = labelSetMnemonicWidget

instance O.OverloadedMethodInfo LabelSetMnemonicWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetMnemonicWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetMnemonicWidget"
        })


#endif

-- method Label::set_pattern
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkLabel you want to set the pattern to."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pattern"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The pattern as described above."
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

foreign import ccall "gtk_label_set_pattern" gtk_label_set_pattern :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CString ->                              -- pattern : TBasicType TUTF8
    IO ()

-- | The pattern of underlines you want under the existing text within the
-- t'GI.Gtk.Objects.Label.Label' widget.  For example if the current text of the label says
-- “FooBarBaz” passing a pattern of “___   ___” will underline
-- “Foo” and “Baz” but not “Bar”.
labelSetPattern ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: The t'GI.Gtk.Objects.Label.Label' you want to set the pattern to.
    -> T.Text
    -- ^ /@pattern@/: The pattern as described above.
    -> m ()
labelSetPattern label pattern = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    pattern' <- textToCString pattern
    gtk_label_set_pattern label' pattern'
    touchManagedPtr label
    freeMem pattern'
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetPatternMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetPatternMethodInfo a signature where
    overloadedMethod = labelSetPattern

instance O.OverloadedMethodInfo LabelSetPatternMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetPattern",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetPattern"
        })


#endif

-- method Label::set_selectable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "%TRUE to allow selecting text in the label"
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

foreign import ccall "gtk_label_set_selectable" gtk_label_set_selectable :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Selectable labels allow the user to select text from the label, for
-- copy-and-paste.
labelSetSelectable ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Bool
    -- ^ /@setting@/: 'P.True' to allow selecting text in the label
    -> m ()
labelSetSelectable label setting = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    let setting' = (fromIntegral . fromEnum) setting
    gtk_label_set_selectable label' setting'
    touchManagedPtr label
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetSelectableMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetSelectableMethodInfo a signature where
    overloadedMethod = labelSetSelectable

instance O.OverloadedMethodInfo LabelSetSelectableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetSelectable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetSelectable"
        })


#endif

-- method Label::set_single_line_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "single_line_mode"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if the label should be in single line mode"
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

foreign import ccall "gtk_label_set_single_line_mode" gtk_label_set_single_line_mode :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CInt ->                                 -- single_line_mode : TBasicType TBoolean
    IO ()

-- | Sets whether the label is in single line mode.
-- 
-- /Since: 2.6/
labelSetSingleLineMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Bool
    -- ^ /@singleLineMode@/: 'P.True' if the label should be in single line mode
    -> m ()
labelSetSingleLineMode label singleLineMode = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    let singleLineMode' = (fromIntegral . fromEnum) singleLineMode
    gtk_label_set_single_line_mode label' singleLineMode'
    touchManagedPtr label
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetSingleLineModeMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetSingleLineModeMethodInfo a signature where
    overloadedMethod = labelSetSingleLineMode

instance O.OverloadedMethodInfo LabelSetSingleLineModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetSingleLineMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetSingleLineMode"
        })


#endif

-- method Label::set_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "str"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The text you want to set"
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

foreign import ccall "gtk_label_set_text" gtk_label_set_text :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CString ->                              -- str : TBasicType TUTF8
    IO ()

-- | Sets the text within the t'GI.Gtk.Objects.Label.Label' widget. It overwrites any text that
-- was there before.
-- 
-- This function will clear any previously set mnemonic accelerators, and
-- set the [Label:useUnderline]("GI.Gtk.Objects.Label#g:attr:useUnderline") property to 'P.False' as a side effect.
-- 
-- This function will set the [Label:useMarkup]("GI.Gtk.Objects.Label#g:attr:useMarkup") property to 'P.False'
-- as a side effect.
-- 
-- See also: 'GI.Gtk.Objects.Label.labelSetMarkup'
labelSetText ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> T.Text
    -- ^ /@str@/: The text you want to set
    -> m ()
labelSetText label str = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    str' <- textToCString str
    gtk_label_set_text label' str'
    touchManagedPtr label
    freeMem str'
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetTextMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetTextMethodInfo a signature where
    overloadedMethod = labelSetText

instance O.OverloadedMethodInfo LabelSetTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetText"
        })


#endif

-- method Label::set_text_with_mnemonic
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "str"
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

foreign import ccall "gtk_label_set_text_with_mnemonic" gtk_label_set_text_with_mnemonic :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CString ->                              -- str : TBasicType TUTF8
    IO ()

-- | Sets the label’s text from the string /@str@/.
-- If characters in /@str@/ are preceded by an underscore, they are underlined
-- indicating that they represent a keyboard accelerator called a mnemonic.
-- The mnemonic key can be used to activate another widget, chosen
-- automatically, or explicitly using 'GI.Gtk.Objects.Label.labelSetMnemonicWidget'.
labelSetTextWithMnemonic ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> T.Text
    -- ^ /@str@/: a string
    -> m ()
labelSetTextWithMnemonic label str = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    str' <- textToCString str
    gtk_label_set_text_with_mnemonic label' str'
    touchManagedPtr label
    freeMem str'
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetTextWithMnemonicMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetTextWithMnemonicMethodInfo a signature where
    overloadedMethod = labelSetTextWithMnemonic

instance O.OverloadedMethodInfo LabelSetTextWithMnemonicMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetTextWithMnemonic",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetTextWithMnemonic"
        })


#endif

-- method Label::set_track_visited_links
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "track_links"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to track visited links"
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

foreign import ccall "gtk_label_set_track_visited_links" gtk_label_set_track_visited_links :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CInt ->                                 -- track_links : TBasicType TBoolean
    IO ()

-- | Sets whether the label should keep track of clicked
-- links (and use a different color for them).
-- 
-- /Since: 2.18/
labelSetTrackVisitedLinks ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Bool
    -- ^ /@trackLinks@/: 'P.True' to track visited links
    -> m ()
labelSetTrackVisitedLinks label trackLinks = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    let trackLinks' = (fromIntegral . fromEnum) trackLinks
    gtk_label_set_track_visited_links label' trackLinks'
    touchManagedPtr label
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetTrackVisitedLinksMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetTrackVisitedLinksMethodInfo a signature where
    overloadedMethod = labelSetTrackVisitedLinks

instance O.OverloadedMethodInfo LabelSetTrackVisitedLinksMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetTrackVisitedLinks",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetTrackVisitedLinks"
        })


#endif

-- method Label::set_use_markup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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
--                     Just "%TRUE if the label\8217s text should be parsed for markup."
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

foreign import ccall "gtk_label_set_use_markup" gtk_label_set_use_markup :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | Sets whether the text of the label contains markup in
-- [Pango’s text markup language][PangoMarkupFormat].
-- See 'GI.Gtk.Objects.Label.labelSetMarkup'.
labelSetUseMarkup ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Bool
    -- ^ /@setting@/: 'P.True' if the label’s text should be parsed for markup.
    -> m ()
labelSetUseMarkup label setting = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    let setting' = (fromIntegral . fromEnum) setting
    gtk_label_set_use_markup label' setting'
    touchManagedPtr label
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetUseMarkupMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetUseMarkupMethodInfo a signature where
    overloadedMethod = labelSetUseMarkup

instance O.OverloadedMethodInfo LabelSetUseMarkupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetUseMarkup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetUseMarkup"
        })


#endif

-- method Label::set_use_underline
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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
--                     Just "%TRUE if underlines in the text indicate mnemonics"
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

foreign import ccall "gtk_label_set_use_underline" gtk_label_set_use_underline :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CInt ->                                 -- setting : TBasicType TBoolean
    IO ()

-- | If true, an underline in the text indicates the next character should be
-- used for the mnemonic accelerator key.
labelSetUseUnderline ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Bool
    -- ^ /@setting@/: 'P.True' if underlines in the text indicate mnemonics
    -> m ()
labelSetUseUnderline label setting = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    let setting' = (fromIntegral . fromEnum) setting
    gtk_label_set_use_underline label' setting'
    touchManagedPtr label
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetUseUnderlineMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetUseUnderlineMethodInfo a signature where
    overloadedMethod = labelSetUseUnderline

instance O.OverloadedMethodInfo LabelSetUseUnderlineMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetUseUnderline",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetUseUnderline"
        })


#endif

-- method Label::set_width_chars
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the new desired width, in characters."
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

foreign import ccall "gtk_label_set_width_chars" gtk_label_set_width_chars :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    Int32 ->                                -- n_chars : TBasicType TInt
    IO ()

-- | Sets the desired width in characters of /@label@/ to /@nChars@/.
-- 
-- /Since: 2.6/
labelSetWidthChars ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Int32
    -- ^ /@nChars@/: the new desired width, in characters.
    -> m ()
labelSetWidthChars label nChars = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    gtk_label_set_width_chars label' nChars
    touchManagedPtr label
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetWidthCharsMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetWidthCharsMethodInfo a signature where
    overloadedMethod = labelSetWidthChars

instance O.OverloadedMethodInfo LabelSetWidthCharsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetWidthChars",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetWidthChars"
        })


#endif

-- method Label::set_xalign
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the new xalign value, between 0 and 1"
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

foreign import ccall "gtk_label_set_xalign" gtk_label_set_xalign :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CFloat ->                               -- xalign : TBasicType TFloat
    IO ()

-- | Sets the [Label:xalign]("GI.Gtk.Objects.Label#g:attr:xalign") property for /@label@/.
-- 
-- /Since: 3.16/
labelSetXalign ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Float
    -- ^ /@xalign@/: the new xalign value, between 0 and 1
    -> m ()
labelSetXalign label xalign = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    let xalign' = realToFrac xalign
    gtk_label_set_xalign label' xalign'
    touchManagedPtr label
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetXalignMethodInfo
instance (signature ~ (Float -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetXalignMethodInfo a signature where
    overloadedMethod = labelSetXalign

instance O.OverloadedMethodInfo LabelSetXalignMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetXalign",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetXalign"
        })


#endif

-- method Label::set_yalign
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "label"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Label" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkLabel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "yalign"
--           , argType = TBasicType TFloat
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the new yalign value, between 0 and 1"
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

foreign import ccall "gtk_label_set_yalign" gtk_label_set_yalign :: 
    Ptr Label ->                            -- label : TInterface (Name {namespace = "Gtk", name = "Label"})
    CFloat ->                               -- yalign : TBasicType TFloat
    IO ()

-- | Sets the [Label:yalign]("GI.Gtk.Objects.Label#g:attr:yalign") property for /@label@/.
-- 
-- /Since: 3.16/
labelSetYalign ::
    (B.CallStack.HasCallStack, MonadIO m, IsLabel a) =>
    a
    -- ^ /@label@/: a t'GI.Gtk.Objects.Label.Label'
    -> Float
    -- ^ /@yalign@/: the new yalign value, between 0 and 1
    -> m ()
labelSetYalign label yalign = liftIO $ do
    label' <- unsafeManagedPtrCastPtr label
    let yalign' = realToFrac yalign
    gtk_label_set_yalign label' yalign'
    touchManagedPtr label
    return ()

#if defined(ENABLE_OVERLOADING)
data LabelSetYalignMethodInfo
instance (signature ~ (Float -> m ()), MonadIO m, IsLabel a) => O.OverloadedMethod LabelSetYalignMethodInfo a signature where
    overloadedMethod = labelSetYalign

instance O.OverloadedMethodInfo LabelSetYalignMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Label.labelSetYalign",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Label.html#v:labelSetYalign"
        })


#endif


