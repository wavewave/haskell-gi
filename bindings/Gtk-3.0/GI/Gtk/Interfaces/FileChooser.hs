{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Interfaces.FileChooser.FileChooser' is an interface that can be implemented by file
-- selection widgets.  In GTK+, the main objects that implement this
-- interface are t'GI.Gtk.Objects.FileChooserWidget.FileChooserWidget', t'GI.Gtk.Objects.FileChooserDialog.FileChooserDialog', and
-- t'GI.Gtk.Objects.FileChooserButton.FileChooserButton'.  You do not need to write an object that
-- implements the t'GI.Gtk.Interfaces.FileChooser.FileChooser' interface unless you are trying to
-- adapt an existing file selector to expose a standard programming
-- interface.
-- 
-- t'GI.Gtk.Interfaces.FileChooser.FileChooser' allows for shortcuts to various places in the filesystem.
-- In the default implementation these are displayed in the left pane. It
-- may be a bit confusing at first that these shortcuts come from various
-- sources and in various flavours, so lets explain the terminology here:
-- 
-- * Bookmarks: are created by the user, by dragging folders from the
-- right pane to the left pane, or by using the “Add”. Bookmarks
-- can be renamed and deleted by the user.
-- * Shortcuts: can be provided by the application. For example, a Paint
-- program may want to add a shortcut for a Clipart folder. Shortcuts
-- cannot be modified by the user.
-- * Volumes: are provided by the underlying filesystem abstraction. They are
-- the “roots” of the filesystem.
-- 
-- 
-- = File Names and Encodings
-- 
-- When the user is finished selecting files in a
-- t'GI.Gtk.Interfaces.FileChooser.FileChooser', your program can get the selected names
-- either as filenames or as URIs.  For URIs, the normal escaping
-- rules are applied if the URI contains non-ASCII characters.
-- However, filenames are always returned in
-- the character set specified by the
-- @G_FILENAME_ENCODING@ environment variable.
-- Please see the GLib documentation for more details about this
-- variable.
-- 
-- This means that while you can pass the result of
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserGetFilename' to @/g_open()/@ or @/g_fopen()/@,
-- you may not be able to directly set it as the text of a
-- t'GI.Gtk.Objects.Label.Label' widget unless you convert it first to UTF-8,
-- which all GTK+ widgets expect. You should use 'GI.GLib.Functions.filenameToUtf8'
-- to convert filenames into strings that can be passed to GTK+
-- widgets.
-- 
-- = Adding a Preview Widget
-- 
-- You can add a custom preview widget to a file chooser and then
-- get notification about when the preview needs to be updated.
-- To install a preview widget, use
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidget'.  Then, connect to the
-- [FileChooser::updatePreview]("GI.Gtk.Interfaces.FileChooser#g:signal:updatePreview") signal to get notified when
-- you need to update the contents of the preview.
-- 
-- Your callback should use
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserGetPreviewFilename' to see what needs
-- previewing.  Once you have generated the preview for the
-- corresponding file, you must call
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidgetActive' with a boolean
-- flag that indicates whether your callback could successfully
-- generate a preview.
-- 
-- ## Example: Using a Preview Widget ## {@/gtkfilechooser/@-preview}
-- 
-- === /C code/
-- >
-- >{
-- >  GtkImage *preview;
-- >
-- >  ...
-- >
-- >  preview = gtk_image_new ();
-- >
-- >  gtk_file_chooser_set_preview_widget (my_file_chooser, preview);
-- >  g_signal_connect (my_file_chooser, "update-preview",
-- >		    G_CALLBACK (update_preview_cb), preview);
-- >}
-- >
-- >static void
-- >update_preview_cb (GtkFileChooser *file_chooser, gpointer data)
-- >{
-- >  GtkWidget *preview;
-- >  char *filename;
-- >  GdkPixbuf *pixbuf;
-- >  gboolean have_preview;
-- >
-- >  preview = GTK_WIDGET (data);
-- >  filename = gtk_file_chooser_get_preview_filename (file_chooser);
-- >
-- >  pixbuf = gdk_pixbuf_new_from_file_at_size (filename, 128, 128, NULL);
-- >  have_preview = (pixbuf != NULL);
-- >  g_free (filename);
-- >
-- >  gtk_image_set_from_pixbuf (GTK_IMAGE (preview), pixbuf);
-- >  if (pixbuf)
-- >    g_object_unref (pixbuf);
-- >
-- >  gtk_file_chooser_set_preview_widget_active (file_chooser, have_preview);
-- >}
-- 
-- 
-- = Adding Extra Widgets
-- 
-- You can add extra widgets to a file chooser to provide options
-- that are not present in the default design.  For example, you
-- can add a toggle button to give the user the option to open a
-- file in read-only mode.  You can use
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetExtraWidget' to insert additional
-- widgets in a file chooser.
-- 
-- An example for adding extra widgets:
-- 
-- === /C code/
-- >
-- >
-- >  GtkWidget *toggle;
-- >
-- >  ...
-- >
-- >  toggle = gtk_check_button_new_with_label ("Open file read-only");
-- >  gtk_widget_show (toggle);
-- >  gtk_file_chooser_set_extra_widget (my_file_chooser, toggle);
-- >}
-- 
-- 
-- If you want to set more than one extra widget in the file
-- chooser, you can a container such as a t'GI.Gtk.Objects.Box.Box' or a t'GI.Gtk.Objects.Grid.Grid'
-- and include your widgets in it.  Then, set the container as
-- the whole extra widget.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Interfaces.FileChooser
    ( 

-- * Exported types
    FileChooser(..)                         ,
    IsFileChooser                           ,
    toFileChooser                           ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addChoice]("GI.Gtk.Interfaces.FileChooser#g:method:addChoice"), [addFilter]("GI.Gtk.Interfaces.FileChooser#g:method:addFilter"), [addShortcutFolder]("GI.Gtk.Interfaces.FileChooser#g:method:addShortcutFolder"), [addShortcutFolderUri]("GI.Gtk.Interfaces.FileChooser#g:method:addShortcutFolderUri"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [listFilters]("GI.Gtk.Interfaces.FileChooser#g:method:listFilters"), [listShortcutFolderUris]("GI.Gtk.Interfaces.FileChooser#g:method:listShortcutFolderUris"), [listShortcutFolders]("GI.Gtk.Interfaces.FileChooser#g:method:listShortcutFolders"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [removeChoice]("GI.Gtk.Interfaces.FileChooser#g:method:removeChoice"), [removeFilter]("GI.Gtk.Interfaces.FileChooser#g:method:removeFilter"), [removeShortcutFolder]("GI.Gtk.Interfaces.FileChooser#g:method:removeShortcutFolder"), [removeShortcutFolderUri]("GI.Gtk.Interfaces.FileChooser#g:method:removeShortcutFolderUri"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [selectAll]("GI.Gtk.Interfaces.FileChooser#g:method:selectAll"), [selectFile]("GI.Gtk.Interfaces.FileChooser#g:method:selectFile"), [selectFilename]("GI.Gtk.Interfaces.FileChooser#g:method:selectFilename"), [selectUri]("GI.Gtk.Interfaces.FileChooser#g:method:selectUri"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unselectAll]("GI.Gtk.Interfaces.FileChooser#g:method:unselectAll"), [unselectFile]("GI.Gtk.Interfaces.FileChooser#g:method:unselectFile"), [unselectFilename]("GI.Gtk.Interfaces.FileChooser#g:method:unselectFilename"), [unselectUri]("GI.Gtk.Interfaces.FileChooser#g:method:unselectUri"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAction]("GI.Gtk.Interfaces.FileChooser#g:method:getAction"), [getChoice]("GI.Gtk.Interfaces.FileChooser#g:method:getChoice"), [getCreateFolders]("GI.Gtk.Interfaces.FileChooser#g:method:getCreateFolders"), [getCurrentFolder]("GI.Gtk.Interfaces.FileChooser#g:method:getCurrentFolder"), [getCurrentFolderFile]("GI.Gtk.Interfaces.FileChooser#g:method:getCurrentFolderFile"), [getCurrentFolderUri]("GI.Gtk.Interfaces.FileChooser#g:method:getCurrentFolderUri"), [getCurrentName]("GI.Gtk.Interfaces.FileChooser#g:method:getCurrentName"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDoOverwriteConfirmation]("GI.Gtk.Interfaces.FileChooser#g:method:getDoOverwriteConfirmation"), [getExtraWidget]("GI.Gtk.Interfaces.FileChooser#g:method:getExtraWidget"), [getFile]("GI.Gtk.Interfaces.FileChooser#g:method:getFile"), [getFilename]("GI.Gtk.Interfaces.FileChooser#g:method:getFilename"), [getFilenames]("GI.Gtk.Interfaces.FileChooser#g:method:getFilenames"), [getFiles]("GI.Gtk.Interfaces.FileChooser#g:method:getFiles"), [getFilter]("GI.Gtk.Interfaces.FileChooser#g:method:getFilter"), [getLocalOnly]("GI.Gtk.Interfaces.FileChooser#g:method:getLocalOnly"), [getPreviewFile]("GI.Gtk.Interfaces.FileChooser#g:method:getPreviewFile"), [getPreviewFilename]("GI.Gtk.Interfaces.FileChooser#g:method:getPreviewFilename"), [getPreviewUri]("GI.Gtk.Interfaces.FileChooser#g:method:getPreviewUri"), [getPreviewWidget]("GI.Gtk.Interfaces.FileChooser#g:method:getPreviewWidget"), [getPreviewWidgetActive]("GI.Gtk.Interfaces.FileChooser#g:method:getPreviewWidgetActive"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSelectMultiple]("GI.Gtk.Interfaces.FileChooser#g:method:getSelectMultiple"), [getShowHidden]("GI.Gtk.Interfaces.FileChooser#g:method:getShowHidden"), [getUri]("GI.Gtk.Interfaces.FileChooser#g:method:getUri"), [getUris]("GI.Gtk.Interfaces.FileChooser#g:method:getUris"), [getUsePreviewLabel]("GI.Gtk.Interfaces.FileChooser#g:method:getUsePreviewLabel").
-- 
-- ==== Setters
-- [setAction]("GI.Gtk.Interfaces.FileChooser#g:method:setAction"), [setChoice]("GI.Gtk.Interfaces.FileChooser#g:method:setChoice"), [setCreateFolders]("GI.Gtk.Interfaces.FileChooser#g:method:setCreateFolders"), [setCurrentFolder]("GI.Gtk.Interfaces.FileChooser#g:method:setCurrentFolder"), [setCurrentFolderFile]("GI.Gtk.Interfaces.FileChooser#g:method:setCurrentFolderFile"), [setCurrentFolderUri]("GI.Gtk.Interfaces.FileChooser#g:method:setCurrentFolderUri"), [setCurrentName]("GI.Gtk.Interfaces.FileChooser#g:method:setCurrentName"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDoOverwriteConfirmation]("GI.Gtk.Interfaces.FileChooser#g:method:setDoOverwriteConfirmation"), [setExtraWidget]("GI.Gtk.Interfaces.FileChooser#g:method:setExtraWidget"), [setFile]("GI.Gtk.Interfaces.FileChooser#g:method:setFile"), [setFilename]("GI.Gtk.Interfaces.FileChooser#g:method:setFilename"), [setFilter]("GI.Gtk.Interfaces.FileChooser#g:method:setFilter"), [setLocalOnly]("GI.Gtk.Interfaces.FileChooser#g:method:setLocalOnly"), [setPreviewWidget]("GI.Gtk.Interfaces.FileChooser#g:method:setPreviewWidget"), [setPreviewWidgetActive]("GI.Gtk.Interfaces.FileChooser#g:method:setPreviewWidgetActive"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSelectMultiple]("GI.Gtk.Interfaces.FileChooser#g:method:setSelectMultiple"), [setShowHidden]("GI.Gtk.Interfaces.FileChooser#g:method:setShowHidden"), [setUri]("GI.Gtk.Interfaces.FileChooser#g:method:setUri"), [setUsePreviewLabel]("GI.Gtk.Interfaces.FileChooser#g:method:setUsePreviewLabel").

#if defined(ENABLE_OVERLOADING)
    ResolveFileChooserMethod                ,
#endif

-- ** addChoice #method:addChoice#

#if defined(ENABLE_OVERLOADING)
    FileChooserAddChoiceMethodInfo          ,
#endif
    fileChooserAddChoice                    ,


-- ** addFilter #method:addFilter#

#if defined(ENABLE_OVERLOADING)
    FileChooserAddFilterMethodInfo          ,
#endif
    fileChooserAddFilter                    ,


-- ** addShortcutFolder #method:addShortcutFolder#

#if defined(ENABLE_OVERLOADING)
    FileChooserAddShortcutFolderMethodInfo  ,
#endif
    fileChooserAddShortcutFolder            ,


-- ** addShortcutFolderUri #method:addShortcutFolderUri#

#if defined(ENABLE_OVERLOADING)
    FileChooserAddShortcutFolderUriMethodInfo,
#endif
    fileChooserAddShortcutFolderUri         ,


-- ** getAction #method:getAction#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetActionMethodInfo          ,
#endif
    fileChooserGetAction                    ,


-- ** getChoice #method:getChoice#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetChoiceMethodInfo          ,
#endif
    fileChooserGetChoice                    ,


-- ** getCreateFolders #method:getCreateFolders#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetCreateFoldersMethodInfo   ,
#endif
    fileChooserGetCreateFolders             ,


-- ** getCurrentFolder #method:getCurrentFolder#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetCurrentFolderMethodInfo   ,
#endif
    fileChooserGetCurrentFolder             ,


-- ** getCurrentFolderFile #method:getCurrentFolderFile#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetCurrentFolderFileMethodInfo,
#endif
    fileChooserGetCurrentFolderFile         ,


-- ** getCurrentFolderUri #method:getCurrentFolderUri#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetCurrentFolderUriMethodInfo,
#endif
    fileChooserGetCurrentFolderUri          ,


-- ** getCurrentName #method:getCurrentName#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetCurrentNameMethodInfo     ,
#endif
    fileChooserGetCurrentName               ,


-- ** getDoOverwriteConfirmation #method:getDoOverwriteConfirmation#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetDoOverwriteConfirmationMethodInfo,
#endif
    fileChooserGetDoOverwriteConfirmation   ,


-- ** getExtraWidget #method:getExtraWidget#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetExtraWidgetMethodInfo     ,
#endif
    fileChooserGetExtraWidget               ,


-- ** getFile #method:getFile#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetFileMethodInfo            ,
#endif
    fileChooserGetFile                      ,


-- ** getFilename #method:getFilename#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetFilenameMethodInfo        ,
#endif
    fileChooserGetFilename                  ,


-- ** getFilenames #method:getFilenames#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetFilenamesMethodInfo       ,
#endif
    fileChooserGetFilenames                 ,


-- ** getFiles #method:getFiles#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetFilesMethodInfo           ,
#endif
    fileChooserGetFiles                     ,


-- ** getFilter #method:getFilter#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetFilterMethodInfo          ,
#endif
    fileChooserGetFilter                    ,


-- ** getLocalOnly #method:getLocalOnly#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetLocalOnlyMethodInfo       ,
#endif
    fileChooserGetLocalOnly                 ,


-- ** getPreviewFile #method:getPreviewFile#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetPreviewFileMethodInfo     ,
#endif
    fileChooserGetPreviewFile               ,


-- ** getPreviewFilename #method:getPreviewFilename#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetPreviewFilenameMethodInfo ,
#endif
    fileChooserGetPreviewFilename           ,


-- ** getPreviewUri #method:getPreviewUri#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetPreviewUriMethodInfo      ,
#endif
    fileChooserGetPreviewUri                ,


-- ** getPreviewWidget #method:getPreviewWidget#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetPreviewWidgetMethodInfo   ,
#endif
    fileChooserGetPreviewWidget             ,


-- ** getPreviewWidgetActive #method:getPreviewWidgetActive#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetPreviewWidgetActiveMethodInfo,
#endif
    fileChooserGetPreviewWidgetActive       ,


-- ** getSelectMultiple #method:getSelectMultiple#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetSelectMultipleMethodInfo  ,
#endif
    fileChooserGetSelectMultiple            ,


-- ** getShowHidden #method:getShowHidden#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetShowHiddenMethodInfo      ,
#endif
    fileChooserGetShowHidden                ,


-- ** getUri #method:getUri#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetUriMethodInfo             ,
#endif
    fileChooserGetUri                       ,


-- ** getUris #method:getUris#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetUrisMethodInfo            ,
#endif
    fileChooserGetUris                      ,


-- ** getUsePreviewLabel #method:getUsePreviewLabel#

#if defined(ENABLE_OVERLOADING)
    FileChooserGetUsePreviewLabelMethodInfo ,
#endif
    fileChooserGetUsePreviewLabel           ,


-- ** listFilters #method:listFilters#

#if defined(ENABLE_OVERLOADING)
    FileChooserListFiltersMethodInfo        ,
#endif
    fileChooserListFilters                  ,


-- ** listShortcutFolderUris #method:listShortcutFolderUris#

#if defined(ENABLE_OVERLOADING)
    FileChooserListShortcutFolderUrisMethodInfo,
#endif
    fileChooserListShortcutFolderUris       ,


-- ** listShortcutFolders #method:listShortcutFolders#

#if defined(ENABLE_OVERLOADING)
    FileChooserListShortcutFoldersMethodInfo,
#endif
    fileChooserListShortcutFolders          ,


-- ** removeChoice #method:removeChoice#

#if defined(ENABLE_OVERLOADING)
    FileChooserRemoveChoiceMethodInfo       ,
#endif
    fileChooserRemoveChoice                 ,


-- ** removeFilter #method:removeFilter#

#if defined(ENABLE_OVERLOADING)
    FileChooserRemoveFilterMethodInfo       ,
#endif
    fileChooserRemoveFilter                 ,


-- ** removeShortcutFolder #method:removeShortcutFolder#

#if defined(ENABLE_OVERLOADING)
    FileChooserRemoveShortcutFolderMethodInfo,
#endif
    fileChooserRemoveShortcutFolder         ,


-- ** removeShortcutFolderUri #method:removeShortcutFolderUri#

#if defined(ENABLE_OVERLOADING)
    FileChooserRemoveShortcutFolderUriMethodInfo,
#endif
    fileChooserRemoveShortcutFolderUri      ,


-- ** selectAll #method:selectAll#

#if defined(ENABLE_OVERLOADING)
    FileChooserSelectAllMethodInfo          ,
#endif
    fileChooserSelectAll                    ,


-- ** selectFile #method:selectFile#

#if defined(ENABLE_OVERLOADING)
    FileChooserSelectFileMethodInfo         ,
#endif
    fileChooserSelectFile                   ,


-- ** selectFilename #method:selectFilename#

#if defined(ENABLE_OVERLOADING)
    FileChooserSelectFilenameMethodInfo     ,
#endif
    fileChooserSelectFilename               ,


-- ** selectUri #method:selectUri#

#if defined(ENABLE_OVERLOADING)
    FileChooserSelectUriMethodInfo          ,
#endif
    fileChooserSelectUri                    ,


-- ** setAction #method:setAction#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetActionMethodInfo          ,
#endif
    fileChooserSetAction                    ,


-- ** setChoice #method:setChoice#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetChoiceMethodInfo          ,
#endif
    fileChooserSetChoice                    ,


-- ** setCreateFolders #method:setCreateFolders#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetCreateFoldersMethodInfo   ,
#endif
    fileChooserSetCreateFolders             ,


-- ** setCurrentFolder #method:setCurrentFolder#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetCurrentFolderMethodInfo   ,
#endif
    fileChooserSetCurrentFolder             ,


-- ** setCurrentFolderFile #method:setCurrentFolderFile#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetCurrentFolderFileMethodInfo,
#endif
    fileChooserSetCurrentFolderFile         ,


-- ** setCurrentFolderUri #method:setCurrentFolderUri#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetCurrentFolderUriMethodInfo,
#endif
    fileChooserSetCurrentFolderUri          ,


-- ** setCurrentName #method:setCurrentName#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetCurrentNameMethodInfo     ,
#endif
    fileChooserSetCurrentName               ,


-- ** setDoOverwriteConfirmation #method:setDoOverwriteConfirmation#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetDoOverwriteConfirmationMethodInfo,
#endif
    fileChooserSetDoOverwriteConfirmation   ,


-- ** setExtraWidget #method:setExtraWidget#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetExtraWidgetMethodInfo     ,
#endif
    fileChooserSetExtraWidget               ,


-- ** setFile #method:setFile#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetFileMethodInfo            ,
#endif
    fileChooserSetFile                      ,


-- ** setFilename #method:setFilename#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetFilenameMethodInfo        ,
#endif
    fileChooserSetFilename                  ,


-- ** setFilter #method:setFilter#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetFilterMethodInfo          ,
#endif
    fileChooserSetFilter                    ,


-- ** setLocalOnly #method:setLocalOnly#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetLocalOnlyMethodInfo       ,
#endif
    fileChooserSetLocalOnly                 ,


-- ** setPreviewWidget #method:setPreviewWidget#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetPreviewWidgetMethodInfo   ,
#endif
    fileChooserSetPreviewWidget             ,


-- ** setPreviewWidgetActive #method:setPreviewWidgetActive#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetPreviewWidgetActiveMethodInfo,
#endif
    fileChooserSetPreviewWidgetActive       ,


-- ** setSelectMultiple #method:setSelectMultiple#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetSelectMultipleMethodInfo  ,
#endif
    fileChooserSetSelectMultiple            ,


-- ** setShowHidden #method:setShowHidden#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetShowHiddenMethodInfo      ,
#endif
    fileChooserSetShowHidden                ,


-- ** setUri #method:setUri#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetUriMethodInfo             ,
#endif
    fileChooserSetUri                       ,


-- ** setUsePreviewLabel #method:setUsePreviewLabel#

#if defined(ENABLE_OVERLOADING)
    FileChooserSetUsePreviewLabelMethodInfo ,
#endif
    fileChooserSetUsePreviewLabel           ,


-- ** unselectAll #method:unselectAll#

#if defined(ENABLE_OVERLOADING)
    FileChooserUnselectAllMethodInfo        ,
#endif
    fileChooserUnselectAll                  ,


-- ** unselectFile #method:unselectFile#

#if defined(ENABLE_OVERLOADING)
    FileChooserUnselectFileMethodInfo       ,
#endif
    fileChooserUnselectFile                 ,


-- ** unselectFilename #method:unselectFilename#

#if defined(ENABLE_OVERLOADING)
    FileChooserUnselectFilenameMethodInfo   ,
#endif
    fileChooserUnselectFilename             ,


-- ** unselectUri #method:unselectUri#

#if defined(ENABLE_OVERLOADING)
    FileChooserUnselectUriMethodInfo        ,
#endif
    fileChooserUnselectUri                  ,




 -- * Properties


-- ** action #attr:action#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    FileChooserActionPropertyInfo           ,
#endif
    constructFileChooserAction              ,
#if defined(ENABLE_OVERLOADING)
    fileChooserAction                       ,
#endif
    getFileChooserAction                    ,
    setFileChooserAction                    ,


-- ** createFolders #attr:createFolders#
-- | Whether a file chooser not in 'GI.Gtk.Enums.FileChooserActionOpen' mode
-- will offer the user to create new folders.
-- 
-- /Since: 2.18/

#if defined(ENABLE_OVERLOADING)
    FileChooserCreateFoldersPropertyInfo    ,
#endif
    constructFileChooserCreateFolders       ,
#if defined(ENABLE_OVERLOADING)
    fileChooserCreateFolders                ,
#endif
    getFileChooserCreateFolders             ,
    setFileChooserCreateFolders             ,


-- ** doOverwriteConfirmation #attr:doOverwriteConfirmation#
-- | Whether a file chooser in 'GI.Gtk.Enums.FileChooserActionSave' mode
-- will present an overwrite confirmation dialog if the user
-- selects a file name that already exists.
-- 
-- /Since: 2.8/

#if defined(ENABLE_OVERLOADING)
    FileChooserDoOverwriteConfirmationPropertyInfo,
#endif
    constructFileChooserDoOverwriteConfirmation,
#if defined(ENABLE_OVERLOADING)
    fileChooserDoOverwriteConfirmation      ,
#endif
    getFileChooserDoOverwriteConfirmation   ,
    setFileChooserDoOverwriteConfirmation   ,


-- ** extraWidget #attr:extraWidget#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    FileChooserExtraWidgetPropertyInfo      ,
#endif
    constructFileChooserExtraWidget         ,
#if defined(ENABLE_OVERLOADING)
    fileChooserExtraWidget                  ,
#endif
    getFileChooserExtraWidget               ,
    setFileChooserExtraWidget               ,


-- ** filter #attr:filter#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    FileChooserFilterPropertyInfo           ,
#endif
    constructFileChooserFilter              ,
#if defined(ENABLE_OVERLOADING)
    fileChooserFilter                       ,
#endif
    getFileChooserFilter                    ,
    setFileChooserFilter                    ,


-- ** localOnly #attr:localOnly#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    FileChooserLocalOnlyPropertyInfo        ,
#endif
    constructFileChooserLocalOnly           ,
#if defined(ENABLE_OVERLOADING)
    fileChooserLocalOnly                    ,
#endif
    getFileChooserLocalOnly                 ,
    setFileChooserLocalOnly                 ,


-- ** previewWidget #attr:previewWidget#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    FileChooserPreviewWidgetPropertyInfo    ,
#endif
    constructFileChooserPreviewWidget       ,
#if defined(ENABLE_OVERLOADING)
    fileChooserPreviewWidget                ,
#endif
    getFileChooserPreviewWidget             ,
    setFileChooserPreviewWidget             ,


-- ** previewWidgetActive #attr:previewWidgetActive#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    FileChooserPreviewWidgetActivePropertyInfo,
#endif
    constructFileChooserPreviewWidgetActive ,
#if defined(ENABLE_OVERLOADING)
    fileChooserPreviewWidgetActive          ,
#endif
    getFileChooserPreviewWidgetActive       ,
    setFileChooserPreviewWidgetActive       ,


-- ** selectMultiple #attr:selectMultiple#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    FileChooserSelectMultiplePropertyInfo   ,
#endif
    constructFileChooserSelectMultiple      ,
#if defined(ENABLE_OVERLOADING)
    fileChooserSelectMultiple               ,
#endif
    getFileChooserSelectMultiple            ,
    setFileChooserSelectMultiple            ,


-- ** showHidden #attr:showHidden#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    FileChooserShowHiddenPropertyInfo       ,
#endif
    constructFileChooserShowHidden          ,
#if defined(ENABLE_OVERLOADING)
    fileChooserShowHidden                   ,
#endif
    getFileChooserShowHidden                ,
    setFileChooserShowHidden                ,


-- ** usePreviewLabel #attr:usePreviewLabel#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    FileChooserUsePreviewLabelPropertyInfo  ,
#endif
    constructFileChooserUsePreviewLabel     ,
#if defined(ENABLE_OVERLOADING)
    fileChooserUsePreviewLabel              ,
#endif
    getFileChooserUsePreviewLabel           ,
    setFileChooserUsePreviewLabel           ,




 -- * Signals


-- ** confirmOverwrite #signal:confirmOverwrite#

    FileChooserConfirmOverwriteCallback     ,
#if defined(ENABLE_OVERLOADING)
    FileChooserConfirmOverwriteSignalInfo   ,
#endif
    afterFileChooserConfirmOverwrite        ,
    onFileChooserConfirmOverwrite           ,


-- ** currentFolderChanged #signal:currentFolderChanged#

    FileChooserCurrentFolderChangedCallback ,
#if defined(ENABLE_OVERLOADING)
    FileChooserCurrentFolderChangedSignalInfo,
#endif
    afterFileChooserCurrentFolderChanged    ,
    onFileChooserCurrentFolderChanged       ,


-- ** fileActivated #signal:fileActivated#

    FileChooserFileActivatedCallback        ,
#if defined(ENABLE_OVERLOADING)
    FileChooserFileActivatedSignalInfo      ,
#endif
    afterFileChooserFileActivated           ,
    onFileChooserFileActivated              ,


-- ** selectionChanged #signal:selectionChanged#

    FileChooserSelectionChangedCallback     ,
#if defined(ENABLE_OVERLOADING)
    FileChooserSelectionChangedSignalInfo   ,
#endif
    afterFileChooserSelectionChanged        ,
    onFileChooserSelectionChanged           ,


-- ** updatePreview #signal:updatePreview#

    FileChooserUpdatePreviewCallback        ,
#if defined(ENABLE_OVERLOADING)
    FileChooserUpdatePreviewSignalInfo      ,
#endif
    afterFileChooserUpdatePreview           ,
    onFileChooserUpdatePreview              ,




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

import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gio.Interfaces.File as Gio.File
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Objects.FileFilter as Gtk.FileFilter
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- interface FileChooser 
-- | Memory-managed wrapper type.
newtype FileChooser = FileChooser (SP.ManagedPtr FileChooser)
    deriving (Eq)

instance SP.ManagedPtrNewtype FileChooser where
    toManagedPtr (FileChooser p) = p

foreign import ccall "gtk_file_chooser_get_type"
    c_gtk_file_chooser_get_type :: IO B.Types.GType

instance B.Types.TypedObject FileChooser where
    glibType = c_gtk_file_chooser_get_type

instance B.Types.GObject FileChooser

-- | Type class for types which can be safely cast to `FileChooser`, for instance with `toFileChooser`.
class (SP.GObject o, O.IsDescendantOf FileChooser o) => IsFileChooser o
instance (SP.GObject o, O.IsDescendantOf FileChooser o) => IsFileChooser o

instance O.HasParentTypes FileChooser
type instance O.ParentTypes FileChooser = '[GObject.Object.Object]

-- | Cast to `FileChooser`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toFileChooser :: (MIO.MonadIO m, IsFileChooser o) => o -> m FileChooser
toFileChooser = MIO.liftIO . B.ManagedPtr.unsafeCastTo FileChooser

-- | Convert 'FileChooser' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe FileChooser) where
    gvalueGType_ = c_gtk_file_chooser_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr FileChooser)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr FileChooser)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject FileChooser ptr
        else return P.Nothing
        
    

-- VVV Prop "action"
   -- Type: TInterface (Name {namespace = "Gtk", name = "FileChooserAction"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@action@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fileChooser #action
-- @
getFileChooserAction :: (MonadIO m, IsFileChooser o) => o -> m Gtk.Enums.FileChooserAction
getFileChooserAction obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "action"

-- | Set the value of the “@action@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fileChooser [ #action 'Data.GI.Base.Attributes.:=' value ]
-- @
setFileChooserAction :: (MonadIO m, IsFileChooser o) => o -> Gtk.Enums.FileChooserAction -> m ()
setFileChooserAction obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "action" val

-- | Construct a `GValueConstruct` with valid value for the “@action@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFileChooserAction :: (IsFileChooser o, MIO.MonadIO m) => Gtk.Enums.FileChooserAction -> m (GValueConstruct o)
constructFileChooserAction val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "action" val

#if defined(ENABLE_OVERLOADING)
data FileChooserActionPropertyInfo
instance AttrInfo FileChooserActionPropertyInfo where
    type AttrAllowedOps FileChooserActionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FileChooserActionPropertyInfo = IsFileChooser
    type AttrSetTypeConstraint FileChooserActionPropertyInfo = (~) Gtk.Enums.FileChooserAction
    type AttrTransferTypeConstraint FileChooserActionPropertyInfo = (~) Gtk.Enums.FileChooserAction
    type AttrTransferType FileChooserActionPropertyInfo = Gtk.Enums.FileChooserAction
    type AttrGetType FileChooserActionPropertyInfo = Gtk.Enums.FileChooserAction
    type AttrLabel FileChooserActionPropertyInfo = "action"
    type AttrOrigin FileChooserActionPropertyInfo = FileChooser
    attrGet = getFileChooserAction
    attrSet = setFileChooserAction
    attrTransfer _ v = do
        return v
    attrConstruct = constructFileChooserAction
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.action"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#g:attr:action"
        })
#endif

-- VVV Prop "create-folders"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@create-folders@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fileChooser #createFolders
-- @
getFileChooserCreateFolders :: (MonadIO m, IsFileChooser o) => o -> m Bool
getFileChooserCreateFolders obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "create-folders"

-- | Set the value of the “@create-folders@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fileChooser [ #createFolders 'Data.GI.Base.Attributes.:=' value ]
-- @
setFileChooserCreateFolders :: (MonadIO m, IsFileChooser o) => o -> Bool -> m ()
setFileChooserCreateFolders obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "create-folders" val

-- | Construct a `GValueConstruct` with valid value for the “@create-folders@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFileChooserCreateFolders :: (IsFileChooser o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructFileChooserCreateFolders val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "create-folders" val

#if defined(ENABLE_OVERLOADING)
data FileChooserCreateFoldersPropertyInfo
instance AttrInfo FileChooserCreateFoldersPropertyInfo where
    type AttrAllowedOps FileChooserCreateFoldersPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FileChooserCreateFoldersPropertyInfo = IsFileChooser
    type AttrSetTypeConstraint FileChooserCreateFoldersPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint FileChooserCreateFoldersPropertyInfo = (~) Bool
    type AttrTransferType FileChooserCreateFoldersPropertyInfo = Bool
    type AttrGetType FileChooserCreateFoldersPropertyInfo = Bool
    type AttrLabel FileChooserCreateFoldersPropertyInfo = "create-folders"
    type AttrOrigin FileChooserCreateFoldersPropertyInfo = FileChooser
    attrGet = getFileChooserCreateFolders
    attrSet = setFileChooserCreateFolders
    attrTransfer _ v = do
        return v
    attrConstruct = constructFileChooserCreateFolders
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.createFolders"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#g:attr:createFolders"
        })
#endif

-- VVV Prop "do-overwrite-confirmation"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@do-overwrite-confirmation@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fileChooser #doOverwriteConfirmation
-- @
getFileChooserDoOverwriteConfirmation :: (MonadIO m, IsFileChooser o) => o -> m Bool
getFileChooserDoOverwriteConfirmation obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "do-overwrite-confirmation"

-- | Set the value of the “@do-overwrite-confirmation@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fileChooser [ #doOverwriteConfirmation 'Data.GI.Base.Attributes.:=' value ]
-- @
setFileChooserDoOverwriteConfirmation :: (MonadIO m, IsFileChooser o) => o -> Bool -> m ()
setFileChooserDoOverwriteConfirmation obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "do-overwrite-confirmation" val

-- | Construct a `GValueConstruct` with valid value for the “@do-overwrite-confirmation@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFileChooserDoOverwriteConfirmation :: (IsFileChooser o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructFileChooserDoOverwriteConfirmation val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "do-overwrite-confirmation" val

#if defined(ENABLE_OVERLOADING)
data FileChooserDoOverwriteConfirmationPropertyInfo
instance AttrInfo FileChooserDoOverwriteConfirmationPropertyInfo where
    type AttrAllowedOps FileChooserDoOverwriteConfirmationPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FileChooserDoOverwriteConfirmationPropertyInfo = IsFileChooser
    type AttrSetTypeConstraint FileChooserDoOverwriteConfirmationPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint FileChooserDoOverwriteConfirmationPropertyInfo = (~) Bool
    type AttrTransferType FileChooserDoOverwriteConfirmationPropertyInfo = Bool
    type AttrGetType FileChooserDoOverwriteConfirmationPropertyInfo = Bool
    type AttrLabel FileChooserDoOverwriteConfirmationPropertyInfo = "do-overwrite-confirmation"
    type AttrOrigin FileChooserDoOverwriteConfirmationPropertyInfo = FileChooser
    attrGet = getFileChooserDoOverwriteConfirmation
    attrSet = setFileChooserDoOverwriteConfirmation
    attrTransfer _ v = do
        return v
    attrConstruct = constructFileChooserDoOverwriteConfirmation
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.doOverwriteConfirmation"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#g:attr:doOverwriteConfirmation"
        })
#endif

-- VVV Prop "extra-widget"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Widget"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just False)

-- | Get the value of the “@extra-widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fileChooser #extraWidget
-- @
getFileChooserExtraWidget :: (MonadIO m, IsFileChooser o) => o -> m (Maybe Gtk.Widget.Widget)
getFileChooserExtraWidget obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "extra-widget" Gtk.Widget.Widget

-- | Set the value of the “@extra-widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fileChooser [ #extraWidget 'Data.GI.Base.Attributes.:=' value ]
-- @
setFileChooserExtraWidget :: (MonadIO m, IsFileChooser o, Gtk.Widget.IsWidget a) => o -> a -> m ()
setFileChooserExtraWidget obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "extra-widget" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@extra-widget@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFileChooserExtraWidget :: (IsFileChooser o, MIO.MonadIO m, Gtk.Widget.IsWidget a) => a -> m (GValueConstruct o)
constructFileChooserExtraWidget val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "extra-widget" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data FileChooserExtraWidgetPropertyInfo
instance AttrInfo FileChooserExtraWidgetPropertyInfo where
    type AttrAllowedOps FileChooserExtraWidgetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FileChooserExtraWidgetPropertyInfo = IsFileChooser
    type AttrSetTypeConstraint FileChooserExtraWidgetPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferTypeConstraint FileChooserExtraWidgetPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferType FileChooserExtraWidgetPropertyInfo = Gtk.Widget.Widget
    type AttrGetType FileChooserExtraWidgetPropertyInfo = (Maybe Gtk.Widget.Widget)
    type AttrLabel FileChooserExtraWidgetPropertyInfo = "extra-widget"
    type AttrOrigin FileChooserExtraWidgetPropertyInfo = FileChooser
    attrGet = getFileChooserExtraWidget
    attrSet = setFileChooserExtraWidget
    attrTransfer _ v = do
        unsafeCastTo Gtk.Widget.Widget v
    attrConstruct = constructFileChooserExtraWidget
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.extraWidget"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#g:attr:extraWidget"
        })
#endif

-- VVV Prop "filter"
   -- Type: TInterface (Name {namespace = "Gtk", name = "FileFilter"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just False)

-- | Get the value of the “@filter@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fileChooser #filter
-- @
getFileChooserFilter :: (MonadIO m, IsFileChooser o) => o -> m (Maybe Gtk.FileFilter.FileFilter)
getFileChooserFilter obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "filter" Gtk.FileFilter.FileFilter

-- | Set the value of the “@filter@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fileChooser [ #filter 'Data.GI.Base.Attributes.:=' value ]
-- @
setFileChooserFilter :: (MonadIO m, IsFileChooser o, Gtk.FileFilter.IsFileFilter a) => o -> a -> m ()
setFileChooserFilter obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "filter" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@filter@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFileChooserFilter :: (IsFileChooser o, MIO.MonadIO m, Gtk.FileFilter.IsFileFilter a) => a -> m (GValueConstruct o)
constructFileChooserFilter val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "filter" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data FileChooserFilterPropertyInfo
instance AttrInfo FileChooserFilterPropertyInfo where
    type AttrAllowedOps FileChooserFilterPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FileChooserFilterPropertyInfo = IsFileChooser
    type AttrSetTypeConstraint FileChooserFilterPropertyInfo = Gtk.FileFilter.IsFileFilter
    type AttrTransferTypeConstraint FileChooserFilterPropertyInfo = Gtk.FileFilter.IsFileFilter
    type AttrTransferType FileChooserFilterPropertyInfo = Gtk.FileFilter.FileFilter
    type AttrGetType FileChooserFilterPropertyInfo = (Maybe Gtk.FileFilter.FileFilter)
    type AttrLabel FileChooserFilterPropertyInfo = "filter"
    type AttrOrigin FileChooserFilterPropertyInfo = FileChooser
    attrGet = getFileChooserFilter
    attrSet = setFileChooserFilter
    attrTransfer _ v = do
        unsafeCastTo Gtk.FileFilter.FileFilter v
    attrConstruct = constructFileChooserFilter
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.filter"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#g:attr:filter"
        })
#endif

-- VVV Prop "local-only"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@local-only@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fileChooser #localOnly
-- @
getFileChooserLocalOnly :: (MonadIO m, IsFileChooser o) => o -> m Bool
getFileChooserLocalOnly obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "local-only"

-- | Set the value of the “@local-only@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fileChooser [ #localOnly 'Data.GI.Base.Attributes.:=' value ]
-- @
setFileChooserLocalOnly :: (MonadIO m, IsFileChooser o) => o -> Bool -> m ()
setFileChooserLocalOnly obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "local-only" val

-- | Construct a `GValueConstruct` with valid value for the “@local-only@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFileChooserLocalOnly :: (IsFileChooser o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructFileChooserLocalOnly val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "local-only" val

#if defined(ENABLE_OVERLOADING)
data FileChooserLocalOnlyPropertyInfo
instance AttrInfo FileChooserLocalOnlyPropertyInfo where
    type AttrAllowedOps FileChooserLocalOnlyPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FileChooserLocalOnlyPropertyInfo = IsFileChooser
    type AttrSetTypeConstraint FileChooserLocalOnlyPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint FileChooserLocalOnlyPropertyInfo = (~) Bool
    type AttrTransferType FileChooserLocalOnlyPropertyInfo = Bool
    type AttrGetType FileChooserLocalOnlyPropertyInfo = Bool
    type AttrLabel FileChooserLocalOnlyPropertyInfo = "local-only"
    type AttrOrigin FileChooserLocalOnlyPropertyInfo = FileChooser
    attrGet = getFileChooserLocalOnly
    attrSet = setFileChooserLocalOnly
    attrTransfer _ v = do
        return v
    attrConstruct = constructFileChooserLocalOnly
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.localOnly"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#g:attr:localOnly"
        })
#endif

-- VVV Prop "preview-widget"
   -- Type: TInterface (Name {namespace = "Gtk", name = "Widget"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just False)

-- | Get the value of the “@preview-widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fileChooser #previewWidget
-- @
getFileChooserPreviewWidget :: (MonadIO m, IsFileChooser o) => o -> m (Maybe Gtk.Widget.Widget)
getFileChooserPreviewWidget obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "preview-widget" Gtk.Widget.Widget

-- | Set the value of the “@preview-widget@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fileChooser [ #previewWidget 'Data.GI.Base.Attributes.:=' value ]
-- @
setFileChooserPreviewWidget :: (MonadIO m, IsFileChooser o, Gtk.Widget.IsWidget a) => o -> a -> m ()
setFileChooserPreviewWidget obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "preview-widget" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@preview-widget@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFileChooserPreviewWidget :: (IsFileChooser o, MIO.MonadIO m, Gtk.Widget.IsWidget a) => a -> m (GValueConstruct o)
constructFileChooserPreviewWidget val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "preview-widget" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data FileChooserPreviewWidgetPropertyInfo
instance AttrInfo FileChooserPreviewWidgetPropertyInfo where
    type AttrAllowedOps FileChooserPreviewWidgetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FileChooserPreviewWidgetPropertyInfo = IsFileChooser
    type AttrSetTypeConstraint FileChooserPreviewWidgetPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferTypeConstraint FileChooserPreviewWidgetPropertyInfo = Gtk.Widget.IsWidget
    type AttrTransferType FileChooserPreviewWidgetPropertyInfo = Gtk.Widget.Widget
    type AttrGetType FileChooserPreviewWidgetPropertyInfo = (Maybe Gtk.Widget.Widget)
    type AttrLabel FileChooserPreviewWidgetPropertyInfo = "preview-widget"
    type AttrOrigin FileChooserPreviewWidgetPropertyInfo = FileChooser
    attrGet = getFileChooserPreviewWidget
    attrSet = setFileChooserPreviewWidget
    attrTransfer _ v = do
        unsafeCastTo Gtk.Widget.Widget v
    attrConstruct = constructFileChooserPreviewWidget
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.previewWidget"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#g:attr:previewWidget"
        })
#endif

-- VVV Prop "preview-widget-active"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@preview-widget-active@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fileChooser #previewWidgetActive
-- @
getFileChooserPreviewWidgetActive :: (MonadIO m, IsFileChooser o) => o -> m Bool
getFileChooserPreviewWidgetActive obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "preview-widget-active"

-- | Set the value of the “@preview-widget-active@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fileChooser [ #previewWidgetActive 'Data.GI.Base.Attributes.:=' value ]
-- @
setFileChooserPreviewWidgetActive :: (MonadIO m, IsFileChooser o) => o -> Bool -> m ()
setFileChooserPreviewWidgetActive obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "preview-widget-active" val

-- | Construct a `GValueConstruct` with valid value for the “@preview-widget-active@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFileChooserPreviewWidgetActive :: (IsFileChooser o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructFileChooserPreviewWidgetActive val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "preview-widget-active" val

#if defined(ENABLE_OVERLOADING)
data FileChooserPreviewWidgetActivePropertyInfo
instance AttrInfo FileChooserPreviewWidgetActivePropertyInfo where
    type AttrAllowedOps FileChooserPreviewWidgetActivePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FileChooserPreviewWidgetActivePropertyInfo = IsFileChooser
    type AttrSetTypeConstraint FileChooserPreviewWidgetActivePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint FileChooserPreviewWidgetActivePropertyInfo = (~) Bool
    type AttrTransferType FileChooserPreviewWidgetActivePropertyInfo = Bool
    type AttrGetType FileChooserPreviewWidgetActivePropertyInfo = Bool
    type AttrLabel FileChooserPreviewWidgetActivePropertyInfo = "preview-widget-active"
    type AttrOrigin FileChooserPreviewWidgetActivePropertyInfo = FileChooser
    attrGet = getFileChooserPreviewWidgetActive
    attrSet = setFileChooserPreviewWidgetActive
    attrTransfer _ v = do
        return v
    attrConstruct = constructFileChooserPreviewWidgetActive
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.previewWidgetActive"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#g:attr:previewWidgetActive"
        })
#endif

-- VVV Prop "select-multiple"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@select-multiple@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fileChooser #selectMultiple
-- @
getFileChooserSelectMultiple :: (MonadIO m, IsFileChooser o) => o -> m Bool
getFileChooserSelectMultiple obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "select-multiple"

-- | Set the value of the “@select-multiple@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fileChooser [ #selectMultiple 'Data.GI.Base.Attributes.:=' value ]
-- @
setFileChooserSelectMultiple :: (MonadIO m, IsFileChooser o) => o -> Bool -> m ()
setFileChooserSelectMultiple obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "select-multiple" val

-- | Construct a `GValueConstruct` with valid value for the “@select-multiple@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFileChooserSelectMultiple :: (IsFileChooser o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructFileChooserSelectMultiple val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "select-multiple" val

#if defined(ENABLE_OVERLOADING)
data FileChooserSelectMultiplePropertyInfo
instance AttrInfo FileChooserSelectMultiplePropertyInfo where
    type AttrAllowedOps FileChooserSelectMultiplePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FileChooserSelectMultiplePropertyInfo = IsFileChooser
    type AttrSetTypeConstraint FileChooserSelectMultiplePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint FileChooserSelectMultiplePropertyInfo = (~) Bool
    type AttrTransferType FileChooserSelectMultiplePropertyInfo = Bool
    type AttrGetType FileChooserSelectMultiplePropertyInfo = Bool
    type AttrLabel FileChooserSelectMultiplePropertyInfo = "select-multiple"
    type AttrOrigin FileChooserSelectMultiplePropertyInfo = FileChooser
    attrGet = getFileChooserSelectMultiple
    attrSet = setFileChooserSelectMultiple
    attrTransfer _ v = do
        return v
    attrConstruct = constructFileChooserSelectMultiple
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.selectMultiple"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#g:attr:selectMultiple"
        })
#endif

-- VVV Prop "show-hidden"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@show-hidden@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fileChooser #showHidden
-- @
getFileChooserShowHidden :: (MonadIO m, IsFileChooser o) => o -> m Bool
getFileChooserShowHidden obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "show-hidden"

-- | Set the value of the “@show-hidden@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fileChooser [ #showHidden 'Data.GI.Base.Attributes.:=' value ]
-- @
setFileChooserShowHidden :: (MonadIO m, IsFileChooser o) => o -> Bool -> m ()
setFileChooserShowHidden obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "show-hidden" val

-- | Construct a `GValueConstruct` with valid value for the “@show-hidden@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFileChooserShowHidden :: (IsFileChooser o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructFileChooserShowHidden val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "show-hidden" val

#if defined(ENABLE_OVERLOADING)
data FileChooserShowHiddenPropertyInfo
instance AttrInfo FileChooserShowHiddenPropertyInfo where
    type AttrAllowedOps FileChooserShowHiddenPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FileChooserShowHiddenPropertyInfo = IsFileChooser
    type AttrSetTypeConstraint FileChooserShowHiddenPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint FileChooserShowHiddenPropertyInfo = (~) Bool
    type AttrTransferType FileChooserShowHiddenPropertyInfo = Bool
    type AttrGetType FileChooserShowHiddenPropertyInfo = Bool
    type AttrLabel FileChooserShowHiddenPropertyInfo = "show-hidden"
    type AttrOrigin FileChooserShowHiddenPropertyInfo = FileChooser
    attrGet = getFileChooserShowHidden
    attrSet = setFileChooserShowHidden
    attrTransfer _ v = do
        return v
    attrConstruct = constructFileChooserShowHidden
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.showHidden"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#g:attr:showHidden"
        })
#endif

-- VVV Prop "use-preview-label"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@use-preview-label@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fileChooser #usePreviewLabel
-- @
getFileChooserUsePreviewLabel :: (MonadIO m, IsFileChooser o) => o -> m Bool
getFileChooserUsePreviewLabel obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "use-preview-label"

-- | Set the value of the “@use-preview-label@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fileChooser [ #usePreviewLabel 'Data.GI.Base.Attributes.:=' value ]
-- @
setFileChooserUsePreviewLabel :: (MonadIO m, IsFileChooser o) => o -> Bool -> m ()
setFileChooserUsePreviewLabel obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "use-preview-label" val

-- | Construct a `GValueConstruct` with valid value for the “@use-preview-label@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructFileChooserUsePreviewLabel :: (IsFileChooser o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructFileChooserUsePreviewLabel val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "use-preview-label" val

#if defined(ENABLE_OVERLOADING)
data FileChooserUsePreviewLabelPropertyInfo
instance AttrInfo FileChooserUsePreviewLabelPropertyInfo where
    type AttrAllowedOps FileChooserUsePreviewLabelPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint FileChooserUsePreviewLabelPropertyInfo = IsFileChooser
    type AttrSetTypeConstraint FileChooserUsePreviewLabelPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint FileChooserUsePreviewLabelPropertyInfo = (~) Bool
    type AttrTransferType FileChooserUsePreviewLabelPropertyInfo = Bool
    type AttrGetType FileChooserUsePreviewLabelPropertyInfo = Bool
    type AttrLabel FileChooserUsePreviewLabelPropertyInfo = "use-preview-label"
    type AttrOrigin FileChooserUsePreviewLabelPropertyInfo = FileChooser
    attrGet = getFileChooserUsePreviewLabel
    attrSet = setFileChooserUsePreviewLabel
    attrTransfer _ v = do
        return v
    attrConstruct = constructFileChooserUsePreviewLabel
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.usePreviewLabel"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#g:attr:usePreviewLabel"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList FileChooser
type instance O.AttributeList FileChooser = FileChooserAttributeList
type FileChooserAttributeList = ('[ '("action", FileChooserActionPropertyInfo), '("createFolders", FileChooserCreateFoldersPropertyInfo), '("doOverwriteConfirmation", FileChooserDoOverwriteConfirmationPropertyInfo), '("extraWidget", FileChooserExtraWidgetPropertyInfo), '("filter", FileChooserFilterPropertyInfo), '("localOnly", FileChooserLocalOnlyPropertyInfo), '("previewWidget", FileChooserPreviewWidgetPropertyInfo), '("previewWidgetActive", FileChooserPreviewWidgetActivePropertyInfo), '("selectMultiple", FileChooserSelectMultiplePropertyInfo), '("showHidden", FileChooserShowHiddenPropertyInfo), '("usePreviewLabel", FileChooserUsePreviewLabelPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
fileChooserAction :: AttrLabelProxy "action"
fileChooserAction = AttrLabelProxy

fileChooserCreateFolders :: AttrLabelProxy "createFolders"
fileChooserCreateFolders = AttrLabelProxy

fileChooserDoOverwriteConfirmation :: AttrLabelProxy "doOverwriteConfirmation"
fileChooserDoOverwriteConfirmation = AttrLabelProxy

fileChooserExtraWidget :: AttrLabelProxy "extraWidget"
fileChooserExtraWidget = AttrLabelProxy

fileChooserFilter :: AttrLabelProxy "filter"
fileChooserFilter = AttrLabelProxy

fileChooserLocalOnly :: AttrLabelProxy "localOnly"
fileChooserLocalOnly = AttrLabelProxy

fileChooserPreviewWidget :: AttrLabelProxy "previewWidget"
fileChooserPreviewWidget = AttrLabelProxy

fileChooserPreviewWidgetActive :: AttrLabelProxy "previewWidgetActive"
fileChooserPreviewWidgetActive = AttrLabelProxy

fileChooserSelectMultiple :: AttrLabelProxy "selectMultiple"
fileChooserSelectMultiple = AttrLabelProxy

fileChooserShowHidden :: AttrLabelProxy "showHidden"
fileChooserShowHidden = AttrLabelProxy

fileChooserUsePreviewLabel :: AttrLabelProxy "usePreviewLabel"
fileChooserUsePreviewLabel = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveFileChooserMethod (t :: Symbol) (o :: *) :: * where
    ResolveFileChooserMethod "addChoice" o = FileChooserAddChoiceMethodInfo
    ResolveFileChooserMethod "addFilter" o = FileChooserAddFilterMethodInfo
    ResolveFileChooserMethod "addShortcutFolder" o = FileChooserAddShortcutFolderMethodInfo
    ResolveFileChooserMethod "addShortcutFolderUri" o = FileChooserAddShortcutFolderUriMethodInfo
    ResolveFileChooserMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveFileChooserMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveFileChooserMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveFileChooserMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveFileChooserMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveFileChooserMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveFileChooserMethod "listFilters" o = FileChooserListFiltersMethodInfo
    ResolveFileChooserMethod "listShortcutFolderUris" o = FileChooserListShortcutFolderUrisMethodInfo
    ResolveFileChooserMethod "listShortcutFolders" o = FileChooserListShortcutFoldersMethodInfo
    ResolveFileChooserMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveFileChooserMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveFileChooserMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveFileChooserMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveFileChooserMethod "removeChoice" o = FileChooserRemoveChoiceMethodInfo
    ResolveFileChooserMethod "removeFilter" o = FileChooserRemoveFilterMethodInfo
    ResolveFileChooserMethod "removeShortcutFolder" o = FileChooserRemoveShortcutFolderMethodInfo
    ResolveFileChooserMethod "removeShortcutFolderUri" o = FileChooserRemoveShortcutFolderUriMethodInfo
    ResolveFileChooserMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveFileChooserMethod "selectAll" o = FileChooserSelectAllMethodInfo
    ResolveFileChooserMethod "selectFile" o = FileChooserSelectFileMethodInfo
    ResolveFileChooserMethod "selectFilename" o = FileChooserSelectFilenameMethodInfo
    ResolveFileChooserMethod "selectUri" o = FileChooserSelectUriMethodInfo
    ResolveFileChooserMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveFileChooserMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveFileChooserMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveFileChooserMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveFileChooserMethod "unselectAll" o = FileChooserUnselectAllMethodInfo
    ResolveFileChooserMethod "unselectFile" o = FileChooserUnselectFileMethodInfo
    ResolveFileChooserMethod "unselectFilename" o = FileChooserUnselectFilenameMethodInfo
    ResolveFileChooserMethod "unselectUri" o = FileChooserUnselectUriMethodInfo
    ResolveFileChooserMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveFileChooserMethod "getAction" o = FileChooserGetActionMethodInfo
    ResolveFileChooserMethod "getChoice" o = FileChooserGetChoiceMethodInfo
    ResolveFileChooserMethod "getCreateFolders" o = FileChooserGetCreateFoldersMethodInfo
    ResolveFileChooserMethod "getCurrentFolder" o = FileChooserGetCurrentFolderMethodInfo
    ResolveFileChooserMethod "getCurrentFolderFile" o = FileChooserGetCurrentFolderFileMethodInfo
    ResolveFileChooserMethod "getCurrentFolderUri" o = FileChooserGetCurrentFolderUriMethodInfo
    ResolveFileChooserMethod "getCurrentName" o = FileChooserGetCurrentNameMethodInfo
    ResolveFileChooserMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveFileChooserMethod "getDoOverwriteConfirmation" o = FileChooserGetDoOverwriteConfirmationMethodInfo
    ResolveFileChooserMethod "getExtraWidget" o = FileChooserGetExtraWidgetMethodInfo
    ResolveFileChooserMethod "getFile" o = FileChooserGetFileMethodInfo
    ResolveFileChooserMethod "getFilename" o = FileChooserGetFilenameMethodInfo
    ResolveFileChooserMethod "getFilenames" o = FileChooserGetFilenamesMethodInfo
    ResolveFileChooserMethod "getFiles" o = FileChooserGetFilesMethodInfo
    ResolveFileChooserMethod "getFilter" o = FileChooserGetFilterMethodInfo
    ResolveFileChooserMethod "getLocalOnly" o = FileChooserGetLocalOnlyMethodInfo
    ResolveFileChooserMethod "getPreviewFile" o = FileChooserGetPreviewFileMethodInfo
    ResolveFileChooserMethod "getPreviewFilename" o = FileChooserGetPreviewFilenameMethodInfo
    ResolveFileChooserMethod "getPreviewUri" o = FileChooserGetPreviewUriMethodInfo
    ResolveFileChooserMethod "getPreviewWidget" o = FileChooserGetPreviewWidgetMethodInfo
    ResolveFileChooserMethod "getPreviewWidgetActive" o = FileChooserGetPreviewWidgetActiveMethodInfo
    ResolveFileChooserMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveFileChooserMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveFileChooserMethod "getSelectMultiple" o = FileChooserGetSelectMultipleMethodInfo
    ResolveFileChooserMethod "getShowHidden" o = FileChooserGetShowHiddenMethodInfo
    ResolveFileChooserMethod "getUri" o = FileChooserGetUriMethodInfo
    ResolveFileChooserMethod "getUris" o = FileChooserGetUrisMethodInfo
    ResolveFileChooserMethod "getUsePreviewLabel" o = FileChooserGetUsePreviewLabelMethodInfo
    ResolveFileChooserMethod "setAction" o = FileChooserSetActionMethodInfo
    ResolveFileChooserMethod "setChoice" o = FileChooserSetChoiceMethodInfo
    ResolveFileChooserMethod "setCreateFolders" o = FileChooserSetCreateFoldersMethodInfo
    ResolveFileChooserMethod "setCurrentFolder" o = FileChooserSetCurrentFolderMethodInfo
    ResolveFileChooserMethod "setCurrentFolderFile" o = FileChooserSetCurrentFolderFileMethodInfo
    ResolveFileChooserMethod "setCurrentFolderUri" o = FileChooserSetCurrentFolderUriMethodInfo
    ResolveFileChooserMethod "setCurrentName" o = FileChooserSetCurrentNameMethodInfo
    ResolveFileChooserMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveFileChooserMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveFileChooserMethod "setDoOverwriteConfirmation" o = FileChooserSetDoOverwriteConfirmationMethodInfo
    ResolveFileChooserMethod "setExtraWidget" o = FileChooserSetExtraWidgetMethodInfo
    ResolveFileChooserMethod "setFile" o = FileChooserSetFileMethodInfo
    ResolveFileChooserMethod "setFilename" o = FileChooserSetFilenameMethodInfo
    ResolveFileChooserMethod "setFilter" o = FileChooserSetFilterMethodInfo
    ResolveFileChooserMethod "setLocalOnly" o = FileChooserSetLocalOnlyMethodInfo
    ResolveFileChooserMethod "setPreviewWidget" o = FileChooserSetPreviewWidgetMethodInfo
    ResolveFileChooserMethod "setPreviewWidgetActive" o = FileChooserSetPreviewWidgetActiveMethodInfo
    ResolveFileChooserMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveFileChooserMethod "setSelectMultiple" o = FileChooserSetSelectMultipleMethodInfo
    ResolveFileChooserMethod "setShowHidden" o = FileChooserSetShowHiddenMethodInfo
    ResolveFileChooserMethod "setUri" o = FileChooserSetUriMethodInfo
    ResolveFileChooserMethod "setUsePreviewLabel" o = FileChooserSetUsePreviewLabelMethodInfo
    ResolveFileChooserMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveFileChooserMethod t FileChooser, O.OverloadedMethod info FileChooser p) => OL.IsLabel t (FileChooser -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveFileChooserMethod t FileChooser, O.OverloadedMethod info FileChooser p, R.HasField t FileChooser p) => R.HasField t FileChooser p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveFileChooserMethod t FileChooser, O.OverloadedMethodInfo info FileChooser) => OL.IsLabel t (O.MethodProxy info FileChooser) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- method FileChooser::add_choice
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "id for the added choice"
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
--                 { rawDocText = Just "user-visible label for the added choice"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "options"
--           , argType = TCArray True (-1) (-1) (TBasicType TUTF8)
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "ids for the options of the choice, or %NULL for a boolean choice"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "option_labels"
--           , argType = TCArray True (-1) (-1) (TBasicType TUTF8)
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "user-visible labels for the options, must be the same length as @options"
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

foreign import ccall "gtk_file_chooser_add_choice" gtk_file_chooser_add_choice :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- id : TBasicType TUTF8
    CString ->                              -- label : TBasicType TUTF8
    Ptr CString ->                          -- options : TCArray True (-1) (-1) (TBasicType TUTF8)
    Ptr CString ->                          -- option_labels : TCArray True (-1) (-1) (TBasicType TUTF8)
    IO ()

-- | Adds a \'choice\' to the file chooser. This is typically implemented
-- as a combobox or, for boolean choices, as a checkbutton. You can select
-- a value using 'GI.Gtk.Interfaces.FileChooser.fileChooserSetChoice' before the dialog is shown,
-- and you can obtain the user-selected value in the [response](#g:signal:response) signal handler
-- using 'GI.Gtk.Interfaces.FileChooser.fileChooserGetChoice'.
-- 
-- Compare 'GI.Gtk.Interfaces.FileChooser.fileChooserSetExtraWidget'.
-- 
-- /Since: 3.22/
fileChooserAddChoice ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> T.Text
    -- ^ /@id@/: id for the added choice
    -> T.Text
    -- ^ /@label@/: user-visible label for the added choice
    -> Maybe ([T.Text])
    -- ^ /@options@/: ids for the options of the choice, or 'P.Nothing' for a boolean choice
    -> Maybe ([T.Text])
    -- ^ /@optionLabels@/: user-visible labels for the options, must be the same length as /@options@/
    -> m ()
fileChooserAddChoice chooser id label options optionLabels = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    id' <- textToCString id
    label' <- textToCString label
    maybeOptions <- case options of
        Nothing -> return nullPtr
        Just jOptions -> do
            jOptions' <- packZeroTerminatedUTF8CArray jOptions
            return jOptions'
    maybeOptionLabels <- case optionLabels of
        Nothing -> return nullPtr
        Just jOptionLabels -> do
            jOptionLabels' <- packZeroTerminatedUTF8CArray jOptionLabels
            return jOptionLabels'
    gtk_file_chooser_add_choice chooser' id' label' maybeOptions maybeOptionLabels
    touchManagedPtr chooser
    freeMem id'
    freeMem label'
    mapZeroTerminatedCArray freeMem maybeOptions
    freeMem maybeOptions
    mapZeroTerminatedCArray freeMem maybeOptionLabels
    freeMem maybeOptionLabels
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserAddChoiceMethodInfo
instance (signature ~ (T.Text -> T.Text -> Maybe ([T.Text]) -> Maybe ([T.Text]) -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserAddChoiceMethodInfo a signature where
    overloadedMethod = fileChooserAddChoice

instance O.OverloadedMethodInfo FileChooserAddChoiceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserAddChoice",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserAddChoice"
        })


#endif

-- method FileChooser::add_filter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileFilter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_add_filter" gtk_file_chooser_add_filter :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    Ptr Gtk.FileFilter.FileFilter ->        -- filter : TInterface (Name {namespace = "Gtk", name = "FileFilter"})
    IO ()

-- | Adds /@filter@/ to the list of filters that the user can select between.
-- When a filter is selected, only files that are passed by that
-- filter are displayed.
-- 
-- Note that the /@chooser@/ takes ownership of the filter, so you have to
-- ref and sink it if you want to keep a reference.
-- 
-- /Since: 2.4/
fileChooserAddFilter ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a, Gtk.FileFilter.IsFileFilter b) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> b
    -- ^ /@filter@/: a t'GI.Gtk.Objects.FileFilter.FileFilter'
    -> m ()
fileChooserAddFilter chooser filter = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    filter' <- B.ManagedPtr.disownObject filter
    gtk_file_chooser_add_filter chooser' filter'
    touchManagedPtr chooser
    touchManagedPtr filter
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserAddFilterMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsFileChooser a, Gtk.FileFilter.IsFileFilter b) => O.OverloadedMethod FileChooserAddFilterMethodInfo a signature where
    overloadedMethod = fileChooserAddFilter

instance O.OverloadedMethodInfo FileChooserAddFilterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserAddFilter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserAddFilter"
        })


#endif

-- method FileChooser::add_shortcut_folder
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "folder"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "filename of the folder to add"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_file_chooser_add_shortcut_folder" gtk_file_chooser_add_shortcut_folder :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- folder : TBasicType TFileName
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Adds a folder to be displayed with the shortcut folders in a file chooser.
-- Note that shortcut folders do not get saved, as they are provided by the
-- application.  For example, you can use this to add a
-- “\/usr\/share\/mydrawprogram\/Clipart” folder to the volume list.
-- 
-- /Since: 2.4/
fileChooserAddShortcutFolder ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> [Char]
    -- ^ /@folder@/: filename of the folder to add
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
fileChooserAddShortcutFolder chooser folder = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    folder' <- stringToCString folder
    onException (do
        _ <- propagateGError $ gtk_file_chooser_add_shortcut_folder chooser' folder'
        touchManagedPtr chooser
        freeMem folder'
        return ()
     ) (do
        freeMem folder'
     )

#if defined(ENABLE_OVERLOADING)
data FileChooserAddShortcutFolderMethodInfo
instance (signature ~ ([Char] -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserAddShortcutFolderMethodInfo a signature where
    overloadedMethod = fileChooserAddShortcutFolder

instance O.OverloadedMethodInfo FileChooserAddShortcutFolderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserAddShortcutFolder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserAddShortcutFolder"
        })


#endif

-- method FileChooser::add_shortcut_folder_uri
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "uri"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "URI of the folder to add"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_file_chooser_add_shortcut_folder_uri" gtk_file_chooser_add_shortcut_folder_uri :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- uri : TBasicType TUTF8
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Adds a folder URI to be displayed with the shortcut folders in a file
-- chooser.  Note that shortcut folders do not get saved, as they are provided
-- by the application.  For example, you can use this to add a
-- “file:\/\/\/usr\/share\/mydrawprogram\/Clipart” folder to the volume list.
-- 
-- /Since: 2.4/
fileChooserAddShortcutFolderUri ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> T.Text
    -- ^ /@uri@/: URI of the folder to add
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
fileChooserAddShortcutFolderUri chooser uri = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    uri' <- textToCString uri
    onException (do
        _ <- propagateGError $ gtk_file_chooser_add_shortcut_folder_uri chooser' uri'
        touchManagedPtr chooser
        freeMem uri'
        return ()
     ) (do
        freeMem uri'
     )

#if defined(ENABLE_OVERLOADING)
data FileChooserAddShortcutFolderUriMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserAddShortcutFolderUriMethodInfo a signature where
    overloadedMethod = fileChooserAddShortcutFolderUri

instance O.OverloadedMethodInfo FileChooserAddShortcutFolderUriMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserAddShortcutFolderUri",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserAddShortcutFolderUri"
        })


#endif

-- method FileChooser::get_action
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface
--                  Name { namespace = "Gtk" , name = "FileChooserAction" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_file_chooser_get_action" gtk_file_chooser_get_action :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO CUInt

-- | Gets the type of operation that the file chooser is performing; see
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetAction'.
-- 
-- /Since: 2.4/
fileChooserGetAction ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m Gtk.Enums.FileChooserAction
    -- ^ __Returns:__ the action that the file selector is performing
fileChooserGetAction chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_action chooser'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserGetActionMethodInfo
instance (signature ~ (m Gtk.Enums.FileChooserAction), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetActionMethodInfo a signature where
    overloadedMethod = fileChooserGetAction

instance O.OverloadedMethodInfo FileChooserGetActionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetAction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetAction"
        })


#endif

-- method FileChooser::get_choice
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the ID of the choice to get"
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

foreign import ccall "gtk_file_chooser_get_choice" gtk_file_chooser_get_choice :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- id : TBasicType TUTF8
    IO CString

-- | Gets the currently selected option in the \'choice\' with the given ID.
-- 
-- /Since: 3.22/
fileChooserGetChoice ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> T.Text
    -- ^ /@id@/: the ID of the choice to get
    -> m T.Text
    -- ^ __Returns:__ the ID of the currenly selected option
fileChooserGetChoice chooser id = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    id' <- textToCString id
    result <- gtk_file_chooser_get_choice chooser' id'
    checkUnexpectedReturnNULL "fileChooserGetChoice" result
    result' <- cstringToText result
    touchManagedPtr chooser
    freeMem id'
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserGetChoiceMethodInfo
instance (signature ~ (T.Text -> m T.Text), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetChoiceMethodInfo a signature where
    overloadedMethod = fileChooserGetChoice

instance O.OverloadedMethodInfo FileChooserGetChoiceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetChoice",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetChoice"
        })


#endif

-- method FileChooser::get_create_folders
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_get_create_folders" gtk_file_chooser_get_create_folders :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO CInt

-- | Gets whether file choser will offer to create new folders.
-- See 'GI.Gtk.Interfaces.FileChooser.fileChooserSetCreateFolders'.
-- 
-- /Since: 2.18/
fileChooserGetCreateFolders ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the Create Folder button should be displayed.
fileChooserGetCreateFolders chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_create_folders chooser'
    let result' = (/= 0) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserGetCreateFoldersMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetCreateFoldersMethodInfo a signature where
    overloadedMethod = fileChooserGetCreateFolders

instance O.OverloadedMethodInfo FileChooserGetCreateFoldersMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetCreateFolders",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetCreateFolders"
        })


#endif

-- method FileChooser::get_current_folder
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TFileName)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_file_chooser_get_current_folder" gtk_file_chooser_get_current_folder :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO CString

-- | Gets the current folder of /@chooser@/ as a local filename.
-- See 'GI.Gtk.Interfaces.FileChooser.fileChooserSetCurrentFolder'.
-- 
-- Note that this is the folder that the file chooser is currently displaying
-- (e.g. \"\/home\/username\/Documents\"), which is not the same
-- as the currently-selected folder if the chooser is in
-- 'GI.Gtk.Enums.FileChooserActionSelectFolder' mode
-- (e.g. \"\/home\/username\/Documents\/selected-folder\/\".  To get the
-- currently-selected folder in that mode, use 'GI.Gtk.Interfaces.FileChooser.fileChooserGetUri' as the
-- usual way to get the selection.
-- 
-- /Since: 2.4/
fileChooserGetCurrentFolder ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m (Maybe [Char])
    -- ^ __Returns:__ the full path of the current
    -- folder, or 'P.Nothing' if the current path cannot be represented as a local
    -- filename.  Free with 'GI.GLib.Functions.free'.  This function will also return
    -- 'P.Nothing' if the file chooser was unable to load the last folder that
    -- was requested from it; for example, as would be for calling
    -- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetCurrentFolder' on a nonexistent folder.
fileChooserGetCurrentFolder chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_current_folder chooser'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToString result'
        freeMem result'
        return result''
    touchManagedPtr chooser
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data FileChooserGetCurrentFolderMethodInfo
instance (signature ~ (m (Maybe [Char])), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetCurrentFolderMethodInfo a signature where
    overloadedMethod = fileChooserGetCurrentFolder

instance O.OverloadedMethodInfo FileChooserGetCurrentFolderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetCurrentFolder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetCurrentFolder"
        })


#endif

-- method FileChooser::get_current_folder_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gio" , name = "File" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_file_chooser_get_current_folder_file" gtk_file_chooser_get_current_folder_file :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO (Ptr Gio.File.File)

-- | Gets the current folder of /@chooser@/ as t'GI.Gio.Interfaces.File.File'.
-- See 'GI.Gtk.Interfaces.FileChooser.fileChooserGetCurrentFolderUri'.
-- 
-- /Since: 2.14/
fileChooserGetCurrentFolderFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m (Maybe Gio.File.File)
    -- ^ __Returns:__ the t'GI.Gio.Interfaces.File.File' for the current folder.
fileChooserGetCurrentFolderFile chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_current_folder_file chooser'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapObject Gio.File.File) result'
        return result''
    touchManagedPtr chooser
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data FileChooserGetCurrentFolderFileMethodInfo
instance (signature ~ (m (Maybe Gio.File.File)), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetCurrentFolderFileMethodInfo a signature where
    overloadedMethod = fileChooserGetCurrentFolderFile

instance O.OverloadedMethodInfo FileChooserGetCurrentFolderFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetCurrentFolderFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetCurrentFolderFile"
        })


#endif

-- method FileChooser::get_current_folder_uri
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_get_current_folder_uri" gtk_file_chooser_get_current_folder_uri :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO CString

-- | Gets the current folder of /@chooser@/ as an URI.
-- See 'GI.Gtk.Interfaces.FileChooser.fileChooserSetCurrentFolderUri'.
-- 
-- Note that this is the folder that the file chooser is currently displaying
-- (e.g. \"file:\/\/\/home\/username\/Documents\"), which is not the same
-- as the currently-selected folder if the chooser is in
-- 'GI.Gtk.Enums.FileChooserActionSelectFolder' mode
-- (e.g. \"file:\/\/\/home\/username\/Documents\/selected-folder\/\".  To get the
-- currently-selected folder in that mode, use 'GI.Gtk.Interfaces.FileChooser.fileChooserGetUri' as the
-- usual way to get the selection.
-- 
-- /Since: 2.4/
fileChooserGetCurrentFolderUri ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the URI for the current folder.
    -- Free with 'GI.GLib.Functions.free'.  This function will also return 'P.Nothing' if the file chooser
    -- was unable to load the last folder that was requested from it; for example,
    -- as would be for calling 'GI.Gtk.Interfaces.FileChooser.fileChooserSetCurrentFolderUri' on a
    -- nonexistent folder.
fileChooserGetCurrentFolderUri chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_current_folder_uri chooser'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        freeMem result'
        return result''
    touchManagedPtr chooser
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data FileChooserGetCurrentFolderUriMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetCurrentFolderUriMethodInfo a signature where
    overloadedMethod = fileChooserGetCurrentFolderUri

instance O.OverloadedMethodInfo FileChooserGetCurrentFolderUriMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetCurrentFolderUri",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetCurrentFolderUri"
        })


#endif

-- method FileChooser::get_current_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_get_current_name" gtk_file_chooser_get_current_name :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO CString

-- | Gets the current name in the file selector, as entered by the user in the
-- text entry for “Name”.
-- 
-- This is meant to be used in save dialogs, to get the currently typed filename
-- when the file itself does not exist yet.  For example, an application that
-- adds a custom extra widget to the file chooser for “file format” may want to
-- change the extension of the typed filename based on the chosen format, say,
-- from “.jpg” to “.png”.
-- 
-- /Since: 3.10/
fileChooserGetCurrentName ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m T.Text
    -- ^ __Returns:__ The raw text from the file chooser’s “Name” entry.  Free this with
    -- 'GI.GLib.Functions.free'.  Note that this string is not a full pathname or URI; it is
    -- whatever the contents of the entry are.  Note also that this string is in
    -- UTF-8 encoding, which is not necessarily the system’s encoding for filenames.
fileChooserGetCurrentName chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_current_name chooser'
    checkUnexpectedReturnNULL "fileChooserGetCurrentName" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserGetCurrentNameMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetCurrentNameMethodInfo a signature where
    overloadedMethod = fileChooserGetCurrentName

instance O.OverloadedMethodInfo FileChooserGetCurrentNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetCurrentName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetCurrentName"
        })


#endif

-- method FileChooser::get_do_overwrite_confirmation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_get_do_overwrite_confirmation" gtk_file_chooser_get_do_overwrite_confirmation :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO CInt

-- | Queries whether a file chooser is set to confirm for overwriting when the user
-- types a file name that already exists.
-- 
-- /Since: 2.8/
fileChooserGetDoOverwriteConfirmation ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the file chooser will present a confirmation dialog;
    -- 'P.False' otherwise.
fileChooserGetDoOverwriteConfirmation chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_do_overwrite_confirmation chooser'
    let result' = (/= 0) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserGetDoOverwriteConfirmationMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetDoOverwriteConfirmationMethodInfo a signature where
    overloadedMethod = fileChooserGetDoOverwriteConfirmation

instance O.OverloadedMethodInfo FileChooserGetDoOverwriteConfirmationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetDoOverwriteConfirmation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetDoOverwriteConfirmation"
        })


#endif

-- method FileChooser::get_extra_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_get_extra_widget" gtk_file_chooser_get_extra_widget :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO (Ptr Gtk.Widget.Widget)

-- | Gets the current extra widget; see
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetExtraWidget'.
-- 
-- /Since: 2.4/
fileChooserGetExtraWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the current extra widget, or 'P.Nothing'
fileChooserGetExtraWidget chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_extra_widget chooser'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr chooser
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data FileChooserGetExtraWidgetMethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetExtraWidgetMethodInfo a signature where
    overloadedMethod = fileChooserGetExtraWidget

instance O.OverloadedMethodInfo FileChooserGetExtraWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetExtraWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetExtraWidget"
        })


#endif

-- method FileChooser::get_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gio" , name = "File" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_file_chooser_get_file" gtk_file_chooser_get_file :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO (Ptr Gio.File.File)

-- | Gets the t'GI.Gio.Interfaces.File.File' for the currently selected file in
-- the file selector. If multiple files are selected,
-- one of the files will be returned at random.
-- 
-- If the file chooser is in folder mode, this function returns the selected
-- folder.
-- 
-- /Since: 2.14/
fileChooserGetFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m Gio.File.File
    -- ^ __Returns:__ a selected t'GI.Gio.Interfaces.File.File'. You own the returned file;
    --     use 'GI.GObject.Objects.Object.objectUnref' to release it.
fileChooserGetFile chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_file chooser'
    checkUnexpectedReturnNULL "fileChooserGetFile" result
    result' <- (wrapObject Gio.File.File) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserGetFileMethodInfo
instance (signature ~ (m Gio.File.File), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetFileMethodInfo a signature where
    overloadedMethod = fileChooserGetFile

instance O.OverloadedMethodInfo FileChooserGetFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetFile"
        })


#endif

-- method FileChooser::get_filename
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TFileName)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_file_chooser_get_filename" gtk_file_chooser_get_filename :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO CString

-- | Gets the filename for the currently selected file in
-- the file selector. The filename is returned as an absolute path. If
-- multiple files are selected, one of the filenames will be returned at
-- random.
-- 
-- If the file chooser is in folder mode, this function returns the selected
-- folder.
-- 
-- /Since: 2.4/
fileChooserGetFilename ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m (Maybe [Char])
    -- ^ __Returns:__ The currently selected filename,
    --  or 'P.Nothing' if no file is selected, or the selected file can\'t
    --  be represented with a local filename. Free with 'GI.GLib.Functions.free'.
fileChooserGetFilename chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_filename chooser'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToString result'
        freeMem result'
        return result''
    touchManagedPtr chooser
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data FileChooserGetFilenameMethodInfo
instance (signature ~ (m (Maybe [Char])), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetFilenameMethodInfo a signature where
    overloadedMethod = fileChooserGetFilename

instance O.OverloadedMethodInfo FileChooserGetFilenameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetFilename",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetFilename"
        })


#endif

-- method FileChooser::get_filenames
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TGSList (TBasicType TFileName))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_file_chooser_get_filenames" gtk_file_chooser_get_filenames :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO (Ptr (GSList CString))

-- | Lists all the selected files and subfolders in the current folder of
-- /@chooser@/. The returned names are full absolute paths. If files in the current
-- folder cannot be represented as local filenames they will be ignored. (See
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserGetUris')
-- 
-- /Since: 2.4/
fileChooserGetFilenames ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m [[Char]]
    -- ^ __Returns:__ a t'GI.GLib.Structs.SList.SList'
    --    containing the filenames of all selected files and subfolders in
    --    the current folder. Free the returned list with @/g_slist_free()/@,
    --    and the filenames with 'GI.GLib.Functions.free'.
fileChooserGetFilenames chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_filenames chooser'
    result' <- unpackGSList result
    result'' <- mapM cstringToString result'
    mapGSList freeMem result
    g_slist_free result
    touchManagedPtr chooser
    return result''

#if defined(ENABLE_OVERLOADING)
data FileChooserGetFilenamesMethodInfo
instance (signature ~ (m [[Char]]), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetFilenamesMethodInfo a signature where
    overloadedMethod = fileChooserGetFilenames

instance O.OverloadedMethodInfo FileChooserGetFilenamesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetFilenames",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetFilenames"
        })


#endif

-- method FileChooser::get_files
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TGSList (TInterface Name { namespace = "Gio" , name = "File" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_file_chooser_get_files" gtk_file_chooser_get_files :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO (Ptr (GSList (Ptr Gio.File.File)))

-- | Lists all the selected files and subfolders in the current folder of /@chooser@/
-- as t'GI.Gio.Interfaces.File.File'. An internal function, see 'GI.Gtk.Interfaces.FileChooser.fileChooserGetUris'.
-- 
-- /Since: 2.14/
fileChooserGetFiles ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m [Gio.File.File]
    -- ^ __Returns:__ a t'GI.GLib.Structs.SList.SList'
    --   containing a t'GI.Gio.Interfaces.File.File' for each selected file and subfolder in the
    --   current folder.  Free the returned list with @/g_slist_free()/@, and
    --   the files with 'GI.GObject.Objects.Object.objectUnref'.
fileChooserGetFiles chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_files chooser'
    result' <- unpackGSList result
    result'' <- mapM (wrapObject Gio.File.File) result'
    g_slist_free result
    touchManagedPtr chooser
    return result''

#if defined(ENABLE_OVERLOADING)
data FileChooserGetFilesMethodInfo
instance (signature ~ (m [Gio.File.File]), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetFilesMethodInfo a signature where
    overloadedMethod = fileChooserGetFiles

instance O.OverloadedMethodInfo FileChooserGetFilesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetFiles",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetFiles"
        })


#endif

-- method FileChooser::get_filter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "FileFilter" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_file_chooser_get_filter" gtk_file_chooser_get_filter :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO (Ptr Gtk.FileFilter.FileFilter)

-- | Gets the current filter; see 'GI.Gtk.Interfaces.FileChooser.fileChooserSetFilter'.
-- 
-- /Since: 2.4/
fileChooserGetFilter ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m (Maybe Gtk.FileFilter.FileFilter)
    -- ^ __Returns:__ the current filter, or 'P.Nothing'
fileChooserGetFilter chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_filter chooser'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.FileFilter.FileFilter) result'
        return result''
    touchManagedPtr chooser
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data FileChooserGetFilterMethodInfo
instance (signature ~ (m (Maybe Gtk.FileFilter.FileFilter)), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetFilterMethodInfo a signature where
    overloadedMethod = fileChooserGetFilter

instance O.OverloadedMethodInfo FileChooserGetFilterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetFilter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetFilter"
        })


#endif

-- method FileChooser::get_local_only
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_get_local_only" gtk_file_chooser_get_local_only :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO CInt

-- | Gets whether only local files can be selected in the
-- file selector. See 'GI.Gtk.Interfaces.FileChooser.fileChooserSetLocalOnly'
-- 
-- /Since: 2.4/
fileChooserGetLocalOnly ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if only local files can be selected.
fileChooserGetLocalOnly chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_local_only chooser'
    let result' = (/= 0) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserGetLocalOnlyMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetLocalOnlyMethodInfo a signature where
    overloadedMethod = fileChooserGetLocalOnly

instance O.OverloadedMethodInfo FileChooserGetLocalOnlyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetLocalOnly",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetLocalOnly"
        })


#endif

-- method FileChooser::get_preview_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gio" , name = "File" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_file_chooser_get_preview_file" gtk_file_chooser_get_preview_file :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO (Ptr Gio.File.File)

-- | Gets the t'GI.Gio.Interfaces.File.File' that should be previewed in a custom preview
-- Internal function, see 'GI.Gtk.Interfaces.FileChooser.fileChooserGetPreviewUri'.
-- 
-- /Since: 2.14/
fileChooserGetPreviewFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m (Maybe Gio.File.File)
    -- ^ __Returns:__ the t'GI.Gio.Interfaces.File.File' for the file to preview,
    --     or 'P.Nothing' if no file is selected. Free with 'GI.GObject.Objects.Object.objectUnref'.
fileChooserGetPreviewFile chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_preview_file chooser'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapObject Gio.File.File) result'
        return result''
    touchManagedPtr chooser
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data FileChooserGetPreviewFileMethodInfo
instance (signature ~ (m (Maybe Gio.File.File)), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetPreviewFileMethodInfo a signature where
    overloadedMethod = fileChooserGetPreviewFile

instance O.OverloadedMethodInfo FileChooserGetPreviewFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetPreviewFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetPreviewFile"
        })


#endif

-- method FileChooser::get_preview_filename
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TFileName)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_file_chooser_get_preview_filename" gtk_file_chooser_get_preview_filename :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO CString

-- | Gets the filename that should be previewed in a custom preview
-- widget. See 'GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidget'.
-- 
-- /Since: 2.4/
fileChooserGetPreviewFilename ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m (Maybe [Char])
    -- ^ __Returns:__ the filename to preview, or 'P.Nothing' if
    --  no file is selected, or if the selected file cannot be represented
    --  as a local filename. Free with 'GI.GLib.Functions.free'
fileChooserGetPreviewFilename chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_preview_filename chooser'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToString result'
        freeMem result'
        return result''
    touchManagedPtr chooser
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data FileChooserGetPreviewFilenameMethodInfo
instance (signature ~ (m (Maybe [Char])), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetPreviewFilenameMethodInfo a signature where
    overloadedMethod = fileChooserGetPreviewFilename

instance O.OverloadedMethodInfo FileChooserGetPreviewFilenameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetPreviewFilename",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetPreviewFilename"
        })


#endif

-- method FileChooser::get_preview_uri
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_get_preview_uri" gtk_file_chooser_get_preview_uri :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO CString

-- | Gets the URI that should be previewed in a custom preview
-- widget. See 'GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidget'.
-- 
-- /Since: 2.4/
fileChooserGetPreviewUri ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the URI for the file to preview,
    --     or 'P.Nothing' if no file is selected. Free with 'GI.GLib.Functions.free'.
fileChooserGetPreviewUri chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_preview_uri chooser'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        freeMem result'
        return result''
    touchManagedPtr chooser
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data FileChooserGetPreviewUriMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetPreviewUriMethodInfo a signature where
    overloadedMethod = fileChooserGetPreviewUri

instance O.OverloadedMethodInfo FileChooserGetPreviewUriMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetPreviewUri",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetPreviewUri"
        })


#endif

-- method FileChooser::get_preview_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_get_preview_widget" gtk_file_chooser_get_preview_widget :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO (Ptr Gtk.Widget.Widget)

-- | Gets the current preview widget; see
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidget'.
-- 
-- /Since: 2.4/
fileChooserGetPreviewWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ the current preview widget, or 'P.Nothing'
fileChooserGetPreviewWidget chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_preview_widget chooser'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr chooser
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data FileChooserGetPreviewWidgetMethodInfo
instance (signature ~ (m (Maybe Gtk.Widget.Widget)), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetPreviewWidgetMethodInfo a signature where
    overloadedMethod = fileChooserGetPreviewWidget

instance O.OverloadedMethodInfo FileChooserGetPreviewWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetPreviewWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetPreviewWidget"
        })


#endif

-- method FileChooser::get_preview_widget_active
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_get_preview_widget_active" gtk_file_chooser_get_preview_widget_active :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO CInt

-- | Gets whether the preview widget set by 'GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidget'
-- should be shown for the current filename. See
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidgetActive'.
-- 
-- /Since: 2.4/
fileChooserGetPreviewWidgetActive ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the preview widget is active for the current filename.
fileChooserGetPreviewWidgetActive chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_preview_widget_active chooser'
    let result' = (/= 0) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserGetPreviewWidgetActiveMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetPreviewWidgetActiveMethodInfo a signature where
    overloadedMethod = fileChooserGetPreviewWidgetActive

instance O.OverloadedMethodInfo FileChooserGetPreviewWidgetActiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetPreviewWidgetActive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetPreviewWidgetActive"
        })


#endif

-- method FileChooser::get_select_multiple
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_get_select_multiple" gtk_file_chooser_get_select_multiple :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO CInt

-- | Gets whether multiple files can be selected in the file
-- selector. See 'GI.Gtk.Interfaces.FileChooser.fileChooserSetSelectMultiple'.
-- 
-- /Since: 2.4/
fileChooserGetSelectMultiple ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if multiple files can be selected.
fileChooserGetSelectMultiple chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_select_multiple chooser'
    let result' = (/= 0) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserGetSelectMultipleMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetSelectMultipleMethodInfo a signature where
    overloadedMethod = fileChooserGetSelectMultiple

instance O.OverloadedMethodInfo FileChooserGetSelectMultipleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetSelectMultiple",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetSelectMultiple"
        })


#endif

-- method FileChooser::get_show_hidden
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_get_show_hidden" gtk_file_chooser_get_show_hidden :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO CInt

-- | Gets whether hidden files and folders are displayed in the file selector.
-- See 'GI.Gtk.Interfaces.FileChooser.fileChooserSetShowHidden'.
-- 
-- /Since: 2.6/
fileChooserGetShowHidden ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if hidden files and folders are displayed.
fileChooserGetShowHidden chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_show_hidden chooser'
    let result' = (/= 0) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserGetShowHiddenMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetShowHiddenMethodInfo a signature where
    overloadedMethod = fileChooserGetShowHidden

instance O.OverloadedMethodInfo FileChooserGetShowHiddenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetShowHidden",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetShowHidden"
        })


#endif

-- method FileChooser::get_uri
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_get_uri" gtk_file_chooser_get_uri :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO CString

-- | Gets the URI for the currently selected file in
-- the file selector. If multiple files are selected,
-- one of the filenames will be returned at random.
-- 
-- If the file chooser is in folder mode, this function returns the selected
-- folder.
-- 
-- /Since: 2.4/
fileChooserGetUri ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ The currently selected URI, or 'P.Nothing'
    --    if no file is selected. If 'GI.Gtk.Interfaces.FileChooser.fileChooserSetLocalOnly' is set to
    --    'P.True' (the default) a local URI will be returned for any FUSE locations.
    --    Free with 'GI.GLib.Functions.free'
fileChooserGetUri chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_uri chooser'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        freeMem result'
        return result''
    touchManagedPtr chooser
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data FileChooserGetUriMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetUriMethodInfo a signature where
    overloadedMethod = fileChooserGetUri

instance O.OverloadedMethodInfo FileChooserGetUriMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetUri",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetUri"
        })


#endif

-- method FileChooser::get_uris
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TGSList (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_file_chooser_get_uris" gtk_file_chooser_get_uris :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO (Ptr (GSList CString))

-- | Lists all the selected files and subfolders in the current folder of
-- /@chooser@/. The returned names are full absolute URIs.
-- 
-- /Since: 2.4/
fileChooserGetUris ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m [T.Text]
    -- ^ __Returns:__ a t'GI.GLib.Structs.SList.SList' containing the URIs of all selected
    --   files and subfolders in the current folder. Free the returned list
    --   with @/g_slist_free()/@, and the filenames with 'GI.GLib.Functions.free'.
fileChooserGetUris chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_uris chooser'
    result' <- unpackGSList result
    result'' <- mapM cstringToText result'
    mapGSList freeMem result
    g_slist_free result
    touchManagedPtr chooser
    return result''

#if defined(ENABLE_OVERLOADING)
data FileChooserGetUrisMethodInfo
instance (signature ~ (m [T.Text]), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetUrisMethodInfo a signature where
    overloadedMethod = fileChooserGetUris

instance O.OverloadedMethodInfo FileChooserGetUrisMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetUris",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetUris"
        })


#endif

-- method FileChooser::get_use_preview_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_get_use_preview_label" gtk_file_chooser_get_use_preview_label :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO CInt

-- | Gets whether a stock label should be drawn with the name of the previewed
-- file.  See 'GI.Gtk.Interfaces.FileChooser.fileChooserSetUsePreviewLabel'.
fileChooserGetUsePreviewLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the file chooser is set to display a label with the
    -- name of the previewed file, 'P.False' otherwise.
fileChooserGetUsePreviewLabel chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_get_use_preview_label chooser'
    let result' = (/= 0) result
    touchManagedPtr chooser
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserGetUsePreviewLabelMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserGetUsePreviewLabelMethodInfo a signature where
    overloadedMethod = fileChooserGetUsePreviewLabel

instance O.OverloadedMethodInfo FileChooserGetUsePreviewLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserGetUsePreviewLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetUsePreviewLabel"
        })


#endif

-- method FileChooser::list_filters
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TGSList
--                  (TInterface Name { namespace = "Gtk" , name = "FileFilter" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_file_chooser_list_filters" gtk_file_chooser_list_filters :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO (Ptr (GSList (Ptr Gtk.FileFilter.FileFilter)))

-- | Lists the current set of user-selectable filters; see
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserAddFilter', 'GI.Gtk.Interfaces.FileChooser.fileChooserRemoveFilter'.
-- 
-- /Since: 2.4/
fileChooserListFilters ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m [Gtk.FileFilter.FileFilter]
    -- ^ __Returns:__ a
    --  t'GI.GLib.Structs.SList.SList' containing the current set of user selectable filters. The
    --  contents of the list are owned by GTK+, but you must free the list
    --  itself with @/g_slist_free()/@ when you are done with it.
fileChooserListFilters chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_list_filters chooser'
    result' <- unpackGSList result
    result'' <- mapM (newObject Gtk.FileFilter.FileFilter) result'
    g_slist_free result
    touchManagedPtr chooser
    return result''

#if defined(ENABLE_OVERLOADING)
data FileChooserListFiltersMethodInfo
instance (signature ~ (m [Gtk.FileFilter.FileFilter]), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserListFiltersMethodInfo a signature where
    overloadedMethod = fileChooserListFilters

instance O.OverloadedMethodInfo FileChooserListFiltersMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserListFilters",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserListFilters"
        })


#endif

-- method FileChooser::list_shortcut_folder_uris
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TGSList (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_file_chooser_list_shortcut_folder_uris" gtk_file_chooser_list_shortcut_folder_uris :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO (Ptr (GSList CString))

-- | Queries the list of shortcut folders in the file chooser, as set by
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserAddShortcutFolderUri'.
-- 
-- /Since: 2.4/
fileChooserListShortcutFolderUris ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m [T.Text]
    -- ^ __Returns:__ A list of
    -- folder URIs, or 'P.Nothing' if there are no shortcut folders.  Free the
    -- returned list with @/g_slist_free()/@, and the URIs with 'GI.GLib.Functions.free'.
fileChooserListShortcutFolderUris chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_list_shortcut_folder_uris chooser'
    result' <- unpackGSList result
    result'' <- mapM cstringToText result'
    mapGSList freeMem result
    g_slist_free result
    touchManagedPtr chooser
    return result''

#if defined(ENABLE_OVERLOADING)
data FileChooserListShortcutFolderUrisMethodInfo
instance (signature ~ (m [T.Text]), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserListShortcutFolderUrisMethodInfo a signature where
    overloadedMethod = fileChooserListShortcutFolderUris

instance O.OverloadedMethodInfo FileChooserListShortcutFolderUrisMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserListShortcutFolderUris",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserListShortcutFolderUris"
        })


#endif

-- method FileChooser::list_shortcut_folders
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TGSList (TBasicType TFileName))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_file_chooser_list_shortcut_folders" gtk_file_chooser_list_shortcut_folders :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO (Ptr (GSList CString))

-- | Queries the list of shortcut folders in the file chooser, as set by
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserAddShortcutFolder'.
-- 
-- /Since: 2.4/
fileChooserListShortcutFolders ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m [[Char]]
    -- ^ __Returns:__ A list
    -- of folder filenames, or 'P.Nothing' if there are no shortcut folders.
    -- Free the returned list with @/g_slist_free()/@, and the filenames with
    -- 'GI.GLib.Functions.free'.
fileChooserListShortcutFolders chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    result <- gtk_file_chooser_list_shortcut_folders chooser'
    result' <- unpackGSList result
    result'' <- mapM cstringToString result'
    mapGSList freeMem result
    g_slist_free result
    touchManagedPtr chooser
    return result''

#if defined(ENABLE_OVERLOADING)
data FileChooserListShortcutFoldersMethodInfo
instance (signature ~ (m [[Char]]), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserListShortcutFoldersMethodInfo a signature where
    overloadedMethod = fileChooserListShortcutFolders

instance O.OverloadedMethodInfo FileChooserListShortcutFoldersMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserListShortcutFolders",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserListShortcutFolders"
        })


#endif

-- method FileChooser::remove_choice
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the ID of the choice to remove"
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

foreign import ccall "gtk_file_chooser_remove_choice" gtk_file_chooser_remove_choice :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- id : TBasicType TUTF8
    IO ()

-- | Removes a \'choice\' that has been added with 'GI.Gtk.Interfaces.FileChooser.fileChooserAddChoice'.
-- 
-- /Since: 3.22/
fileChooserRemoveChoice ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> T.Text
    -- ^ /@id@/: the ID of the choice to remove
    -> m ()
fileChooserRemoveChoice chooser id = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    id' <- textToCString id
    gtk_file_chooser_remove_choice chooser' id'
    touchManagedPtr chooser
    freeMem id'
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserRemoveChoiceMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserRemoveChoiceMethodInfo a signature where
    overloadedMethod = fileChooserRemoveChoice

instance O.OverloadedMethodInfo FileChooserRemoveChoiceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserRemoveChoice",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserRemoveChoice"
        })


#endif

-- method FileChooser::remove_filter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileFilter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_remove_filter" gtk_file_chooser_remove_filter :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    Ptr Gtk.FileFilter.FileFilter ->        -- filter : TInterface (Name {namespace = "Gtk", name = "FileFilter"})
    IO ()

-- | Removes /@filter@/ from the list of filters that the user can select between.
-- 
-- /Since: 2.4/
fileChooserRemoveFilter ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a, Gtk.FileFilter.IsFileFilter b) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> b
    -- ^ /@filter@/: a t'GI.Gtk.Objects.FileFilter.FileFilter'
    -> m ()
fileChooserRemoveFilter chooser filter = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    filter' <- unsafeManagedPtrCastPtr filter
    gtk_file_chooser_remove_filter chooser' filter'
    touchManagedPtr chooser
    touchManagedPtr filter
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserRemoveFilterMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsFileChooser a, Gtk.FileFilter.IsFileFilter b) => O.OverloadedMethod FileChooserRemoveFilterMethodInfo a signature where
    overloadedMethod = fileChooserRemoveFilter

instance O.OverloadedMethodInfo FileChooserRemoveFilterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserRemoveFilter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserRemoveFilter"
        })


#endif

-- method FileChooser::remove_shortcut_folder
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "folder"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "filename of the folder to remove"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_file_chooser_remove_shortcut_folder" gtk_file_chooser_remove_shortcut_folder :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- folder : TBasicType TFileName
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Removes a folder from a file chooser’s list of shortcut folders.
-- 
-- /Since: 2.4/
fileChooserRemoveShortcutFolder ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> [Char]
    -- ^ /@folder@/: filename of the folder to remove
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
fileChooserRemoveShortcutFolder chooser folder = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    folder' <- stringToCString folder
    onException (do
        _ <- propagateGError $ gtk_file_chooser_remove_shortcut_folder chooser' folder'
        touchManagedPtr chooser
        freeMem folder'
        return ()
     ) (do
        freeMem folder'
     )

#if defined(ENABLE_OVERLOADING)
data FileChooserRemoveShortcutFolderMethodInfo
instance (signature ~ ([Char] -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserRemoveShortcutFolderMethodInfo a signature where
    overloadedMethod = fileChooserRemoveShortcutFolder

instance O.OverloadedMethodInfo FileChooserRemoveShortcutFolderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserRemoveShortcutFolder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserRemoveShortcutFolder"
        })


#endif

-- method FileChooser::remove_shortcut_folder_uri
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "uri"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "URI of the folder to remove"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_file_chooser_remove_shortcut_folder_uri" gtk_file_chooser_remove_shortcut_folder_uri :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- uri : TBasicType TUTF8
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Removes a folder URI from a file chooser’s list of shortcut folders.
-- 
-- /Since: 2.4/
fileChooserRemoveShortcutFolderUri ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> T.Text
    -- ^ /@uri@/: URI of the folder to remove
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
fileChooserRemoveShortcutFolderUri chooser uri = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    uri' <- textToCString uri
    onException (do
        _ <- propagateGError $ gtk_file_chooser_remove_shortcut_folder_uri chooser' uri'
        touchManagedPtr chooser
        freeMem uri'
        return ()
     ) (do
        freeMem uri'
     )

#if defined(ENABLE_OVERLOADING)
data FileChooserRemoveShortcutFolderUriMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserRemoveShortcutFolderUriMethodInfo a signature where
    overloadedMethod = fileChooserRemoveShortcutFolderUri

instance O.OverloadedMethodInfo FileChooserRemoveShortcutFolderUriMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserRemoveShortcutFolderUri",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserRemoveShortcutFolderUri"
        })


#endif

-- method FileChooser::select_all
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_select_all" gtk_file_chooser_select_all :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO ()

-- | Selects all the files in the current folder of a file chooser.
-- 
-- /Since: 2.4/
fileChooserSelectAll ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m ()
fileChooserSelectAll chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    gtk_file_chooser_select_all chooser'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserSelectAllMethodInfo
instance (signature ~ (m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSelectAllMethodInfo a signature where
    overloadedMethod = fileChooserSelectAll

instance O.OverloadedMethodInfo FileChooserSelectAllMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSelectAll",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSelectAll"
        })


#endif

-- method FileChooser::select_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "file"
--           , argType = TInterface Name { namespace = "Gio" , name = "File" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the file to select" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : True
-- Skip return : False

foreign import ccall "gtk_file_chooser_select_file" gtk_file_chooser_select_file :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    Ptr Gio.File.File ->                    -- file : TInterface (Name {namespace = "Gio", name = "File"})
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Selects the file referred to by /@file@/. An internal function. See
-- @/_gtk_file_chooser_select_uri()/@.
-- 
-- /Since: 2.14/
fileChooserSelectFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a, Gio.File.IsFile b) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> b
    -- ^ /@file@/: the file to select
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
fileChooserSelectFile chooser file = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    file' <- unsafeManagedPtrCastPtr file
    onException (do
        _ <- propagateGError $ gtk_file_chooser_select_file chooser' file'
        touchManagedPtr chooser
        touchManagedPtr file
        return ()
     ) (do
        return ()
     )

#if defined(ENABLE_OVERLOADING)
data FileChooserSelectFileMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsFileChooser a, Gio.File.IsFile b) => O.OverloadedMethod FileChooserSelectFileMethodInfo a signature where
    overloadedMethod = fileChooserSelectFile

instance O.OverloadedMethodInfo FileChooserSelectFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSelectFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSelectFile"
        })


#endif

-- method FileChooser::select_filename
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filename"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the filename to select"
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

foreign import ccall "gtk_file_chooser_select_filename" gtk_file_chooser_select_filename :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- filename : TBasicType TFileName
    IO CInt

-- | Selects a filename. If the file name isn’t in the current
-- folder of /@chooser@/, then the current folder of /@chooser@/ will
-- be changed to the folder containing /@filename@/.
-- 
-- /Since: 2.4/
fileChooserSelectFilename ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> [Char]
    -- ^ /@filename@/: the filename to select
    -> m Bool
    -- ^ __Returns:__ Not useful.
    -- 
    -- See also: 'GI.Gtk.Interfaces.FileChooser.fileChooserSetFilename'
fileChooserSelectFilename chooser filename = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    filename' <- stringToCString filename
    result <- gtk_file_chooser_select_filename chooser' filename'
    let result' = (/= 0) result
    touchManagedPtr chooser
    freeMem filename'
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserSelectFilenameMethodInfo
instance (signature ~ ([Char] -> m Bool), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSelectFilenameMethodInfo a signature where
    overloadedMethod = fileChooserSelectFilename

instance O.OverloadedMethodInfo FileChooserSelectFilenameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSelectFilename",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSelectFilename"
        })


#endif

-- method FileChooser::select_uri
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "uri"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the URI to select" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_select_uri" gtk_file_chooser_select_uri :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- uri : TBasicType TUTF8
    IO CInt

-- | Selects the file to by /@uri@/. If the URI doesn’t refer to a
-- file in the current folder of /@chooser@/, then the current folder of
-- /@chooser@/ will be changed to the folder containing /@filename@/.
-- 
-- /Since: 2.4/
fileChooserSelectUri ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> T.Text
    -- ^ /@uri@/: the URI to select
    -> m Bool
    -- ^ __Returns:__ Not useful.
fileChooserSelectUri chooser uri = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    uri' <- textToCString uri
    result <- gtk_file_chooser_select_uri chooser' uri'
    let result' = (/= 0) result
    touchManagedPtr chooser
    freeMem uri'
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserSelectUriMethodInfo
instance (signature ~ (T.Text -> m Bool), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSelectUriMethodInfo a signature where
    overloadedMethod = fileChooserSelectUri

instance O.OverloadedMethodInfo FileChooserSelectUriMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSelectUri",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSelectUri"
        })


#endif

-- method FileChooser::set_action
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "action"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooserAction" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the action that the file selector is performing"
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

foreign import ccall "gtk_file_chooser_set_action" gtk_file_chooser_set_action :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CUInt ->                                -- action : TInterface (Name {namespace = "Gtk", name = "FileChooserAction"})
    IO ()

-- | Sets the type of operation that the chooser is performing; the
-- user interface is adapted to suit the selected action. For example,
-- an option to create a new folder might be shown if the action is
-- 'GI.Gtk.Enums.FileChooserActionSave' but not if the action is
-- 'GI.Gtk.Enums.FileChooserActionOpen'.
-- 
-- /Since: 2.4/
fileChooserSetAction ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> Gtk.Enums.FileChooserAction
    -- ^ /@action@/: the action that the file selector is performing
    -> m ()
fileChooserSetAction chooser action = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    let action' = (fromIntegral . fromEnum) action
    gtk_file_chooser_set_action chooser' action'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserSetActionMethodInfo
instance (signature ~ (Gtk.Enums.FileChooserAction -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSetActionMethodInfo a signature where
    overloadedMethod = fileChooserSetAction

instance O.OverloadedMethodInfo FileChooserSetActionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetAction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetAction"
        })


#endif

-- method FileChooser::set_choice
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the ID of the choice to set"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "option"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the ID of the option to select"
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

foreign import ccall "gtk_file_chooser_set_choice" gtk_file_chooser_set_choice :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- id : TBasicType TUTF8
    CString ->                              -- option : TBasicType TUTF8
    IO ()

-- | Selects an option in a \'choice\' that has been added with
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserAddChoice'. For a boolean choice, the
-- possible options are \"true\" and \"false\".
-- 
-- /Since: 3.22/
fileChooserSetChoice ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> T.Text
    -- ^ /@id@/: the ID of the choice to set
    -> T.Text
    -- ^ /@option@/: the ID of the option to select
    -> m ()
fileChooserSetChoice chooser id option = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    id' <- textToCString id
    option' <- textToCString option
    gtk_file_chooser_set_choice chooser' id' option'
    touchManagedPtr chooser
    freeMem id'
    freeMem option'
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserSetChoiceMethodInfo
instance (signature ~ (T.Text -> T.Text -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSetChoiceMethodInfo a signature where
    overloadedMethod = fileChooserSetChoice

instance O.OverloadedMethodInfo FileChooserSetChoiceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetChoice",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetChoice"
        })


#endif

-- method FileChooser::set_create_folders
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "create_folders"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if the Create Folder button should be displayed"
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

foreign import ccall "gtk_file_chooser_set_create_folders" gtk_file_chooser_set_create_folders :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CInt ->                                 -- create_folders : TBasicType TBoolean
    IO ()

-- | Sets whether file choser will offer to create new folders.
-- This is only relevant if the action is not set to be
-- 'GI.Gtk.Enums.FileChooserActionOpen'.
-- 
-- /Since: 2.18/
fileChooserSetCreateFolders ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> Bool
    -- ^ /@createFolders@/: 'P.True' if the Create Folder button should be displayed
    -> m ()
fileChooserSetCreateFolders chooser createFolders = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    let createFolders' = (fromIntegral . fromEnum) createFolders
    gtk_file_chooser_set_create_folders chooser' createFolders'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserSetCreateFoldersMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSetCreateFoldersMethodInfo a signature where
    overloadedMethod = fileChooserSetCreateFolders

instance O.OverloadedMethodInfo FileChooserSetCreateFoldersMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetCreateFolders",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetCreateFolders"
        })


#endif

-- method FileChooser::set_current_folder
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filename"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the full path of the new current folder"
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

foreign import ccall "gtk_file_chooser_set_current_folder" gtk_file_chooser_set_current_folder :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- filename : TBasicType TFileName
    IO CInt

-- | Sets the current folder for /@chooser@/ from a local filename.
-- The user will be shown the full contents of the current folder,
-- plus user interface elements for navigating to other folders.
-- 
-- In general, you should not use this function.  See the
-- [section on setting up a file chooser dialog][gtkfilechooserdialog-setting-up]
-- for the rationale behind this.
-- 
-- /Since: 2.4/
fileChooserSetCurrentFolder ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> [Char]
    -- ^ /@filename@/: the full path of the new current folder
    -> m Bool
    -- ^ __Returns:__ Not useful.
fileChooserSetCurrentFolder chooser filename = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    filename' <- stringToCString filename
    result <- gtk_file_chooser_set_current_folder chooser' filename'
    let result' = (/= 0) result
    touchManagedPtr chooser
    freeMem filename'
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserSetCurrentFolderMethodInfo
instance (signature ~ ([Char] -> m Bool), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSetCurrentFolderMethodInfo a signature where
    overloadedMethod = fileChooserSetCurrentFolder

instance O.OverloadedMethodInfo FileChooserSetCurrentFolderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetCurrentFolder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetCurrentFolder"
        })


#endif

-- method FileChooser::set_current_folder_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "file"
--           , argType = TInterface Name { namespace = "Gio" , name = "File" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GFile for the new folder"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_file_chooser_set_current_folder_file" gtk_file_chooser_set_current_folder_file :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    Ptr Gio.File.File ->                    -- file : TInterface (Name {namespace = "Gio", name = "File"})
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Sets the current folder for /@chooser@/ from a t'GI.Gio.Interfaces.File.File'.
-- Internal function, see 'GI.Gtk.Interfaces.FileChooser.fileChooserSetCurrentFolderUri'.
-- 
-- /Since: 2.14/
fileChooserSetCurrentFolderFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a, Gio.File.IsFile b) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> b
    -- ^ /@file@/: the t'GI.Gio.Interfaces.File.File' for the new folder
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
fileChooserSetCurrentFolderFile chooser file = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    file' <- unsafeManagedPtrCastPtr file
    onException (do
        _ <- propagateGError $ gtk_file_chooser_set_current_folder_file chooser' file'
        touchManagedPtr chooser
        touchManagedPtr file
        return ()
     ) (do
        return ()
     )

#if defined(ENABLE_OVERLOADING)
data FileChooserSetCurrentFolderFileMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsFileChooser a, Gio.File.IsFile b) => O.OverloadedMethod FileChooserSetCurrentFolderFileMethodInfo a signature where
    overloadedMethod = fileChooserSetCurrentFolderFile

instance O.OverloadedMethodInfo FileChooserSetCurrentFolderFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetCurrentFolderFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetCurrentFolderFile"
        })


#endif

-- method FileChooser::set_current_folder_uri
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "uri"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the URI for the new current folder"
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

foreign import ccall "gtk_file_chooser_set_current_folder_uri" gtk_file_chooser_set_current_folder_uri :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- uri : TBasicType TUTF8
    IO CInt

-- | Sets the current folder for /@chooser@/ from an URI.
-- The user will be shown the full contents of the current folder,
-- plus user interface elements for navigating to other folders.
-- 
-- In general, you should not use this function.  See the
-- [section on setting up a file chooser dialog][gtkfilechooserdialog-setting-up]
-- for the rationale behind this.
-- 
-- /Since: 2.4/
fileChooserSetCurrentFolderUri ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> T.Text
    -- ^ /@uri@/: the URI for the new current folder
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the folder could be changed successfully, 'P.False'
    -- otherwise.
fileChooserSetCurrentFolderUri chooser uri = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    uri' <- textToCString uri
    result <- gtk_file_chooser_set_current_folder_uri chooser' uri'
    let result' = (/= 0) result
    touchManagedPtr chooser
    freeMem uri'
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserSetCurrentFolderUriMethodInfo
instance (signature ~ (T.Text -> m Bool), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSetCurrentFolderUriMethodInfo a signature where
    overloadedMethod = fileChooserSetCurrentFolderUri

instance O.OverloadedMethodInfo FileChooserSetCurrentFolderUriMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetCurrentFolderUri",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetCurrentFolderUri"
        })


#endif

-- method FileChooser::set_current_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the filename to use, as a UTF-8 string"
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

foreign import ccall "gtk_file_chooser_set_current_name" gtk_file_chooser_set_current_name :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Sets the current name in the file selector, as if entered
-- by the user. Note that the name passed in here is a UTF-8
-- string rather than a filename. This function is meant for
-- such uses as a suggested name in a “Save As...” dialog.  You can
-- pass “Untitled.doc” or a similarly suitable suggestion for the /@name@/.
-- 
-- If you want to preselect a particular existing file, you should use
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetFilename' or 'GI.Gtk.Interfaces.FileChooser.fileChooserSetUri' instead.
-- Please see the documentation for those functions for an example of using
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetCurrentName' as well.
-- 
-- /Since: 2.4/
fileChooserSetCurrentName ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> T.Text
    -- ^ /@name@/: the filename to use, as a UTF-8 string
    -> m ()
fileChooserSetCurrentName chooser name = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    name' <- textToCString name
    gtk_file_chooser_set_current_name chooser' name'
    touchManagedPtr chooser
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserSetCurrentNameMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSetCurrentNameMethodInfo a signature where
    overloadedMethod = fileChooserSetCurrentName

instance O.OverloadedMethodInfo FileChooserSetCurrentNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetCurrentName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetCurrentName"
        })


#endif

-- method FileChooser::set_do_overwrite_confirmation
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "do_overwrite_confirmation"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "whether to confirm overwriting in save mode"
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

foreign import ccall "gtk_file_chooser_set_do_overwrite_confirmation" gtk_file_chooser_set_do_overwrite_confirmation :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CInt ->                                 -- do_overwrite_confirmation : TBasicType TBoolean
    IO ()

-- | Sets whether a file chooser in 'GI.Gtk.Enums.FileChooserActionSave' mode will present
-- a confirmation dialog if the user types a file name that already exists.  This
-- is 'P.False' by default.
-- 
-- If set to 'P.True', the /@chooser@/ will emit the
-- [FileChooser::confirmOverwrite]("GI.Gtk.Interfaces.FileChooser#g:signal:confirmOverwrite") signal when appropriate.
-- 
-- If all you need is the stock confirmation dialog, set this property to 'P.True'.
-- You can override the way confirmation is done by actually handling the
-- [FileChooser::confirmOverwrite]("GI.Gtk.Interfaces.FileChooser#g:signal:confirmOverwrite") signal; please refer to its documentation
-- for the details.
-- 
-- /Since: 2.8/
fileChooserSetDoOverwriteConfirmation ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> Bool
    -- ^ /@doOverwriteConfirmation@/: whether to confirm overwriting in save mode
    -> m ()
fileChooserSetDoOverwriteConfirmation chooser doOverwriteConfirmation = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    let doOverwriteConfirmation' = (fromIntegral . fromEnum) doOverwriteConfirmation
    gtk_file_chooser_set_do_overwrite_confirmation chooser' doOverwriteConfirmation'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserSetDoOverwriteConfirmationMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSetDoOverwriteConfirmationMethodInfo a signature where
    overloadedMethod = fileChooserSetDoOverwriteConfirmation

instance O.OverloadedMethodInfo FileChooserSetDoOverwriteConfirmationMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetDoOverwriteConfirmation",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetDoOverwriteConfirmation"
        })


#endif

-- method FileChooser::set_extra_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "extra_widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "widget for extra options"
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

foreign import ccall "gtk_file_chooser_set_extra_widget" gtk_file_chooser_set_extra_widget :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    Ptr Gtk.Widget.Widget ->                -- extra_widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Sets an application-supplied widget to provide extra options to the user.
-- 
-- /Since: 2.4/
fileChooserSetExtraWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> b
    -- ^ /@extraWidget@/: widget for extra options
    -> m ()
fileChooserSetExtraWidget chooser extraWidget = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    extraWidget' <- unsafeManagedPtrCastPtr extraWidget
    gtk_file_chooser_set_extra_widget chooser' extraWidget'
    touchManagedPtr chooser
    touchManagedPtr extraWidget
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserSetExtraWidgetMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsFileChooser a, Gtk.Widget.IsWidget b) => O.OverloadedMethod FileChooserSetExtraWidgetMethodInfo a signature where
    overloadedMethod = fileChooserSetExtraWidget

instance O.OverloadedMethodInfo FileChooserSetExtraWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetExtraWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetExtraWidget"
        })


#endif

-- method FileChooser::set_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "file"
--           , argType = TInterface Name { namespace = "Gio" , name = "File" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GFile to set as current"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_file_chooser_set_file" gtk_file_chooser_set_file :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    Ptr Gio.File.File ->                    -- file : TInterface (Name {namespace = "Gio", name = "File"})
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Sets /@file@/ as the current filename for the file chooser, by changing
-- to the file’s parent folder and actually selecting the file in list.  If
-- the /@chooser@/ is in 'GI.Gtk.Enums.FileChooserActionSave' mode, the file’s base name
-- will also appear in the dialog’s file name entry.
-- 
-- If the file name isn’t in the current folder of /@chooser@/, then the current
-- folder of /@chooser@/ will be changed to the folder containing /@filename@/. This
-- is equivalent to a sequence of 'GI.Gtk.Interfaces.FileChooser.fileChooserUnselectAll' followed by
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSelectFilename'.
-- 
-- Note that the file must exist, or nothing will be done except
-- for the directory change.
-- 
-- If you are implementing a save dialog,
-- you should use this function if you already have a file name to which the
-- user may save; for example, when the user opens an existing file and then
-- does Save As...  If you don’t have
-- a file name already — for example, if the user just created a new
-- file and is saving it for the first time, do not call this function.
-- Instead, use something similar to this:
-- 
-- === /C code/
-- >
-- >if (document_is_new)
-- >  {
-- >    // the user just created a new document
-- >    gtk_file_chooser_set_current_folder_file (chooser, default_file_for_saving);
-- >    gtk_file_chooser_set_current_name (chooser, "Untitled document");
-- >  }
-- >else
-- >  {
-- >    // the user edited an existing document
-- >    gtk_file_chooser_set_file (chooser, existing_file);
-- >  }
-- 
-- 
-- /Since: 2.14/
fileChooserSetFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a, Gio.File.IsFile b) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> b
    -- ^ /@file@/: the t'GI.Gio.Interfaces.File.File' to set as current
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
fileChooserSetFile chooser file = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    file' <- unsafeManagedPtrCastPtr file
    onException (do
        _ <- propagateGError $ gtk_file_chooser_set_file chooser' file'
        touchManagedPtr chooser
        touchManagedPtr file
        return ()
     ) (do
        return ()
     )

#if defined(ENABLE_OVERLOADING)
data FileChooserSetFileMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsFileChooser a, Gio.File.IsFile b) => O.OverloadedMethod FileChooserSetFileMethodInfo a signature where
    overloadedMethod = fileChooserSetFile

instance O.OverloadedMethodInfo FileChooserSetFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetFile"
        })


#endif

-- method FileChooser::set_filename
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filename"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the filename to set as current"
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

foreign import ccall "gtk_file_chooser_set_filename" gtk_file_chooser_set_filename :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- filename : TBasicType TFileName
    IO CInt

-- | Sets /@filename@/ as the current filename for the file chooser, by changing to
-- the file’s parent folder and actually selecting the file in list; all other
-- files will be unselected.  If the /@chooser@/ is in
-- 'GI.Gtk.Enums.FileChooserActionSave' mode, the file’s base name will also appear in
-- the dialog’s file name entry.
-- 
-- Note that the file must exist, or nothing will be done except
-- for the directory change.
-- 
-- You should use this function only when implementing a save
-- dialog for which you already have a file name to which
-- the user may save.  For example, when the user opens an existing file and
-- then does Save As... to save a copy or
-- a modified version.  If you don’t have a file name already — for
-- example, if the user just created a new file and is saving it for the first
-- time, do not call this function.  Instead, use something similar to this:
-- 
-- === /C code/
-- >
-- >if (document_is_new)
-- >  {
-- >    // the user just created a new document
-- >    gtk_file_chooser_set_current_name (chooser, "Untitled document");
-- >  }
-- >else
-- >  {
-- >    // the user edited an existing document
-- >    gtk_file_chooser_set_filename (chooser, existing_filename);
-- >  }
-- 
-- 
-- In the first case, the file chooser will present the user with useful suggestions
-- as to where to save his new file.  In the second case, the file’s existing location
-- is already known, so the file chooser will use it.
-- 
-- /Since: 2.4/
fileChooserSetFilename ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> [Char]
    -- ^ /@filename@/: the filename to set as current
    -> m Bool
    -- ^ __Returns:__ Not useful.
fileChooserSetFilename chooser filename = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    filename' <- stringToCString filename
    result <- gtk_file_chooser_set_filename chooser' filename'
    let result' = (/= 0) result
    touchManagedPtr chooser
    freeMem filename'
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserSetFilenameMethodInfo
instance (signature ~ ([Char] -> m Bool), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSetFilenameMethodInfo a signature where
    overloadedMethod = fileChooserSetFilename

instance O.OverloadedMethodInfo FileChooserSetFilenameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetFilename",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetFilename"
        })


#endif

-- method FileChooser::set_filter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileFilter" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_set_filter" gtk_file_chooser_set_filter :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    Ptr Gtk.FileFilter.FileFilter ->        -- filter : TInterface (Name {namespace = "Gtk", name = "FileFilter"})
    IO ()

-- | Sets the current filter; only the files that pass the
-- filter will be displayed. If the user-selectable list of filters
-- is non-empty, then the filter should be one of the filters
-- in that list. Setting the current filter when the list of
-- filters is empty is useful if you want to restrict the displayed
-- set of files without letting the user change it.
-- 
-- /Since: 2.4/
fileChooserSetFilter ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a, Gtk.FileFilter.IsFileFilter b) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> b
    -- ^ /@filter@/: a t'GI.Gtk.Objects.FileFilter.FileFilter'
    -> m ()
fileChooserSetFilter chooser filter = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    filter' <- unsafeManagedPtrCastPtr filter
    gtk_file_chooser_set_filter chooser' filter'
    touchManagedPtr chooser
    touchManagedPtr filter
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserSetFilterMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsFileChooser a, Gtk.FileFilter.IsFileFilter b) => O.OverloadedMethod FileChooserSetFilterMethodInfo a signature where
    overloadedMethod = fileChooserSetFilter

instance O.OverloadedMethodInfo FileChooserSetFilterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetFilter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetFilter"
        })


#endif

-- method FileChooser::set_local_only
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "local_only"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if only local files can be selected"
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

foreign import ccall "gtk_file_chooser_set_local_only" gtk_file_chooser_set_local_only :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CInt ->                                 -- local_only : TBasicType TBoolean
    IO ()

-- | Sets whether only local files can be selected in the
-- file selector. If /@localOnly@/ is 'P.True' (the default),
-- then the selected file or files are guaranteed to be
-- accessible through the operating systems native file
-- system and therefore the application only
-- needs to worry about the filename functions in
-- t'GI.Gtk.Interfaces.FileChooser.FileChooser', like 'GI.Gtk.Interfaces.FileChooser.fileChooserGetFilename',
-- rather than the URI functions like
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserGetUri',
-- 
-- On some systems non-native files may still be
-- available using the native filesystem via a userspace
-- filesystem (FUSE).
-- 
-- /Since: 2.4/
fileChooserSetLocalOnly ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> Bool
    -- ^ /@localOnly@/: 'P.True' if only local files can be selected
    -> m ()
fileChooserSetLocalOnly chooser localOnly = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    let localOnly' = (fromIntegral . fromEnum) localOnly
    gtk_file_chooser_set_local_only chooser' localOnly'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserSetLocalOnlyMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSetLocalOnlyMethodInfo a signature where
    overloadedMethod = fileChooserSetLocalOnly

instance O.OverloadedMethodInfo FileChooserSetLocalOnlyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetLocalOnly",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetLocalOnly"
        })


#endif

-- method FileChooser::set_preview_widget
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "preview_widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "widget for displaying preview."
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

foreign import ccall "gtk_file_chooser_set_preview_widget" gtk_file_chooser_set_preview_widget :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    Ptr Gtk.Widget.Widget ->                -- preview_widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO ()

-- | Sets an application-supplied widget to use to display a custom preview
-- of the currently selected file. To implement a preview, after setting the
-- preview widget, you connect to the [FileChooser::updatePreview]("GI.Gtk.Interfaces.FileChooser#g:signal:updatePreview")
-- signal, and call 'GI.Gtk.Interfaces.FileChooser.fileChooserGetPreviewFilename' or
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserGetPreviewUri' on each change. If you can
-- display a preview of the new file, update your widget and
-- set the preview active using 'GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidgetActive'.
-- Otherwise, set the preview inactive.
-- 
-- When there is no application-supplied preview widget, or the
-- application-supplied preview widget is not active, the file chooser
-- will display no preview at all.
-- 
-- /Since: 2.4/
fileChooserSetPreviewWidget ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> b
    -- ^ /@previewWidget@/: widget for displaying preview.
    -> m ()
fileChooserSetPreviewWidget chooser previewWidget = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    previewWidget' <- unsafeManagedPtrCastPtr previewWidget
    gtk_file_chooser_set_preview_widget chooser' previewWidget'
    touchManagedPtr chooser
    touchManagedPtr previewWidget
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserSetPreviewWidgetMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsFileChooser a, Gtk.Widget.IsWidget b) => O.OverloadedMethod FileChooserSetPreviewWidgetMethodInfo a signature where
    overloadedMethod = fileChooserSetPreviewWidget

instance O.OverloadedMethodInfo FileChooserSetPreviewWidgetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidget",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetPreviewWidget"
        })


#endif

-- method FileChooser::set_preview_widget_active
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "active"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "whether to display the user-specified preview widget"
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

foreign import ccall "gtk_file_chooser_set_preview_widget_active" gtk_file_chooser_set_preview_widget_active :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CInt ->                                 -- active : TBasicType TBoolean
    IO ()

-- | Sets whether the preview widget set by
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidget' should be shown for the
-- current filename. When /@active@/ is set to false, the file chooser
-- may display an internally generated preview of the current file
-- or it may display no preview at all. See
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidget' for more details.
-- 
-- /Since: 2.4/
fileChooserSetPreviewWidgetActive ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> Bool
    -- ^ /@active@/: whether to display the user-specified preview widget
    -> m ()
fileChooserSetPreviewWidgetActive chooser active = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    let active' = (fromIntegral . fromEnum) active
    gtk_file_chooser_set_preview_widget_active chooser' active'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserSetPreviewWidgetActiveMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSetPreviewWidgetActiveMethodInfo a signature where
    overloadedMethod = fileChooserSetPreviewWidgetActive

instance O.OverloadedMethodInfo FileChooserSetPreviewWidgetActiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidgetActive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetPreviewWidgetActive"
        })


#endif

-- method FileChooser::set_select_multiple
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "select_multiple"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if multiple files can be selected."
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

foreign import ccall "gtk_file_chooser_set_select_multiple" gtk_file_chooser_set_select_multiple :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CInt ->                                 -- select_multiple : TBasicType TBoolean
    IO ()

-- | Sets whether multiple files can be selected in the file selector.  This is
-- only relevant if the action is set to be 'GI.Gtk.Enums.FileChooserActionOpen' or
-- 'GI.Gtk.Enums.FileChooserActionSelectFolder'.
-- 
-- /Since: 2.4/
fileChooserSetSelectMultiple ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> Bool
    -- ^ /@selectMultiple@/: 'P.True' if multiple files can be selected.
    -> m ()
fileChooserSetSelectMultiple chooser selectMultiple = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    let selectMultiple' = (fromIntegral . fromEnum) selectMultiple
    gtk_file_chooser_set_select_multiple chooser' selectMultiple'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserSetSelectMultipleMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSetSelectMultipleMethodInfo a signature where
    overloadedMethod = fileChooserSetSelectMultiple

instance O.OverloadedMethodInfo FileChooserSetSelectMultipleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetSelectMultiple",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetSelectMultiple"
        })


#endif

-- method FileChooser::set_show_hidden
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "show_hidden"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE if hidden files and folders should be displayed."
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

foreign import ccall "gtk_file_chooser_set_show_hidden" gtk_file_chooser_set_show_hidden :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CInt ->                                 -- show_hidden : TBasicType TBoolean
    IO ()

-- | Sets whether hidden files and folders are displayed in the file selector.
-- 
-- /Since: 2.6/
fileChooserSetShowHidden ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> Bool
    -- ^ /@showHidden@/: 'P.True' if hidden files and folders should be displayed.
    -> m ()
fileChooserSetShowHidden chooser showHidden = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    let showHidden' = (fromIntegral . fromEnum) showHidden
    gtk_file_chooser_set_show_hidden chooser' showHidden'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserSetShowHiddenMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSetShowHiddenMethodInfo a signature where
    overloadedMethod = fileChooserSetShowHidden

instance O.OverloadedMethodInfo FileChooserSetShowHiddenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetShowHidden",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetShowHidden"
        })


#endif

-- method FileChooser::set_uri
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "uri"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the URI to set as current"
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

foreign import ccall "gtk_file_chooser_set_uri" gtk_file_chooser_set_uri :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- uri : TBasicType TUTF8
    IO CInt

-- | Sets the file referred to by /@uri@/ as the current file for the file chooser,
-- by changing to the URI’s parent folder and actually selecting the URI in the
-- list.  If the /@chooser@/ is 'GI.Gtk.Enums.FileChooserActionSave' mode, the URI’s base
-- name will also appear in the dialog’s file name entry.
-- 
-- Note that the URI must exist, or nothing will be done except for the
-- directory change.
-- 
-- You should use this function only when implementing a save
-- dialog for which you already have a file name to which
-- the user may save.  For example, when the user opens an existing file and then
-- does Save As... to save a copy or a
-- modified version.  If you don’t have a file name already — for example,
-- if the user just created a new file and is saving it for the first time, do
-- not call this function.  Instead, use something similar to this:
-- 
-- === /C code/
-- >
-- >if (document_is_new)
-- >  {
-- >    // the user just created a new document
-- >    gtk_file_chooser_set_current_name (chooser, "Untitled document");
-- >  }
-- >else
-- >  {
-- >    // the user edited an existing document
-- >    gtk_file_chooser_set_uri (chooser, existing_uri);
-- >  }
-- 
-- 
-- 
-- In the first case, the file chooser will present the user with useful suggestions
-- as to where to save his new file.  In the second case, the file’s existing location
-- is already known, so the file chooser will use it.
-- 
-- /Since: 2.4/
fileChooserSetUri ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> T.Text
    -- ^ /@uri@/: the URI to set as current
    -> m Bool
    -- ^ __Returns:__ Not useful.
fileChooserSetUri chooser uri = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    uri' <- textToCString uri
    result <- gtk_file_chooser_set_uri chooser' uri'
    let result' = (/= 0) result
    touchManagedPtr chooser
    freeMem uri'
    return result'

#if defined(ENABLE_OVERLOADING)
data FileChooserSetUriMethodInfo
instance (signature ~ (T.Text -> m Bool), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSetUriMethodInfo a signature where
    overloadedMethod = fileChooserSetUri

instance O.OverloadedMethodInfo FileChooserSetUriMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetUri",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetUri"
        })


#endif

-- method FileChooser::set_use_preview_label
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "use_label"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "whether to display a stock label with the name of the previewed file"
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

foreign import ccall "gtk_file_chooser_set_use_preview_label" gtk_file_chooser_set_use_preview_label :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CInt ->                                 -- use_label : TBasicType TBoolean
    IO ()

-- | Sets whether the file chooser should display a stock label with the name of
-- the file that is being previewed; the default is 'P.True'.  Applications that
-- want to draw the whole preview area themselves should set this to 'P.False' and
-- display the name themselves in their preview widget.
-- 
-- See also: 'GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidget'
-- 
-- /Since: 2.4/
fileChooserSetUsePreviewLabel ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> Bool
    -- ^ /@useLabel@/: whether to display a stock label with the name of the previewed file
    -> m ()
fileChooserSetUsePreviewLabel chooser useLabel = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    let useLabel' = (fromIntegral . fromEnum) useLabel
    gtk_file_chooser_set_use_preview_label chooser' useLabel'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserSetUsePreviewLabelMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserSetUsePreviewLabelMethodInfo a signature where
    overloadedMethod = fileChooserSetUsePreviewLabel

instance O.OverloadedMethodInfo FileChooserSetUsePreviewLabelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserSetUsePreviewLabel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserSetUsePreviewLabel"
        })


#endif

-- method FileChooser::unselect_all
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_unselect_all" gtk_file_chooser_unselect_all :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    IO ()

-- | Unselects all the files in the current folder of a file chooser.
-- 
-- /Since: 2.4/
fileChooserUnselectAll ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> m ()
fileChooserUnselectAll chooser = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    gtk_file_chooser_unselect_all chooser'
    touchManagedPtr chooser
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserUnselectAllMethodInfo
instance (signature ~ (m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserUnselectAllMethodInfo a signature where
    overloadedMethod = fileChooserUnselectAll

instance O.OverloadedMethodInfo FileChooserUnselectAllMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserUnselectAll",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserUnselectAll"
        })


#endif

-- method FileChooser::unselect_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "file"
--           , argType = TInterface Name { namespace = "Gio" , name = "File" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GFile" , sinceVersion = Nothing }
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

foreign import ccall "gtk_file_chooser_unselect_file" gtk_file_chooser_unselect_file :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    Ptr Gio.File.File ->                    -- file : TInterface (Name {namespace = "Gio", name = "File"})
    IO ()

-- | Unselects the file referred to by /@file@/. If the file is not in the current
-- directory, does not exist, or is otherwise not currently selected, does nothing.
-- 
-- /Since: 2.14/
fileChooserUnselectFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a, Gio.File.IsFile b) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> b
    -- ^ /@file@/: a t'GI.Gio.Interfaces.File.File'
    -> m ()
fileChooserUnselectFile chooser file = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    file' <- unsafeManagedPtrCastPtr file
    gtk_file_chooser_unselect_file chooser' file'
    touchManagedPtr chooser
    touchManagedPtr file
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserUnselectFileMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsFileChooser a, Gio.File.IsFile b) => O.OverloadedMethod FileChooserUnselectFileMethodInfo a signature where
    overloadedMethod = fileChooserUnselectFile

instance O.OverloadedMethodInfo FileChooserUnselectFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserUnselectFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserUnselectFile"
        })


#endif

-- method FileChooser::unselect_filename
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filename"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the filename to unselect"
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

foreign import ccall "gtk_file_chooser_unselect_filename" gtk_file_chooser_unselect_filename :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- filename : TBasicType TFileName
    IO ()

-- | Unselects a currently selected filename. If the filename
-- is not in the current directory, does not exist, or
-- is otherwise not currently selected, does nothing.
-- 
-- /Since: 2.4/
fileChooserUnselectFilename ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> [Char]
    -- ^ /@filename@/: the filename to unselect
    -> m ()
fileChooserUnselectFilename chooser filename = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    filename' <- stringToCString filename
    gtk_file_chooser_unselect_filename chooser' filename'
    touchManagedPtr chooser
    freeMem filename'
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserUnselectFilenameMethodInfo
instance (signature ~ ([Char] -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserUnselectFilenameMethodInfo a signature where
    overloadedMethod = fileChooserUnselectFilename

instance O.OverloadedMethodInfo FileChooserUnselectFilenameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserUnselectFilename",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserUnselectFilename"
        })


#endif

-- method FileChooser::unselect_uri
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "chooser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "FileChooser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkFileChooser" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "uri"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the URI to unselect"
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

foreign import ccall "gtk_file_chooser_unselect_uri" gtk_file_chooser_unselect_uri :: 
    Ptr FileChooser ->                      -- chooser : TInterface (Name {namespace = "Gtk", name = "FileChooser"})
    CString ->                              -- uri : TBasicType TUTF8
    IO ()

-- | Unselects the file referred to by /@uri@/. If the file
-- is not in the current directory, does not exist, or
-- is otherwise not currently selected, does nothing.
-- 
-- /Since: 2.4/
fileChooserUnselectUri ::
    (B.CallStack.HasCallStack, MonadIO m, IsFileChooser a) =>
    a
    -- ^ /@chooser@/: a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
    -> T.Text
    -- ^ /@uri@/: the URI to unselect
    -> m ()
fileChooserUnselectUri chooser uri = liftIO $ do
    chooser' <- unsafeManagedPtrCastPtr chooser
    uri' <- textToCString uri
    gtk_file_chooser_unselect_uri chooser' uri'
    touchManagedPtr chooser
    freeMem uri'
    return ()

#if defined(ENABLE_OVERLOADING)
data FileChooserUnselectUriMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsFileChooser a) => O.OverloadedMethod FileChooserUnselectUriMethodInfo a signature where
    overloadedMethod = fileChooserUnselectUri

instance O.OverloadedMethodInfo FileChooserUnselectUriMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser.fileChooserUnselectUri",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserUnselectUri"
        })


#endif

-- signal FileChooser::confirm-overwrite
-- | This signal gets emitted whenever it is appropriate to present a
-- confirmation dialog when the user has selected a file name that
-- already exists.  The signal only gets emitted when the file
-- chooser is in 'GI.Gtk.Enums.FileChooserActionSave' mode.
-- 
-- Most applications just need to turn on the
-- t'GI.Gtk.Interfaces.FileChooser.FileChooser':@/do-overwrite-confirmation/@ property (or call the
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetDoOverwriteConfirmation' function), and
-- they will automatically get a stock confirmation dialog.
-- Applications which need to customize this behavior should do
-- that, and also connect to the [FileChooser::confirmOverwrite]("GI.Gtk.Interfaces.FileChooser#g:signal:confirmOverwrite")
-- signal.
-- 
-- A signal handler for this signal must return a
-- t'GI.Gtk.Enums.FileChooserConfirmation' value, which indicates the action to
-- take.  If the handler determines that the user wants to select a
-- different filename, it should return
-- 'GI.Gtk.Enums.FileChooserConfirmationSelectAgain'.  If it determines
-- that the user is satisfied with his choice of file name, it
-- should return 'GI.Gtk.Enums.FileChooserConfirmationAcceptFilename'.
-- On the other hand, if it determines that the stock confirmation
-- dialog should be used, it should return
-- 'GI.Gtk.Enums.FileChooserConfirmationConfirm'. The following example
-- illustrates this.
-- 
-- ## Custom confirmation ## {@/gtkfilechooser/@-confirmation}
-- 
-- 
-- === /C code/
-- >
-- >static GtkFileChooserConfirmation
-- >confirm_overwrite_callback (GtkFileChooser *chooser, gpointer data)
-- >{
-- >  char *uri;
-- >
-- >  uri = gtk_file_chooser_get_uri (chooser);
-- >
-- >  if (is_uri_read_only (uri))
-- >    {
-- >      if (user_wants_to_replace_read_only_file (uri))
-- >        return GTK_FILE_CHOOSER_CONFIRMATION_ACCEPT_FILENAME;
-- >      else
-- >        return GTK_FILE_CHOOSER_CONFIRMATION_SELECT_AGAIN;
-- >    } else
-- >      return GTK_FILE_CHOOSER_CONFIRMATION_CONFIRM; // fall back to the default dialog
-- >}
-- >
-- >...
-- >
-- >chooser = gtk_file_chooser_dialog_new (...);
-- >
-- >gtk_file_chooser_set_do_overwrite_confirmation (GTK_FILE_CHOOSER (dialog), TRUE);
-- >g_signal_connect (chooser, "confirm-overwrite",
-- >                  G_CALLBACK (confirm_overwrite_callback), NULL);
-- >
-- >if (gtk_dialog_run (chooser) == GTK_RESPONSE_ACCEPT)
-- >        save_to_file (gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (chooser));
-- >
-- >gtk_widget_destroy (chooser);
-- 
-- 
-- /Since: 2.8/
type FileChooserConfirmOverwriteCallback =
    IO Gtk.Enums.FileChooserConfirmation
    -- ^ __Returns:__ a t'GI.Gtk.Enums.FileChooserConfirmation' value that indicates which
    --  action to take after emitting the signal.

type C_FileChooserConfirmOverwriteCallback =
    Ptr FileChooser ->                      -- object
    Ptr () ->                               -- user_data
    IO CUInt

-- | Generate a function pointer callable from C code, from a `C_FileChooserConfirmOverwriteCallback`.
foreign import ccall "wrapper"
    mk_FileChooserConfirmOverwriteCallback :: C_FileChooserConfirmOverwriteCallback -> IO (FunPtr C_FileChooserConfirmOverwriteCallback)

wrap_FileChooserConfirmOverwriteCallback :: 
    GObject a => (a -> FileChooserConfirmOverwriteCallback) ->
    C_FileChooserConfirmOverwriteCallback
wrap_FileChooserConfirmOverwriteCallback gi'cb gi'selfPtr _ = do
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [confirmOverwrite](#signal:confirmOverwrite) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' fileChooser #confirmOverwrite callback
-- @
-- 
-- 
onFileChooserConfirmOverwrite :: (IsFileChooser a, MonadIO m) => a -> ((?self :: a) => FileChooserConfirmOverwriteCallback) -> m SignalHandlerId
onFileChooserConfirmOverwrite obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FileChooserConfirmOverwriteCallback wrapped
    wrapped'' <- mk_FileChooserConfirmOverwriteCallback wrapped'
    connectSignalFunPtr obj "confirm-overwrite" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [confirmOverwrite](#signal:confirmOverwrite) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' fileChooser #confirmOverwrite callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterFileChooserConfirmOverwrite :: (IsFileChooser a, MonadIO m) => a -> ((?self :: a) => FileChooserConfirmOverwriteCallback) -> m SignalHandlerId
afterFileChooserConfirmOverwrite obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FileChooserConfirmOverwriteCallback wrapped
    wrapped'' <- mk_FileChooserConfirmOverwriteCallback wrapped'
    connectSignalFunPtr obj "confirm-overwrite" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data FileChooserConfirmOverwriteSignalInfo
instance SignalInfo FileChooserConfirmOverwriteSignalInfo where
    type HaskellCallbackType FileChooserConfirmOverwriteSignalInfo = FileChooserConfirmOverwriteCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_FileChooserConfirmOverwriteCallback cb
        cb'' <- mk_FileChooserConfirmOverwriteCallback cb'
        connectSignalFunPtr obj "confirm-overwrite" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser::confirm-overwrite"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#g:signal:confirmOverwrite"})

#endif

-- signal FileChooser::current-folder-changed
-- | This signal is emitted when the current folder in a t'GI.Gtk.Interfaces.FileChooser.FileChooser'
-- changes.  This can happen due to the user performing some action that
-- changes folders, such as selecting a bookmark or visiting a folder on the
-- file list.  It can also happen as a result of calling a function to
-- explicitly change the current folder in a file chooser.
-- 
-- Normally you do not need to connect to this signal, unless you need to keep
-- track of which folder a file chooser is showing.
-- 
-- See also:  'GI.Gtk.Interfaces.FileChooser.fileChooserSetCurrentFolder',
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserGetCurrentFolder',
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetCurrentFolderUri',
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserGetCurrentFolderUri'.
type FileChooserCurrentFolderChangedCallback =
    IO ()

type C_FileChooserCurrentFolderChangedCallback =
    Ptr FileChooser ->                      -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_FileChooserCurrentFolderChangedCallback`.
foreign import ccall "wrapper"
    mk_FileChooserCurrentFolderChangedCallback :: C_FileChooserCurrentFolderChangedCallback -> IO (FunPtr C_FileChooserCurrentFolderChangedCallback)

wrap_FileChooserCurrentFolderChangedCallback :: 
    GObject a => (a -> FileChooserCurrentFolderChangedCallback) ->
    C_FileChooserCurrentFolderChangedCallback
wrap_FileChooserCurrentFolderChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [currentFolderChanged](#signal:currentFolderChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' fileChooser #currentFolderChanged callback
-- @
-- 
-- 
onFileChooserCurrentFolderChanged :: (IsFileChooser a, MonadIO m) => a -> ((?self :: a) => FileChooserCurrentFolderChangedCallback) -> m SignalHandlerId
onFileChooserCurrentFolderChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FileChooserCurrentFolderChangedCallback wrapped
    wrapped'' <- mk_FileChooserCurrentFolderChangedCallback wrapped'
    connectSignalFunPtr obj "current-folder-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [currentFolderChanged](#signal:currentFolderChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' fileChooser #currentFolderChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterFileChooserCurrentFolderChanged :: (IsFileChooser a, MonadIO m) => a -> ((?self :: a) => FileChooserCurrentFolderChangedCallback) -> m SignalHandlerId
afterFileChooserCurrentFolderChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FileChooserCurrentFolderChangedCallback wrapped
    wrapped'' <- mk_FileChooserCurrentFolderChangedCallback wrapped'
    connectSignalFunPtr obj "current-folder-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data FileChooserCurrentFolderChangedSignalInfo
instance SignalInfo FileChooserCurrentFolderChangedSignalInfo where
    type HaskellCallbackType FileChooserCurrentFolderChangedSignalInfo = FileChooserCurrentFolderChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_FileChooserCurrentFolderChangedCallback cb
        cb'' <- mk_FileChooserCurrentFolderChangedCallback cb'
        connectSignalFunPtr obj "current-folder-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser::current-folder-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#g:signal:currentFolderChanged"})

#endif

-- signal FileChooser::file-activated
-- | This signal is emitted when the user \"activates\" a file in the file
-- chooser.  This can happen by double-clicking on a file in the file list, or
-- by pressing @Enter@.
-- 
-- Normally you do not need to connect to this signal.  It is used internally
-- by t'GI.Gtk.Objects.FileChooserDialog.FileChooserDialog' to know when to activate the default button in the
-- dialog.
-- 
-- See also: 'GI.Gtk.Interfaces.FileChooser.fileChooserGetFilename',
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserGetFilenames', 'GI.Gtk.Interfaces.FileChooser.fileChooserGetUri',
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserGetUris'.
type FileChooserFileActivatedCallback =
    IO ()

type C_FileChooserFileActivatedCallback =
    Ptr FileChooser ->                      -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_FileChooserFileActivatedCallback`.
foreign import ccall "wrapper"
    mk_FileChooserFileActivatedCallback :: C_FileChooserFileActivatedCallback -> IO (FunPtr C_FileChooserFileActivatedCallback)

wrap_FileChooserFileActivatedCallback :: 
    GObject a => (a -> FileChooserFileActivatedCallback) ->
    C_FileChooserFileActivatedCallback
wrap_FileChooserFileActivatedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [fileActivated](#signal:fileActivated) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' fileChooser #fileActivated callback
-- @
-- 
-- 
onFileChooserFileActivated :: (IsFileChooser a, MonadIO m) => a -> ((?self :: a) => FileChooserFileActivatedCallback) -> m SignalHandlerId
onFileChooserFileActivated obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FileChooserFileActivatedCallback wrapped
    wrapped'' <- mk_FileChooserFileActivatedCallback wrapped'
    connectSignalFunPtr obj "file-activated" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [fileActivated](#signal:fileActivated) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' fileChooser #fileActivated callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterFileChooserFileActivated :: (IsFileChooser a, MonadIO m) => a -> ((?self :: a) => FileChooserFileActivatedCallback) -> m SignalHandlerId
afterFileChooserFileActivated obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FileChooserFileActivatedCallback wrapped
    wrapped'' <- mk_FileChooserFileActivatedCallback wrapped'
    connectSignalFunPtr obj "file-activated" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data FileChooserFileActivatedSignalInfo
instance SignalInfo FileChooserFileActivatedSignalInfo where
    type HaskellCallbackType FileChooserFileActivatedSignalInfo = FileChooserFileActivatedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_FileChooserFileActivatedCallback cb
        cb'' <- mk_FileChooserFileActivatedCallback cb'
        connectSignalFunPtr obj "file-activated" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser::file-activated"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#g:signal:fileActivated"})

#endif

-- signal FileChooser::selection-changed
-- | This signal is emitted when there is a change in the set of selected files
-- in a t'GI.Gtk.Interfaces.FileChooser.FileChooser'.  This can happen when the user modifies the selection
-- with the mouse or the keyboard, or when explicitly calling functions to
-- change the selection.
-- 
-- Normally you do not need to connect to this signal, as it is easier to wait
-- for the file chooser to finish running, and then to get the list of
-- selected files using the functions mentioned below.
-- 
-- See also: 'GI.Gtk.Interfaces.FileChooser.fileChooserSelectFilename',
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserUnselectFilename', 'GI.Gtk.Interfaces.FileChooser.fileChooserGetFilename',
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserGetFilenames', 'GI.Gtk.Interfaces.FileChooser.fileChooserSelectUri',
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserUnselectUri', 'GI.Gtk.Interfaces.FileChooser.fileChooserGetUri',
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserGetUris'.
type FileChooserSelectionChangedCallback =
    IO ()

type C_FileChooserSelectionChangedCallback =
    Ptr FileChooser ->                      -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_FileChooserSelectionChangedCallback`.
foreign import ccall "wrapper"
    mk_FileChooserSelectionChangedCallback :: C_FileChooserSelectionChangedCallback -> IO (FunPtr C_FileChooserSelectionChangedCallback)

wrap_FileChooserSelectionChangedCallback :: 
    GObject a => (a -> FileChooserSelectionChangedCallback) ->
    C_FileChooserSelectionChangedCallback
wrap_FileChooserSelectionChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [selectionChanged](#signal:selectionChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' fileChooser #selectionChanged callback
-- @
-- 
-- 
onFileChooserSelectionChanged :: (IsFileChooser a, MonadIO m) => a -> ((?self :: a) => FileChooserSelectionChangedCallback) -> m SignalHandlerId
onFileChooserSelectionChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FileChooserSelectionChangedCallback wrapped
    wrapped'' <- mk_FileChooserSelectionChangedCallback wrapped'
    connectSignalFunPtr obj "selection-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [selectionChanged](#signal:selectionChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' fileChooser #selectionChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterFileChooserSelectionChanged :: (IsFileChooser a, MonadIO m) => a -> ((?self :: a) => FileChooserSelectionChangedCallback) -> m SignalHandlerId
afterFileChooserSelectionChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FileChooserSelectionChangedCallback wrapped
    wrapped'' <- mk_FileChooserSelectionChangedCallback wrapped'
    connectSignalFunPtr obj "selection-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data FileChooserSelectionChangedSignalInfo
instance SignalInfo FileChooserSelectionChangedSignalInfo where
    type HaskellCallbackType FileChooserSelectionChangedSignalInfo = FileChooserSelectionChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_FileChooserSelectionChangedCallback cb
        cb'' <- mk_FileChooserSelectionChangedCallback cb'
        connectSignalFunPtr obj "selection-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser::selection-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#g:signal:selectionChanged"})

#endif

-- signal FileChooser::update-preview
-- | This signal is emitted when the preview in a file chooser should be
-- regenerated.  For example, this can happen when the currently selected file
-- changes.  You should use this signal if you want your file chooser to have
-- a preview widget.
-- 
-- Once you have installed a preview widget with
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidget', you should update it when this
-- signal is emitted.  You can use the functions
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserGetPreviewFilename' or
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserGetPreviewUri' to get the name of the file to preview.
-- Your widget may not be able to preview all kinds of files; your callback
-- must call 'GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidgetActive' to inform the file
-- chooser about whether the preview was generated successfully or not.
-- 
-- Please see the example code in
-- [Using a Preview Widget][gtkfilechooser-preview].
-- 
-- See also: 'GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidget',
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetPreviewWidgetActive',
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserSetUsePreviewLabel',
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserGetPreviewFilename',
-- 'GI.Gtk.Interfaces.FileChooser.fileChooserGetPreviewUri'.
type FileChooserUpdatePreviewCallback =
    IO ()

type C_FileChooserUpdatePreviewCallback =
    Ptr FileChooser ->                      -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_FileChooserUpdatePreviewCallback`.
foreign import ccall "wrapper"
    mk_FileChooserUpdatePreviewCallback :: C_FileChooserUpdatePreviewCallback -> IO (FunPtr C_FileChooserUpdatePreviewCallback)

wrap_FileChooserUpdatePreviewCallback :: 
    GObject a => (a -> FileChooserUpdatePreviewCallback) ->
    C_FileChooserUpdatePreviewCallback
wrap_FileChooserUpdatePreviewCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [updatePreview](#signal:updatePreview) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' fileChooser #updatePreview callback
-- @
-- 
-- 
onFileChooserUpdatePreview :: (IsFileChooser a, MonadIO m) => a -> ((?self :: a) => FileChooserUpdatePreviewCallback) -> m SignalHandlerId
onFileChooserUpdatePreview obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FileChooserUpdatePreviewCallback wrapped
    wrapped'' <- mk_FileChooserUpdatePreviewCallback wrapped'
    connectSignalFunPtr obj "update-preview" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [updatePreview](#signal:updatePreview) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' fileChooser #updatePreview callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterFileChooserUpdatePreview :: (IsFileChooser a, MonadIO m) => a -> ((?self :: a) => FileChooserUpdatePreviewCallback) -> m SignalHandlerId
afterFileChooserUpdatePreview obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_FileChooserUpdatePreviewCallback wrapped
    wrapped'' <- mk_FileChooserUpdatePreviewCallback wrapped'
    connectSignalFunPtr obj "update-preview" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data FileChooserUpdatePreviewSignalInfo
instance SignalInfo FileChooserUpdatePreviewSignalInfo where
    type HaskellCallbackType FileChooserUpdatePreviewSignalInfo = FileChooserUpdatePreviewCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_FileChooserUpdatePreviewCallback cb
        cb'' <- mk_FileChooserUpdatePreviewCallback cb'
        connectSignalFunPtr obj "update-preview" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.FileChooser::update-preview"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-FileChooser.html#g:signal:updatePreview"})

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList FileChooser = FileChooserSignalList
type FileChooserSignalList = ('[ '("confirmOverwrite", FileChooserConfirmOverwriteSignalInfo), '("currentFolderChanged", FileChooserCurrentFolderChangedSignalInfo), '("fileActivated", FileChooserFileActivatedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("selectionChanged", FileChooserSelectionChangedSignalInfo), '("updatePreview", FileChooserUpdatePreviewSignalInfo)] :: [(Symbol, *)])

#endif


