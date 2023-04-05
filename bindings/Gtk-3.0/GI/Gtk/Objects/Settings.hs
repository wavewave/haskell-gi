{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- GtkSettings provide a mechanism to share global settings between
-- applications.
-- 
-- On the X window system, this sharing is realized by an
-- <http://www.freedesktop.org/wiki/Specifications/xsettings-spec XSettings>
-- manager that is usually part of the desktop environment, along with
-- utilities that let the user change these settings. In the absence of
-- an Xsettings manager, GTK+ reads default values for settings from
-- @settings.ini@ files in
-- @\/etc\/gtk-3.0@, @$XDG_CONFIG_DIRS\/gtk-3.0@
-- and @$XDG_CONFIG_HOME\/gtk-3.0@.
-- These files must be valid key files (see t'GI.GLib.Structs.KeyFile.KeyFile'), and have
-- a section called Settings. Themes can also provide default values
-- for settings by installing a @settings.ini@ file
-- next to their @gtk.css@ file.
-- 
-- Applications can override system-wide settings by setting the property
-- of the GtkSettings object with @/g_object_set()/@. This should be restricted
-- to special cases though; GtkSettings are not meant as an application
-- configuration facility. When doing so, you need to be aware that settings
-- that are specific to individual widgets may not be available before the
-- widget type has been realized at least once. The following example
-- demonstrates a way to do this:
-- 
-- === /C code/
-- >
-- >  gtk_init (&argc, &argv);
-- >
-- >  // make sure the type is realized
-- >  g_type_class_unref (g_type_class_ref (GTK_TYPE_IMAGE_MENU_ITEM));
-- >
-- >  g_object_set (gtk_settings_get_default (), "gtk-enable-animations", FALSE, NULL);
-- 
-- 
-- There is one GtkSettings instance per screen. It can be obtained with
-- 'GI.Gtk.Objects.Settings.settingsGetForScreen', but in many cases, it is more convenient
-- to use 'GI.Gtk.Objects.Widget.widgetGetSettings'. 'GI.Gtk.Objects.Settings.settingsGetDefault' returns the
-- GtkSettings instance for the default screen.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Settings
    ( 

-- * Exported types
    Settings(..)                            ,
    IsSettings                              ,
    toSettings                              ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [resetProperty]("GI.Gtk.Objects.Settings#g:method:resetProperty"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getIconFactory]("GI.Gtk.Interfaces.StyleProvider#g:method:getIconFactory"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getStyle]("GI.Gtk.Interfaces.StyleProvider#g:method:getStyle"), [getStyleProperty]("GI.Gtk.Interfaces.StyleProvider#g:method:getStyleProperty").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDoubleProperty]("GI.Gtk.Objects.Settings#g:method:setDoubleProperty"), [setLongProperty]("GI.Gtk.Objects.Settings#g:method:setLongProperty"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setPropertyValue]("GI.Gtk.Objects.Settings#g:method:setPropertyValue"), [setStringProperty]("GI.Gtk.Objects.Settings#g:method:setStringProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveSettingsMethod                   ,
#endif

-- ** getDefault #method:getDefault#

    settingsGetDefault                      ,


-- ** getForScreen #method:getForScreen#

    settingsGetForScreen                    ,


-- ** installProperty #method:installProperty#

    settingsInstallProperty                 ,


-- ** installPropertyParser #method:installPropertyParser#

    settingsInstallPropertyParser           ,


-- ** resetProperty #method:resetProperty#

#if defined(ENABLE_OVERLOADING)
    SettingsResetPropertyMethodInfo         ,
#endif
    settingsResetProperty                   ,


-- ** setDoubleProperty #method:setDoubleProperty#

#if defined(ENABLE_OVERLOADING)
    SettingsSetDoublePropertyMethodInfo     ,
#endif
    settingsSetDoubleProperty               ,


-- ** setLongProperty #method:setLongProperty#

#if defined(ENABLE_OVERLOADING)
    SettingsSetLongPropertyMethodInfo       ,
#endif
    settingsSetLongProperty                 ,


-- ** setPropertyValue #method:setPropertyValue#

#if defined(ENABLE_OVERLOADING)
    SettingsSetPropertyValueMethodInfo      ,
#endif
    settingsSetPropertyValue                ,


-- ** setStringProperty #method:setStringProperty#

#if defined(ENABLE_OVERLOADING)
    SettingsSetStringPropertyMethodInfo     ,
#endif
    settingsSetStringProperty               ,




 -- * Properties


-- ** colorHash #attr:colorHash#

#if defined(ENABLE_OVERLOADING)
    SettingsColorHashPropertyInfo           ,
#endif
#if defined(ENABLE_OVERLOADING)
    settingsColorHash                       ,
#endif


-- ** gtkAlternativeButtonOrder #attr:gtkAlternativeButtonOrder#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkAlternativeButtonOrderPropertyInfo,
#endif
    constructSettingsGtkAlternativeButtonOrder,
    getSettingsGtkAlternativeButtonOrder    ,
    setSettingsGtkAlternativeButtonOrder    ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkAlternativeButtonOrder       ,
#endif


-- ** gtkAlternativeSortArrows #attr:gtkAlternativeSortArrows#
-- | Controls the direction of the sort indicators in sorted list and tree
-- views. By default an arrow pointing down means the column is sorted
-- in ascending order. When set to 'P.True', this order will be inverted.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkAlternativeSortArrowsPropertyInfo,
#endif
    constructSettingsGtkAlternativeSortArrows,
    getSettingsGtkAlternativeSortArrows     ,
    setSettingsGtkAlternativeSortArrows     ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkAlternativeSortArrows        ,
#endif


-- ** gtkApplicationPreferDarkTheme #attr:gtkApplicationPreferDarkTheme#
-- | Whether the application prefers to use a dark theme. If a GTK+ theme
-- includes a dark variant, it will be used instead of the configured
-- theme.
-- 
-- Some applications benefit from minimizing the amount of light pollution that
-- interferes with the content. Good candidates for dark themes are photo and
-- video editors that make the actual content get all the attention and minimize
-- the distraction of the chrome.
-- 
-- Dark themes should not be used for documents, where large spaces are white\/light
-- and the dark chrome creates too much contrast (web browser, text editor...).
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkApplicationPreferDarkThemePropertyInfo,
#endif
    constructSettingsGtkApplicationPreferDarkTheme,
    getSettingsGtkApplicationPreferDarkTheme,
    setSettingsGtkApplicationPreferDarkTheme,
#if defined(ENABLE_OVERLOADING)
    settingsGtkApplicationPreferDarkTheme   ,
#endif


-- ** gtkAutoMnemonics #attr:gtkAutoMnemonics#
-- | Whether mnemonics should be automatically shown and hidden when the user
-- presses the mnemonic activator.
-- 
-- /Since: 2.20/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkAutoMnemonicsPropertyInfo    ,
#endif
    constructSettingsGtkAutoMnemonics       ,
    getSettingsGtkAutoMnemonics             ,
    setSettingsGtkAutoMnemonics             ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkAutoMnemonics                ,
#endif


-- ** gtkButtonImages #attr:gtkButtonImages#
-- | Whether images should be shown on buttons
-- 
-- /Since: 2.4/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkButtonImagesPropertyInfo     ,
#endif
    constructSettingsGtkButtonImages        ,
    getSettingsGtkButtonImages              ,
    setSettingsGtkButtonImages              ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkButtonImages                 ,
#endif


-- ** gtkCanChangeAccels #attr:gtkCanChangeAccels#
-- | Whether menu accelerators can be changed by pressing a key over the menu item.

#if defined(ENABLE_OVERLOADING)
    SettingsGtkCanChangeAccelsPropertyInfo  ,
#endif
    constructSettingsGtkCanChangeAccels     ,
    getSettingsGtkCanChangeAccels           ,
    setSettingsGtkCanChangeAccels           ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkCanChangeAccels              ,
#endif


-- ** gtkColorPalette #attr:gtkColorPalette#
-- | Palette to use in the deprecated color selector.

#if defined(ENABLE_OVERLOADING)
    SettingsGtkColorPalettePropertyInfo     ,
#endif
    clearSettingsGtkColorPalette            ,
    constructSettingsGtkColorPalette        ,
    getSettingsGtkColorPalette              ,
    setSettingsGtkColorPalette              ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkColorPalette                 ,
#endif


-- ** gtkColorScheme #attr:gtkColorScheme#
-- | A palette of named colors for use in themes. The format of the string is
-- >
-- >name1: color1
-- >name2: color2
-- >...
-- 
-- Color names must be acceptable as identifiers in the
-- [gtkrc][gtk3-Resource-Files] syntax, and
-- color specifications must be in the format accepted by
-- 'GI.Gdk.Functions.colorParse'.
-- 
-- Note that due to the way the color tables from different sources are
-- merged, color specifications will be converted to hexadecimal form
-- when getting this property.
-- 
-- Starting with GTK+ 2.12, the entries can alternatively be separated
-- by \';\' instead of newlines:
-- >
-- >name1: color1; name2: color2; ...
-- 
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkColorSchemePropertyInfo      ,
#endif
    clearSettingsGtkColorScheme             ,
    constructSettingsGtkColorScheme         ,
    getSettingsGtkColorScheme               ,
    setSettingsGtkColorScheme               ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkColorScheme                  ,
#endif


-- ** gtkCursorAspectRatio #attr:gtkCursorAspectRatio#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkCursorAspectRatioPropertyInfo,
#endif
    constructSettingsGtkCursorAspectRatio   ,
    getSettingsGtkCursorAspectRatio         ,
    setSettingsGtkCursorAspectRatio         ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkCursorAspectRatio            ,
#endif


-- ** gtkCursorBlink #attr:gtkCursorBlink#
-- | Whether the cursor should blink.
-- 
-- Also see the [Settings:gtkCursorBlinkTimeout]("GI.Gtk.Objects.Settings#g:attr:gtkCursorBlinkTimeout") setting,
-- which allows more flexible control over cursor blinking.

#if defined(ENABLE_OVERLOADING)
    SettingsGtkCursorBlinkPropertyInfo      ,
#endif
    constructSettingsGtkCursorBlink         ,
    getSettingsGtkCursorBlink               ,
    setSettingsGtkCursorBlink               ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkCursorBlink                  ,
#endif


-- ** gtkCursorBlinkTime #attr:gtkCursorBlinkTime#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkCursorBlinkTimePropertyInfo  ,
#endif
    constructSettingsGtkCursorBlinkTime     ,
    getSettingsGtkCursorBlinkTime           ,
    setSettingsGtkCursorBlinkTime           ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkCursorBlinkTime              ,
#endif


-- ** gtkCursorBlinkTimeout #attr:gtkCursorBlinkTimeout#
-- | Time after which the cursor stops blinking, in seconds.
-- The timer is reset after each user interaction.
-- 
-- Setting this to zero has the same effect as setting
-- [Settings:gtkCursorBlink]("GI.Gtk.Objects.Settings#g:attr:gtkCursorBlink") to 'P.False'.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkCursorBlinkTimeoutPropertyInfo,
#endif
    constructSettingsGtkCursorBlinkTimeout  ,
    getSettingsGtkCursorBlinkTimeout        ,
    setSettingsGtkCursorBlinkTimeout        ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkCursorBlinkTimeout           ,
#endif


-- ** gtkCursorThemeName #attr:gtkCursorThemeName#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkCursorThemeNamePropertyInfo  ,
#endif
    clearSettingsGtkCursorThemeName         ,
    constructSettingsGtkCursorThemeName     ,
    getSettingsGtkCursorThemeName           ,
    setSettingsGtkCursorThemeName           ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkCursorThemeName              ,
#endif


-- ** gtkCursorThemeSize #attr:gtkCursorThemeSize#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkCursorThemeSizePropertyInfo  ,
#endif
    constructSettingsGtkCursorThemeSize     ,
    getSettingsGtkCursorThemeSize           ,
    setSettingsGtkCursorThemeSize           ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkCursorThemeSize              ,
#endif


-- ** gtkDecorationLayout #attr:gtkDecorationLayout#
-- | This setting determines which buttons should be put in the
-- titlebar of client-side decorated windows, and whether they
-- should be placed at the left of right.
-- 
-- The format of the string is button names, separated by commas.
-- A colon separates the buttons that should appear on the left
-- from those on the right. Recognized button names are minimize,
-- maximize, close, icon (the window icon) and menu (a menu button
-- for the fallback app menu).
-- 
-- For example, \"menu:minimize,maximize,close\" specifies a menu
-- on the left, and minimize, maximize and close buttons on the right.
-- 
-- Note that buttons will only be shown when they are meaningful.
-- E.g. a menu button only appears when the desktop shell does not
-- show the app menu, and a close button only appears on a window
-- that can be closed.
-- 
-- Also note that the setting can be overridden with the
-- [HeaderBar:decorationLayout]("GI.Gtk.Objects.HeaderBar#g:attr:decorationLayout") property.
-- 
-- /Since: 3.12/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkDecorationLayoutPropertyInfo ,
#endif
    clearSettingsGtkDecorationLayout        ,
    constructSettingsGtkDecorationLayout    ,
    getSettingsGtkDecorationLayout          ,
    setSettingsGtkDecorationLayout          ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkDecorationLayout             ,
#endif


-- ** gtkDialogsUseHeader #attr:gtkDialogsUseHeader#
-- | Whether builtin GTK+ dialogs such as the file chooser, the
-- color chooser or the font chooser will use a header bar at
-- the top to show action widgets, or an action area at the bottom.
-- 
-- This setting does not affect custom dialogs using GtkDialog
-- directly, or message dialogs.
-- 
-- /Since: 3.12/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkDialogsUseHeaderPropertyInfo ,
#endif
    constructSettingsGtkDialogsUseHeader    ,
    getSettingsGtkDialogsUseHeader          ,
    setSettingsGtkDialogsUseHeader          ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkDialogsUseHeader             ,
#endif


-- ** gtkDndDragThreshold #attr:gtkDndDragThreshold#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkDndDragThresholdPropertyInfo ,
#endif
    constructSettingsGtkDndDragThreshold    ,
    getSettingsGtkDndDragThreshold          ,
    setSettingsGtkDndDragThreshold          ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkDndDragThreshold             ,
#endif


-- ** gtkDoubleClickDistance #attr:gtkDoubleClickDistance#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkDoubleClickDistancePropertyInfo,
#endif
    constructSettingsGtkDoubleClickDistance ,
    getSettingsGtkDoubleClickDistance       ,
    setSettingsGtkDoubleClickDistance       ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkDoubleClickDistance          ,
#endif


-- ** gtkDoubleClickTime #attr:gtkDoubleClickTime#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkDoubleClickTimePropertyInfo  ,
#endif
    constructSettingsGtkDoubleClickTime     ,
    getSettingsGtkDoubleClickTime           ,
    setSettingsGtkDoubleClickTime           ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkDoubleClickTime              ,
#endif


-- ** gtkEnableAccels #attr:gtkEnableAccels#
-- | Whether menu items should have visible accelerators which can be
-- activated.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkEnableAccelsPropertyInfo     ,
#endif
    constructSettingsGtkEnableAccels        ,
    getSettingsGtkEnableAccels              ,
    setSettingsGtkEnableAccels              ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkEnableAccels                 ,
#endif


-- ** gtkEnableAnimations #attr:gtkEnableAnimations#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkEnableAnimationsPropertyInfo ,
#endif
    constructSettingsGtkEnableAnimations    ,
    getSettingsGtkEnableAnimations          ,
    setSettingsGtkEnableAnimations          ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkEnableAnimations             ,
#endif


-- ** gtkEnableEventSounds #attr:gtkEnableEventSounds#
-- | Whether to play any event sounds at all.
-- 
-- See the <http://www.freedesktop.org/wiki/Specifications/sound-theme-spec Sound Theme Specifications>
-- for more information on event sounds and sound themes.
-- 
-- GTK+ itself does not support event sounds, you have to use a loadable
-- module like the one that comes with libcanberra.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkEnableEventSoundsPropertyInfo,
#endif
    constructSettingsGtkEnableEventSounds   ,
    getSettingsGtkEnableEventSounds         ,
    setSettingsGtkEnableEventSounds         ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkEnableEventSounds            ,
#endif


-- ** gtkEnableInputFeedbackSounds #attr:gtkEnableInputFeedbackSounds#
-- | Whether to play event sounds as feedback to user input.
-- 
-- See the <http://www.freedesktop.org/wiki/Specifications/sound-theme-spec Sound Theme Specifications>
-- for more information on event sounds and sound themes.
-- 
-- GTK+ itself does not support event sounds, you have to use a loadable
-- module like the one that comes with libcanberra.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkEnableInputFeedbackSoundsPropertyInfo,
#endif
    constructSettingsGtkEnableInputFeedbackSounds,
    getSettingsGtkEnableInputFeedbackSounds ,
    setSettingsGtkEnableInputFeedbackSounds ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkEnableInputFeedbackSounds    ,
#endif


-- ** gtkEnableMnemonics #attr:gtkEnableMnemonics#
-- | Whether labels and menu items should have visible mnemonics which
-- can be activated.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkEnableMnemonicsPropertyInfo  ,
#endif
    constructSettingsGtkEnableMnemonics     ,
    getSettingsGtkEnableMnemonics           ,
    setSettingsGtkEnableMnemonics           ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkEnableMnemonics              ,
#endif


-- ** gtkEnablePrimaryPaste #attr:gtkEnablePrimaryPaste#
-- | Whether a middle click on a mouse should paste the
-- \'PRIMARY\' clipboard content at the cursor location.
-- 
-- /Since: 3.4/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkEnablePrimaryPastePropertyInfo,
#endif
    constructSettingsGtkEnablePrimaryPaste  ,
    getSettingsGtkEnablePrimaryPaste        ,
    setSettingsGtkEnablePrimaryPaste        ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkEnablePrimaryPaste           ,
#endif


-- ** gtkEnableTooltips #attr:gtkEnableTooltips#
-- | Whether tooltips should be shown on widgets.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkEnableTooltipsPropertyInfo   ,
#endif
    constructSettingsGtkEnableTooltips      ,
    getSettingsGtkEnableTooltips            ,
    setSettingsGtkEnableTooltips            ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkEnableTooltips               ,
#endif


-- ** gtkEntryPasswordHintTimeout #attr:gtkEntryPasswordHintTimeout#
-- | How long to show the last input character in hidden
-- entries. This value is in milliseconds. 0 disables showing the
-- last char. 600 is a good value for enabling it.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkEntryPasswordHintTimeoutPropertyInfo,
#endif
    constructSettingsGtkEntryPasswordHintTimeout,
    getSettingsGtkEntryPasswordHintTimeout  ,
    setSettingsGtkEntryPasswordHintTimeout  ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkEntryPasswordHintTimeout     ,
#endif


-- ** gtkEntrySelectOnFocus #attr:gtkEntrySelectOnFocus#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkEntrySelectOnFocusPropertyInfo,
#endif
    constructSettingsGtkEntrySelectOnFocus  ,
    getSettingsGtkEntrySelectOnFocus        ,
    setSettingsGtkEntrySelectOnFocus        ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkEntrySelectOnFocus           ,
#endif


-- ** gtkErrorBell #attr:gtkErrorBell#
-- | When 'P.True', keyboard navigation and other input-related errors
-- will cause a beep. Since the error bell is implemented using
-- 'GI.Gdk.Objects.Window.windowBeep', the windowing system may offer ways to
-- configure the error bell in many ways, such as flashing the
-- window or similar visual effects.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkErrorBellPropertyInfo        ,
#endif
    constructSettingsGtkErrorBell           ,
    getSettingsGtkErrorBell                 ,
    setSettingsGtkErrorBell                 ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkErrorBell                    ,
#endif


-- ** gtkFallbackIconTheme #attr:gtkFallbackIconTheme#
-- | Name of a icon theme to fall back to.

#if defined(ENABLE_OVERLOADING)
    SettingsGtkFallbackIconThemePropertyInfo,
#endif
    clearSettingsGtkFallbackIconTheme       ,
    constructSettingsGtkFallbackIconTheme   ,
    getSettingsGtkFallbackIconTheme         ,
    setSettingsGtkFallbackIconTheme         ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkFallbackIconTheme            ,
#endif


-- ** gtkFileChooserBackend #attr:gtkFileChooserBackend#
-- | Name of the GtkFileChooser backend to use by default.

#if defined(ENABLE_OVERLOADING)
    SettingsGtkFileChooserBackendPropertyInfo,
#endif
    clearSettingsGtkFileChooserBackend      ,
    constructSettingsGtkFileChooserBackend  ,
    getSettingsGtkFileChooserBackend        ,
    setSettingsGtkFileChooserBackend        ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkFileChooserBackend           ,
#endif


-- ** gtkFontName #attr:gtkFontName#
-- | The default font to use. GTK+ uses the family name and size from this string.

#if defined(ENABLE_OVERLOADING)
    SettingsGtkFontNamePropertyInfo         ,
#endif
    clearSettingsGtkFontName                ,
    constructSettingsGtkFontName            ,
    getSettingsGtkFontName                  ,
    setSettingsGtkFontName                  ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkFontName                     ,
#endif


-- ** gtkFontconfigTimestamp #attr:gtkFontconfigTimestamp#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkFontconfigTimestampPropertyInfo,
#endif
    constructSettingsGtkFontconfigTimestamp ,
    getSettingsGtkFontconfigTimestamp       ,
    setSettingsGtkFontconfigTimestamp       ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkFontconfigTimestamp          ,
#endif


-- ** gtkIconSizes #attr:gtkIconSizes#
-- | A list of icon sizes. The list is separated by colons, and
-- item has the form:
-- 
-- @size-name@ = @width@ , @height@
-- 
-- E.g. \"gtk-menu=16,16:gtk-button=20,20:gtk-dialog=48,48\".
-- GTK+ itself use the following named icon sizes: gtk-menu,
-- gtk-button, gtk-small-toolbar, gtk-large-toolbar, gtk-dnd,
-- gtk-dialog. Applications can register their own named icon
-- sizes with 'GI.Gtk.Functions.iconSizeRegister'.

#if defined(ENABLE_OVERLOADING)
    SettingsGtkIconSizesPropertyInfo        ,
#endif
    clearSettingsGtkIconSizes               ,
    constructSettingsGtkIconSizes           ,
    getSettingsGtkIconSizes                 ,
    setSettingsGtkIconSizes                 ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkIconSizes                    ,
#endif


-- ** gtkIconThemeName #attr:gtkIconThemeName#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkIconThemeNamePropertyInfo    ,
#endif
    clearSettingsGtkIconThemeName           ,
    constructSettingsGtkIconThemeName       ,
    getSettingsGtkIconThemeName             ,
    setSettingsGtkIconThemeName             ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkIconThemeName                ,
#endif


-- ** gtkImModule #attr:gtkImModule#
-- | Which IM (input method) module should be used by default. This is the
-- input method that will be used if the user has not explicitly chosen
-- another input method from the IM context menu.
-- This also can be a colon-separated list of input methods, which GTK+
-- will try in turn until it finds one available on the system.
-- 
-- See t'GI.Gtk.Objects.IMContext.IMContext'.

#if defined(ENABLE_OVERLOADING)
    SettingsGtkImModulePropertyInfo         ,
#endif
    clearSettingsGtkImModule                ,
    constructSettingsGtkImModule            ,
    getSettingsGtkImModule                  ,
    setSettingsGtkImModule                  ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkImModule                     ,
#endif


-- ** gtkImPreeditStyle #attr:gtkImPreeditStyle#
-- | How to draw the input method preedit string.

#if defined(ENABLE_OVERLOADING)
    SettingsGtkImPreeditStylePropertyInfo   ,
#endif
    constructSettingsGtkImPreeditStyle      ,
    getSettingsGtkImPreeditStyle            ,
    setSettingsGtkImPreeditStyle            ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkImPreeditStyle               ,
#endif


-- ** gtkImStatusStyle #attr:gtkImStatusStyle#
-- | How to draw the input method statusbar.

#if defined(ENABLE_OVERLOADING)
    SettingsGtkImStatusStylePropertyInfo    ,
#endif
    constructSettingsGtkImStatusStyle       ,
    getSettingsGtkImStatusStyle             ,
    setSettingsGtkImStatusStyle             ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkImStatusStyle                ,
#endif


-- ** gtkKeyThemeName #attr:gtkKeyThemeName#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkKeyThemeNamePropertyInfo     ,
#endif
    clearSettingsGtkKeyThemeName            ,
    constructSettingsGtkKeyThemeName        ,
    getSettingsGtkKeyThemeName              ,
    setSettingsGtkKeyThemeName              ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkKeyThemeName                 ,
#endif


-- ** gtkKeynavCursorOnly #attr:gtkKeynavCursorOnly#
-- | When 'P.True', keyboard navigation should be able to reach all widgets
-- by using the cursor keys only. Tab, Shift etc. keys can\'t be expected
-- to be present on the used input device.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkKeynavCursorOnlyPropertyInfo ,
#endif
    constructSettingsGtkKeynavCursorOnly    ,
    getSettingsGtkKeynavCursorOnly          ,
    setSettingsGtkKeynavCursorOnly          ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkKeynavCursorOnly             ,
#endif


-- ** gtkKeynavUseCaret #attr:gtkKeynavUseCaret#
-- | Whether GTK+ should make sure that text can be navigated with
-- a caret, even if it is not editable. This is useful when using
-- a screen reader.
-- 
-- /Since: 3.20/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkKeynavUseCaretPropertyInfo   ,
#endif
    constructSettingsGtkKeynavUseCaret      ,
    getSettingsGtkKeynavUseCaret            ,
    setSettingsGtkKeynavUseCaret            ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkKeynavUseCaret               ,
#endif


-- ** gtkKeynavWrapAround #attr:gtkKeynavWrapAround#
-- | When 'P.True', some widgets will wrap around when doing keyboard
-- navigation, such as menus, menubars and notebooks.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkKeynavWrapAroundPropertyInfo ,
#endif
    constructSettingsGtkKeynavWrapAround    ,
    getSettingsGtkKeynavWrapAround          ,
    setSettingsGtkKeynavWrapAround          ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkKeynavWrapAround             ,
#endif


-- ** gtkLabelSelectOnFocus #attr:gtkLabelSelectOnFocus#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkLabelSelectOnFocusPropertyInfo,
#endif
    constructSettingsGtkLabelSelectOnFocus  ,
    getSettingsGtkLabelSelectOnFocus        ,
    setSettingsGtkLabelSelectOnFocus        ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkLabelSelectOnFocus           ,
#endif


-- ** gtkLongPressTime #attr:gtkLongPressTime#
-- | The time for a button or touch press to be considered a \"long press\".
-- 
-- /Since: 3.14/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkLongPressTimePropertyInfo    ,
#endif
    constructSettingsGtkLongPressTime       ,
    getSettingsGtkLongPressTime             ,
    setSettingsGtkLongPressTime             ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkLongPressTime                ,
#endif


-- ** gtkMenuBarAccel #attr:gtkMenuBarAccel#
-- | Keybinding to activate the menu bar.

#if defined(ENABLE_OVERLOADING)
    SettingsGtkMenuBarAccelPropertyInfo     ,
#endif
    clearSettingsGtkMenuBarAccel            ,
    constructSettingsGtkMenuBarAccel        ,
    getSettingsGtkMenuBarAccel              ,
    setSettingsGtkMenuBarAccel              ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkMenuBarAccel                 ,
#endif


-- ** gtkMenuBarPopupDelay #attr:gtkMenuBarPopupDelay#
-- | Delay before the submenus of a menu bar appear.

#if defined(ENABLE_OVERLOADING)
    SettingsGtkMenuBarPopupDelayPropertyInfo,
#endif
    constructSettingsGtkMenuBarPopupDelay   ,
    getSettingsGtkMenuBarPopupDelay         ,
    setSettingsGtkMenuBarPopupDelay         ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkMenuBarPopupDelay            ,
#endif


-- ** gtkMenuImages #attr:gtkMenuImages#
-- | Whether images should be shown in menu items

#if defined(ENABLE_OVERLOADING)
    SettingsGtkMenuImagesPropertyInfo       ,
#endif
    constructSettingsGtkMenuImages          ,
    getSettingsGtkMenuImages                ,
    setSettingsGtkMenuImages                ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkMenuImages                   ,
#endif


-- ** gtkMenuPopdownDelay #attr:gtkMenuPopdownDelay#
-- | The time before hiding a submenu when the pointer is moving towards the submenu.

#if defined(ENABLE_OVERLOADING)
    SettingsGtkMenuPopdownDelayPropertyInfo ,
#endif
    constructSettingsGtkMenuPopdownDelay    ,
    getSettingsGtkMenuPopdownDelay          ,
    setSettingsGtkMenuPopdownDelay          ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkMenuPopdownDelay             ,
#endif


-- ** gtkMenuPopupDelay #attr:gtkMenuPopupDelay#
-- | Minimum time the pointer must stay over a menu item before the submenu appear.

#if defined(ENABLE_OVERLOADING)
    SettingsGtkMenuPopupDelayPropertyInfo   ,
#endif
    constructSettingsGtkMenuPopupDelay      ,
    getSettingsGtkMenuPopupDelay            ,
    setSettingsGtkMenuPopupDelay            ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkMenuPopupDelay               ,
#endif


-- ** gtkModules #attr:gtkModules#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkModulesPropertyInfo          ,
#endif
    clearSettingsGtkModules                 ,
    constructSettingsGtkModules             ,
    getSettingsGtkModules                   ,
    setSettingsGtkModules                   ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkModules                      ,
#endif


-- ** gtkOverlayScrolling #attr:gtkOverlayScrolling#
-- | Whether scrolled windows may use overlayed scrolling indicators.
-- If this is set to 'P.False', scrolled windows will have permanent
-- scrollbars.
-- 
-- /Since: 3.24.9/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkOverlayScrollingPropertyInfo ,
#endif
    constructSettingsGtkOverlayScrolling    ,
    getSettingsGtkOverlayScrolling          ,
    setSettingsGtkOverlayScrolling          ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkOverlayScrolling             ,
#endif


-- ** gtkPrimaryButtonWarpsSlider #attr:gtkPrimaryButtonWarpsSlider#
-- | If the value of this setting is 'P.True', clicking the primary button in a
-- t'GI.Gtk.Objects.Range.Range' trough will move the slider, and hence set the range’s value, to
-- the point that you clicked. If it is 'P.False', a primary click will cause the
-- slider\/value to move by the range’s page-size towards the point clicked.
-- 
-- Whichever action you choose for the primary button, the other action will
-- be available by holding Shift and primary-clicking, or (since GTK+ 3.22.25)
-- clicking the middle mouse button.
-- 
-- /Since: 3.6/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkPrimaryButtonWarpsSliderPropertyInfo,
#endif
    constructSettingsGtkPrimaryButtonWarpsSlider,
    getSettingsGtkPrimaryButtonWarpsSlider  ,
    setSettingsGtkPrimaryButtonWarpsSlider  ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkPrimaryButtonWarpsSlider     ,
#endif


-- ** gtkPrintBackends #attr:gtkPrintBackends#
-- | A comma-separated list of print backends to use in the print
-- dialog. Available print backends depend on the GTK+ installation,
-- and may include \"file\", \"cups\", \"lpr\" or \"papi\".
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkPrintBackendsPropertyInfo    ,
#endif
    clearSettingsGtkPrintBackends           ,
    constructSettingsGtkPrintBackends       ,
    getSettingsGtkPrintBackends             ,
    setSettingsGtkPrintBackends             ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkPrintBackends                ,
#endif


-- ** gtkPrintPreviewCommand #attr:gtkPrintPreviewCommand#
-- | A command to run for displaying the print preview. The command
-- should contain a @%f@ placeholder, which will get replaced by
-- the path to the pdf file. The command may also contain a @%s@
-- placeholder, which will get replaced by the path to a file
-- containing the print settings in the format produced by
-- 'GI.Gtk.Objects.PrintSettings.printSettingsToFile'.
-- 
-- The preview application is responsible for removing the pdf file
-- and the print settings file when it is done.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkPrintPreviewCommandPropertyInfo,
#endif
    clearSettingsGtkPrintPreviewCommand     ,
    constructSettingsGtkPrintPreviewCommand ,
    getSettingsGtkPrintPreviewCommand       ,
    setSettingsGtkPrintPreviewCommand       ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkPrintPreviewCommand          ,
#endif


-- ** gtkRecentFilesEnabled #attr:gtkRecentFilesEnabled#
-- | Whether GTK+ should keep track of items inside the recently used
-- resources list. If set to 'P.False', the list will always be empty.
-- 
-- /Since: 3.8/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkRecentFilesEnabledPropertyInfo,
#endif
    constructSettingsGtkRecentFilesEnabled  ,
    getSettingsGtkRecentFilesEnabled        ,
    setSettingsGtkRecentFilesEnabled        ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkRecentFilesEnabled           ,
#endif


-- ** gtkRecentFilesLimit #attr:gtkRecentFilesLimit#
-- | The number of recently used files that should be displayed by default by
-- t'GI.Gtk.Interfaces.RecentChooser.RecentChooser' implementations and by the t'GI.Gtk.Interfaces.FileChooser.FileChooser'. A value of
-- -1 means every recently used file stored.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkRecentFilesLimitPropertyInfo ,
#endif
    constructSettingsGtkRecentFilesLimit    ,
    getSettingsGtkRecentFilesLimit          ,
    setSettingsGtkRecentFilesLimit          ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkRecentFilesLimit             ,
#endif


-- ** gtkRecentFilesMaxAge #attr:gtkRecentFilesMaxAge#
-- | The maximum age, in days, of the items inside the recently used
-- resources list. Items older than this setting will be excised
-- from the list. If set to 0, the list will always be empty; if
-- set to -1, no item will be removed.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkRecentFilesMaxAgePropertyInfo,
#endif
    constructSettingsGtkRecentFilesMaxAge   ,
    getSettingsGtkRecentFilesMaxAge         ,
    setSettingsGtkRecentFilesMaxAge         ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkRecentFilesMaxAge            ,
#endif


-- ** gtkScrolledWindowPlacement #attr:gtkScrolledWindowPlacement#
-- | Where the contents of scrolled windows are located with respect to the
-- scrollbars, if not overridden by the scrolled window\'s own placement.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkScrolledWindowPlacementPropertyInfo,
#endif
    constructSettingsGtkScrolledWindowPlacement,
    getSettingsGtkScrolledWindowPlacement   ,
    setSettingsGtkScrolledWindowPlacement   ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkScrolledWindowPlacement      ,
#endif


-- ** gtkShellShowsAppMenu #attr:gtkShellShowsAppMenu#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkShellShowsAppMenuPropertyInfo,
#endif
    constructSettingsGtkShellShowsAppMenu   ,
    getSettingsGtkShellShowsAppMenu         ,
    setSettingsGtkShellShowsAppMenu         ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkShellShowsAppMenu            ,
#endif


-- ** gtkShellShowsDesktop #attr:gtkShellShowsDesktop#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkShellShowsDesktopPropertyInfo,
#endif
    constructSettingsGtkShellShowsDesktop   ,
    getSettingsGtkShellShowsDesktop         ,
    setSettingsGtkShellShowsDesktop         ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkShellShowsDesktop            ,
#endif


-- ** gtkShellShowsMenubar #attr:gtkShellShowsMenubar#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkShellShowsMenubarPropertyInfo,
#endif
    constructSettingsGtkShellShowsMenubar   ,
    getSettingsGtkShellShowsMenubar         ,
    setSettingsGtkShellShowsMenubar         ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkShellShowsMenubar            ,
#endif


-- ** gtkShowInputMethodMenu #attr:gtkShowInputMethodMenu#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkShowInputMethodMenuPropertyInfo,
#endif
    constructSettingsGtkShowInputMethodMenu ,
    getSettingsGtkShowInputMethodMenu       ,
    setSettingsGtkShowInputMethodMenu       ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkShowInputMethodMenu          ,
#endif


-- ** gtkShowUnicodeMenu #attr:gtkShowUnicodeMenu#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkShowUnicodeMenuPropertyInfo  ,
#endif
    constructSettingsGtkShowUnicodeMenu     ,
    getSettingsGtkShowUnicodeMenu           ,
    setSettingsGtkShowUnicodeMenu           ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkShowUnicodeMenu              ,
#endif


-- ** gtkSoundThemeName #attr:gtkSoundThemeName#
-- | The XDG sound theme to use for event sounds.
-- 
-- See the <http://www.freedesktop.org/wiki/Specifications/sound-theme-spec Sound Theme Specifications>
-- for more information on event sounds and sound themes.
-- 
-- GTK+ itself does not support event sounds, you have to use a loadable
-- module like the one that comes with libcanberra.
-- 
-- /Since: 2.14/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkSoundThemeNamePropertyInfo   ,
#endif
    clearSettingsGtkSoundThemeName          ,
    constructSettingsGtkSoundThemeName      ,
    getSettingsGtkSoundThemeName            ,
    setSettingsGtkSoundThemeName            ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkSoundThemeName               ,
#endif


-- ** gtkSplitCursor #attr:gtkSplitCursor#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkSplitCursorPropertyInfo      ,
#endif
    constructSettingsGtkSplitCursor         ,
    getSettingsGtkSplitCursor               ,
    setSettingsGtkSplitCursor               ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkSplitCursor                  ,
#endif


-- ** gtkThemeName #attr:gtkThemeName#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkThemeNamePropertyInfo        ,
#endif
    clearSettingsGtkThemeName               ,
    constructSettingsGtkThemeName           ,
    getSettingsGtkThemeName                 ,
    setSettingsGtkThemeName                 ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkThemeName                    ,
#endif


-- ** gtkTimeoutExpand #attr:gtkTimeoutExpand#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkTimeoutExpandPropertyInfo    ,
#endif
    constructSettingsGtkTimeoutExpand       ,
    getSettingsGtkTimeoutExpand             ,
    setSettingsGtkTimeoutExpand             ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkTimeoutExpand                ,
#endif


-- ** gtkTimeoutInitial #attr:gtkTimeoutInitial#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkTimeoutInitialPropertyInfo   ,
#endif
    constructSettingsGtkTimeoutInitial      ,
    getSettingsGtkTimeoutInitial            ,
    setSettingsGtkTimeoutInitial            ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkTimeoutInitial               ,
#endif


-- ** gtkTimeoutRepeat #attr:gtkTimeoutRepeat#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkTimeoutRepeatPropertyInfo    ,
#endif
    constructSettingsGtkTimeoutRepeat       ,
    getSettingsGtkTimeoutRepeat             ,
    setSettingsGtkTimeoutRepeat             ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkTimeoutRepeat                ,
#endif


-- ** gtkTitlebarDoubleClick #attr:gtkTitlebarDoubleClick#
-- | This setting determines the action to take when a double-click
-- occurs on the titlebar of client-side decorated windows.
-- 
-- Recognized actions are minimize, toggle-maximize, menu, lower
-- or none.
-- 
-- /Since: 3.14/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkTitlebarDoubleClickPropertyInfo,
#endif
    clearSettingsGtkTitlebarDoubleClick     ,
    constructSettingsGtkTitlebarDoubleClick ,
    getSettingsGtkTitlebarDoubleClick       ,
    setSettingsGtkTitlebarDoubleClick       ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkTitlebarDoubleClick          ,
#endif


-- ** gtkTitlebarMiddleClick #attr:gtkTitlebarMiddleClick#
-- | This setting determines the action to take when a middle-click
-- occurs on the titlebar of client-side decorated windows.
-- 
-- Recognized actions are minimize, toggle-maximize, menu, lower
-- or none.
-- 
-- /Since: 3.14/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkTitlebarMiddleClickPropertyInfo,
#endif
    clearSettingsGtkTitlebarMiddleClick     ,
    constructSettingsGtkTitlebarMiddleClick ,
    getSettingsGtkTitlebarMiddleClick       ,
    setSettingsGtkTitlebarMiddleClick       ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkTitlebarMiddleClick          ,
#endif


-- ** gtkTitlebarRightClick #attr:gtkTitlebarRightClick#
-- | This setting determines the action to take when a right-click
-- occurs on the titlebar of client-side decorated windows.
-- 
-- Recognized actions are minimize, toggle-maximize, menu, lower
-- or none.
-- 
-- /Since: 3.14/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkTitlebarRightClickPropertyInfo,
#endif
    clearSettingsGtkTitlebarRightClick      ,
    constructSettingsGtkTitlebarRightClick  ,
    getSettingsGtkTitlebarRightClick        ,
    setSettingsGtkTitlebarRightClick        ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkTitlebarRightClick           ,
#endif


-- ** gtkToolbarIconSize #attr:gtkToolbarIconSize#
-- | The size of icons in default toolbars.

#if defined(ENABLE_OVERLOADING)
    SettingsGtkToolbarIconSizePropertyInfo  ,
#endif
    constructSettingsGtkToolbarIconSize     ,
    getSettingsGtkToolbarIconSize           ,
    setSettingsGtkToolbarIconSize           ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkToolbarIconSize              ,
#endif


-- ** gtkToolbarStyle #attr:gtkToolbarStyle#
-- | The size of icons in default toolbars.

#if defined(ENABLE_OVERLOADING)
    SettingsGtkToolbarStylePropertyInfo     ,
#endif
    constructSettingsGtkToolbarStyle        ,
    getSettingsGtkToolbarStyle              ,
    setSettingsGtkToolbarStyle              ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkToolbarStyle                 ,
#endif


-- ** gtkTooltipBrowseModeTimeout #attr:gtkTooltipBrowseModeTimeout#
-- | Amount of time, in milliseconds, after which the browse mode
-- will be disabled.
-- 
-- See [Settings:gtkTooltipBrowseTimeout]("GI.Gtk.Objects.Settings#g:attr:gtkTooltipBrowseTimeout") for more information
-- about browse mode.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkTooltipBrowseModeTimeoutPropertyInfo,
#endif
    constructSettingsGtkTooltipBrowseModeTimeout,
    getSettingsGtkTooltipBrowseModeTimeout  ,
    setSettingsGtkTooltipBrowseModeTimeout  ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkTooltipBrowseModeTimeout     ,
#endif


-- ** gtkTooltipBrowseTimeout #attr:gtkTooltipBrowseTimeout#
-- | Controls the time after which tooltips will appear when
-- browse mode is enabled, in milliseconds.
-- 
-- Browse mode is enabled when the mouse pointer moves off an object
-- where a tooltip was currently being displayed. If the mouse pointer
-- hits another object before the browse mode timeout expires (see
-- [Settings:gtkTooltipBrowseModeTimeout]("GI.Gtk.Objects.Settings#g:attr:gtkTooltipBrowseModeTimeout")), it will take the
-- amount of milliseconds specified by this setting to popup the tooltip
-- for the new object.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkTooltipBrowseTimeoutPropertyInfo,
#endif
    constructSettingsGtkTooltipBrowseTimeout,
    getSettingsGtkTooltipBrowseTimeout      ,
    setSettingsGtkTooltipBrowseTimeout      ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkTooltipBrowseTimeout         ,
#endif


-- ** gtkTooltipTimeout #attr:gtkTooltipTimeout#
-- | Time, in milliseconds, after which a tooltip could appear if the
-- cursor is hovering on top of a widget.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkTooltipTimeoutPropertyInfo   ,
#endif
    constructSettingsGtkTooltipTimeout      ,
    getSettingsGtkTooltipTimeout            ,
    setSettingsGtkTooltipTimeout            ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkTooltipTimeout               ,
#endif


-- ** gtkTouchscreenMode #attr:gtkTouchscreenMode#
-- | When 'P.True', there are no motion notify events delivered on this screen,
-- and widgets can\'t use the pointer hovering them for any essential
-- functionality.
-- 
-- /Since: 2.10/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkTouchscreenModePropertyInfo  ,
#endif
    constructSettingsGtkTouchscreenMode     ,
    getSettingsGtkTouchscreenMode           ,
    setSettingsGtkTouchscreenMode           ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkTouchscreenMode              ,
#endif


-- ** gtkVisibleFocus #attr:gtkVisibleFocus#
-- | Whether \'focus rectangles\' should be always visible, never visible,
-- or hidden until the user starts to use the keyboard.
-- 
-- /Since: 3.2/

#if defined(ENABLE_OVERLOADING)
    SettingsGtkVisibleFocusPropertyInfo     ,
#endif
    constructSettingsGtkVisibleFocus        ,
    getSettingsGtkVisibleFocus              ,
    setSettingsGtkVisibleFocus              ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkVisibleFocus                 ,
#endif


-- ** gtkXftAntialias #attr:gtkXftAntialias#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkXftAntialiasPropertyInfo     ,
#endif
    constructSettingsGtkXftAntialias        ,
    getSettingsGtkXftAntialias              ,
    setSettingsGtkXftAntialias              ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkXftAntialias                 ,
#endif


-- ** gtkXftDpi #attr:gtkXftDpi#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkXftDpiPropertyInfo           ,
#endif
    constructSettingsGtkXftDpi              ,
    getSettingsGtkXftDpi                    ,
    setSettingsGtkXftDpi                    ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkXftDpi                       ,
#endif


-- ** gtkXftHinting #attr:gtkXftHinting#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkXftHintingPropertyInfo       ,
#endif
    constructSettingsGtkXftHinting          ,
    getSettingsGtkXftHinting                ,
    setSettingsGtkXftHinting                ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkXftHinting                   ,
#endif


-- ** gtkXftHintstyle #attr:gtkXftHintstyle#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkXftHintstylePropertyInfo     ,
#endif
    clearSettingsGtkXftHintstyle            ,
    constructSettingsGtkXftHintstyle        ,
    getSettingsGtkXftHintstyle              ,
    setSettingsGtkXftHintstyle              ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkXftHintstyle                 ,
#endif


-- ** gtkXftRgba #attr:gtkXftRgba#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    SettingsGtkXftRgbaPropertyInfo          ,
#endif
    clearSettingsGtkXftRgba                 ,
    constructSettingsGtkXftRgba             ,
    getSettingsGtkXftRgba                   ,
    setSettingsGtkXftRgba                   ,
#if defined(ENABLE_OVERLOADING)
    settingsGtkXftRgba                      ,
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

import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gdk.Objects.Screen as Gdk.Screen
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.StyleProvider as Gtk.StyleProvider
import {-# SOURCE #-} qualified GI.Gtk.Structs.SettingsValue as Gtk.SettingsValue

-- | Memory-managed wrapper type.
newtype Settings = Settings (SP.ManagedPtr Settings)
    deriving (Eq)

instance SP.ManagedPtrNewtype Settings where
    toManagedPtr (Settings p) = p

foreign import ccall "gtk_settings_get_type"
    c_gtk_settings_get_type :: IO B.Types.GType

instance B.Types.TypedObject Settings where
    glibType = c_gtk_settings_get_type

instance B.Types.GObject Settings

-- | Type class for types which can be safely cast to `Settings`, for instance with `toSettings`.
class (SP.GObject o, O.IsDescendantOf Settings o) => IsSettings o
instance (SP.GObject o, O.IsDescendantOf Settings o) => IsSettings o

instance O.HasParentTypes Settings
type instance O.ParentTypes Settings = '[GObject.Object.Object, Gtk.StyleProvider.StyleProvider]

-- | Cast to `Settings`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toSettings :: (MIO.MonadIO m, IsSettings o) => o -> m Settings
toSettings = MIO.liftIO . B.ManagedPtr.unsafeCastTo Settings

-- | Convert 'Settings' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Settings) where
    gvalueGType_ = c_gtk_settings_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Settings)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Settings)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Settings ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveSettingsMethod (t :: Symbol) (o :: *) :: * where
    ResolveSettingsMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveSettingsMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveSettingsMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveSettingsMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveSettingsMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveSettingsMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveSettingsMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveSettingsMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveSettingsMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveSettingsMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveSettingsMethod "resetProperty" o = SettingsResetPropertyMethodInfo
    ResolveSettingsMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveSettingsMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveSettingsMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveSettingsMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveSettingsMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveSettingsMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveSettingsMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveSettingsMethod "getIconFactory" o = Gtk.StyleProvider.StyleProviderGetIconFactoryMethodInfo
    ResolveSettingsMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveSettingsMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveSettingsMethod "getStyle" o = Gtk.StyleProvider.StyleProviderGetStyleMethodInfo
    ResolveSettingsMethod "getStyleProperty" o = Gtk.StyleProvider.StyleProviderGetStylePropertyMethodInfo
    ResolveSettingsMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveSettingsMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveSettingsMethod "setDoubleProperty" o = SettingsSetDoublePropertyMethodInfo
    ResolveSettingsMethod "setLongProperty" o = SettingsSetLongPropertyMethodInfo
    ResolveSettingsMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveSettingsMethod "setPropertyValue" o = SettingsSetPropertyValueMethodInfo
    ResolveSettingsMethod "setStringProperty" o = SettingsSetStringPropertyMethodInfo
    ResolveSettingsMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveSettingsMethod t Settings, O.OverloadedMethod info Settings p) => OL.IsLabel t (Settings -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveSettingsMethod t Settings, O.OverloadedMethod info Settings p, R.HasField t Settings p) => R.HasField t Settings p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveSettingsMethod t Settings, O.OverloadedMethodInfo info Settings) => OL.IsLabel t (O.MethodProxy info Settings) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- XXX Generation of property "color-hash" of object "Settings" failed.
-- Not implemented: Property SettingsColorHash has unsupported transfer type TransferContainer
#if defined(ENABLE_OVERLOADING)
-- XXX Placeholder
data SettingsColorHashPropertyInfo
instance AttrInfo SettingsColorHashPropertyInfo where
    type AttrAllowedOps SettingsColorHashPropertyInfo = '[]
    type AttrSetTypeConstraint SettingsColorHashPropertyInfo = (~) ()
    type AttrTransferTypeConstraint SettingsColorHashPropertyInfo = (~) ()
    type AttrTransferType SettingsColorHashPropertyInfo = ()
    type AttrBaseTypeConstraint SettingsColorHashPropertyInfo = (~) ()
    type AttrGetType SettingsColorHashPropertyInfo = ()
    type AttrLabel SettingsColorHashPropertyInfo = ""
    type AttrOrigin SettingsColorHashPropertyInfo = Settings
    attrGet = undefined
    attrSet = undefined
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer = undefined
#endif

-- VVV Prop "gtk-alternative-button-order"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-alternative-button-order@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkAlternativeButtonOrder
-- @
getSettingsGtkAlternativeButtonOrder :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkAlternativeButtonOrder obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-alternative-button-order"

-- | Set the value of the “@gtk-alternative-button-order@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkAlternativeButtonOrder 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkAlternativeButtonOrder :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkAlternativeButtonOrder obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-alternative-button-order" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-alternative-button-order@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkAlternativeButtonOrder :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkAlternativeButtonOrder val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-alternative-button-order" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkAlternativeButtonOrderPropertyInfo
instance AttrInfo SettingsGtkAlternativeButtonOrderPropertyInfo where
    type AttrAllowedOps SettingsGtkAlternativeButtonOrderPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkAlternativeButtonOrderPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkAlternativeButtonOrderPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkAlternativeButtonOrderPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkAlternativeButtonOrderPropertyInfo = Bool
    type AttrGetType SettingsGtkAlternativeButtonOrderPropertyInfo = Bool
    type AttrLabel SettingsGtkAlternativeButtonOrderPropertyInfo = "gtk-alternative-button-order"
    type AttrOrigin SettingsGtkAlternativeButtonOrderPropertyInfo = Settings
    attrGet = getSettingsGtkAlternativeButtonOrder
    attrSet = setSettingsGtkAlternativeButtonOrder
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkAlternativeButtonOrder
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkAlternativeButtonOrder"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkAlternativeButtonOrder"
        })
#endif

-- VVV Prop "gtk-alternative-sort-arrows"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-alternative-sort-arrows@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkAlternativeSortArrows
-- @
getSettingsGtkAlternativeSortArrows :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkAlternativeSortArrows obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-alternative-sort-arrows"

-- | Set the value of the “@gtk-alternative-sort-arrows@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkAlternativeSortArrows 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkAlternativeSortArrows :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkAlternativeSortArrows obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-alternative-sort-arrows" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-alternative-sort-arrows@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkAlternativeSortArrows :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkAlternativeSortArrows val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-alternative-sort-arrows" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkAlternativeSortArrowsPropertyInfo
instance AttrInfo SettingsGtkAlternativeSortArrowsPropertyInfo where
    type AttrAllowedOps SettingsGtkAlternativeSortArrowsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkAlternativeSortArrowsPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkAlternativeSortArrowsPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkAlternativeSortArrowsPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkAlternativeSortArrowsPropertyInfo = Bool
    type AttrGetType SettingsGtkAlternativeSortArrowsPropertyInfo = Bool
    type AttrLabel SettingsGtkAlternativeSortArrowsPropertyInfo = "gtk-alternative-sort-arrows"
    type AttrOrigin SettingsGtkAlternativeSortArrowsPropertyInfo = Settings
    attrGet = getSettingsGtkAlternativeSortArrows
    attrSet = setSettingsGtkAlternativeSortArrows
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkAlternativeSortArrows
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkAlternativeSortArrows"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkAlternativeSortArrows"
        })
#endif

-- VVV Prop "gtk-application-prefer-dark-theme"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-application-prefer-dark-theme@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkApplicationPreferDarkTheme
-- @
getSettingsGtkApplicationPreferDarkTheme :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkApplicationPreferDarkTheme obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-application-prefer-dark-theme"

-- | Set the value of the “@gtk-application-prefer-dark-theme@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkApplicationPreferDarkTheme 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkApplicationPreferDarkTheme :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkApplicationPreferDarkTheme obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-application-prefer-dark-theme" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-application-prefer-dark-theme@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkApplicationPreferDarkTheme :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkApplicationPreferDarkTheme val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-application-prefer-dark-theme" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkApplicationPreferDarkThemePropertyInfo
instance AttrInfo SettingsGtkApplicationPreferDarkThemePropertyInfo where
    type AttrAllowedOps SettingsGtkApplicationPreferDarkThemePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkApplicationPreferDarkThemePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkApplicationPreferDarkThemePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkApplicationPreferDarkThemePropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkApplicationPreferDarkThemePropertyInfo = Bool
    type AttrGetType SettingsGtkApplicationPreferDarkThemePropertyInfo = Bool
    type AttrLabel SettingsGtkApplicationPreferDarkThemePropertyInfo = "gtk-application-prefer-dark-theme"
    type AttrOrigin SettingsGtkApplicationPreferDarkThemePropertyInfo = Settings
    attrGet = getSettingsGtkApplicationPreferDarkTheme
    attrSet = setSettingsGtkApplicationPreferDarkTheme
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkApplicationPreferDarkTheme
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkApplicationPreferDarkTheme"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkApplicationPreferDarkTheme"
        })
#endif

-- VVV Prop "gtk-auto-mnemonics"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-auto-mnemonics@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkAutoMnemonics
-- @
getSettingsGtkAutoMnemonics :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkAutoMnemonics obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-auto-mnemonics"

-- | Set the value of the “@gtk-auto-mnemonics@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkAutoMnemonics 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkAutoMnemonics :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkAutoMnemonics obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-auto-mnemonics" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-auto-mnemonics@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkAutoMnemonics :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkAutoMnemonics val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-auto-mnemonics" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkAutoMnemonicsPropertyInfo
instance AttrInfo SettingsGtkAutoMnemonicsPropertyInfo where
    type AttrAllowedOps SettingsGtkAutoMnemonicsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkAutoMnemonicsPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkAutoMnemonicsPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkAutoMnemonicsPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkAutoMnemonicsPropertyInfo = Bool
    type AttrGetType SettingsGtkAutoMnemonicsPropertyInfo = Bool
    type AttrLabel SettingsGtkAutoMnemonicsPropertyInfo = "gtk-auto-mnemonics"
    type AttrOrigin SettingsGtkAutoMnemonicsPropertyInfo = Settings
    attrGet = getSettingsGtkAutoMnemonics
    attrSet = setSettingsGtkAutoMnemonics
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkAutoMnemonics
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkAutoMnemonics"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkAutoMnemonics"
        })
#endif

-- VVV Prop "gtk-button-images"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-button-images@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkButtonImages
-- @
getSettingsGtkButtonImages :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkButtonImages obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-button-images"

-- | Set the value of the “@gtk-button-images@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkButtonImages 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkButtonImages :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkButtonImages obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-button-images" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-button-images@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkButtonImages :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkButtonImages val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-button-images" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkButtonImagesPropertyInfo
instance AttrInfo SettingsGtkButtonImagesPropertyInfo where
    type AttrAllowedOps SettingsGtkButtonImagesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkButtonImagesPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkButtonImagesPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkButtonImagesPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkButtonImagesPropertyInfo = Bool
    type AttrGetType SettingsGtkButtonImagesPropertyInfo = Bool
    type AttrLabel SettingsGtkButtonImagesPropertyInfo = "gtk-button-images"
    type AttrOrigin SettingsGtkButtonImagesPropertyInfo = Settings
    attrGet = getSettingsGtkButtonImages
    attrSet = setSettingsGtkButtonImages
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkButtonImages
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkButtonImages"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkButtonImages"
        })
#endif

-- VVV Prop "gtk-can-change-accels"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-can-change-accels@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkCanChangeAccels
-- @
getSettingsGtkCanChangeAccels :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkCanChangeAccels obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-can-change-accels"

-- | Set the value of the “@gtk-can-change-accels@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkCanChangeAccels 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkCanChangeAccels :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkCanChangeAccels obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-can-change-accels" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-can-change-accels@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkCanChangeAccels :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkCanChangeAccels val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-can-change-accels" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkCanChangeAccelsPropertyInfo
instance AttrInfo SettingsGtkCanChangeAccelsPropertyInfo where
    type AttrAllowedOps SettingsGtkCanChangeAccelsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkCanChangeAccelsPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkCanChangeAccelsPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkCanChangeAccelsPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkCanChangeAccelsPropertyInfo = Bool
    type AttrGetType SettingsGtkCanChangeAccelsPropertyInfo = Bool
    type AttrLabel SettingsGtkCanChangeAccelsPropertyInfo = "gtk-can-change-accels"
    type AttrOrigin SettingsGtkCanChangeAccelsPropertyInfo = Settings
    attrGet = getSettingsGtkCanChangeAccels
    attrSet = setSettingsGtkCanChangeAccels
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkCanChangeAccels
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkCanChangeAccels"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkCanChangeAccels"
        })
#endif

-- VVV Prop "gtk-color-palette"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-color-palette@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkColorPalette
-- @
getSettingsGtkColorPalette :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkColorPalette obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-color-palette"

-- | Set the value of the “@gtk-color-palette@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkColorPalette 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkColorPalette :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkColorPalette obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-color-palette" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-color-palette@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkColorPalette :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkColorPalette val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-color-palette" (P.Just val)

-- | Set the value of the “@gtk-color-palette@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkColorPalette
-- @
clearSettingsGtkColorPalette :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkColorPalette obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-color-palette" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkColorPalettePropertyInfo
instance AttrInfo SettingsGtkColorPalettePropertyInfo where
    type AttrAllowedOps SettingsGtkColorPalettePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkColorPalettePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkColorPalettePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkColorPalettePropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkColorPalettePropertyInfo = T.Text
    type AttrGetType SettingsGtkColorPalettePropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkColorPalettePropertyInfo = "gtk-color-palette"
    type AttrOrigin SettingsGtkColorPalettePropertyInfo = Settings
    attrGet = getSettingsGtkColorPalette
    attrSet = setSettingsGtkColorPalette
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkColorPalette
    attrClear = clearSettingsGtkColorPalette
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkColorPalette"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkColorPalette"
        })
#endif

-- VVV Prop "gtk-color-scheme"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-color-scheme@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkColorScheme
-- @
getSettingsGtkColorScheme :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkColorScheme obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-color-scheme"

-- | Set the value of the “@gtk-color-scheme@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkColorScheme 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkColorScheme :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkColorScheme obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-color-scheme" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-color-scheme@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkColorScheme :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkColorScheme val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-color-scheme" (P.Just val)

-- | Set the value of the “@gtk-color-scheme@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkColorScheme
-- @
clearSettingsGtkColorScheme :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkColorScheme obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-color-scheme" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkColorSchemePropertyInfo
instance AttrInfo SettingsGtkColorSchemePropertyInfo where
    type AttrAllowedOps SettingsGtkColorSchemePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkColorSchemePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkColorSchemePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkColorSchemePropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkColorSchemePropertyInfo = T.Text
    type AttrGetType SettingsGtkColorSchemePropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkColorSchemePropertyInfo = "gtk-color-scheme"
    type AttrOrigin SettingsGtkColorSchemePropertyInfo = Settings
    attrGet = getSettingsGtkColorScheme
    attrSet = setSettingsGtkColorScheme
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkColorScheme
    attrClear = clearSettingsGtkColorScheme
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkColorScheme"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkColorScheme"
        })
#endif

-- VVV Prop "gtk-cursor-aspect-ratio"
   -- Type: TBasicType TFloat
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-cursor-aspect-ratio@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkCursorAspectRatio
-- @
getSettingsGtkCursorAspectRatio :: (MonadIO m, IsSettings o) => o -> m Float
getSettingsGtkCursorAspectRatio obj = MIO.liftIO $ B.Properties.getObjectPropertyFloat obj "gtk-cursor-aspect-ratio"

-- | Set the value of the “@gtk-cursor-aspect-ratio@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkCursorAspectRatio 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkCursorAspectRatio :: (MonadIO m, IsSettings o) => o -> Float -> m ()
setSettingsGtkCursorAspectRatio obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFloat obj "gtk-cursor-aspect-ratio" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-cursor-aspect-ratio@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkCursorAspectRatio :: (IsSettings o, MIO.MonadIO m) => Float -> m (GValueConstruct o)
constructSettingsGtkCursorAspectRatio val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFloat "gtk-cursor-aspect-ratio" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkCursorAspectRatioPropertyInfo
instance AttrInfo SettingsGtkCursorAspectRatioPropertyInfo where
    type AttrAllowedOps SettingsGtkCursorAspectRatioPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkCursorAspectRatioPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkCursorAspectRatioPropertyInfo = (~) Float
    type AttrTransferTypeConstraint SettingsGtkCursorAspectRatioPropertyInfo = (~) Float
    type AttrTransferType SettingsGtkCursorAspectRatioPropertyInfo = Float
    type AttrGetType SettingsGtkCursorAspectRatioPropertyInfo = Float
    type AttrLabel SettingsGtkCursorAspectRatioPropertyInfo = "gtk-cursor-aspect-ratio"
    type AttrOrigin SettingsGtkCursorAspectRatioPropertyInfo = Settings
    attrGet = getSettingsGtkCursorAspectRatio
    attrSet = setSettingsGtkCursorAspectRatio
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkCursorAspectRatio
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkCursorAspectRatio"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkCursorAspectRatio"
        })
#endif

-- VVV Prop "gtk-cursor-blink"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-cursor-blink@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkCursorBlink
-- @
getSettingsGtkCursorBlink :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkCursorBlink obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-cursor-blink"

-- | Set the value of the “@gtk-cursor-blink@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkCursorBlink 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkCursorBlink :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkCursorBlink obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-cursor-blink" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-cursor-blink@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkCursorBlink :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkCursorBlink val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-cursor-blink" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkCursorBlinkPropertyInfo
instance AttrInfo SettingsGtkCursorBlinkPropertyInfo where
    type AttrAllowedOps SettingsGtkCursorBlinkPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkCursorBlinkPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkCursorBlinkPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkCursorBlinkPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkCursorBlinkPropertyInfo = Bool
    type AttrGetType SettingsGtkCursorBlinkPropertyInfo = Bool
    type AttrLabel SettingsGtkCursorBlinkPropertyInfo = "gtk-cursor-blink"
    type AttrOrigin SettingsGtkCursorBlinkPropertyInfo = Settings
    attrGet = getSettingsGtkCursorBlink
    attrSet = setSettingsGtkCursorBlink
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkCursorBlink
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkCursorBlink"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkCursorBlink"
        })
#endif

-- VVV Prop "gtk-cursor-blink-time"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-cursor-blink-time@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkCursorBlinkTime
-- @
getSettingsGtkCursorBlinkTime :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkCursorBlinkTime obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-cursor-blink-time"

-- | Set the value of the “@gtk-cursor-blink-time@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkCursorBlinkTime 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkCursorBlinkTime :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkCursorBlinkTime obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-cursor-blink-time" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-cursor-blink-time@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkCursorBlinkTime :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkCursorBlinkTime val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-cursor-blink-time" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkCursorBlinkTimePropertyInfo
instance AttrInfo SettingsGtkCursorBlinkTimePropertyInfo where
    type AttrAllowedOps SettingsGtkCursorBlinkTimePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkCursorBlinkTimePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkCursorBlinkTimePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkCursorBlinkTimePropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkCursorBlinkTimePropertyInfo = Int32
    type AttrGetType SettingsGtkCursorBlinkTimePropertyInfo = Int32
    type AttrLabel SettingsGtkCursorBlinkTimePropertyInfo = "gtk-cursor-blink-time"
    type AttrOrigin SettingsGtkCursorBlinkTimePropertyInfo = Settings
    attrGet = getSettingsGtkCursorBlinkTime
    attrSet = setSettingsGtkCursorBlinkTime
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkCursorBlinkTime
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkCursorBlinkTime"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkCursorBlinkTime"
        })
#endif

-- VVV Prop "gtk-cursor-blink-timeout"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-cursor-blink-timeout@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkCursorBlinkTimeout
-- @
getSettingsGtkCursorBlinkTimeout :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkCursorBlinkTimeout obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-cursor-blink-timeout"

-- | Set the value of the “@gtk-cursor-blink-timeout@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkCursorBlinkTimeout 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkCursorBlinkTimeout :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkCursorBlinkTimeout obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-cursor-blink-timeout" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-cursor-blink-timeout@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkCursorBlinkTimeout :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkCursorBlinkTimeout val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-cursor-blink-timeout" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkCursorBlinkTimeoutPropertyInfo
instance AttrInfo SettingsGtkCursorBlinkTimeoutPropertyInfo where
    type AttrAllowedOps SettingsGtkCursorBlinkTimeoutPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkCursorBlinkTimeoutPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkCursorBlinkTimeoutPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkCursorBlinkTimeoutPropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkCursorBlinkTimeoutPropertyInfo = Int32
    type AttrGetType SettingsGtkCursorBlinkTimeoutPropertyInfo = Int32
    type AttrLabel SettingsGtkCursorBlinkTimeoutPropertyInfo = "gtk-cursor-blink-timeout"
    type AttrOrigin SettingsGtkCursorBlinkTimeoutPropertyInfo = Settings
    attrGet = getSettingsGtkCursorBlinkTimeout
    attrSet = setSettingsGtkCursorBlinkTimeout
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkCursorBlinkTimeout
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkCursorBlinkTimeout"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkCursorBlinkTimeout"
        })
#endif

-- VVV Prop "gtk-cursor-theme-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-cursor-theme-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkCursorThemeName
-- @
getSettingsGtkCursorThemeName :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkCursorThemeName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-cursor-theme-name"

-- | Set the value of the “@gtk-cursor-theme-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkCursorThemeName 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkCursorThemeName :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkCursorThemeName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-cursor-theme-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-cursor-theme-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkCursorThemeName :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkCursorThemeName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-cursor-theme-name" (P.Just val)

-- | Set the value of the “@gtk-cursor-theme-name@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkCursorThemeName
-- @
clearSettingsGtkCursorThemeName :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkCursorThemeName obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-cursor-theme-name" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkCursorThemeNamePropertyInfo
instance AttrInfo SettingsGtkCursorThemeNamePropertyInfo where
    type AttrAllowedOps SettingsGtkCursorThemeNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkCursorThemeNamePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkCursorThemeNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkCursorThemeNamePropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkCursorThemeNamePropertyInfo = T.Text
    type AttrGetType SettingsGtkCursorThemeNamePropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkCursorThemeNamePropertyInfo = "gtk-cursor-theme-name"
    type AttrOrigin SettingsGtkCursorThemeNamePropertyInfo = Settings
    attrGet = getSettingsGtkCursorThemeName
    attrSet = setSettingsGtkCursorThemeName
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkCursorThemeName
    attrClear = clearSettingsGtkCursorThemeName
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkCursorThemeName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkCursorThemeName"
        })
#endif

-- VVV Prop "gtk-cursor-theme-size"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-cursor-theme-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkCursorThemeSize
-- @
getSettingsGtkCursorThemeSize :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkCursorThemeSize obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-cursor-theme-size"

-- | Set the value of the “@gtk-cursor-theme-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkCursorThemeSize 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkCursorThemeSize :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkCursorThemeSize obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-cursor-theme-size" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-cursor-theme-size@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkCursorThemeSize :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkCursorThemeSize val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-cursor-theme-size" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkCursorThemeSizePropertyInfo
instance AttrInfo SettingsGtkCursorThemeSizePropertyInfo where
    type AttrAllowedOps SettingsGtkCursorThemeSizePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkCursorThemeSizePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkCursorThemeSizePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkCursorThemeSizePropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkCursorThemeSizePropertyInfo = Int32
    type AttrGetType SettingsGtkCursorThemeSizePropertyInfo = Int32
    type AttrLabel SettingsGtkCursorThemeSizePropertyInfo = "gtk-cursor-theme-size"
    type AttrOrigin SettingsGtkCursorThemeSizePropertyInfo = Settings
    attrGet = getSettingsGtkCursorThemeSize
    attrSet = setSettingsGtkCursorThemeSize
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkCursorThemeSize
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkCursorThemeSize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkCursorThemeSize"
        })
#endif

-- VVV Prop "gtk-decoration-layout"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-decoration-layout@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkDecorationLayout
-- @
getSettingsGtkDecorationLayout :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkDecorationLayout obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-decoration-layout"

-- | Set the value of the “@gtk-decoration-layout@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkDecorationLayout 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkDecorationLayout :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkDecorationLayout obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-decoration-layout" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-decoration-layout@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkDecorationLayout :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkDecorationLayout val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-decoration-layout" (P.Just val)

-- | Set the value of the “@gtk-decoration-layout@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkDecorationLayout
-- @
clearSettingsGtkDecorationLayout :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkDecorationLayout obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-decoration-layout" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkDecorationLayoutPropertyInfo
instance AttrInfo SettingsGtkDecorationLayoutPropertyInfo where
    type AttrAllowedOps SettingsGtkDecorationLayoutPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkDecorationLayoutPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkDecorationLayoutPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkDecorationLayoutPropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkDecorationLayoutPropertyInfo = T.Text
    type AttrGetType SettingsGtkDecorationLayoutPropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkDecorationLayoutPropertyInfo = "gtk-decoration-layout"
    type AttrOrigin SettingsGtkDecorationLayoutPropertyInfo = Settings
    attrGet = getSettingsGtkDecorationLayout
    attrSet = setSettingsGtkDecorationLayout
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkDecorationLayout
    attrClear = clearSettingsGtkDecorationLayout
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkDecorationLayout"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkDecorationLayout"
        })
#endif

-- VVV Prop "gtk-dialogs-use-header"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-dialogs-use-header@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkDialogsUseHeader
-- @
getSettingsGtkDialogsUseHeader :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkDialogsUseHeader obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-dialogs-use-header"

-- | Set the value of the “@gtk-dialogs-use-header@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkDialogsUseHeader 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkDialogsUseHeader :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkDialogsUseHeader obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-dialogs-use-header" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-dialogs-use-header@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkDialogsUseHeader :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkDialogsUseHeader val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-dialogs-use-header" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkDialogsUseHeaderPropertyInfo
instance AttrInfo SettingsGtkDialogsUseHeaderPropertyInfo where
    type AttrAllowedOps SettingsGtkDialogsUseHeaderPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkDialogsUseHeaderPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkDialogsUseHeaderPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkDialogsUseHeaderPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkDialogsUseHeaderPropertyInfo = Bool
    type AttrGetType SettingsGtkDialogsUseHeaderPropertyInfo = Bool
    type AttrLabel SettingsGtkDialogsUseHeaderPropertyInfo = "gtk-dialogs-use-header"
    type AttrOrigin SettingsGtkDialogsUseHeaderPropertyInfo = Settings
    attrGet = getSettingsGtkDialogsUseHeader
    attrSet = setSettingsGtkDialogsUseHeader
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkDialogsUseHeader
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkDialogsUseHeader"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkDialogsUseHeader"
        })
#endif

-- VVV Prop "gtk-dnd-drag-threshold"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-dnd-drag-threshold@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkDndDragThreshold
-- @
getSettingsGtkDndDragThreshold :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkDndDragThreshold obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-dnd-drag-threshold"

-- | Set the value of the “@gtk-dnd-drag-threshold@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkDndDragThreshold 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkDndDragThreshold :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkDndDragThreshold obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-dnd-drag-threshold" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-dnd-drag-threshold@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkDndDragThreshold :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkDndDragThreshold val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-dnd-drag-threshold" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkDndDragThresholdPropertyInfo
instance AttrInfo SettingsGtkDndDragThresholdPropertyInfo where
    type AttrAllowedOps SettingsGtkDndDragThresholdPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkDndDragThresholdPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkDndDragThresholdPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkDndDragThresholdPropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkDndDragThresholdPropertyInfo = Int32
    type AttrGetType SettingsGtkDndDragThresholdPropertyInfo = Int32
    type AttrLabel SettingsGtkDndDragThresholdPropertyInfo = "gtk-dnd-drag-threshold"
    type AttrOrigin SettingsGtkDndDragThresholdPropertyInfo = Settings
    attrGet = getSettingsGtkDndDragThreshold
    attrSet = setSettingsGtkDndDragThreshold
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkDndDragThreshold
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkDndDragThreshold"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkDndDragThreshold"
        })
#endif

-- VVV Prop "gtk-double-click-distance"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-double-click-distance@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkDoubleClickDistance
-- @
getSettingsGtkDoubleClickDistance :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkDoubleClickDistance obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-double-click-distance"

-- | Set the value of the “@gtk-double-click-distance@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkDoubleClickDistance 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkDoubleClickDistance :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkDoubleClickDistance obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-double-click-distance" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-double-click-distance@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkDoubleClickDistance :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkDoubleClickDistance val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-double-click-distance" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkDoubleClickDistancePropertyInfo
instance AttrInfo SettingsGtkDoubleClickDistancePropertyInfo where
    type AttrAllowedOps SettingsGtkDoubleClickDistancePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkDoubleClickDistancePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkDoubleClickDistancePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkDoubleClickDistancePropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkDoubleClickDistancePropertyInfo = Int32
    type AttrGetType SettingsGtkDoubleClickDistancePropertyInfo = Int32
    type AttrLabel SettingsGtkDoubleClickDistancePropertyInfo = "gtk-double-click-distance"
    type AttrOrigin SettingsGtkDoubleClickDistancePropertyInfo = Settings
    attrGet = getSettingsGtkDoubleClickDistance
    attrSet = setSettingsGtkDoubleClickDistance
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkDoubleClickDistance
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkDoubleClickDistance"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkDoubleClickDistance"
        })
#endif

-- VVV Prop "gtk-double-click-time"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-double-click-time@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkDoubleClickTime
-- @
getSettingsGtkDoubleClickTime :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkDoubleClickTime obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-double-click-time"

-- | Set the value of the “@gtk-double-click-time@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkDoubleClickTime 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkDoubleClickTime :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkDoubleClickTime obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-double-click-time" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-double-click-time@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkDoubleClickTime :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkDoubleClickTime val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-double-click-time" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkDoubleClickTimePropertyInfo
instance AttrInfo SettingsGtkDoubleClickTimePropertyInfo where
    type AttrAllowedOps SettingsGtkDoubleClickTimePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkDoubleClickTimePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkDoubleClickTimePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkDoubleClickTimePropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkDoubleClickTimePropertyInfo = Int32
    type AttrGetType SettingsGtkDoubleClickTimePropertyInfo = Int32
    type AttrLabel SettingsGtkDoubleClickTimePropertyInfo = "gtk-double-click-time"
    type AttrOrigin SettingsGtkDoubleClickTimePropertyInfo = Settings
    attrGet = getSettingsGtkDoubleClickTime
    attrSet = setSettingsGtkDoubleClickTime
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkDoubleClickTime
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkDoubleClickTime"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkDoubleClickTime"
        })
#endif

-- VVV Prop "gtk-enable-accels"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-enable-accels@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkEnableAccels
-- @
getSettingsGtkEnableAccels :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkEnableAccels obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-enable-accels"

-- | Set the value of the “@gtk-enable-accels@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkEnableAccels 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkEnableAccels :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkEnableAccels obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-enable-accels" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-enable-accels@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkEnableAccels :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkEnableAccels val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-enable-accels" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkEnableAccelsPropertyInfo
instance AttrInfo SettingsGtkEnableAccelsPropertyInfo where
    type AttrAllowedOps SettingsGtkEnableAccelsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkEnableAccelsPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkEnableAccelsPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkEnableAccelsPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkEnableAccelsPropertyInfo = Bool
    type AttrGetType SettingsGtkEnableAccelsPropertyInfo = Bool
    type AttrLabel SettingsGtkEnableAccelsPropertyInfo = "gtk-enable-accels"
    type AttrOrigin SettingsGtkEnableAccelsPropertyInfo = Settings
    attrGet = getSettingsGtkEnableAccels
    attrSet = setSettingsGtkEnableAccels
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkEnableAccels
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkEnableAccels"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkEnableAccels"
        })
#endif

-- VVV Prop "gtk-enable-animations"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-enable-animations@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkEnableAnimations
-- @
getSettingsGtkEnableAnimations :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkEnableAnimations obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-enable-animations"

-- | Set the value of the “@gtk-enable-animations@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkEnableAnimations 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkEnableAnimations :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkEnableAnimations obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-enable-animations" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-enable-animations@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkEnableAnimations :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkEnableAnimations val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-enable-animations" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkEnableAnimationsPropertyInfo
instance AttrInfo SettingsGtkEnableAnimationsPropertyInfo where
    type AttrAllowedOps SettingsGtkEnableAnimationsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkEnableAnimationsPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkEnableAnimationsPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkEnableAnimationsPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkEnableAnimationsPropertyInfo = Bool
    type AttrGetType SettingsGtkEnableAnimationsPropertyInfo = Bool
    type AttrLabel SettingsGtkEnableAnimationsPropertyInfo = "gtk-enable-animations"
    type AttrOrigin SettingsGtkEnableAnimationsPropertyInfo = Settings
    attrGet = getSettingsGtkEnableAnimations
    attrSet = setSettingsGtkEnableAnimations
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkEnableAnimations
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkEnableAnimations"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkEnableAnimations"
        })
#endif

-- VVV Prop "gtk-enable-event-sounds"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-enable-event-sounds@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkEnableEventSounds
-- @
getSettingsGtkEnableEventSounds :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkEnableEventSounds obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-enable-event-sounds"

-- | Set the value of the “@gtk-enable-event-sounds@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkEnableEventSounds 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkEnableEventSounds :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkEnableEventSounds obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-enable-event-sounds" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-enable-event-sounds@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkEnableEventSounds :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkEnableEventSounds val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-enable-event-sounds" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkEnableEventSoundsPropertyInfo
instance AttrInfo SettingsGtkEnableEventSoundsPropertyInfo where
    type AttrAllowedOps SettingsGtkEnableEventSoundsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkEnableEventSoundsPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkEnableEventSoundsPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkEnableEventSoundsPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkEnableEventSoundsPropertyInfo = Bool
    type AttrGetType SettingsGtkEnableEventSoundsPropertyInfo = Bool
    type AttrLabel SettingsGtkEnableEventSoundsPropertyInfo = "gtk-enable-event-sounds"
    type AttrOrigin SettingsGtkEnableEventSoundsPropertyInfo = Settings
    attrGet = getSettingsGtkEnableEventSounds
    attrSet = setSettingsGtkEnableEventSounds
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkEnableEventSounds
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkEnableEventSounds"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkEnableEventSounds"
        })
#endif

-- VVV Prop "gtk-enable-input-feedback-sounds"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-enable-input-feedback-sounds@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkEnableInputFeedbackSounds
-- @
getSettingsGtkEnableInputFeedbackSounds :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkEnableInputFeedbackSounds obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-enable-input-feedback-sounds"

-- | Set the value of the “@gtk-enable-input-feedback-sounds@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkEnableInputFeedbackSounds 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkEnableInputFeedbackSounds :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkEnableInputFeedbackSounds obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-enable-input-feedback-sounds" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-enable-input-feedback-sounds@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkEnableInputFeedbackSounds :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkEnableInputFeedbackSounds val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-enable-input-feedback-sounds" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkEnableInputFeedbackSoundsPropertyInfo
instance AttrInfo SettingsGtkEnableInputFeedbackSoundsPropertyInfo where
    type AttrAllowedOps SettingsGtkEnableInputFeedbackSoundsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkEnableInputFeedbackSoundsPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkEnableInputFeedbackSoundsPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkEnableInputFeedbackSoundsPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkEnableInputFeedbackSoundsPropertyInfo = Bool
    type AttrGetType SettingsGtkEnableInputFeedbackSoundsPropertyInfo = Bool
    type AttrLabel SettingsGtkEnableInputFeedbackSoundsPropertyInfo = "gtk-enable-input-feedback-sounds"
    type AttrOrigin SettingsGtkEnableInputFeedbackSoundsPropertyInfo = Settings
    attrGet = getSettingsGtkEnableInputFeedbackSounds
    attrSet = setSettingsGtkEnableInputFeedbackSounds
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkEnableInputFeedbackSounds
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkEnableInputFeedbackSounds"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkEnableInputFeedbackSounds"
        })
#endif

-- VVV Prop "gtk-enable-mnemonics"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-enable-mnemonics@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkEnableMnemonics
-- @
getSettingsGtkEnableMnemonics :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkEnableMnemonics obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-enable-mnemonics"

-- | Set the value of the “@gtk-enable-mnemonics@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkEnableMnemonics 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkEnableMnemonics :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkEnableMnemonics obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-enable-mnemonics" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-enable-mnemonics@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkEnableMnemonics :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkEnableMnemonics val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-enable-mnemonics" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkEnableMnemonicsPropertyInfo
instance AttrInfo SettingsGtkEnableMnemonicsPropertyInfo where
    type AttrAllowedOps SettingsGtkEnableMnemonicsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkEnableMnemonicsPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkEnableMnemonicsPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkEnableMnemonicsPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkEnableMnemonicsPropertyInfo = Bool
    type AttrGetType SettingsGtkEnableMnemonicsPropertyInfo = Bool
    type AttrLabel SettingsGtkEnableMnemonicsPropertyInfo = "gtk-enable-mnemonics"
    type AttrOrigin SettingsGtkEnableMnemonicsPropertyInfo = Settings
    attrGet = getSettingsGtkEnableMnemonics
    attrSet = setSettingsGtkEnableMnemonics
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkEnableMnemonics
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkEnableMnemonics"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkEnableMnemonics"
        })
#endif

-- VVV Prop "gtk-enable-primary-paste"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-enable-primary-paste@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkEnablePrimaryPaste
-- @
getSettingsGtkEnablePrimaryPaste :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkEnablePrimaryPaste obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-enable-primary-paste"

-- | Set the value of the “@gtk-enable-primary-paste@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkEnablePrimaryPaste 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkEnablePrimaryPaste :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkEnablePrimaryPaste obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-enable-primary-paste" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-enable-primary-paste@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkEnablePrimaryPaste :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkEnablePrimaryPaste val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-enable-primary-paste" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkEnablePrimaryPastePropertyInfo
instance AttrInfo SettingsGtkEnablePrimaryPastePropertyInfo where
    type AttrAllowedOps SettingsGtkEnablePrimaryPastePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkEnablePrimaryPastePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkEnablePrimaryPastePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkEnablePrimaryPastePropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkEnablePrimaryPastePropertyInfo = Bool
    type AttrGetType SettingsGtkEnablePrimaryPastePropertyInfo = Bool
    type AttrLabel SettingsGtkEnablePrimaryPastePropertyInfo = "gtk-enable-primary-paste"
    type AttrOrigin SettingsGtkEnablePrimaryPastePropertyInfo = Settings
    attrGet = getSettingsGtkEnablePrimaryPaste
    attrSet = setSettingsGtkEnablePrimaryPaste
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkEnablePrimaryPaste
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkEnablePrimaryPaste"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkEnablePrimaryPaste"
        })
#endif

-- VVV Prop "gtk-enable-tooltips"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-enable-tooltips@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkEnableTooltips
-- @
getSettingsGtkEnableTooltips :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkEnableTooltips obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-enable-tooltips"

-- | Set the value of the “@gtk-enable-tooltips@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkEnableTooltips 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkEnableTooltips :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkEnableTooltips obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-enable-tooltips" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-enable-tooltips@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkEnableTooltips :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkEnableTooltips val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-enable-tooltips" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkEnableTooltipsPropertyInfo
instance AttrInfo SettingsGtkEnableTooltipsPropertyInfo where
    type AttrAllowedOps SettingsGtkEnableTooltipsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkEnableTooltipsPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkEnableTooltipsPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkEnableTooltipsPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkEnableTooltipsPropertyInfo = Bool
    type AttrGetType SettingsGtkEnableTooltipsPropertyInfo = Bool
    type AttrLabel SettingsGtkEnableTooltipsPropertyInfo = "gtk-enable-tooltips"
    type AttrOrigin SettingsGtkEnableTooltipsPropertyInfo = Settings
    attrGet = getSettingsGtkEnableTooltips
    attrSet = setSettingsGtkEnableTooltips
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkEnableTooltips
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkEnableTooltips"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkEnableTooltips"
        })
#endif

-- VVV Prop "gtk-entry-password-hint-timeout"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-entry-password-hint-timeout@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkEntryPasswordHintTimeout
-- @
getSettingsGtkEntryPasswordHintTimeout :: (MonadIO m, IsSettings o) => o -> m Word32
getSettingsGtkEntryPasswordHintTimeout obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "gtk-entry-password-hint-timeout"

-- | Set the value of the “@gtk-entry-password-hint-timeout@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkEntryPasswordHintTimeout 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkEntryPasswordHintTimeout :: (MonadIO m, IsSettings o) => o -> Word32 -> m ()
setSettingsGtkEntryPasswordHintTimeout obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "gtk-entry-password-hint-timeout" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-entry-password-hint-timeout@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkEntryPasswordHintTimeout :: (IsSettings o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructSettingsGtkEntryPasswordHintTimeout val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "gtk-entry-password-hint-timeout" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkEntryPasswordHintTimeoutPropertyInfo
instance AttrInfo SettingsGtkEntryPasswordHintTimeoutPropertyInfo where
    type AttrAllowedOps SettingsGtkEntryPasswordHintTimeoutPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkEntryPasswordHintTimeoutPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkEntryPasswordHintTimeoutPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint SettingsGtkEntryPasswordHintTimeoutPropertyInfo = (~) Word32
    type AttrTransferType SettingsGtkEntryPasswordHintTimeoutPropertyInfo = Word32
    type AttrGetType SettingsGtkEntryPasswordHintTimeoutPropertyInfo = Word32
    type AttrLabel SettingsGtkEntryPasswordHintTimeoutPropertyInfo = "gtk-entry-password-hint-timeout"
    type AttrOrigin SettingsGtkEntryPasswordHintTimeoutPropertyInfo = Settings
    attrGet = getSettingsGtkEntryPasswordHintTimeout
    attrSet = setSettingsGtkEntryPasswordHintTimeout
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkEntryPasswordHintTimeout
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkEntryPasswordHintTimeout"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkEntryPasswordHintTimeout"
        })
#endif

-- VVV Prop "gtk-entry-select-on-focus"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-entry-select-on-focus@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkEntrySelectOnFocus
-- @
getSettingsGtkEntrySelectOnFocus :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkEntrySelectOnFocus obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-entry-select-on-focus"

-- | Set the value of the “@gtk-entry-select-on-focus@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkEntrySelectOnFocus 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkEntrySelectOnFocus :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkEntrySelectOnFocus obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-entry-select-on-focus" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-entry-select-on-focus@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkEntrySelectOnFocus :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkEntrySelectOnFocus val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-entry-select-on-focus" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkEntrySelectOnFocusPropertyInfo
instance AttrInfo SettingsGtkEntrySelectOnFocusPropertyInfo where
    type AttrAllowedOps SettingsGtkEntrySelectOnFocusPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkEntrySelectOnFocusPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkEntrySelectOnFocusPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkEntrySelectOnFocusPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkEntrySelectOnFocusPropertyInfo = Bool
    type AttrGetType SettingsGtkEntrySelectOnFocusPropertyInfo = Bool
    type AttrLabel SettingsGtkEntrySelectOnFocusPropertyInfo = "gtk-entry-select-on-focus"
    type AttrOrigin SettingsGtkEntrySelectOnFocusPropertyInfo = Settings
    attrGet = getSettingsGtkEntrySelectOnFocus
    attrSet = setSettingsGtkEntrySelectOnFocus
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkEntrySelectOnFocus
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkEntrySelectOnFocus"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkEntrySelectOnFocus"
        })
#endif

-- VVV Prop "gtk-error-bell"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-error-bell@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkErrorBell
-- @
getSettingsGtkErrorBell :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkErrorBell obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-error-bell"

-- | Set the value of the “@gtk-error-bell@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkErrorBell 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkErrorBell :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkErrorBell obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-error-bell" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-error-bell@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkErrorBell :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkErrorBell val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-error-bell" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkErrorBellPropertyInfo
instance AttrInfo SettingsGtkErrorBellPropertyInfo where
    type AttrAllowedOps SettingsGtkErrorBellPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkErrorBellPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkErrorBellPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkErrorBellPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkErrorBellPropertyInfo = Bool
    type AttrGetType SettingsGtkErrorBellPropertyInfo = Bool
    type AttrLabel SettingsGtkErrorBellPropertyInfo = "gtk-error-bell"
    type AttrOrigin SettingsGtkErrorBellPropertyInfo = Settings
    attrGet = getSettingsGtkErrorBell
    attrSet = setSettingsGtkErrorBell
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkErrorBell
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkErrorBell"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkErrorBell"
        })
#endif

-- VVV Prop "gtk-fallback-icon-theme"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-fallback-icon-theme@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkFallbackIconTheme
-- @
getSettingsGtkFallbackIconTheme :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkFallbackIconTheme obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-fallback-icon-theme"

-- | Set the value of the “@gtk-fallback-icon-theme@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkFallbackIconTheme 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkFallbackIconTheme :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkFallbackIconTheme obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-fallback-icon-theme" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-fallback-icon-theme@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkFallbackIconTheme :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkFallbackIconTheme val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-fallback-icon-theme" (P.Just val)

-- | Set the value of the “@gtk-fallback-icon-theme@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkFallbackIconTheme
-- @
clearSettingsGtkFallbackIconTheme :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkFallbackIconTheme obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-fallback-icon-theme" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkFallbackIconThemePropertyInfo
instance AttrInfo SettingsGtkFallbackIconThemePropertyInfo where
    type AttrAllowedOps SettingsGtkFallbackIconThemePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkFallbackIconThemePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkFallbackIconThemePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkFallbackIconThemePropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkFallbackIconThemePropertyInfo = T.Text
    type AttrGetType SettingsGtkFallbackIconThemePropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkFallbackIconThemePropertyInfo = "gtk-fallback-icon-theme"
    type AttrOrigin SettingsGtkFallbackIconThemePropertyInfo = Settings
    attrGet = getSettingsGtkFallbackIconTheme
    attrSet = setSettingsGtkFallbackIconTheme
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkFallbackIconTheme
    attrClear = clearSettingsGtkFallbackIconTheme
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkFallbackIconTheme"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkFallbackIconTheme"
        })
#endif

-- VVV Prop "gtk-file-chooser-backend"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-file-chooser-backend@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkFileChooserBackend
-- @
getSettingsGtkFileChooserBackend :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkFileChooserBackend obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-file-chooser-backend"

-- | Set the value of the “@gtk-file-chooser-backend@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkFileChooserBackend 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkFileChooserBackend :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkFileChooserBackend obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-file-chooser-backend" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-file-chooser-backend@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkFileChooserBackend :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkFileChooserBackend val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-file-chooser-backend" (P.Just val)

-- | Set the value of the “@gtk-file-chooser-backend@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkFileChooserBackend
-- @
clearSettingsGtkFileChooserBackend :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkFileChooserBackend obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-file-chooser-backend" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkFileChooserBackendPropertyInfo
instance AttrInfo SettingsGtkFileChooserBackendPropertyInfo where
    type AttrAllowedOps SettingsGtkFileChooserBackendPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkFileChooserBackendPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkFileChooserBackendPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkFileChooserBackendPropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkFileChooserBackendPropertyInfo = T.Text
    type AttrGetType SettingsGtkFileChooserBackendPropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkFileChooserBackendPropertyInfo = "gtk-file-chooser-backend"
    type AttrOrigin SettingsGtkFileChooserBackendPropertyInfo = Settings
    attrGet = getSettingsGtkFileChooserBackend
    attrSet = setSettingsGtkFileChooserBackend
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkFileChooserBackend
    attrClear = clearSettingsGtkFileChooserBackend
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkFileChooserBackend"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkFileChooserBackend"
        })
#endif

-- VVV Prop "gtk-font-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-font-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkFontName
-- @
getSettingsGtkFontName :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkFontName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-font-name"

-- | Set the value of the “@gtk-font-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkFontName 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkFontName :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkFontName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-font-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-font-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkFontName :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkFontName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-font-name" (P.Just val)

-- | Set the value of the “@gtk-font-name@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkFontName
-- @
clearSettingsGtkFontName :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkFontName obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-font-name" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkFontNamePropertyInfo
instance AttrInfo SettingsGtkFontNamePropertyInfo where
    type AttrAllowedOps SettingsGtkFontNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkFontNamePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkFontNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkFontNamePropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkFontNamePropertyInfo = T.Text
    type AttrGetType SettingsGtkFontNamePropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkFontNamePropertyInfo = "gtk-font-name"
    type AttrOrigin SettingsGtkFontNamePropertyInfo = Settings
    attrGet = getSettingsGtkFontName
    attrSet = setSettingsGtkFontName
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkFontName
    attrClear = clearSettingsGtkFontName
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkFontName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkFontName"
        })
#endif

-- VVV Prop "gtk-fontconfig-timestamp"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-fontconfig-timestamp@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkFontconfigTimestamp
-- @
getSettingsGtkFontconfigTimestamp :: (MonadIO m, IsSettings o) => o -> m Word32
getSettingsGtkFontconfigTimestamp obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "gtk-fontconfig-timestamp"

-- | Set the value of the “@gtk-fontconfig-timestamp@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkFontconfigTimestamp 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkFontconfigTimestamp :: (MonadIO m, IsSettings o) => o -> Word32 -> m ()
setSettingsGtkFontconfigTimestamp obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "gtk-fontconfig-timestamp" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-fontconfig-timestamp@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkFontconfigTimestamp :: (IsSettings o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructSettingsGtkFontconfigTimestamp val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "gtk-fontconfig-timestamp" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkFontconfigTimestampPropertyInfo
instance AttrInfo SettingsGtkFontconfigTimestampPropertyInfo where
    type AttrAllowedOps SettingsGtkFontconfigTimestampPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkFontconfigTimestampPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkFontconfigTimestampPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint SettingsGtkFontconfigTimestampPropertyInfo = (~) Word32
    type AttrTransferType SettingsGtkFontconfigTimestampPropertyInfo = Word32
    type AttrGetType SettingsGtkFontconfigTimestampPropertyInfo = Word32
    type AttrLabel SettingsGtkFontconfigTimestampPropertyInfo = "gtk-fontconfig-timestamp"
    type AttrOrigin SettingsGtkFontconfigTimestampPropertyInfo = Settings
    attrGet = getSettingsGtkFontconfigTimestamp
    attrSet = setSettingsGtkFontconfigTimestamp
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkFontconfigTimestamp
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkFontconfigTimestamp"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkFontconfigTimestamp"
        })
#endif

-- VVV Prop "gtk-icon-sizes"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-icon-sizes@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkIconSizes
-- @
getSettingsGtkIconSizes :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkIconSizes obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-icon-sizes"

-- | Set the value of the “@gtk-icon-sizes@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkIconSizes 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkIconSizes :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkIconSizes obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-icon-sizes" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-icon-sizes@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkIconSizes :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkIconSizes val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-icon-sizes" (P.Just val)

-- | Set the value of the “@gtk-icon-sizes@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkIconSizes
-- @
clearSettingsGtkIconSizes :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkIconSizes obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-icon-sizes" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkIconSizesPropertyInfo
instance AttrInfo SettingsGtkIconSizesPropertyInfo where
    type AttrAllowedOps SettingsGtkIconSizesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkIconSizesPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkIconSizesPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkIconSizesPropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkIconSizesPropertyInfo = T.Text
    type AttrGetType SettingsGtkIconSizesPropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkIconSizesPropertyInfo = "gtk-icon-sizes"
    type AttrOrigin SettingsGtkIconSizesPropertyInfo = Settings
    attrGet = getSettingsGtkIconSizes
    attrSet = setSettingsGtkIconSizes
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkIconSizes
    attrClear = clearSettingsGtkIconSizes
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkIconSizes"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkIconSizes"
        })
#endif

-- VVV Prop "gtk-icon-theme-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-icon-theme-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkIconThemeName
-- @
getSettingsGtkIconThemeName :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkIconThemeName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-icon-theme-name"

-- | Set the value of the “@gtk-icon-theme-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkIconThemeName 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkIconThemeName :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkIconThemeName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-icon-theme-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-icon-theme-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkIconThemeName :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkIconThemeName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-icon-theme-name" (P.Just val)

-- | Set the value of the “@gtk-icon-theme-name@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkIconThemeName
-- @
clearSettingsGtkIconThemeName :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkIconThemeName obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-icon-theme-name" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkIconThemeNamePropertyInfo
instance AttrInfo SettingsGtkIconThemeNamePropertyInfo where
    type AttrAllowedOps SettingsGtkIconThemeNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkIconThemeNamePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkIconThemeNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkIconThemeNamePropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkIconThemeNamePropertyInfo = T.Text
    type AttrGetType SettingsGtkIconThemeNamePropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkIconThemeNamePropertyInfo = "gtk-icon-theme-name"
    type AttrOrigin SettingsGtkIconThemeNamePropertyInfo = Settings
    attrGet = getSettingsGtkIconThemeName
    attrSet = setSettingsGtkIconThemeName
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkIconThemeName
    attrClear = clearSettingsGtkIconThemeName
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkIconThemeName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkIconThemeName"
        })
#endif

-- VVV Prop "gtk-im-module"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-im-module@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkImModule
-- @
getSettingsGtkImModule :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkImModule obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-im-module"

-- | Set the value of the “@gtk-im-module@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkImModule 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkImModule :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkImModule obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-im-module" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-im-module@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkImModule :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkImModule val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-im-module" (P.Just val)

-- | Set the value of the “@gtk-im-module@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkImModule
-- @
clearSettingsGtkImModule :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkImModule obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-im-module" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkImModulePropertyInfo
instance AttrInfo SettingsGtkImModulePropertyInfo where
    type AttrAllowedOps SettingsGtkImModulePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkImModulePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkImModulePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkImModulePropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkImModulePropertyInfo = T.Text
    type AttrGetType SettingsGtkImModulePropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkImModulePropertyInfo = "gtk-im-module"
    type AttrOrigin SettingsGtkImModulePropertyInfo = Settings
    attrGet = getSettingsGtkImModule
    attrSet = setSettingsGtkImModule
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkImModule
    attrClear = clearSettingsGtkImModule
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkImModule"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkImModule"
        })
#endif

-- VVV Prop "gtk-im-preedit-style"
   -- Type: TInterface (Name {namespace = "Gtk", name = "IMPreeditStyle"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-im-preedit-style@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkImPreeditStyle
-- @
getSettingsGtkImPreeditStyle :: (MonadIO m, IsSettings o) => o -> m Gtk.Enums.IMPreeditStyle
getSettingsGtkImPreeditStyle obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "gtk-im-preedit-style"

-- | Set the value of the “@gtk-im-preedit-style@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkImPreeditStyle 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkImPreeditStyle :: (MonadIO m, IsSettings o) => o -> Gtk.Enums.IMPreeditStyle -> m ()
setSettingsGtkImPreeditStyle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "gtk-im-preedit-style" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-im-preedit-style@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkImPreeditStyle :: (IsSettings o, MIO.MonadIO m) => Gtk.Enums.IMPreeditStyle -> m (GValueConstruct o)
constructSettingsGtkImPreeditStyle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "gtk-im-preedit-style" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkImPreeditStylePropertyInfo
instance AttrInfo SettingsGtkImPreeditStylePropertyInfo where
    type AttrAllowedOps SettingsGtkImPreeditStylePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkImPreeditStylePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkImPreeditStylePropertyInfo = (~) Gtk.Enums.IMPreeditStyle
    type AttrTransferTypeConstraint SettingsGtkImPreeditStylePropertyInfo = (~) Gtk.Enums.IMPreeditStyle
    type AttrTransferType SettingsGtkImPreeditStylePropertyInfo = Gtk.Enums.IMPreeditStyle
    type AttrGetType SettingsGtkImPreeditStylePropertyInfo = Gtk.Enums.IMPreeditStyle
    type AttrLabel SettingsGtkImPreeditStylePropertyInfo = "gtk-im-preedit-style"
    type AttrOrigin SettingsGtkImPreeditStylePropertyInfo = Settings
    attrGet = getSettingsGtkImPreeditStyle
    attrSet = setSettingsGtkImPreeditStyle
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkImPreeditStyle
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkImPreeditStyle"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkImPreeditStyle"
        })
#endif

-- VVV Prop "gtk-im-status-style"
   -- Type: TInterface (Name {namespace = "Gtk", name = "IMStatusStyle"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-im-status-style@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkImStatusStyle
-- @
getSettingsGtkImStatusStyle :: (MonadIO m, IsSettings o) => o -> m Gtk.Enums.IMStatusStyle
getSettingsGtkImStatusStyle obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "gtk-im-status-style"

-- | Set the value of the “@gtk-im-status-style@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkImStatusStyle 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkImStatusStyle :: (MonadIO m, IsSettings o) => o -> Gtk.Enums.IMStatusStyle -> m ()
setSettingsGtkImStatusStyle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "gtk-im-status-style" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-im-status-style@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkImStatusStyle :: (IsSettings o, MIO.MonadIO m) => Gtk.Enums.IMStatusStyle -> m (GValueConstruct o)
constructSettingsGtkImStatusStyle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "gtk-im-status-style" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkImStatusStylePropertyInfo
instance AttrInfo SettingsGtkImStatusStylePropertyInfo where
    type AttrAllowedOps SettingsGtkImStatusStylePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkImStatusStylePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkImStatusStylePropertyInfo = (~) Gtk.Enums.IMStatusStyle
    type AttrTransferTypeConstraint SettingsGtkImStatusStylePropertyInfo = (~) Gtk.Enums.IMStatusStyle
    type AttrTransferType SettingsGtkImStatusStylePropertyInfo = Gtk.Enums.IMStatusStyle
    type AttrGetType SettingsGtkImStatusStylePropertyInfo = Gtk.Enums.IMStatusStyle
    type AttrLabel SettingsGtkImStatusStylePropertyInfo = "gtk-im-status-style"
    type AttrOrigin SettingsGtkImStatusStylePropertyInfo = Settings
    attrGet = getSettingsGtkImStatusStyle
    attrSet = setSettingsGtkImStatusStyle
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkImStatusStyle
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkImStatusStyle"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkImStatusStyle"
        })
#endif

-- VVV Prop "gtk-key-theme-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-key-theme-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkKeyThemeName
-- @
getSettingsGtkKeyThemeName :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkKeyThemeName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-key-theme-name"

-- | Set the value of the “@gtk-key-theme-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkKeyThemeName 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkKeyThemeName :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkKeyThemeName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-key-theme-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-key-theme-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkKeyThemeName :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkKeyThemeName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-key-theme-name" (P.Just val)

-- | Set the value of the “@gtk-key-theme-name@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkKeyThemeName
-- @
clearSettingsGtkKeyThemeName :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkKeyThemeName obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-key-theme-name" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkKeyThemeNamePropertyInfo
instance AttrInfo SettingsGtkKeyThemeNamePropertyInfo where
    type AttrAllowedOps SettingsGtkKeyThemeNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkKeyThemeNamePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkKeyThemeNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkKeyThemeNamePropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkKeyThemeNamePropertyInfo = T.Text
    type AttrGetType SettingsGtkKeyThemeNamePropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkKeyThemeNamePropertyInfo = "gtk-key-theme-name"
    type AttrOrigin SettingsGtkKeyThemeNamePropertyInfo = Settings
    attrGet = getSettingsGtkKeyThemeName
    attrSet = setSettingsGtkKeyThemeName
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkKeyThemeName
    attrClear = clearSettingsGtkKeyThemeName
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkKeyThemeName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkKeyThemeName"
        })
#endif

-- VVV Prop "gtk-keynav-cursor-only"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-keynav-cursor-only@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkKeynavCursorOnly
-- @
getSettingsGtkKeynavCursorOnly :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkKeynavCursorOnly obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-keynav-cursor-only"

-- | Set the value of the “@gtk-keynav-cursor-only@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkKeynavCursorOnly 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkKeynavCursorOnly :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkKeynavCursorOnly obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-keynav-cursor-only" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-keynav-cursor-only@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkKeynavCursorOnly :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkKeynavCursorOnly val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-keynav-cursor-only" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkKeynavCursorOnlyPropertyInfo
instance AttrInfo SettingsGtkKeynavCursorOnlyPropertyInfo where
    type AttrAllowedOps SettingsGtkKeynavCursorOnlyPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkKeynavCursorOnlyPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkKeynavCursorOnlyPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkKeynavCursorOnlyPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkKeynavCursorOnlyPropertyInfo = Bool
    type AttrGetType SettingsGtkKeynavCursorOnlyPropertyInfo = Bool
    type AttrLabel SettingsGtkKeynavCursorOnlyPropertyInfo = "gtk-keynav-cursor-only"
    type AttrOrigin SettingsGtkKeynavCursorOnlyPropertyInfo = Settings
    attrGet = getSettingsGtkKeynavCursorOnly
    attrSet = setSettingsGtkKeynavCursorOnly
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkKeynavCursorOnly
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkKeynavCursorOnly"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkKeynavCursorOnly"
        })
#endif

-- VVV Prop "gtk-keynav-use-caret"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-keynav-use-caret@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkKeynavUseCaret
-- @
getSettingsGtkKeynavUseCaret :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkKeynavUseCaret obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-keynav-use-caret"

-- | Set the value of the “@gtk-keynav-use-caret@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkKeynavUseCaret 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkKeynavUseCaret :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkKeynavUseCaret obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-keynav-use-caret" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-keynav-use-caret@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkKeynavUseCaret :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkKeynavUseCaret val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-keynav-use-caret" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkKeynavUseCaretPropertyInfo
instance AttrInfo SettingsGtkKeynavUseCaretPropertyInfo where
    type AttrAllowedOps SettingsGtkKeynavUseCaretPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkKeynavUseCaretPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkKeynavUseCaretPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkKeynavUseCaretPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkKeynavUseCaretPropertyInfo = Bool
    type AttrGetType SettingsGtkKeynavUseCaretPropertyInfo = Bool
    type AttrLabel SettingsGtkKeynavUseCaretPropertyInfo = "gtk-keynav-use-caret"
    type AttrOrigin SettingsGtkKeynavUseCaretPropertyInfo = Settings
    attrGet = getSettingsGtkKeynavUseCaret
    attrSet = setSettingsGtkKeynavUseCaret
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkKeynavUseCaret
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkKeynavUseCaret"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkKeynavUseCaret"
        })
#endif

-- VVV Prop "gtk-keynav-wrap-around"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-keynav-wrap-around@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkKeynavWrapAround
-- @
getSettingsGtkKeynavWrapAround :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkKeynavWrapAround obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-keynav-wrap-around"

-- | Set the value of the “@gtk-keynav-wrap-around@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkKeynavWrapAround 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkKeynavWrapAround :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkKeynavWrapAround obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-keynav-wrap-around" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-keynav-wrap-around@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkKeynavWrapAround :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkKeynavWrapAround val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-keynav-wrap-around" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkKeynavWrapAroundPropertyInfo
instance AttrInfo SettingsGtkKeynavWrapAroundPropertyInfo where
    type AttrAllowedOps SettingsGtkKeynavWrapAroundPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkKeynavWrapAroundPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkKeynavWrapAroundPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkKeynavWrapAroundPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkKeynavWrapAroundPropertyInfo = Bool
    type AttrGetType SettingsGtkKeynavWrapAroundPropertyInfo = Bool
    type AttrLabel SettingsGtkKeynavWrapAroundPropertyInfo = "gtk-keynav-wrap-around"
    type AttrOrigin SettingsGtkKeynavWrapAroundPropertyInfo = Settings
    attrGet = getSettingsGtkKeynavWrapAround
    attrSet = setSettingsGtkKeynavWrapAround
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkKeynavWrapAround
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkKeynavWrapAround"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkKeynavWrapAround"
        })
#endif

-- VVV Prop "gtk-label-select-on-focus"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-label-select-on-focus@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkLabelSelectOnFocus
-- @
getSettingsGtkLabelSelectOnFocus :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkLabelSelectOnFocus obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-label-select-on-focus"

-- | Set the value of the “@gtk-label-select-on-focus@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkLabelSelectOnFocus 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkLabelSelectOnFocus :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkLabelSelectOnFocus obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-label-select-on-focus" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-label-select-on-focus@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkLabelSelectOnFocus :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkLabelSelectOnFocus val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-label-select-on-focus" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkLabelSelectOnFocusPropertyInfo
instance AttrInfo SettingsGtkLabelSelectOnFocusPropertyInfo where
    type AttrAllowedOps SettingsGtkLabelSelectOnFocusPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkLabelSelectOnFocusPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkLabelSelectOnFocusPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkLabelSelectOnFocusPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkLabelSelectOnFocusPropertyInfo = Bool
    type AttrGetType SettingsGtkLabelSelectOnFocusPropertyInfo = Bool
    type AttrLabel SettingsGtkLabelSelectOnFocusPropertyInfo = "gtk-label-select-on-focus"
    type AttrOrigin SettingsGtkLabelSelectOnFocusPropertyInfo = Settings
    attrGet = getSettingsGtkLabelSelectOnFocus
    attrSet = setSettingsGtkLabelSelectOnFocus
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkLabelSelectOnFocus
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkLabelSelectOnFocus"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkLabelSelectOnFocus"
        })
#endif

-- VVV Prop "gtk-long-press-time"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-long-press-time@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkLongPressTime
-- @
getSettingsGtkLongPressTime :: (MonadIO m, IsSettings o) => o -> m Word32
getSettingsGtkLongPressTime obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "gtk-long-press-time"

-- | Set the value of the “@gtk-long-press-time@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkLongPressTime 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkLongPressTime :: (MonadIO m, IsSettings o) => o -> Word32 -> m ()
setSettingsGtkLongPressTime obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "gtk-long-press-time" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-long-press-time@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkLongPressTime :: (IsSettings o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructSettingsGtkLongPressTime val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "gtk-long-press-time" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkLongPressTimePropertyInfo
instance AttrInfo SettingsGtkLongPressTimePropertyInfo where
    type AttrAllowedOps SettingsGtkLongPressTimePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkLongPressTimePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkLongPressTimePropertyInfo = (~) Word32
    type AttrTransferTypeConstraint SettingsGtkLongPressTimePropertyInfo = (~) Word32
    type AttrTransferType SettingsGtkLongPressTimePropertyInfo = Word32
    type AttrGetType SettingsGtkLongPressTimePropertyInfo = Word32
    type AttrLabel SettingsGtkLongPressTimePropertyInfo = "gtk-long-press-time"
    type AttrOrigin SettingsGtkLongPressTimePropertyInfo = Settings
    attrGet = getSettingsGtkLongPressTime
    attrSet = setSettingsGtkLongPressTime
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkLongPressTime
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkLongPressTime"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkLongPressTime"
        })
#endif

-- VVV Prop "gtk-menu-bar-accel"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-menu-bar-accel@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkMenuBarAccel
-- @
getSettingsGtkMenuBarAccel :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkMenuBarAccel obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-menu-bar-accel"

-- | Set the value of the “@gtk-menu-bar-accel@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkMenuBarAccel 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkMenuBarAccel :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkMenuBarAccel obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-menu-bar-accel" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-menu-bar-accel@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkMenuBarAccel :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkMenuBarAccel val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-menu-bar-accel" (P.Just val)

-- | Set the value of the “@gtk-menu-bar-accel@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkMenuBarAccel
-- @
clearSettingsGtkMenuBarAccel :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkMenuBarAccel obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-menu-bar-accel" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkMenuBarAccelPropertyInfo
instance AttrInfo SettingsGtkMenuBarAccelPropertyInfo where
    type AttrAllowedOps SettingsGtkMenuBarAccelPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkMenuBarAccelPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkMenuBarAccelPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkMenuBarAccelPropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkMenuBarAccelPropertyInfo = T.Text
    type AttrGetType SettingsGtkMenuBarAccelPropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkMenuBarAccelPropertyInfo = "gtk-menu-bar-accel"
    type AttrOrigin SettingsGtkMenuBarAccelPropertyInfo = Settings
    attrGet = getSettingsGtkMenuBarAccel
    attrSet = setSettingsGtkMenuBarAccel
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkMenuBarAccel
    attrClear = clearSettingsGtkMenuBarAccel
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkMenuBarAccel"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkMenuBarAccel"
        })
#endif

-- VVV Prop "gtk-menu-bar-popup-delay"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-menu-bar-popup-delay@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkMenuBarPopupDelay
-- @
getSettingsGtkMenuBarPopupDelay :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkMenuBarPopupDelay obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-menu-bar-popup-delay"

-- | Set the value of the “@gtk-menu-bar-popup-delay@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkMenuBarPopupDelay 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkMenuBarPopupDelay :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkMenuBarPopupDelay obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-menu-bar-popup-delay" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-menu-bar-popup-delay@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkMenuBarPopupDelay :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkMenuBarPopupDelay val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-menu-bar-popup-delay" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkMenuBarPopupDelayPropertyInfo
instance AttrInfo SettingsGtkMenuBarPopupDelayPropertyInfo where
    type AttrAllowedOps SettingsGtkMenuBarPopupDelayPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkMenuBarPopupDelayPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkMenuBarPopupDelayPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkMenuBarPopupDelayPropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkMenuBarPopupDelayPropertyInfo = Int32
    type AttrGetType SettingsGtkMenuBarPopupDelayPropertyInfo = Int32
    type AttrLabel SettingsGtkMenuBarPopupDelayPropertyInfo = "gtk-menu-bar-popup-delay"
    type AttrOrigin SettingsGtkMenuBarPopupDelayPropertyInfo = Settings
    attrGet = getSettingsGtkMenuBarPopupDelay
    attrSet = setSettingsGtkMenuBarPopupDelay
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkMenuBarPopupDelay
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkMenuBarPopupDelay"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkMenuBarPopupDelay"
        })
#endif

-- VVV Prop "gtk-menu-images"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-menu-images@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkMenuImages
-- @
getSettingsGtkMenuImages :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkMenuImages obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-menu-images"

-- | Set the value of the “@gtk-menu-images@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkMenuImages 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkMenuImages :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkMenuImages obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-menu-images" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-menu-images@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkMenuImages :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkMenuImages val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-menu-images" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkMenuImagesPropertyInfo
instance AttrInfo SettingsGtkMenuImagesPropertyInfo where
    type AttrAllowedOps SettingsGtkMenuImagesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkMenuImagesPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkMenuImagesPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkMenuImagesPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkMenuImagesPropertyInfo = Bool
    type AttrGetType SettingsGtkMenuImagesPropertyInfo = Bool
    type AttrLabel SettingsGtkMenuImagesPropertyInfo = "gtk-menu-images"
    type AttrOrigin SettingsGtkMenuImagesPropertyInfo = Settings
    attrGet = getSettingsGtkMenuImages
    attrSet = setSettingsGtkMenuImages
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkMenuImages
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkMenuImages"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkMenuImages"
        })
#endif

-- VVV Prop "gtk-menu-popdown-delay"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-menu-popdown-delay@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkMenuPopdownDelay
-- @
getSettingsGtkMenuPopdownDelay :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkMenuPopdownDelay obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-menu-popdown-delay"

-- | Set the value of the “@gtk-menu-popdown-delay@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkMenuPopdownDelay 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkMenuPopdownDelay :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkMenuPopdownDelay obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-menu-popdown-delay" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-menu-popdown-delay@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkMenuPopdownDelay :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkMenuPopdownDelay val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-menu-popdown-delay" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkMenuPopdownDelayPropertyInfo
instance AttrInfo SettingsGtkMenuPopdownDelayPropertyInfo where
    type AttrAllowedOps SettingsGtkMenuPopdownDelayPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkMenuPopdownDelayPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkMenuPopdownDelayPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkMenuPopdownDelayPropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkMenuPopdownDelayPropertyInfo = Int32
    type AttrGetType SettingsGtkMenuPopdownDelayPropertyInfo = Int32
    type AttrLabel SettingsGtkMenuPopdownDelayPropertyInfo = "gtk-menu-popdown-delay"
    type AttrOrigin SettingsGtkMenuPopdownDelayPropertyInfo = Settings
    attrGet = getSettingsGtkMenuPopdownDelay
    attrSet = setSettingsGtkMenuPopdownDelay
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkMenuPopdownDelay
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkMenuPopdownDelay"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkMenuPopdownDelay"
        })
#endif

-- VVV Prop "gtk-menu-popup-delay"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-menu-popup-delay@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkMenuPopupDelay
-- @
getSettingsGtkMenuPopupDelay :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkMenuPopupDelay obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-menu-popup-delay"

-- | Set the value of the “@gtk-menu-popup-delay@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkMenuPopupDelay 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkMenuPopupDelay :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkMenuPopupDelay obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-menu-popup-delay" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-menu-popup-delay@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkMenuPopupDelay :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkMenuPopupDelay val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-menu-popup-delay" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkMenuPopupDelayPropertyInfo
instance AttrInfo SettingsGtkMenuPopupDelayPropertyInfo where
    type AttrAllowedOps SettingsGtkMenuPopupDelayPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkMenuPopupDelayPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkMenuPopupDelayPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkMenuPopupDelayPropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkMenuPopupDelayPropertyInfo = Int32
    type AttrGetType SettingsGtkMenuPopupDelayPropertyInfo = Int32
    type AttrLabel SettingsGtkMenuPopupDelayPropertyInfo = "gtk-menu-popup-delay"
    type AttrOrigin SettingsGtkMenuPopupDelayPropertyInfo = Settings
    attrGet = getSettingsGtkMenuPopupDelay
    attrSet = setSettingsGtkMenuPopupDelay
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkMenuPopupDelay
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkMenuPopupDelay"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkMenuPopupDelay"
        })
#endif

-- VVV Prop "gtk-modules"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-modules@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkModules
-- @
getSettingsGtkModules :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkModules obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-modules"

-- | Set the value of the “@gtk-modules@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkModules 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkModules :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkModules obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-modules" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-modules@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkModules :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkModules val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-modules" (P.Just val)

-- | Set the value of the “@gtk-modules@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkModules
-- @
clearSettingsGtkModules :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkModules obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-modules" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkModulesPropertyInfo
instance AttrInfo SettingsGtkModulesPropertyInfo where
    type AttrAllowedOps SettingsGtkModulesPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkModulesPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkModulesPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkModulesPropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkModulesPropertyInfo = T.Text
    type AttrGetType SettingsGtkModulesPropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkModulesPropertyInfo = "gtk-modules"
    type AttrOrigin SettingsGtkModulesPropertyInfo = Settings
    attrGet = getSettingsGtkModules
    attrSet = setSettingsGtkModules
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkModules
    attrClear = clearSettingsGtkModules
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkModules"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkModules"
        })
#endif

-- VVV Prop "gtk-overlay-scrolling"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-overlay-scrolling@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkOverlayScrolling
-- @
getSettingsGtkOverlayScrolling :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkOverlayScrolling obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-overlay-scrolling"

-- | Set the value of the “@gtk-overlay-scrolling@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkOverlayScrolling 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkOverlayScrolling :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkOverlayScrolling obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-overlay-scrolling" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-overlay-scrolling@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkOverlayScrolling :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkOverlayScrolling val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-overlay-scrolling" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkOverlayScrollingPropertyInfo
instance AttrInfo SettingsGtkOverlayScrollingPropertyInfo where
    type AttrAllowedOps SettingsGtkOverlayScrollingPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkOverlayScrollingPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkOverlayScrollingPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkOverlayScrollingPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkOverlayScrollingPropertyInfo = Bool
    type AttrGetType SettingsGtkOverlayScrollingPropertyInfo = Bool
    type AttrLabel SettingsGtkOverlayScrollingPropertyInfo = "gtk-overlay-scrolling"
    type AttrOrigin SettingsGtkOverlayScrollingPropertyInfo = Settings
    attrGet = getSettingsGtkOverlayScrolling
    attrSet = setSettingsGtkOverlayScrolling
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkOverlayScrolling
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkOverlayScrolling"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkOverlayScrolling"
        })
#endif

-- VVV Prop "gtk-primary-button-warps-slider"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-primary-button-warps-slider@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkPrimaryButtonWarpsSlider
-- @
getSettingsGtkPrimaryButtonWarpsSlider :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkPrimaryButtonWarpsSlider obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-primary-button-warps-slider"

-- | Set the value of the “@gtk-primary-button-warps-slider@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkPrimaryButtonWarpsSlider 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkPrimaryButtonWarpsSlider :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkPrimaryButtonWarpsSlider obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-primary-button-warps-slider" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-primary-button-warps-slider@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkPrimaryButtonWarpsSlider :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkPrimaryButtonWarpsSlider val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-primary-button-warps-slider" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkPrimaryButtonWarpsSliderPropertyInfo
instance AttrInfo SettingsGtkPrimaryButtonWarpsSliderPropertyInfo where
    type AttrAllowedOps SettingsGtkPrimaryButtonWarpsSliderPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkPrimaryButtonWarpsSliderPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkPrimaryButtonWarpsSliderPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkPrimaryButtonWarpsSliderPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkPrimaryButtonWarpsSliderPropertyInfo = Bool
    type AttrGetType SettingsGtkPrimaryButtonWarpsSliderPropertyInfo = Bool
    type AttrLabel SettingsGtkPrimaryButtonWarpsSliderPropertyInfo = "gtk-primary-button-warps-slider"
    type AttrOrigin SettingsGtkPrimaryButtonWarpsSliderPropertyInfo = Settings
    attrGet = getSettingsGtkPrimaryButtonWarpsSlider
    attrSet = setSettingsGtkPrimaryButtonWarpsSlider
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkPrimaryButtonWarpsSlider
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkPrimaryButtonWarpsSlider"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkPrimaryButtonWarpsSlider"
        })
#endif

-- VVV Prop "gtk-print-backends"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-print-backends@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkPrintBackends
-- @
getSettingsGtkPrintBackends :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkPrintBackends obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-print-backends"

-- | Set the value of the “@gtk-print-backends@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkPrintBackends 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkPrintBackends :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkPrintBackends obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-print-backends" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-print-backends@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkPrintBackends :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkPrintBackends val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-print-backends" (P.Just val)

-- | Set the value of the “@gtk-print-backends@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkPrintBackends
-- @
clearSettingsGtkPrintBackends :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkPrintBackends obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-print-backends" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkPrintBackendsPropertyInfo
instance AttrInfo SettingsGtkPrintBackendsPropertyInfo where
    type AttrAllowedOps SettingsGtkPrintBackendsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkPrintBackendsPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkPrintBackendsPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkPrintBackendsPropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkPrintBackendsPropertyInfo = T.Text
    type AttrGetType SettingsGtkPrintBackendsPropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkPrintBackendsPropertyInfo = "gtk-print-backends"
    type AttrOrigin SettingsGtkPrintBackendsPropertyInfo = Settings
    attrGet = getSettingsGtkPrintBackends
    attrSet = setSettingsGtkPrintBackends
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkPrintBackends
    attrClear = clearSettingsGtkPrintBackends
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkPrintBackends"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkPrintBackends"
        })
#endif

-- VVV Prop "gtk-print-preview-command"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-print-preview-command@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkPrintPreviewCommand
-- @
getSettingsGtkPrintPreviewCommand :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkPrintPreviewCommand obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-print-preview-command"

-- | Set the value of the “@gtk-print-preview-command@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkPrintPreviewCommand 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkPrintPreviewCommand :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkPrintPreviewCommand obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-print-preview-command" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-print-preview-command@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkPrintPreviewCommand :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkPrintPreviewCommand val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-print-preview-command" (P.Just val)

-- | Set the value of the “@gtk-print-preview-command@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkPrintPreviewCommand
-- @
clearSettingsGtkPrintPreviewCommand :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkPrintPreviewCommand obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-print-preview-command" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkPrintPreviewCommandPropertyInfo
instance AttrInfo SettingsGtkPrintPreviewCommandPropertyInfo where
    type AttrAllowedOps SettingsGtkPrintPreviewCommandPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkPrintPreviewCommandPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkPrintPreviewCommandPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkPrintPreviewCommandPropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkPrintPreviewCommandPropertyInfo = T.Text
    type AttrGetType SettingsGtkPrintPreviewCommandPropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkPrintPreviewCommandPropertyInfo = "gtk-print-preview-command"
    type AttrOrigin SettingsGtkPrintPreviewCommandPropertyInfo = Settings
    attrGet = getSettingsGtkPrintPreviewCommand
    attrSet = setSettingsGtkPrintPreviewCommand
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkPrintPreviewCommand
    attrClear = clearSettingsGtkPrintPreviewCommand
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkPrintPreviewCommand"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkPrintPreviewCommand"
        })
#endif

-- VVV Prop "gtk-recent-files-enabled"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-recent-files-enabled@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkRecentFilesEnabled
-- @
getSettingsGtkRecentFilesEnabled :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkRecentFilesEnabled obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-recent-files-enabled"

-- | Set the value of the “@gtk-recent-files-enabled@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkRecentFilesEnabled 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkRecentFilesEnabled :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkRecentFilesEnabled obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-recent-files-enabled" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-recent-files-enabled@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkRecentFilesEnabled :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkRecentFilesEnabled val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-recent-files-enabled" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkRecentFilesEnabledPropertyInfo
instance AttrInfo SettingsGtkRecentFilesEnabledPropertyInfo where
    type AttrAllowedOps SettingsGtkRecentFilesEnabledPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkRecentFilesEnabledPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkRecentFilesEnabledPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkRecentFilesEnabledPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkRecentFilesEnabledPropertyInfo = Bool
    type AttrGetType SettingsGtkRecentFilesEnabledPropertyInfo = Bool
    type AttrLabel SettingsGtkRecentFilesEnabledPropertyInfo = "gtk-recent-files-enabled"
    type AttrOrigin SettingsGtkRecentFilesEnabledPropertyInfo = Settings
    attrGet = getSettingsGtkRecentFilesEnabled
    attrSet = setSettingsGtkRecentFilesEnabled
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkRecentFilesEnabled
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkRecentFilesEnabled"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkRecentFilesEnabled"
        })
#endif

-- VVV Prop "gtk-recent-files-limit"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-recent-files-limit@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkRecentFilesLimit
-- @
getSettingsGtkRecentFilesLimit :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkRecentFilesLimit obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-recent-files-limit"

-- | Set the value of the “@gtk-recent-files-limit@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkRecentFilesLimit 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkRecentFilesLimit :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkRecentFilesLimit obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-recent-files-limit" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-recent-files-limit@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkRecentFilesLimit :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkRecentFilesLimit val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-recent-files-limit" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkRecentFilesLimitPropertyInfo
instance AttrInfo SettingsGtkRecentFilesLimitPropertyInfo where
    type AttrAllowedOps SettingsGtkRecentFilesLimitPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkRecentFilesLimitPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkRecentFilesLimitPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkRecentFilesLimitPropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkRecentFilesLimitPropertyInfo = Int32
    type AttrGetType SettingsGtkRecentFilesLimitPropertyInfo = Int32
    type AttrLabel SettingsGtkRecentFilesLimitPropertyInfo = "gtk-recent-files-limit"
    type AttrOrigin SettingsGtkRecentFilesLimitPropertyInfo = Settings
    attrGet = getSettingsGtkRecentFilesLimit
    attrSet = setSettingsGtkRecentFilesLimit
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkRecentFilesLimit
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkRecentFilesLimit"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkRecentFilesLimit"
        })
#endif

-- VVV Prop "gtk-recent-files-max-age"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-recent-files-max-age@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkRecentFilesMaxAge
-- @
getSettingsGtkRecentFilesMaxAge :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkRecentFilesMaxAge obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-recent-files-max-age"

-- | Set the value of the “@gtk-recent-files-max-age@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkRecentFilesMaxAge 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkRecentFilesMaxAge :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkRecentFilesMaxAge obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-recent-files-max-age" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-recent-files-max-age@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkRecentFilesMaxAge :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkRecentFilesMaxAge val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-recent-files-max-age" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkRecentFilesMaxAgePropertyInfo
instance AttrInfo SettingsGtkRecentFilesMaxAgePropertyInfo where
    type AttrAllowedOps SettingsGtkRecentFilesMaxAgePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkRecentFilesMaxAgePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkRecentFilesMaxAgePropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkRecentFilesMaxAgePropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkRecentFilesMaxAgePropertyInfo = Int32
    type AttrGetType SettingsGtkRecentFilesMaxAgePropertyInfo = Int32
    type AttrLabel SettingsGtkRecentFilesMaxAgePropertyInfo = "gtk-recent-files-max-age"
    type AttrOrigin SettingsGtkRecentFilesMaxAgePropertyInfo = Settings
    attrGet = getSettingsGtkRecentFilesMaxAge
    attrSet = setSettingsGtkRecentFilesMaxAge
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkRecentFilesMaxAge
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkRecentFilesMaxAge"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkRecentFilesMaxAge"
        })
#endif

-- VVV Prop "gtk-scrolled-window-placement"
   -- Type: TInterface (Name {namespace = "Gtk", name = "CornerType"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-scrolled-window-placement@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkScrolledWindowPlacement
-- @
getSettingsGtkScrolledWindowPlacement :: (MonadIO m, IsSettings o) => o -> m Gtk.Enums.CornerType
getSettingsGtkScrolledWindowPlacement obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "gtk-scrolled-window-placement"

-- | Set the value of the “@gtk-scrolled-window-placement@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkScrolledWindowPlacement 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkScrolledWindowPlacement :: (MonadIO m, IsSettings o) => o -> Gtk.Enums.CornerType -> m ()
setSettingsGtkScrolledWindowPlacement obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "gtk-scrolled-window-placement" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-scrolled-window-placement@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkScrolledWindowPlacement :: (IsSettings o, MIO.MonadIO m) => Gtk.Enums.CornerType -> m (GValueConstruct o)
constructSettingsGtkScrolledWindowPlacement val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "gtk-scrolled-window-placement" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkScrolledWindowPlacementPropertyInfo
instance AttrInfo SettingsGtkScrolledWindowPlacementPropertyInfo where
    type AttrAllowedOps SettingsGtkScrolledWindowPlacementPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkScrolledWindowPlacementPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkScrolledWindowPlacementPropertyInfo = (~) Gtk.Enums.CornerType
    type AttrTransferTypeConstraint SettingsGtkScrolledWindowPlacementPropertyInfo = (~) Gtk.Enums.CornerType
    type AttrTransferType SettingsGtkScrolledWindowPlacementPropertyInfo = Gtk.Enums.CornerType
    type AttrGetType SettingsGtkScrolledWindowPlacementPropertyInfo = Gtk.Enums.CornerType
    type AttrLabel SettingsGtkScrolledWindowPlacementPropertyInfo = "gtk-scrolled-window-placement"
    type AttrOrigin SettingsGtkScrolledWindowPlacementPropertyInfo = Settings
    attrGet = getSettingsGtkScrolledWindowPlacement
    attrSet = setSettingsGtkScrolledWindowPlacement
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkScrolledWindowPlacement
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkScrolledWindowPlacement"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkScrolledWindowPlacement"
        })
#endif

-- VVV Prop "gtk-shell-shows-app-menu"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-shell-shows-app-menu@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkShellShowsAppMenu
-- @
getSettingsGtkShellShowsAppMenu :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkShellShowsAppMenu obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-shell-shows-app-menu"

-- | Set the value of the “@gtk-shell-shows-app-menu@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkShellShowsAppMenu 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkShellShowsAppMenu :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkShellShowsAppMenu obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-shell-shows-app-menu" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-shell-shows-app-menu@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkShellShowsAppMenu :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkShellShowsAppMenu val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-shell-shows-app-menu" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkShellShowsAppMenuPropertyInfo
instance AttrInfo SettingsGtkShellShowsAppMenuPropertyInfo where
    type AttrAllowedOps SettingsGtkShellShowsAppMenuPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkShellShowsAppMenuPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkShellShowsAppMenuPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkShellShowsAppMenuPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkShellShowsAppMenuPropertyInfo = Bool
    type AttrGetType SettingsGtkShellShowsAppMenuPropertyInfo = Bool
    type AttrLabel SettingsGtkShellShowsAppMenuPropertyInfo = "gtk-shell-shows-app-menu"
    type AttrOrigin SettingsGtkShellShowsAppMenuPropertyInfo = Settings
    attrGet = getSettingsGtkShellShowsAppMenu
    attrSet = setSettingsGtkShellShowsAppMenu
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkShellShowsAppMenu
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkShellShowsAppMenu"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkShellShowsAppMenu"
        })
#endif

-- VVV Prop "gtk-shell-shows-desktop"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-shell-shows-desktop@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkShellShowsDesktop
-- @
getSettingsGtkShellShowsDesktop :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkShellShowsDesktop obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-shell-shows-desktop"

-- | Set the value of the “@gtk-shell-shows-desktop@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkShellShowsDesktop 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkShellShowsDesktop :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkShellShowsDesktop obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-shell-shows-desktop" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-shell-shows-desktop@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkShellShowsDesktop :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkShellShowsDesktop val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-shell-shows-desktop" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkShellShowsDesktopPropertyInfo
instance AttrInfo SettingsGtkShellShowsDesktopPropertyInfo where
    type AttrAllowedOps SettingsGtkShellShowsDesktopPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkShellShowsDesktopPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkShellShowsDesktopPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkShellShowsDesktopPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkShellShowsDesktopPropertyInfo = Bool
    type AttrGetType SettingsGtkShellShowsDesktopPropertyInfo = Bool
    type AttrLabel SettingsGtkShellShowsDesktopPropertyInfo = "gtk-shell-shows-desktop"
    type AttrOrigin SettingsGtkShellShowsDesktopPropertyInfo = Settings
    attrGet = getSettingsGtkShellShowsDesktop
    attrSet = setSettingsGtkShellShowsDesktop
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkShellShowsDesktop
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkShellShowsDesktop"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkShellShowsDesktop"
        })
#endif

-- VVV Prop "gtk-shell-shows-menubar"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-shell-shows-menubar@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkShellShowsMenubar
-- @
getSettingsGtkShellShowsMenubar :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkShellShowsMenubar obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-shell-shows-menubar"

-- | Set the value of the “@gtk-shell-shows-menubar@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkShellShowsMenubar 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkShellShowsMenubar :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkShellShowsMenubar obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-shell-shows-menubar" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-shell-shows-menubar@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkShellShowsMenubar :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkShellShowsMenubar val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-shell-shows-menubar" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkShellShowsMenubarPropertyInfo
instance AttrInfo SettingsGtkShellShowsMenubarPropertyInfo where
    type AttrAllowedOps SettingsGtkShellShowsMenubarPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkShellShowsMenubarPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkShellShowsMenubarPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkShellShowsMenubarPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkShellShowsMenubarPropertyInfo = Bool
    type AttrGetType SettingsGtkShellShowsMenubarPropertyInfo = Bool
    type AttrLabel SettingsGtkShellShowsMenubarPropertyInfo = "gtk-shell-shows-menubar"
    type AttrOrigin SettingsGtkShellShowsMenubarPropertyInfo = Settings
    attrGet = getSettingsGtkShellShowsMenubar
    attrSet = setSettingsGtkShellShowsMenubar
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkShellShowsMenubar
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkShellShowsMenubar"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkShellShowsMenubar"
        })
#endif

-- VVV Prop "gtk-show-input-method-menu"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-show-input-method-menu@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkShowInputMethodMenu
-- @
getSettingsGtkShowInputMethodMenu :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkShowInputMethodMenu obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-show-input-method-menu"

-- | Set the value of the “@gtk-show-input-method-menu@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkShowInputMethodMenu 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkShowInputMethodMenu :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkShowInputMethodMenu obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-show-input-method-menu" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-show-input-method-menu@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkShowInputMethodMenu :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkShowInputMethodMenu val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-show-input-method-menu" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkShowInputMethodMenuPropertyInfo
instance AttrInfo SettingsGtkShowInputMethodMenuPropertyInfo where
    type AttrAllowedOps SettingsGtkShowInputMethodMenuPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkShowInputMethodMenuPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkShowInputMethodMenuPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkShowInputMethodMenuPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkShowInputMethodMenuPropertyInfo = Bool
    type AttrGetType SettingsGtkShowInputMethodMenuPropertyInfo = Bool
    type AttrLabel SettingsGtkShowInputMethodMenuPropertyInfo = "gtk-show-input-method-menu"
    type AttrOrigin SettingsGtkShowInputMethodMenuPropertyInfo = Settings
    attrGet = getSettingsGtkShowInputMethodMenu
    attrSet = setSettingsGtkShowInputMethodMenu
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkShowInputMethodMenu
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkShowInputMethodMenu"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkShowInputMethodMenu"
        })
#endif

-- VVV Prop "gtk-show-unicode-menu"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-show-unicode-menu@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkShowUnicodeMenu
-- @
getSettingsGtkShowUnicodeMenu :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkShowUnicodeMenu obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-show-unicode-menu"

-- | Set the value of the “@gtk-show-unicode-menu@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkShowUnicodeMenu 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkShowUnicodeMenu :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkShowUnicodeMenu obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-show-unicode-menu" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-show-unicode-menu@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkShowUnicodeMenu :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkShowUnicodeMenu val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-show-unicode-menu" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkShowUnicodeMenuPropertyInfo
instance AttrInfo SettingsGtkShowUnicodeMenuPropertyInfo where
    type AttrAllowedOps SettingsGtkShowUnicodeMenuPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkShowUnicodeMenuPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkShowUnicodeMenuPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkShowUnicodeMenuPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkShowUnicodeMenuPropertyInfo = Bool
    type AttrGetType SettingsGtkShowUnicodeMenuPropertyInfo = Bool
    type AttrLabel SettingsGtkShowUnicodeMenuPropertyInfo = "gtk-show-unicode-menu"
    type AttrOrigin SettingsGtkShowUnicodeMenuPropertyInfo = Settings
    attrGet = getSettingsGtkShowUnicodeMenu
    attrSet = setSettingsGtkShowUnicodeMenu
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkShowUnicodeMenu
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkShowUnicodeMenu"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkShowUnicodeMenu"
        })
#endif

-- VVV Prop "gtk-sound-theme-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-sound-theme-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkSoundThemeName
-- @
getSettingsGtkSoundThemeName :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkSoundThemeName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-sound-theme-name"

-- | Set the value of the “@gtk-sound-theme-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkSoundThemeName 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkSoundThemeName :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkSoundThemeName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-sound-theme-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-sound-theme-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkSoundThemeName :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkSoundThemeName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-sound-theme-name" (P.Just val)

-- | Set the value of the “@gtk-sound-theme-name@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkSoundThemeName
-- @
clearSettingsGtkSoundThemeName :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkSoundThemeName obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-sound-theme-name" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkSoundThemeNamePropertyInfo
instance AttrInfo SettingsGtkSoundThemeNamePropertyInfo where
    type AttrAllowedOps SettingsGtkSoundThemeNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkSoundThemeNamePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkSoundThemeNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkSoundThemeNamePropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkSoundThemeNamePropertyInfo = T.Text
    type AttrGetType SettingsGtkSoundThemeNamePropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkSoundThemeNamePropertyInfo = "gtk-sound-theme-name"
    type AttrOrigin SettingsGtkSoundThemeNamePropertyInfo = Settings
    attrGet = getSettingsGtkSoundThemeName
    attrSet = setSettingsGtkSoundThemeName
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkSoundThemeName
    attrClear = clearSettingsGtkSoundThemeName
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkSoundThemeName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkSoundThemeName"
        })
#endif

-- VVV Prop "gtk-split-cursor"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-split-cursor@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkSplitCursor
-- @
getSettingsGtkSplitCursor :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkSplitCursor obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-split-cursor"

-- | Set the value of the “@gtk-split-cursor@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkSplitCursor 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkSplitCursor :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkSplitCursor obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-split-cursor" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-split-cursor@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkSplitCursor :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkSplitCursor val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-split-cursor" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkSplitCursorPropertyInfo
instance AttrInfo SettingsGtkSplitCursorPropertyInfo where
    type AttrAllowedOps SettingsGtkSplitCursorPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkSplitCursorPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkSplitCursorPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkSplitCursorPropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkSplitCursorPropertyInfo = Bool
    type AttrGetType SettingsGtkSplitCursorPropertyInfo = Bool
    type AttrLabel SettingsGtkSplitCursorPropertyInfo = "gtk-split-cursor"
    type AttrOrigin SettingsGtkSplitCursorPropertyInfo = Settings
    attrGet = getSettingsGtkSplitCursor
    attrSet = setSettingsGtkSplitCursor
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkSplitCursor
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkSplitCursor"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkSplitCursor"
        })
#endif

-- VVV Prop "gtk-theme-name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-theme-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkThemeName
-- @
getSettingsGtkThemeName :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkThemeName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-theme-name"

-- | Set the value of the “@gtk-theme-name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkThemeName 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkThemeName :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkThemeName obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-theme-name" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-theme-name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkThemeName :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkThemeName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-theme-name" (P.Just val)

-- | Set the value of the “@gtk-theme-name@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkThemeName
-- @
clearSettingsGtkThemeName :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkThemeName obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-theme-name" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkThemeNamePropertyInfo
instance AttrInfo SettingsGtkThemeNamePropertyInfo where
    type AttrAllowedOps SettingsGtkThemeNamePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkThemeNamePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkThemeNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkThemeNamePropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkThemeNamePropertyInfo = T.Text
    type AttrGetType SettingsGtkThemeNamePropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkThemeNamePropertyInfo = "gtk-theme-name"
    type AttrOrigin SettingsGtkThemeNamePropertyInfo = Settings
    attrGet = getSettingsGtkThemeName
    attrSet = setSettingsGtkThemeName
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkThemeName
    attrClear = clearSettingsGtkThemeName
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkThemeName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkThemeName"
        })
#endif

-- VVV Prop "gtk-timeout-expand"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-timeout-expand@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkTimeoutExpand
-- @
getSettingsGtkTimeoutExpand :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkTimeoutExpand obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-timeout-expand"

-- | Set the value of the “@gtk-timeout-expand@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkTimeoutExpand 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkTimeoutExpand :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkTimeoutExpand obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-timeout-expand" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-timeout-expand@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkTimeoutExpand :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkTimeoutExpand val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-timeout-expand" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkTimeoutExpandPropertyInfo
instance AttrInfo SettingsGtkTimeoutExpandPropertyInfo where
    type AttrAllowedOps SettingsGtkTimeoutExpandPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkTimeoutExpandPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkTimeoutExpandPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkTimeoutExpandPropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkTimeoutExpandPropertyInfo = Int32
    type AttrGetType SettingsGtkTimeoutExpandPropertyInfo = Int32
    type AttrLabel SettingsGtkTimeoutExpandPropertyInfo = "gtk-timeout-expand"
    type AttrOrigin SettingsGtkTimeoutExpandPropertyInfo = Settings
    attrGet = getSettingsGtkTimeoutExpand
    attrSet = setSettingsGtkTimeoutExpand
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkTimeoutExpand
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkTimeoutExpand"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkTimeoutExpand"
        })
#endif

-- VVV Prop "gtk-timeout-initial"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-timeout-initial@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkTimeoutInitial
-- @
getSettingsGtkTimeoutInitial :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkTimeoutInitial obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-timeout-initial"

-- | Set the value of the “@gtk-timeout-initial@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkTimeoutInitial 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkTimeoutInitial :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkTimeoutInitial obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-timeout-initial" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-timeout-initial@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkTimeoutInitial :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkTimeoutInitial val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-timeout-initial" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkTimeoutInitialPropertyInfo
instance AttrInfo SettingsGtkTimeoutInitialPropertyInfo where
    type AttrAllowedOps SettingsGtkTimeoutInitialPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkTimeoutInitialPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkTimeoutInitialPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkTimeoutInitialPropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkTimeoutInitialPropertyInfo = Int32
    type AttrGetType SettingsGtkTimeoutInitialPropertyInfo = Int32
    type AttrLabel SettingsGtkTimeoutInitialPropertyInfo = "gtk-timeout-initial"
    type AttrOrigin SettingsGtkTimeoutInitialPropertyInfo = Settings
    attrGet = getSettingsGtkTimeoutInitial
    attrSet = setSettingsGtkTimeoutInitial
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkTimeoutInitial
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkTimeoutInitial"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkTimeoutInitial"
        })
#endif

-- VVV Prop "gtk-timeout-repeat"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-timeout-repeat@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkTimeoutRepeat
-- @
getSettingsGtkTimeoutRepeat :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkTimeoutRepeat obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-timeout-repeat"

-- | Set the value of the “@gtk-timeout-repeat@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkTimeoutRepeat 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkTimeoutRepeat :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkTimeoutRepeat obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-timeout-repeat" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-timeout-repeat@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkTimeoutRepeat :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkTimeoutRepeat val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-timeout-repeat" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkTimeoutRepeatPropertyInfo
instance AttrInfo SettingsGtkTimeoutRepeatPropertyInfo where
    type AttrAllowedOps SettingsGtkTimeoutRepeatPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkTimeoutRepeatPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkTimeoutRepeatPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkTimeoutRepeatPropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkTimeoutRepeatPropertyInfo = Int32
    type AttrGetType SettingsGtkTimeoutRepeatPropertyInfo = Int32
    type AttrLabel SettingsGtkTimeoutRepeatPropertyInfo = "gtk-timeout-repeat"
    type AttrOrigin SettingsGtkTimeoutRepeatPropertyInfo = Settings
    attrGet = getSettingsGtkTimeoutRepeat
    attrSet = setSettingsGtkTimeoutRepeat
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkTimeoutRepeat
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkTimeoutRepeat"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkTimeoutRepeat"
        })
#endif

-- VVV Prop "gtk-titlebar-double-click"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-titlebar-double-click@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkTitlebarDoubleClick
-- @
getSettingsGtkTitlebarDoubleClick :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkTitlebarDoubleClick obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-titlebar-double-click"

-- | Set the value of the “@gtk-titlebar-double-click@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkTitlebarDoubleClick 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkTitlebarDoubleClick :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkTitlebarDoubleClick obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-titlebar-double-click" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-titlebar-double-click@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkTitlebarDoubleClick :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkTitlebarDoubleClick val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-titlebar-double-click" (P.Just val)

-- | Set the value of the “@gtk-titlebar-double-click@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkTitlebarDoubleClick
-- @
clearSettingsGtkTitlebarDoubleClick :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkTitlebarDoubleClick obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-titlebar-double-click" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkTitlebarDoubleClickPropertyInfo
instance AttrInfo SettingsGtkTitlebarDoubleClickPropertyInfo where
    type AttrAllowedOps SettingsGtkTitlebarDoubleClickPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkTitlebarDoubleClickPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkTitlebarDoubleClickPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkTitlebarDoubleClickPropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkTitlebarDoubleClickPropertyInfo = T.Text
    type AttrGetType SettingsGtkTitlebarDoubleClickPropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkTitlebarDoubleClickPropertyInfo = "gtk-titlebar-double-click"
    type AttrOrigin SettingsGtkTitlebarDoubleClickPropertyInfo = Settings
    attrGet = getSettingsGtkTitlebarDoubleClick
    attrSet = setSettingsGtkTitlebarDoubleClick
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkTitlebarDoubleClick
    attrClear = clearSettingsGtkTitlebarDoubleClick
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkTitlebarDoubleClick"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkTitlebarDoubleClick"
        })
#endif

-- VVV Prop "gtk-titlebar-middle-click"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-titlebar-middle-click@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkTitlebarMiddleClick
-- @
getSettingsGtkTitlebarMiddleClick :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkTitlebarMiddleClick obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-titlebar-middle-click"

-- | Set the value of the “@gtk-titlebar-middle-click@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkTitlebarMiddleClick 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkTitlebarMiddleClick :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkTitlebarMiddleClick obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-titlebar-middle-click" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-titlebar-middle-click@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkTitlebarMiddleClick :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkTitlebarMiddleClick val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-titlebar-middle-click" (P.Just val)

-- | Set the value of the “@gtk-titlebar-middle-click@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkTitlebarMiddleClick
-- @
clearSettingsGtkTitlebarMiddleClick :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkTitlebarMiddleClick obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-titlebar-middle-click" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkTitlebarMiddleClickPropertyInfo
instance AttrInfo SettingsGtkTitlebarMiddleClickPropertyInfo where
    type AttrAllowedOps SettingsGtkTitlebarMiddleClickPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkTitlebarMiddleClickPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkTitlebarMiddleClickPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkTitlebarMiddleClickPropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkTitlebarMiddleClickPropertyInfo = T.Text
    type AttrGetType SettingsGtkTitlebarMiddleClickPropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkTitlebarMiddleClickPropertyInfo = "gtk-titlebar-middle-click"
    type AttrOrigin SettingsGtkTitlebarMiddleClickPropertyInfo = Settings
    attrGet = getSettingsGtkTitlebarMiddleClick
    attrSet = setSettingsGtkTitlebarMiddleClick
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkTitlebarMiddleClick
    attrClear = clearSettingsGtkTitlebarMiddleClick
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkTitlebarMiddleClick"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkTitlebarMiddleClick"
        })
#endif

-- VVV Prop "gtk-titlebar-right-click"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-titlebar-right-click@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkTitlebarRightClick
-- @
getSettingsGtkTitlebarRightClick :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkTitlebarRightClick obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-titlebar-right-click"

-- | Set the value of the “@gtk-titlebar-right-click@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkTitlebarRightClick 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkTitlebarRightClick :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkTitlebarRightClick obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-titlebar-right-click" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-titlebar-right-click@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkTitlebarRightClick :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkTitlebarRightClick val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-titlebar-right-click" (P.Just val)

-- | Set the value of the “@gtk-titlebar-right-click@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkTitlebarRightClick
-- @
clearSettingsGtkTitlebarRightClick :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkTitlebarRightClick obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-titlebar-right-click" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkTitlebarRightClickPropertyInfo
instance AttrInfo SettingsGtkTitlebarRightClickPropertyInfo where
    type AttrAllowedOps SettingsGtkTitlebarRightClickPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkTitlebarRightClickPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkTitlebarRightClickPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkTitlebarRightClickPropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkTitlebarRightClickPropertyInfo = T.Text
    type AttrGetType SettingsGtkTitlebarRightClickPropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkTitlebarRightClickPropertyInfo = "gtk-titlebar-right-click"
    type AttrOrigin SettingsGtkTitlebarRightClickPropertyInfo = Settings
    attrGet = getSettingsGtkTitlebarRightClick
    attrSet = setSettingsGtkTitlebarRightClick
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkTitlebarRightClick
    attrClear = clearSettingsGtkTitlebarRightClick
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkTitlebarRightClick"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkTitlebarRightClick"
        })
#endif

-- VVV Prop "gtk-toolbar-icon-size"
   -- Type: TInterface (Name {namespace = "Gtk", name = "IconSize"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-toolbar-icon-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkToolbarIconSize
-- @
getSettingsGtkToolbarIconSize :: (MonadIO m, IsSettings o) => o -> m Gtk.Enums.IconSize
getSettingsGtkToolbarIconSize obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "gtk-toolbar-icon-size"

-- | Set the value of the “@gtk-toolbar-icon-size@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkToolbarIconSize 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkToolbarIconSize :: (MonadIO m, IsSettings o) => o -> Gtk.Enums.IconSize -> m ()
setSettingsGtkToolbarIconSize obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "gtk-toolbar-icon-size" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-toolbar-icon-size@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkToolbarIconSize :: (IsSettings o, MIO.MonadIO m) => Gtk.Enums.IconSize -> m (GValueConstruct o)
constructSettingsGtkToolbarIconSize val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "gtk-toolbar-icon-size" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkToolbarIconSizePropertyInfo
instance AttrInfo SettingsGtkToolbarIconSizePropertyInfo where
    type AttrAllowedOps SettingsGtkToolbarIconSizePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkToolbarIconSizePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkToolbarIconSizePropertyInfo = (~) Gtk.Enums.IconSize
    type AttrTransferTypeConstraint SettingsGtkToolbarIconSizePropertyInfo = (~) Gtk.Enums.IconSize
    type AttrTransferType SettingsGtkToolbarIconSizePropertyInfo = Gtk.Enums.IconSize
    type AttrGetType SettingsGtkToolbarIconSizePropertyInfo = Gtk.Enums.IconSize
    type AttrLabel SettingsGtkToolbarIconSizePropertyInfo = "gtk-toolbar-icon-size"
    type AttrOrigin SettingsGtkToolbarIconSizePropertyInfo = Settings
    attrGet = getSettingsGtkToolbarIconSize
    attrSet = setSettingsGtkToolbarIconSize
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkToolbarIconSize
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkToolbarIconSize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkToolbarIconSize"
        })
#endif

-- VVV Prop "gtk-toolbar-style"
   -- Type: TInterface (Name {namespace = "Gtk", name = "ToolbarStyle"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-toolbar-style@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkToolbarStyle
-- @
getSettingsGtkToolbarStyle :: (MonadIO m, IsSettings o) => o -> m Gtk.Enums.ToolbarStyle
getSettingsGtkToolbarStyle obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "gtk-toolbar-style"

-- | Set the value of the “@gtk-toolbar-style@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkToolbarStyle 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkToolbarStyle :: (MonadIO m, IsSettings o) => o -> Gtk.Enums.ToolbarStyle -> m ()
setSettingsGtkToolbarStyle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "gtk-toolbar-style" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-toolbar-style@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkToolbarStyle :: (IsSettings o, MIO.MonadIO m) => Gtk.Enums.ToolbarStyle -> m (GValueConstruct o)
constructSettingsGtkToolbarStyle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "gtk-toolbar-style" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkToolbarStylePropertyInfo
instance AttrInfo SettingsGtkToolbarStylePropertyInfo where
    type AttrAllowedOps SettingsGtkToolbarStylePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkToolbarStylePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkToolbarStylePropertyInfo = (~) Gtk.Enums.ToolbarStyle
    type AttrTransferTypeConstraint SettingsGtkToolbarStylePropertyInfo = (~) Gtk.Enums.ToolbarStyle
    type AttrTransferType SettingsGtkToolbarStylePropertyInfo = Gtk.Enums.ToolbarStyle
    type AttrGetType SettingsGtkToolbarStylePropertyInfo = Gtk.Enums.ToolbarStyle
    type AttrLabel SettingsGtkToolbarStylePropertyInfo = "gtk-toolbar-style"
    type AttrOrigin SettingsGtkToolbarStylePropertyInfo = Settings
    attrGet = getSettingsGtkToolbarStyle
    attrSet = setSettingsGtkToolbarStyle
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkToolbarStyle
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkToolbarStyle"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkToolbarStyle"
        })
#endif

-- VVV Prop "gtk-tooltip-browse-mode-timeout"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-tooltip-browse-mode-timeout@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkTooltipBrowseModeTimeout
-- @
getSettingsGtkTooltipBrowseModeTimeout :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkTooltipBrowseModeTimeout obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-tooltip-browse-mode-timeout"

-- | Set the value of the “@gtk-tooltip-browse-mode-timeout@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkTooltipBrowseModeTimeout 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkTooltipBrowseModeTimeout :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkTooltipBrowseModeTimeout obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-tooltip-browse-mode-timeout" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-tooltip-browse-mode-timeout@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkTooltipBrowseModeTimeout :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkTooltipBrowseModeTimeout val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-tooltip-browse-mode-timeout" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkTooltipBrowseModeTimeoutPropertyInfo
instance AttrInfo SettingsGtkTooltipBrowseModeTimeoutPropertyInfo where
    type AttrAllowedOps SettingsGtkTooltipBrowseModeTimeoutPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkTooltipBrowseModeTimeoutPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkTooltipBrowseModeTimeoutPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkTooltipBrowseModeTimeoutPropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkTooltipBrowseModeTimeoutPropertyInfo = Int32
    type AttrGetType SettingsGtkTooltipBrowseModeTimeoutPropertyInfo = Int32
    type AttrLabel SettingsGtkTooltipBrowseModeTimeoutPropertyInfo = "gtk-tooltip-browse-mode-timeout"
    type AttrOrigin SettingsGtkTooltipBrowseModeTimeoutPropertyInfo = Settings
    attrGet = getSettingsGtkTooltipBrowseModeTimeout
    attrSet = setSettingsGtkTooltipBrowseModeTimeout
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkTooltipBrowseModeTimeout
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkTooltipBrowseModeTimeout"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkTooltipBrowseModeTimeout"
        })
#endif

-- VVV Prop "gtk-tooltip-browse-timeout"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-tooltip-browse-timeout@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkTooltipBrowseTimeout
-- @
getSettingsGtkTooltipBrowseTimeout :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkTooltipBrowseTimeout obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-tooltip-browse-timeout"

-- | Set the value of the “@gtk-tooltip-browse-timeout@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkTooltipBrowseTimeout 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkTooltipBrowseTimeout :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkTooltipBrowseTimeout obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-tooltip-browse-timeout" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-tooltip-browse-timeout@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkTooltipBrowseTimeout :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkTooltipBrowseTimeout val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-tooltip-browse-timeout" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkTooltipBrowseTimeoutPropertyInfo
instance AttrInfo SettingsGtkTooltipBrowseTimeoutPropertyInfo where
    type AttrAllowedOps SettingsGtkTooltipBrowseTimeoutPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkTooltipBrowseTimeoutPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkTooltipBrowseTimeoutPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkTooltipBrowseTimeoutPropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkTooltipBrowseTimeoutPropertyInfo = Int32
    type AttrGetType SettingsGtkTooltipBrowseTimeoutPropertyInfo = Int32
    type AttrLabel SettingsGtkTooltipBrowseTimeoutPropertyInfo = "gtk-tooltip-browse-timeout"
    type AttrOrigin SettingsGtkTooltipBrowseTimeoutPropertyInfo = Settings
    attrGet = getSettingsGtkTooltipBrowseTimeout
    attrSet = setSettingsGtkTooltipBrowseTimeout
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkTooltipBrowseTimeout
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkTooltipBrowseTimeout"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkTooltipBrowseTimeout"
        })
#endif

-- VVV Prop "gtk-tooltip-timeout"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-tooltip-timeout@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkTooltipTimeout
-- @
getSettingsGtkTooltipTimeout :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkTooltipTimeout obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-tooltip-timeout"

-- | Set the value of the “@gtk-tooltip-timeout@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkTooltipTimeout 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkTooltipTimeout :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkTooltipTimeout obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-tooltip-timeout" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-tooltip-timeout@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkTooltipTimeout :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkTooltipTimeout val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-tooltip-timeout" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkTooltipTimeoutPropertyInfo
instance AttrInfo SettingsGtkTooltipTimeoutPropertyInfo where
    type AttrAllowedOps SettingsGtkTooltipTimeoutPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkTooltipTimeoutPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkTooltipTimeoutPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkTooltipTimeoutPropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkTooltipTimeoutPropertyInfo = Int32
    type AttrGetType SettingsGtkTooltipTimeoutPropertyInfo = Int32
    type AttrLabel SettingsGtkTooltipTimeoutPropertyInfo = "gtk-tooltip-timeout"
    type AttrOrigin SettingsGtkTooltipTimeoutPropertyInfo = Settings
    attrGet = getSettingsGtkTooltipTimeout
    attrSet = setSettingsGtkTooltipTimeout
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkTooltipTimeout
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkTooltipTimeout"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkTooltipTimeout"
        })
#endif

-- VVV Prop "gtk-touchscreen-mode"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-touchscreen-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkTouchscreenMode
-- @
getSettingsGtkTouchscreenMode :: (MonadIO m, IsSettings o) => o -> m Bool
getSettingsGtkTouchscreenMode obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "gtk-touchscreen-mode"

-- | Set the value of the “@gtk-touchscreen-mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkTouchscreenMode 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkTouchscreenMode :: (MonadIO m, IsSettings o) => o -> Bool -> m ()
setSettingsGtkTouchscreenMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "gtk-touchscreen-mode" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-touchscreen-mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkTouchscreenMode :: (IsSettings o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructSettingsGtkTouchscreenMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "gtk-touchscreen-mode" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkTouchscreenModePropertyInfo
instance AttrInfo SettingsGtkTouchscreenModePropertyInfo where
    type AttrAllowedOps SettingsGtkTouchscreenModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkTouchscreenModePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkTouchscreenModePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint SettingsGtkTouchscreenModePropertyInfo = (~) Bool
    type AttrTransferType SettingsGtkTouchscreenModePropertyInfo = Bool
    type AttrGetType SettingsGtkTouchscreenModePropertyInfo = Bool
    type AttrLabel SettingsGtkTouchscreenModePropertyInfo = "gtk-touchscreen-mode"
    type AttrOrigin SettingsGtkTouchscreenModePropertyInfo = Settings
    attrGet = getSettingsGtkTouchscreenMode
    attrSet = setSettingsGtkTouchscreenMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkTouchscreenMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkTouchscreenMode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkTouchscreenMode"
        })
#endif

-- VVV Prop "gtk-visible-focus"
   -- Type: TInterface (Name {namespace = "Gtk", name = "PolicyType"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-visible-focus@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkVisibleFocus
-- @
getSettingsGtkVisibleFocus :: (MonadIO m, IsSettings o) => o -> m Gtk.Enums.PolicyType
getSettingsGtkVisibleFocus obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "gtk-visible-focus"

-- | Set the value of the “@gtk-visible-focus@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkVisibleFocus 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkVisibleFocus :: (MonadIO m, IsSettings o) => o -> Gtk.Enums.PolicyType -> m ()
setSettingsGtkVisibleFocus obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "gtk-visible-focus" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-visible-focus@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkVisibleFocus :: (IsSettings o, MIO.MonadIO m) => Gtk.Enums.PolicyType -> m (GValueConstruct o)
constructSettingsGtkVisibleFocus val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "gtk-visible-focus" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkVisibleFocusPropertyInfo
instance AttrInfo SettingsGtkVisibleFocusPropertyInfo where
    type AttrAllowedOps SettingsGtkVisibleFocusPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkVisibleFocusPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkVisibleFocusPropertyInfo = (~) Gtk.Enums.PolicyType
    type AttrTransferTypeConstraint SettingsGtkVisibleFocusPropertyInfo = (~) Gtk.Enums.PolicyType
    type AttrTransferType SettingsGtkVisibleFocusPropertyInfo = Gtk.Enums.PolicyType
    type AttrGetType SettingsGtkVisibleFocusPropertyInfo = Gtk.Enums.PolicyType
    type AttrLabel SettingsGtkVisibleFocusPropertyInfo = "gtk-visible-focus"
    type AttrOrigin SettingsGtkVisibleFocusPropertyInfo = Settings
    attrGet = getSettingsGtkVisibleFocus
    attrSet = setSettingsGtkVisibleFocus
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkVisibleFocus
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkVisibleFocus"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkVisibleFocus"
        })
#endif

-- VVV Prop "gtk-xft-antialias"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-xft-antialias@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkXftAntialias
-- @
getSettingsGtkXftAntialias :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkXftAntialias obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-xft-antialias"

-- | Set the value of the “@gtk-xft-antialias@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkXftAntialias 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkXftAntialias :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkXftAntialias obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-xft-antialias" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-xft-antialias@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkXftAntialias :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkXftAntialias val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-xft-antialias" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkXftAntialiasPropertyInfo
instance AttrInfo SettingsGtkXftAntialiasPropertyInfo where
    type AttrAllowedOps SettingsGtkXftAntialiasPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkXftAntialiasPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkXftAntialiasPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkXftAntialiasPropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkXftAntialiasPropertyInfo = Int32
    type AttrGetType SettingsGtkXftAntialiasPropertyInfo = Int32
    type AttrLabel SettingsGtkXftAntialiasPropertyInfo = "gtk-xft-antialias"
    type AttrOrigin SettingsGtkXftAntialiasPropertyInfo = Settings
    attrGet = getSettingsGtkXftAntialias
    attrSet = setSettingsGtkXftAntialias
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkXftAntialias
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkXftAntialias"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkXftAntialias"
        })
#endif

-- VVV Prop "gtk-xft-dpi"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-xft-dpi@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkXftDpi
-- @
getSettingsGtkXftDpi :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkXftDpi obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-xft-dpi"

-- | Set the value of the “@gtk-xft-dpi@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkXftDpi 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkXftDpi :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkXftDpi obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-xft-dpi" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-xft-dpi@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkXftDpi :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkXftDpi val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-xft-dpi" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkXftDpiPropertyInfo
instance AttrInfo SettingsGtkXftDpiPropertyInfo where
    type AttrAllowedOps SettingsGtkXftDpiPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkXftDpiPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkXftDpiPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkXftDpiPropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkXftDpiPropertyInfo = Int32
    type AttrGetType SettingsGtkXftDpiPropertyInfo = Int32
    type AttrLabel SettingsGtkXftDpiPropertyInfo = "gtk-xft-dpi"
    type AttrOrigin SettingsGtkXftDpiPropertyInfo = Settings
    attrGet = getSettingsGtkXftDpi
    attrSet = setSettingsGtkXftDpi
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkXftDpi
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkXftDpi"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkXftDpi"
        })
#endif

-- VVV Prop "gtk-xft-hinting"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-xft-hinting@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkXftHinting
-- @
getSettingsGtkXftHinting :: (MonadIO m, IsSettings o) => o -> m Int32
getSettingsGtkXftHinting obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "gtk-xft-hinting"

-- | Set the value of the “@gtk-xft-hinting@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkXftHinting 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkXftHinting :: (MonadIO m, IsSettings o) => o -> Int32 -> m ()
setSettingsGtkXftHinting obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "gtk-xft-hinting" val

-- | Construct a `GValueConstruct` with valid value for the “@gtk-xft-hinting@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkXftHinting :: (IsSettings o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructSettingsGtkXftHinting val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "gtk-xft-hinting" val

#if defined(ENABLE_OVERLOADING)
data SettingsGtkXftHintingPropertyInfo
instance AttrInfo SettingsGtkXftHintingPropertyInfo where
    type AttrAllowedOps SettingsGtkXftHintingPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint SettingsGtkXftHintingPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkXftHintingPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint SettingsGtkXftHintingPropertyInfo = (~) Int32
    type AttrTransferType SettingsGtkXftHintingPropertyInfo = Int32
    type AttrGetType SettingsGtkXftHintingPropertyInfo = Int32
    type AttrLabel SettingsGtkXftHintingPropertyInfo = "gtk-xft-hinting"
    type AttrOrigin SettingsGtkXftHintingPropertyInfo = Settings
    attrGet = getSettingsGtkXftHinting
    attrSet = setSettingsGtkXftHinting
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkXftHinting
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkXftHinting"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkXftHinting"
        })
#endif

-- VVV Prop "gtk-xft-hintstyle"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-xft-hintstyle@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkXftHintstyle
-- @
getSettingsGtkXftHintstyle :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkXftHintstyle obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-xft-hintstyle"

-- | Set the value of the “@gtk-xft-hintstyle@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkXftHintstyle 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkXftHintstyle :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkXftHintstyle obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-xft-hintstyle" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-xft-hintstyle@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkXftHintstyle :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkXftHintstyle val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-xft-hintstyle" (P.Just val)

-- | Set the value of the “@gtk-xft-hintstyle@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkXftHintstyle
-- @
clearSettingsGtkXftHintstyle :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkXftHintstyle obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-xft-hintstyle" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkXftHintstylePropertyInfo
instance AttrInfo SettingsGtkXftHintstylePropertyInfo where
    type AttrAllowedOps SettingsGtkXftHintstylePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkXftHintstylePropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkXftHintstylePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkXftHintstylePropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkXftHintstylePropertyInfo = T.Text
    type AttrGetType SettingsGtkXftHintstylePropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkXftHintstylePropertyInfo = "gtk-xft-hintstyle"
    type AttrOrigin SettingsGtkXftHintstylePropertyInfo = Settings
    attrGet = getSettingsGtkXftHintstyle
    attrSet = setSettingsGtkXftHintstyle
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkXftHintstyle
    attrClear = clearSettingsGtkXftHintstyle
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkXftHintstyle"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkXftHintstyle"
        })
#endif

-- VVV Prop "gtk-xft-rgba"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@gtk-xft-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settings #gtkXftRgba
-- @
getSettingsGtkXftRgba :: (MonadIO m, IsSettings o) => o -> m (Maybe T.Text)
getSettingsGtkXftRgba obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "gtk-xft-rgba"

-- | Set the value of the “@gtk-xft-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settings [ #gtkXftRgba 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsGtkXftRgba :: (MonadIO m, IsSettings o) => o -> T.Text -> m ()
setSettingsGtkXftRgba obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "gtk-xft-rgba" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@gtk-xft-rgba@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructSettingsGtkXftRgba :: (IsSettings o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructSettingsGtkXftRgba val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "gtk-xft-rgba" (P.Just val)

-- | Set the value of the “@gtk-xft-rgba@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #gtkXftRgba
-- @
clearSettingsGtkXftRgba :: (MonadIO m, IsSettings o) => o -> m ()
clearSettingsGtkXftRgba obj = liftIO $ B.Properties.setObjectPropertyString obj "gtk-xft-rgba" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data SettingsGtkXftRgbaPropertyInfo
instance AttrInfo SettingsGtkXftRgbaPropertyInfo where
    type AttrAllowedOps SettingsGtkXftRgbaPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint SettingsGtkXftRgbaPropertyInfo = IsSettings
    type AttrSetTypeConstraint SettingsGtkXftRgbaPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint SettingsGtkXftRgbaPropertyInfo = (~) T.Text
    type AttrTransferType SettingsGtkXftRgbaPropertyInfo = T.Text
    type AttrGetType SettingsGtkXftRgbaPropertyInfo = (Maybe T.Text)
    type AttrLabel SettingsGtkXftRgbaPropertyInfo = "gtk-xft-rgba"
    type AttrOrigin SettingsGtkXftRgbaPropertyInfo = Settings
    attrGet = getSettingsGtkXftRgba
    attrSet = setSettingsGtkXftRgba
    attrTransfer _ v = do
        return v
    attrConstruct = constructSettingsGtkXftRgba
    attrClear = clearSettingsGtkXftRgba
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.gtkXftRgba"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#g:attr:gtkXftRgba"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Settings
type instance O.AttributeList Settings = SettingsAttributeList
type SettingsAttributeList = ('[ '("colorHash", SettingsColorHashPropertyInfo), '("gtkAlternativeButtonOrder", SettingsGtkAlternativeButtonOrderPropertyInfo), '("gtkAlternativeSortArrows", SettingsGtkAlternativeSortArrowsPropertyInfo), '("gtkApplicationPreferDarkTheme", SettingsGtkApplicationPreferDarkThemePropertyInfo), '("gtkAutoMnemonics", SettingsGtkAutoMnemonicsPropertyInfo), '("gtkButtonImages", SettingsGtkButtonImagesPropertyInfo), '("gtkCanChangeAccels", SettingsGtkCanChangeAccelsPropertyInfo), '("gtkColorPalette", SettingsGtkColorPalettePropertyInfo), '("gtkColorScheme", SettingsGtkColorSchemePropertyInfo), '("gtkCursorAspectRatio", SettingsGtkCursorAspectRatioPropertyInfo), '("gtkCursorBlink", SettingsGtkCursorBlinkPropertyInfo), '("gtkCursorBlinkTime", SettingsGtkCursorBlinkTimePropertyInfo), '("gtkCursorBlinkTimeout", SettingsGtkCursorBlinkTimeoutPropertyInfo), '("gtkCursorThemeName", SettingsGtkCursorThemeNamePropertyInfo), '("gtkCursorThemeSize", SettingsGtkCursorThemeSizePropertyInfo), '("gtkDecorationLayout", SettingsGtkDecorationLayoutPropertyInfo), '("gtkDialogsUseHeader", SettingsGtkDialogsUseHeaderPropertyInfo), '("gtkDndDragThreshold", SettingsGtkDndDragThresholdPropertyInfo), '("gtkDoubleClickDistance", SettingsGtkDoubleClickDistancePropertyInfo), '("gtkDoubleClickTime", SettingsGtkDoubleClickTimePropertyInfo), '("gtkEnableAccels", SettingsGtkEnableAccelsPropertyInfo), '("gtkEnableAnimations", SettingsGtkEnableAnimationsPropertyInfo), '("gtkEnableEventSounds", SettingsGtkEnableEventSoundsPropertyInfo), '("gtkEnableInputFeedbackSounds", SettingsGtkEnableInputFeedbackSoundsPropertyInfo), '("gtkEnableMnemonics", SettingsGtkEnableMnemonicsPropertyInfo), '("gtkEnablePrimaryPaste", SettingsGtkEnablePrimaryPastePropertyInfo), '("gtkEnableTooltips", SettingsGtkEnableTooltipsPropertyInfo), '("gtkEntryPasswordHintTimeout", SettingsGtkEntryPasswordHintTimeoutPropertyInfo), '("gtkEntrySelectOnFocus", SettingsGtkEntrySelectOnFocusPropertyInfo), '("gtkErrorBell", SettingsGtkErrorBellPropertyInfo), '("gtkFallbackIconTheme", SettingsGtkFallbackIconThemePropertyInfo), '("gtkFileChooserBackend", SettingsGtkFileChooserBackendPropertyInfo), '("gtkFontName", SettingsGtkFontNamePropertyInfo), '("gtkFontconfigTimestamp", SettingsGtkFontconfigTimestampPropertyInfo), '("gtkIconSizes", SettingsGtkIconSizesPropertyInfo), '("gtkIconThemeName", SettingsGtkIconThemeNamePropertyInfo), '("gtkImModule", SettingsGtkImModulePropertyInfo), '("gtkImPreeditStyle", SettingsGtkImPreeditStylePropertyInfo), '("gtkImStatusStyle", SettingsGtkImStatusStylePropertyInfo), '("gtkKeyThemeName", SettingsGtkKeyThemeNamePropertyInfo), '("gtkKeynavCursorOnly", SettingsGtkKeynavCursorOnlyPropertyInfo), '("gtkKeynavUseCaret", SettingsGtkKeynavUseCaretPropertyInfo), '("gtkKeynavWrapAround", SettingsGtkKeynavWrapAroundPropertyInfo), '("gtkLabelSelectOnFocus", SettingsGtkLabelSelectOnFocusPropertyInfo), '("gtkLongPressTime", SettingsGtkLongPressTimePropertyInfo), '("gtkMenuBarAccel", SettingsGtkMenuBarAccelPropertyInfo), '("gtkMenuBarPopupDelay", SettingsGtkMenuBarPopupDelayPropertyInfo), '("gtkMenuImages", SettingsGtkMenuImagesPropertyInfo), '("gtkMenuPopdownDelay", SettingsGtkMenuPopdownDelayPropertyInfo), '("gtkMenuPopupDelay", SettingsGtkMenuPopupDelayPropertyInfo), '("gtkModules", SettingsGtkModulesPropertyInfo), '("gtkOverlayScrolling", SettingsGtkOverlayScrollingPropertyInfo), '("gtkPrimaryButtonWarpsSlider", SettingsGtkPrimaryButtonWarpsSliderPropertyInfo), '("gtkPrintBackends", SettingsGtkPrintBackendsPropertyInfo), '("gtkPrintPreviewCommand", SettingsGtkPrintPreviewCommandPropertyInfo), '("gtkRecentFilesEnabled", SettingsGtkRecentFilesEnabledPropertyInfo), '("gtkRecentFilesLimit", SettingsGtkRecentFilesLimitPropertyInfo), '("gtkRecentFilesMaxAge", SettingsGtkRecentFilesMaxAgePropertyInfo), '("gtkScrolledWindowPlacement", SettingsGtkScrolledWindowPlacementPropertyInfo), '("gtkShellShowsAppMenu", SettingsGtkShellShowsAppMenuPropertyInfo), '("gtkShellShowsDesktop", SettingsGtkShellShowsDesktopPropertyInfo), '("gtkShellShowsMenubar", SettingsGtkShellShowsMenubarPropertyInfo), '("gtkShowInputMethodMenu", SettingsGtkShowInputMethodMenuPropertyInfo), '("gtkShowUnicodeMenu", SettingsGtkShowUnicodeMenuPropertyInfo), '("gtkSoundThemeName", SettingsGtkSoundThemeNamePropertyInfo), '("gtkSplitCursor", SettingsGtkSplitCursorPropertyInfo), '("gtkThemeName", SettingsGtkThemeNamePropertyInfo), '("gtkTimeoutExpand", SettingsGtkTimeoutExpandPropertyInfo), '("gtkTimeoutInitial", SettingsGtkTimeoutInitialPropertyInfo), '("gtkTimeoutRepeat", SettingsGtkTimeoutRepeatPropertyInfo), '("gtkTitlebarDoubleClick", SettingsGtkTitlebarDoubleClickPropertyInfo), '("gtkTitlebarMiddleClick", SettingsGtkTitlebarMiddleClickPropertyInfo), '("gtkTitlebarRightClick", SettingsGtkTitlebarRightClickPropertyInfo), '("gtkToolbarIconSize", SettingsGtkToolbarIconSizePropertyInfo), '("gtkToolbarStyle", SettingsGtkToolbarStylePropertyInfo), '("gtkTooltipBrowseModeTimeout", SettingsGtkTooltipBrowseModeTimeoutPropertyInfo), '("gtkTooltipBrowseTimeout", SettingsGtkTooltipBrowseTimeoutPropertyInfo), '("gtkTooltipTimeout", SettingsGtkTooltipTimeoutPropertyInfo), '("gtkTouchscreenMode", SettingsGtkTouchscreenModePropertyInfo), '("gtkVisibleFocus", SettingsGtkVisibleFocusPropertyInfo), '("gtkXftAntialias", SettingsGtkXftAntialiasPropertyInfo), '("gtkXftDpi", SettingsGtkXftDpiPropertyInfo), '("gtkXftHinting", SettingsGtkXftHintingPropertyInfo), '("gtkXftHintstyle", SettingsGtkXftHintstylePropertyInfo), '("gtkXftRgba", SettingsGtkXftRgbaPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
settingsColorHash :: AttrLabelProxy "colorHash"
settingsColorHash = AttrLabelProxy

settingsGtkAlternativeButtonOrder :: AttrLabelProxy "gtkAlternativeButtonOrder"
settingsGtkAlternativeButtonOrder = AttrLabelProxy

settingsGtkAlternativeSortArrows :: AttrLabelProxy "gtkAlternativeSortArrows"
settingsGtkAlternativeSortArrows = AttrLabelProxy

settingsGtkApplicationPreferDarkTheme :: AttrLabelProxy "gtkApplicationPreferDarkTheme"
settingsGtkApplicationPreferDarkTheme = AttrLabelProxy

settingsGtkAutoMnemonics :: AttrLabelProxy "gtkAutoMnemonics"
settingsGtkAutoMnemonics = AttrLabelProxy

settingsGtkButtonImages :: AttrLabelProxy "gtkButtonImages"
settingsGtkButtonImages = AttrLabelProxy

settingsGtkCanChangeAccels :: AttrLabelProxy "gtkCanChangeAccels"
settingsGtkCanChangeAccels = AttrLabelProxy

settingsGtkColorPalette :: AttrLabelProxy "gtkColorPalette"
settingsGtkColorPalette = AttrLabelProxy

settingsGtkColorScheme :: AttrLabelProxy "gtkColorScheme"
settingsGtkColorScheme = AttrLabelProxy

settingsGtkCursorAspectRatio :: AttrLabelProxy "gtkCursorAspectRatio"
settingsGtkCursorAspectRatio = AttrLabelProxy

settingsGtkCursorBlink :: AttrLabelProxy "gtkCursorBlink"
settingsGtkCursorBlink = AttrLabelProxy

settingsGtkCursorBlinkTime :: AttrLabelProxy "gtkCursorBlinkTime"
settingsGtkCursorBlinkTime = AttrLabelProxy

settingsGtkCursorBlinkTimeout :: AttrLabelProxy "gtkCursorBlinkTimeout"
settingsGtkCursorBlinkTimeout = AttrLabelProxy

settingsGtkCursorThemeName :: AttrLabelProxy "gtkCursorThemeName"
settingsGtkCursorThemeName = AttrLabelProxy

settingsGtkCursorThemeSize :: AttrLabelProxy "gtkCursorThemeSize"
settingsGtkCursorThemeSize = AttrLabelProxy

settingsGtkDecorationLayout :: AttrLabelProxy "gtkDecorationLayout"
settingsGtkDecorationLayout = AttrLabelProxy

settingsGtkDialogsUseHeader :: AttrLabelProxy "gtkDialogsUseHeader"
settingsGtkDialogsUseHeader = AttrLabelProxy

settingsGtkDndDragThreshold :: AttrLabelProxy "gtkDndDragThreshold"
settingsGtkDndDragThreshold = AttrLabelProxy

settingsGtkDoubleClickDistance :: AttrLabelProxy "gtkDoubleClickDistance"
settingsGtkDoubleClickDistance = AttrLabelProxy

settingsGtkDoubleClickTime :: AttrLabelProxy "gtkDoubleClickTime"
settingsGtkDoubleClickTime = AttrLabelProxy

settingsGtkEnableAccels :: AttrLabelProxy "gtkEnableAccels"
settingsGtkEnableAccels = AttrLabelProxy

settingsGtkEnableAnimations :: AttrLabelProxy "gtkEnableAnimations"
settingsGtkEnableAnimations = AttrLabelProxy

settingsGtkEnableEventSounds :: AttrLabelProxy "gtkEnableEventSounds"
settingsGtkEnableEventSounds = AttrLabelProxy

settingsGtkEnableInputFeedbackSounds :: AttrLabelProxy "gtkEnableInputFeedbackSounds"
settingsGtkEnableInputFeedbackSounds = AttrLabelProxy

settingsGtkEnableMnemonics :: AttrLabelProxy "gtkEnableMnemonics"
settingsGtkEnableMnemonics = AttrLabelProxy

settingsGtkEnablePrimaryPaste :: AttrLabelProxy "gtkEnablePrimaryPaste"
settingsGtkEnablePrimaryPaste = AttrLabelProxy

settingsGtkEnableTooltips :: AttrLabelProxy "gtkEnableTooltips"
settingsGtkEnableTooltips = AttrLabelProxy

settingsGtkEntryPasswordHintTimeout :: AttrLabelProxy "gtkEntryPasswordHintTimeout"
settingsGtkEntryPasswordHintTimeout = AttrLabelProxy

settingsGtkEntrySelectOnFocus :: AttrLabelProxy "gtkEntrySelectOnFocus"
settingsGtkEntrySelectOnFocus = AttrLabelProxy

settingsGtkErrorBell :: AttrLabelProxy "gtkErrorBell"
settingsGtkErrorBell = AttrLabelProxy

settingsGtkFallbackIconTheme :: AttrLabelProxy "gtkFallbackIconTheme"
settingsGtkFallbackIconTheme = AttrLabelProxy

settingsGtkFileChooserBackend :: AttrLabelProxy "gtkFileChooserBackend"
settingsGtkFileChooserBackend = AttrLabelProxy

settingsGtkFontName :: AttrLabelProxy "gtkFontName"
settingsGtkFontName = AttrLabelProxy

settingsGtkFontconfigTimestamp :: AttrLabelProxy "gtkFontconfigTimestamp"
settingsGtkFontconfigTimestamp = AttrLabelProxy

settingsGtkIconSizes :: AttrLabelProxy "gtkIconSizes"
settingsGtkIconSizes = AttrLabelProxy

settingsGtkIconThemeName :: AttrLabelProxy "gtkIconThemeName"
settingsGtkIconThemeName = AttrLabelProxy

settingsGtkImModule :: AttrLabelProxy "gtkImModule"
settingsGtkImModule = AttrLabelProxy

settingsGtkImPreeditStyle :: AttrLabelProxy "gtkImPreeditStyle"
settingsGtkImPreeditStyle = AttrLabelProxy

settingsGtkImStatusStyle :: AttrLabelProxy "gtkImStatusStyle"
settingsGtkImStatusStyle = AttrLabelProxy

settingsGtkKeyThemeName :: AttrLabelProxy "gtkKeyThemeName"
settingsGtkKeyThemeName = AttrLabelProxy

settingsGtkKeynavCursorOnly :: AttrLabelProxy "gtkKeynavCursorOnly"
settingsGtkKeynavCursorOnly = AttrLabelProxy

settingsGtkKeynavUseCaret :: AttrLabelProxy "gtkKeynavUseCaret"
settingsGtkKeynavUseCaret = AttrLabelProxy

settingsGtkKeynavWrapAround :: AttrLabelProxy "gtkKeynavWrapAround"
settingsGtkKeynavWrapAround = AttrLabelProxy

settingsGtkLabelSelectOnFocus :: AttrLabelProxy "gtkLabelSelectOnFocus"
settingsGtkLabelSelectOnFocus = AttrLabelProxy

settingsGtkLongPressTime :: AttrLabelProxy "gtkLongPressTime"
settingsGtkLongPressTime = AttrLabelProxy

settingsGtkMenuBarAccel :: AttrLabelProxy "gtkMenuBarAccel"
settingsGtkMenuBarAccel = AttrLabelProxy

settingsGtkMenuBarPopupDelay :: AttrLabelProxy "gtkMenuBarPopupDelay"
settingsGtkMenuBarPopupDelay = AttrLabelProxy

settingsGtkMenuImages :: AttrLabelProxy "gtkMenuImages"
settingsGtkMenuImages = AttrLabelProxy

settingsGtkMenuPopdownDelay :: AttrLabelProxy "gtkMenuPopdownDelay"
settingsGtkMenuPopdownDelay = AttrLabelProxy

settingsGtkMenuPopupDelay :: AttrLabelProxy "gtkMenuPopupDelay"
settingsGtkMenuPopupDelay = AttrLabelProxy

settingsGtkModules :: AttrLabelProxy "gtkModules"
settingsGtkModules = AttrLabelProxy

settingsGtkOverlayScrolling :: AttrLabelProxy "gtkOverlayScrolling"
settingsGtkOverlayScrolling = AttrLabelProxy

settingsGtkPrimaryButtonWarpsSlider :: AttrLabelProxy "gtkPrimaryButtonWarpsSlider"
settingsGtkPrimaryButtonWarpsSlider = AttrLabelProxy

settingsGtkPrintBackends :: AttrLabelProxy "gtkPrintBackends"
settingsGtkPrintBackends = AttrLabelProxy

settingsGtkPrintPreviewCommand :: AttrLabelProxy "gtkPrintPreviewCommand"
settingsGtkPrintPreviewCommand = AttrLabelProxy

settingsGtkRecentFilesEnabled :: AttrLabelProxy "gtkRecentFilesEnabled"
settingsGtkRecentFilesEnabled = AttrLabelProxy

settingsGtkRecentFilesLimit :: AttrLabelProxy "gtkRecentFilesLimit"
settingsGtkRecentFilesLimit = AttrLabelProxy

settingsGtkRecentFilesMaxAge :: AttrLabelProxy "gtkRecentFilesMaxAge"
settingsGtkRecentFilesMaxAge = AttrLabelProxy

settingsGtkScrolledWindowPlacement :: AttrLabelProxy "gtkScrolledWindowPlacement"
settingsGtkScrolledWindowPlacement = AttrLabelProxy

settingsGtkShellShowsAppMenu :: AttrLabelProxy "gtkShellShowsAppMenu"
settingsGtkShellShowsAppMenu = AttrLabelProxy

settingsGtkShellShowsDesktop :: AttrLabelProxy "gtkShellShowsDesktop"
settingsGtkShellShowsDesktop = AttrLabelProxy

settingsGtkShellShowsMenubar :: AttrLabelProxy "gtkShellShowsMenubar"
settingsGtkShellShowsMenubar = AttrLabelProxy

settingsGtkShowInputMethodMenu :: AttrLabelProxy "gtkShowInputMethodMenu"
settingsGtkShowInputMethodMenu = AttrLabelProxy

settingsGtkShowUnicodeMenu :: AttrLabelProxy "gtkShowUnicodeMenu"
settingsGtkShowUnicodeMenu = AttrLabelProxy

settingsGtkSoundThemeName :: AttrLabelProxy "gtkSoundThemeName"
settingsGtkSoundThemeName = AttrLabelProxy

settingsGtkSplitCursor :: AttrLabelProxy "gtkSplitCursor"
settingsGtkSplitCursor = AttrLabelProxy

settingsGtkThemeName :: AttrLabelProxy "gtkThemeName"
settingsGtkThemeName = AttrLabelProxy

settingsGtkTimeoutExpand :: AttrLabelProxy "gtkTimeoutExpand"
settingsGtkTimeoutExpand = AttrLabelProxy

settingsGtkTimeoutInitial :: AttrLabelProxy "gtkTimeoutInitial"
settingsGtkTimeoutInitial = AttrLabelProxy

settingsGtkTimeoutRepeat :: AttrLabelProxy "gtkTimeoutRepeat"
settingsGtkTimeoutRepeat = AttrLabelProxy

settingsGtkTitlebarDoubleClick :: AttrLabelProxy "gtkTitlebarDoubleClick"
settingsGtkTitlebarDoubleClick = AttrLabelProxy

settingsGtkTitlebarMiddleClick :: AttrLabelProxy "gtkTitlebarMiddleClick"
settingsGtkTitlebarMiddleClick = AttrLabelProxy

settingsGtkTitlebarRightClick :: AttrLabelProxy "gtkTitlebarRightClick"
settingsGtkTitlebarRightClick = AttrLabelProxy

settingsGtkToolbarIconSize :: AttrLabelProxy "gtkToolbarIconSize"
settingsGtkToolbarIconSize = AttrLabelProxy

settingsGtkToolbarStyle :: AttrLabelProxy "gtkToolbarStyle"
settingsGtkToolbarStyle = AttrLabelProxy

settingsGtkTooltipBrowseModeTimeout :: AttrLabelProxy "gtkTooltipBrowseModeTimeout"
settingsGtkTooltipBrowseModeTimeout = AttrLabelProxy

settingsGtkTooltipBrowseTimeout :: AttrLabelProxy "gtkTooltipBrowseTimeout"
settingsGtkTooltipBrowseTimeout = AttrLabelProxy

settingsGtkTooltipTimeout :: AttrLabelProxy "gtkTooltipTimeout"
settingsGtkTooltipTimeout = AttrLabelProxy

settingsGtkTouchscreenMode :: AttrLabelProxy "gtkTouchscreenMode"
settingsGtkTouchscreenMode = AttrLabelProxy

settingsGtkVisibleFocus :: AttrLabelProxy "gtkVisibleFocus"
settingsGtkVisibleFocus = AttrLabelProxy

settingsGtkXftAntialias :: AttrLabelProxy "gtkXftAntialias"
settingsGtkXftAntialias = AttrLabelProxy

settingsGtkXftDpi :: AttrLabelProxy "gtkXftDpi"
settingsGtkXftDpi = AttrLabelProxy

settingsGtkXftHinting :: AttrLabelProxy "gtkXftHinting"
settingsGtkXftHinting = AttrLabelProxy

settingsGtkXftHintstyle :: AttrLabelProxy "gtkXftHintstyle"
settingsGtkXftHintstyle = AttrLabelProxy

settingsGtkXftRgba :: AttrLabelProxy "gtkXftRgba"
settingsGtkXftRgba = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Settings = SettingsSignalList
type SettingsSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method Settings::reset_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Settings" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkSettings object"
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
--                 { rawDocText = Just "the name of the setting to reset"
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

foreign import ccall "gtk_settings_reset_property" gtk_settings_reset_property :: 
    Ptr Settings ->                         -- settings : TInterface (Name {namespace = "Gtk", name = "Settings"})
    CString ->                              -- name : TBasicType TUTF8
    IO ()

-- | Undoes the effect of calling @/g_object_set()/@ to install an
-- application-specific value for a setting. After this call,
-- the setting will again follow the session-wide value for
-- this setting.
-- 
-- /Since: 3.20/
settingsResetProperty ::
    (B.CallStack.HasCallStack, MonadIO m, IsSettings a) =>
    a
    -- ^ /@settings@/: a t'GI.Gtk.Objects.Settings.Settings' object
    -> T.Text
    -- ^ /@name@/: the name of the setting to reset
    -> m ()
settingsResetProperty settings name = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    name' <- textToCString name
    gtk_settings_reset_property settings' name'
    touchManagedPtr settings
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data SettingsResetPropertyMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsSettings a) => O.OverloadedMethod SettingsResetPropertyMethodInfo a signature where
    overloadedMethod = settingsResetProperty

instance O.OverloadedMethodInfo SettingsResetPropertyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.settingsResetProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#v:settingsResetProperty"
        })


#endif

-- method Settings::set_double_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Settings" }
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
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
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
--       , Arg
--           { argCName = "v_double"
--           , argType = TBasicType TDouble
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
--       , Arg
--           { argCName = "origin"
--           , argType = TBasicType TUTF8
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

foreign import ccall "gtk_settings_set_double_property" gtk_settings_set_double_property :: 
    Ptr Settings ->                         -- settings : TInterface (Name {namespace = "Gtk", name = "Settings"})
    CString ->                              -- name : TBasicType TUTF8
    CDouble ->                              -- v_double : TBasicType TDouble
    CString ->                              -- origin : TBasicType TUTF8
    IO ()

{-# DEPRECATED settingsSetDoubleProperty ["(Since version 3.16)","Use @/g_object_set()/@ instead."] #-}
-- | /No description available in the introspection data./
settingsSetDoubleProperty ::
    (B.CallStack.HasCallStack, MonadIO m, IsSettings a) =>
    a
    -> T.Text
    -> Double
    -> T.Text
    -> m ()
settingsSetDoubleProperty settings name vDouble origin = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    name' <- textToCString name
    let vDouble' = realToFrac vDouble
    origin' <- textToCString origin
    gtk_settings_set_double_property settings' name' vDouble' origin'
    touchManagedPtr settings
    freeMem name'
    freeMem origin'
    return ()

#if defined(ENABLE_OVERLOADING)
data SettingsSetDoublePropertyMethodInfo
instance (signature ~ (T.Text -> Double -> T.Text -> m ()), MonadIO m, IsSettings a) => O.OverloadedMethod SettingsSetDoublePropertyMethodInfo a signature where
    overloadedMethod = settingsSetDoubleProperty

instance O.OverloadedMethodInfo SettingsSetDoublePropertyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.settingsSetDoubleProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#v:settingsSetDoubleProperty"
        })


#endif

-- method Settings::set_long_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Settings" }
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
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
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
--       , Arg
--           { argCName = "v_long"
--           , argType = TBasicType TLong
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
--       , Arg
--           { argCName = "origin"
--           , argType = TBasicType TUTF8
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

foreign import ccall "gtk_settings_set_long_property" gtk_settings_set_long_property :: 
    Ptr Settings ->                         -- settings : TInterface (Name {namespace = "Gtk", name = "Settings"})
    CString ->                              -- name : TBasicType TUTF8
    CLong ->                                -- v_long : TBasicType TLong
    CString ->                              -- origin : TBasicType TUTF8
    IO ()

{-# DEPRECATED settingsSetLongProperty ["(Since version 3.16)","Use @/g_object_set()/@ instead."] #-}
-- | /No description available in the introspection data./
settingsSetLongProperty ::
    (B.CallStack.HasCallStack, MonadIO m, IsSettings a) =>
    a
    -> T.Text
    -> CLong
    -> T.Text
    -> m ()
settingsSetLongProperty settings name vLong origin = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    name' <- textToCString name
    origin' <- textToCString origin
    gtk_settings_set_long_property settings' name' vLong origin'
    touchManagedPtr settings
    freeMem name'
    freeMem origin'
    return ()

#if defined(ENABLE_OVERLOADING)
data SettingsSetLongPropertyMethodInfo
instance (signature ~ (T.Text -> CLong -> T.Text -> m ()), MonadIO m, IsSettings a) => O.OverloadedMethod SettingsSetLongPropertyMethodInfo a signature where
    overloadedMethod = settingsSetLongProperty

instance O.OverloadedMethodInfo SettingsSetLongPropertyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.settingsSetLongProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#v:settingsSetLongProperty"
        })


#endif

-- method Settings::set_property_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Settings" }
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
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
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
--       , Arg
--           { argCName = "svalue"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SettingsValue" }
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

foreign import ccall "gtk_settings_set_property_value" gtk_settings_set_property_value :: 
    Ptr Settings ->                         -- settings : TInterface (Name {namespace = "Gtk", name = "Settings"})
    CString ->                              -- name : TBasicType TUTF8
    Ptr Gtk.SettingsValue.SettingsValue ->  -- svalue : TInterface (Name {namespace = "Gtk", name = "SettingsValue"})
    IO ()

{-# DEPRECATED settingsSetPropertyValue ["(Since version 3.16)","Use @/g_object_set()/@ instead."] #-}
-- | /No description available in the introspection data./
settingsSetPropertyValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsSettings a) =>
    a
    -> T.Text
    -> Gtk.SettingsValue.SettingsValue
    -> m ()
settingsSetPropertyValue settings name svalue = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    name' <- textToCString name
    svalue' <- unsafeManagedPtrGetPtr svalue
    gtk_settings_set_property_value settings' name' svalue'
    touchManagedPtr settings
    touchManagedPtr svalue
    freeMem name'
    return ()

#if defined(ENABLE_OVERLOADING)
data SettingsSetPropertyValueMethodInfo
instance (signature ~ (T.Text -> Gtk.SettingsValue.SettingsValue -> m ()), MonadIO m, IsSettings a) => O.OverloadedMethod SettingsSetPropertyValueMethodInfo a signature where
    overloadedMethod = settingsSetPropertyValue

instance O.OverloadedMethodInfo SettingsSetPropertyValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.settingsSetPropertyValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#v:settingsSetPropertyValue"
        })


#endif

-- method Settings::set_string_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "settings"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Settings" }
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
--       , Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
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
--       , Arg
--           { argCName = "v_string"
--           , argType = TBasicType TUTF8
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
--       , Arg
--           { argCName = "origin"
--           , argType = TBasicType TUTF8
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

foreign import ccall "gtk_settings_set_string_property" gtk_settings_set_string_property :: 
    Ptr Settings ->                         -- settings : TInterface (Name {namespace = "Gtk", name = "Settings"})
    CString ->                              -- name : TBasicType TUTF8
    CString ->                              -- v_string : TBasicType TUTF8
    CString ->                              -- origin : TBasicType TUTF8
    IO ()

{-# DEPRECATED settingsSetStringProperty ["(Since version 3.16)","Use @/g_object_set()/@ instead."] #-}
-- | /No description available in the introspection data./
settingsSetStringProperty ::
    (B.CallStack.HasCallStack, MonadIO m, IsSettings a) =>
    a
    -> T.Text
    -> T.Text
    -> T.Text
    -> m ()
settingsSetStringProperty settings name vString origin = liftIO $ do
    settings' <- unsafeManagedPtrCastPtr settings
    name' <- textToCString name
    vString' <- textToCString vString
    origin' <- textToCString origin
    gtk_settings_set_string_property settings' name' vString' origin'
    touchManagedPtr settings
    freeMem name'
    freeMem vString'
    freeMem origin'
    return ()

#if defined(ENABLE_OVERLOADING)
data SettingsSetStringPropertyMethodInfo
instance (signature ~ (T.Text -> T.Text -> T.Text -> m ()), MonadIO m, IsSettings a) => O.OverloadedMethod SettingsSetStringPropertyMethodInfo a signature where
    overloadedMethod = settingsSetStringProperty

instance O.OverloadedMethodInfo SettingsSetStringPropertyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Settings.settingsSetStringProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Settings.html#v:settingsSetStringProperty"
        })


#endif

-- method Settings::get_default
-- method type : MemberFunction
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Settings" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_settings_get_default" gtk_settings_get_default :: 
    IO (Ptr Settings)

-- | Gets the t'GI.Gtk.Objects.Settings.Settings' object for the default GDK screen, creating
-- it if necessary. See 'GI.Gtk.Objects.Settings.settingsGetForScreen'.
settingsGetDefault ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m (Maybe Settings)
    -- ^ __Returns:__ a t'GI.Gtk.Objects.Settings.Settings' object. If there is
    -- no default screen, then returns 'P.Nothing'.
settingsGetDefault  = liftIO $ do
    result <- gtk_settings_get_default
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Settings) result'
        return result''
    return maybeResult

#if defined(ENABLE_OVERLOADING)
#endif

-- method Settings::get_for_screen
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "screen"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Screen" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkScreen." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Settings" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_settings_get_for_screen" gtk_settings_get_for_screen :: 
    Ptr Gdk.Screen.Screen ->                -- screen : TInterface (Name {namespace = "Gdk", name = "Screen"})
    IO (Ptr Settings)

-- | Gets the t'GI.Gtk.Objects.Settings.Settings' object for /@screen@/, creating it if necessary.
-- 
-- /Since: 2.2/
settingsGetForScreen ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.Screen.IsScreen a) =>
    a
    -- ^ /@screen@/: a t'GI.Gdk.Objects.Screen.Screen'.
    -> m Settings
    -- ^ __Returns:__ a t'GI.Gtk.Objects.Settings.Settings' object.
settingsGetForScreen screen = liftIO $ do
    screen' <- unsafeManagedPtrCastPtr screen
    result <- gtk_settings_get_for_screen screen'
    checkUnexpectedReturnNULL "settingsGetForScreen" result
    result' <- (newObject Settings) result
    touchManagedPtr screen
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Settings::install_property
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "pspec"
--           , argType = TParamSpec
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

foreign import ccall "gtk_settings_install_property" gtk_settings_install_property :: 
    Ptr GParamSpec ->                       -- pspec : TParamSpec
    IO ()

{-# DEPRECATED settingsInstallProperty ["(Since version 3.16)","This function is not useful outside GTK+."] #-}
-- | /No description available in the introspection data./
settingsInstallProperty ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    GParamSpec
    -> m ()
settingsInstallProperty pspec = liftIO $ do
    pspec' <- unsafeManagedPtrGetPtr pspec
    gtk_settings_install_property pspec'
    touchManagedPtr pspec
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method Settings::install_property_parser
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "pspec"
--           , argType = TParamSpec
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
--       , Arg
--           { argCName = "parser"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RcPropertyParser" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeCall
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

foreign import ccall "gtk_settings_install_property_parser" gtk_settings_install_property_parser :: 
    Ptr GParamSpec ->                       -- pspec : TParamSpec
    FunPtr Gtk.Callbacks.C_RcPropertyParser -> -- parser : TInterface (Name {namespace = "Gtk", name = "RcPropertyParser"})
    IO ()

{-# DEPRECATED settingsInstallPropertyParser ["(Since version 3.16)","This function is not useful outside GTK+."] #-}
-- | /No description available in the introspection data./
settingsInstallPropertyParser ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    GParamSpec
    -> Gtk.Callbacks.RcPropertyParser
    -> m ()
settingsInstallPropertyParser pspec parser = liftIO $ do
    pspec' <- unsafeManagedPtrGetPtr pspec
    parser' <- Gtk.Callbacks.mk_RcPropertyParser (Gtk.Callbacks.wrap_RcPropertyParser Nothing parser)
    gtk_settings_install_property_parser pspec' parser'
    safeFreeFunPtr $ castFunPtrToPtr parser'
    touchManagedPtr pspec
    return ()

#if defined(ENABLE_OVERLOADING)
#endif


