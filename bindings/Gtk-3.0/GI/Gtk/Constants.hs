{-# LANGUAGE PatternSynonyms, ScopedTypeVariables, ViewPatterns #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Constants
    ( 
    pattern TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID,
    pattern TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID,
    pattern TEXT_VIEW_PRIORITY_VALIDATE     ,
    pattern STYLE_REGION_TAB                ,
    pattern STYLE_REGION_ROW                ,
    pattern STYLE_REGION_COLUMN_HEADER      ,
    pattern STYLE_REGION_COLUMN             ,
    pattern STYLE_PROVIDER_PRIORITY_USER    ,
    pattern STYLE_PROVIDER_PRIORITY_THEME   ,
    pattern STYLE_PROVIDER_PRIORITY_SETTINGS,
    pattern STYLE_PROVIDER_PRIORITY_FALLBACK,
    pattern STYLE_PROVIDER_PRIORITY_APPLICATION,
    pattern STYLE_PROPERTY_PADDING          ,
    pattern STYLE_PROPERTY_MARGIN           ,
    pattern STYLE_PROPERTY_FONT             ,
    pattern STYLE_PROPERTY_COLOR            ,
    pattern STYLE_PROPERTY_BORDER_WIDTH     ,
    pattern STYLE_PROPERTY_BORDER_STYLE     ,
    pattern STYLE_PROPERTY_BORDER_RADIUS    ,
    pattern STYLE_PROPERTY_BORDER_COLOR     ,
    pattern STYLE_PROPERTY_BACKGROUND_IMAGE ,
    pattern STYLE_PROPERTY_BACKGROUND_COLOR ,
    pattern STYLE_CLASS_WIDE                ,
    pattern STYLE_CLASS_WARNING             ,
    pattern STYLE_CLASS_VIEW                ,
    pattern STYLE_CLASS_VERTICAL            ,
    pattern STYLE_CLASS_UNDERSHOOT          ,
    pattern STYLE_CLASS_TROUGH              ,
    pattern STYLE_CLASS_TOUCH_SELECTION     ,
    pattern STYLE_CLASS_TOP                 ,
    pattern STYLE_CLASS_TOOLTIP             ,
    pattern STYLE_CLASS_TOOLBAR             ,
    pattern STYLE_CLASS_TITLEBAR            ,
    pattern STYLE_CLASS_TITLE               ,
    pattern STYLE_CLASS_SUGGESTED_ACTION    ,
    pattern STYLE_CLASS_SUBTITLE            ,
    pattern STYLE_CLASS_STATUSBAR           ,
    pattern STYLE_CLASS_SPINNER             ,
    pattern STYLE_CLASS_SPINBUTTON          ,
    pattern STYLE_CLASS_SLIDER              ,
    pattern STYLE_CLASS_SIDEBAR             ,
    pattern STYLE_CLASS_SEPARATOR           ,
    pattern STYLE_CLASS_SCROLLBARS_JUNCTION ,
    pattern STYLE_CLASS_SCROLLBAR           ,
    pattern STYLE_CLASS_SCALE_HAS_MARKS_BELOW,
    pattern STYLE_CLASS_SCALE_HAS_MARKS_ABOVE,
    pattern STYLE_CLASS_SCALE               ,
    pattern STYLE_CLASS_RUBBERBAND          ,
    pattern STYLE_CLASS_RIGHT               ,
    pattern STYLE_CLASS_READ_ONLY           ,
    pattern STYLE_CLASS_RAISED              ,
    pattern STYLE_CLASS_RADIO               ,
    pattern STYLE_CLASS_QUESTION            ,
    pattern STYLE_CLASS_PULSE               ,
    pattern STYLE_CLASS_PROGRESSBAR         ,
    pattern STYLE_CLASS_PRIMARY_TOOLBAR     ,
    pattern STYLE_CLASS_POPUP               ,
    pattern STYLE_CLASS_POPOVER             ,
    pattern STYLE_CLASS_PAPER               ,
    pattern STYLE_CLASS_PANE_SEPARATOR      ,
    pattern STYLE_CLASS_OVERSHOOT           ,
    pattern STYLE_CLASS_OSD                 ,
    pattern STYLE_CLASS_NOTEBOOK            ,
    pattern STYLE_CLASS_NEEDS_ATTENTION     ,
    pattern STYLE_CLASS_MONOSPACE           ,
    pattern STYLE_CLASS_MESSAGE_DIALOG      ,
    pattern STYLE_CLASS_MENUITEM            ,
    pattern STYLE_CLASS_MENUBAR             ,
    pattern STYLE_CLASS_MENU                ,
    pattern STYLE_CLASS_MARK                ,
    pattern STYLE_CLASS_LIST_ROW            ,
    pattern STYLE_CLASS_LIST                ,
    pattern STYLE_CLASS_LINKED              ,
    pattern STYLE_CLASS_LEVEL_BAR           ,
    pattern STYLE_CLASS_LEFT                ,
    pattern STYLE_CLASS_LABEL               ,
    pattern STYLE_CLASS_INSERTION_CURSOR    ,
    pattern STYLE_CLASS_INLINE_TOOLBAR      ,
    pattern STYLE_CLASS_INFO                ,
    pattern STYLE_CLASS_IMAGE               ,
    pattern STYLE_CLASS_HORIZONTAL          ,
    pattern STYLE_CLASS_HIGHLIGHT           ,
    pattern STYLE_CLASS_HEADER              ,
    pattern STYLE_CLASS_GRIP                ,
    pattern STYLE_CLASS_FRAME               ,
    pattern STYLE_CLASS_FLAT                ,
    pattern STYLE_CLASS_EXPANDER            ,
    pattern STYLE_CLASS_ERROR               ,
    pattern STYLE_CLASS_ENTRY               ,
    pattern STYLE_CLASS_DOCK                ,
    pattern STYLE_CLASS_DND                 ,
    pattern STYLE_CLASS_DIM_LABEL           ,
    pattern STYLE_CLASS_DESTRUCTIVE_ACTION  ,
    pattern STYLE_CLASS_DEFAULT             ,
    pattern STYLE_CLASS_CURSOR_HANDLE       ,
    pattern STYLE_CLASS_CSD                 ,
    pattern STYLE_CLASS_CONTEXT_MENU        ,
    pattern STYLE_CLASS_COMBOBOX_ENTRY      ,
    pattern STYLE_CLASS_CHECK               ,
    pattern STYLE_CLASS_CELL                ,
    pattern STYLE_CLASS_CALENDAR            ,
    pattern STYLE_CLASS_BUTTON              ,
    pattern STYLE_CLASS_BOTTOM              ,
    pattern STYLE_CLASS_BACKGROUND          ,
    pattern STYLE_CLASS_ARROW               ,
    pattern STYLE_CLASS_ACCELERATOR         ,
    pattern STOCK_ZOOM_OUT                  ,
    pattern STOCK_ZOOM_IN                   ,
    pattern STOCK_ZOOM_FIT                  ,
    pattern STOCK_ZOOM_100                  ,
    pattern STOCK_YES                       ,
    pattern STOCK_UNINDENT                  ,
    pattern STOCK_UNDO                      ,
    pattern STOCK_UNDERLINE                 ,
    pattern STOCK_UNDELETE                  ,
    pattern STOCK_STRIKETHROUGH             ,
    pattern STOCK_STOP                      ,
    pattern STOCK_SPELL_CHECK               ,
    pattern STOCK_SORT_DESCENDING           ,
    pattern STOCK_SORT_ASCENDING            ,
    pattern STOCK_SELECT_FONT               ,
    pattern STOCK_SELECT_COLOR              ,
    pattern STOCK_SELECT_ALL                ,
    pattern STOCK_SAVE_AS                   ,
    pattern STOCK_SAVE                      ,
    pattern STOCK_REVERT_TO_SAVED           ,
    pattern STOCK_REMOVE                    ,
    pattern STOCK_REFRESH                   ,
    pattern STOCK_REDO                      ,
    pattern STOCK_QUIT                      ,
    pattern STOCK_PROPERTIES                ,
    pattern STOCK_PRINT_WARNING             ,
    pattern STOCK_PRINT_REPORT              ,
    pattern STOCK_PRINT_PREVIEW             ,
    pattern STOCK_PRINT_PAUSED              ,
    pattern STOCK_PRINT_ERROR               ,
    pattern STOCK_PRINT                     ,
    pattern STOCK_PREFERENCES               ,
    pattern STOCK_PASTE                     ,
    pattern STOCK_PAGE_SETUP                ,
    pattern STOCK_ORIENTATION_REVERSE_PORTRAIT,
    pattern STOCK_ORIENTATION_REVERSE_LANDSCAPE,
    pattern STOCK_ORIENTATION_PORTRAIT      ,
    pattern STOCK_ORIENTATION_LANDSCAPE     ,
    pattern STOCK_OPEN                      ,
    pattern STOCK_OK                        ,
    pattern STOCK_NO                        ,
    pattern STOCK_NEW                       ,
    pattern STOCK_NETWORK                   ,
    pattern STOCK_MISSING_IMAGE             ,
    pattern STOCK_MEDIA_STOP                ,
    pattern STOCK_MEDIA_REWIND              ,
    pattern STOCK_MEDIA_RECORD              ,
    pattern STOCK_MEDIA_PREVIOUS            ,
    pattern STOCK_MEDIA_PLAY                ,
    pattern STOCK_MEDIA_PAUSE               ,
    pattern STOCK_MEDIA_NEXT                ,
    pattern STOCK_MEDIA_FORWARD             ,
    pattern STOCK_LEAVE_FULLSCREEN          ,
    pattern STOCK_JUSTIFY_RIGHT             ,
    pattern STOCK_JUSTIFY_LEFT              ,
    pattern STOCK_JUSTIFY_FILL              ,
    pattern STOCK_JUSTIFY_CENTER            ,
    pattern STOCK_JUMP_TO                   ,
    pattern STOCK_ITALIC                    ,
    pattern STOCK_INFO                      ,
    pattern STOCK_INDEX                     ,
    pattern STOCK_INDENT                    ,
    pattern STOCK_HOME                      ,
    pattern STOCK_HELP                      ,
    pattern STOCK_HARDDISK                  ,
    pattern STOCK_GO_UP                     ,
    pattern STOCK_GO_FORWARD                ,
    pattern STOCK_GO_DOWN                   ,
    pattern STOCK_GO_BACK                   ,
    pattern STOCK_GOTO_TOP                  ,
    pattern STOCK_GOTO_LAST                 ,
    pattern STOCK_GOTO_FIRST                ,
    pattern STOCK_GOTO_BOTTOM               ,
    pattern STOCK_FULLSCREEN                ,
    pattern STOCK_FLOPPY                    ,
    pattern STOCK_FIND_AND_REPLACE          ,
    pattern STOCK_FIND                      ,
    pattern STOCK_FILE                      ,
    pattern STOCK_EXECUTE                   ,
    pattern STOCK_EDIT                      ,
    pattern STOCK_DND_MULTIPLE              ,
    pattern STOCK_DND                       ,
    pattern STOCK_DISCONNECT                ,
    pattern STOCK_DISCARD                   ,
    pattern STOCK_DIRECTORY                 ,
    pattern STOCK_DIALOG_WARNING            ,
    pattern STOCK_DIALOG_QUESTION           ,
    pattern STOCK_DIALOG_INFO               ,
    pattern STOCK_DIALOG_ERROR              ,
    pattern STOCK_DIALOG_AUTHENTICATION     ,
    pattern STOCK_DELETE                    ,
    pattern STOCK_CUT                       ,
    pattern STOCK_COPY                      ,
    pattern STOCK_CONVERT                   ,
    pattern STOCK_CONNECT                   ,
    pattern STOCK_COLOR_PICKER              ,
    pattern STOCK_CLOSE                     ,
    pattern STOCK_CLEAR                     ,
    pattern STOCK_CDROM                     ,
    pattern STOCK_CAPS_LOCK_WARNING         ,
    pattern STOCK_CANCEL                    ,
    pattern STOCK_BOLD                      ,
    pattern STOCK_APPLY                     ,
    pattern STOCK_ADD                       ,
    pattern STOCK_ABOUT                     ,
    pattern PRIORITY_RESIZE                 ,
    pattern PRINT_SETTINGS_WIN32_DRIVER_VERSION,
    pattern PRINT_SETTINGS_WIN32_DRIVER_EXTRA,
    pattern PRINT_SETTINGS_USE_COLOR        ,
    pattern PRINT_SETTINGS_SCALE            ,
    pattern PRINT_SETTINGS_REVERSE          ,
    pattern PRINT_SETTINGS_RESOLUTION_Y     ,
    pattern PRINT_SETTINGS_RESOLUTION_X     ,
    pattern PRINT_SETTINGS_RESOLUTION       ,
    pattern PRINT_SETTINGS_QUALITY          ,
    pattern PRINT_SETTINGS_PRINT_PAGES      ,
    pattern PRINT_SETTINGS_PRINTER_LPI      ,
    pattern PRINT_SETTINGS_PRINTER          ,
    pattern PRINT_SETTINGS_PAPER_WIDTH      ,
    pattern PRINT_SETTINGS_PAPER_HEIGHT     ,
    pattern PRINT_SETTINGS_PAPER_FORMAT     ,
    pattern PRINT_SETTINGS_PAGE_SET         ,
    pattern PRINT_SETTINGS_PAGE_RANGES      ,
    pattern PRINT_SETTINGS_OUTPUT_URI       ,
    pattern PRINT_SETTINGS_OUTPUT_FILE_FORMAT,
    pattern PRINT_SETTINGS_OUTPUT_DIR       ,
    pattern PRINT_SETTINGS_OUTPUT_BIN       ,
    pattern PRINT_SETTINGS_OUTPUT_BASENAME  ,
    pattern PRINT_SETTINGS_ORIENTATION      ,
    pattern PRINT_SETTINGS_N_COPIES         ,
    pattern PRINT_SETTINGS_NUMBER_UP_LAYOUT ,
    pattern PRINT_SETTINGS_NUMBER_UP        ,
    pattern PRINT_SETTINGS_MEDIA_TYPE       ,
    pattern PRINT_SETTINGS_FINISHINGS       ,
    pattern PRINT_SETTINGS_DUPLEX           ,
    pattern PRINT_SETTINGS_DITHER           ,
    pattern PRINT_SETTINGS_DEFAULT_SOURCE   ,
    pattern PRINT_SETTINGS_COLLATE          ,
    pattern PATH_PRIO_MASK                  ,
    pattern PAPER_NAME_LETTER               ,
    pattern PAPER_NAME_LEGAL                ,
    pattern PAPER_NAME_EXECUTIVE            ,
    pattern PAPER_NAME_B5                   ,
    pattern PAPER_NAME_A5                   ,
    pattern PAPER_NAME_A4                   ,
    pattern PAPER_NAME_A3                   ,
    pattern MINOR_VERSION                   ,
    pattern MICRO_VERSION                   ,
    pattern MAX_COMPOSE_LEN                 ,
    pattern MAJOR_VERSION                   ,
    pattern LEVEL_BAR_OFFSET_LOW            ,
    pattern LEVEL_BAR_OFFSET_HIGH           ,
    pattern LEVEL_BAR_OFFSET_FULL           ,
    pattern INTERFACE_AGE                   ,
    pattern INPUT_ERROR                     ,
    pattern BINARY_AGE                      ,

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


-- | The GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID can be used to make a
-- t'GI.Gtk.Interfaces.TreeSortable.TreeSortable' use no sorting.
-- 
-- See also 'GI.Gtk.Interfaces.TreeSortable.treeSortableSetSortColumnId'
pattern TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID = -2 :: Int32

-- | The GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID can be used to make a
-- t'GI.Gtk.Interfaces.TreeSortable.TreeSortable' use the default sort function.
-- 
-- See also 'GI.Gtk.Interfaces.TreeSortable.treeSortableSetSortColumnId'
pattern TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID = -1 :: Int32

-- | The priority at which the text view validates onscreen lines
-- in an idle job in the background.
pattern TEXT_VIEW_PRIORITY_VALIDATE = 125 :: Int32

{-# DEPRECATED STYLE_REGION_TAB ["(Since version 3.20)","Don\\'t use regions."] #-}
-- | A widget region name to define a notebook tab.
pattern STYLE_REGION_TAB = "tab" :: T.Text

{-# DEPRECATED STYLE_REGION_ROW ["(Since version 3.20)","Don\\'t use regions."] #-}
-- | A widget region name to define a treeview row.
pattern STYLE_REGION_ROW = "row" :: T.Text

{-# DEPRECATED STYLE_REGION_COLUMN_HEADER ["(Since version 3.20)","Don\\'t use regions."] #-}
-- | A widget region name to define a treeview column header.
pattern STYLE_REGION_COLUMN_HEADER = "column-header" :: T.Text

{-# DEPRECATED STYLE_REGION_COLUMN ["(Since version 3.20)","Don\\'t use regions."] #-}
-- | A widget region name to define a treeview column.
pattern STYLE_REGION_COLUMN = "column" :: T.Text

-- | The priority used for the style information from
-- @XDG_CONFIG_HOME\/gtk-3.0\/gtk.css@.
-- 
-- You should not use priorities higher than this, to
-- give the user the last word.
pattern STYLE_PROVIDER_PRIORITY_USER = 800 :: Int32

-- | The priority used for style information provided
-- by themes.
pattern STYLE_PROVIDER_PRIORITY_THEME = 200 :: Int32

-- | The priority used for style information provided
-- via t'GI.Gtk.Objects.Settings.Settings'.
-- 
-- This priority is higher than 'GI.Gtk.Constants.STYLE_PROVIDER_PRIORITY_THEME'
-- to let settings override themes.
pattern STYLE_PROVIDER_PRIORITY_SETTINGS = 400 :: Int32

-- | The priority used for default style information
-- that is used in the absence of themes.
-- 
-- Note that this is not very useful for providing default
-- styling for custom style classes - themes are likely to
-- override styling provided at this priority with
-- catch-all @* {...}@ rules.
pattern STYLE_PROVIDER_PRIORITY_FALLBACK = 1 :: Int32

-- | A priority that can be used when adding a t'GI.Gtk.Interfaces.StyleProvider.StyleProvider'
-- for application-specific style information.
pattern STYLE_PROVIDER_PRIORITY_APPLICATION = 600 :: Int32

-- | A property holding the rendered element’s padding as a t'GI.Gtk.Structs.Border.Border'. The
-- padding is defined as the spacing between the inner part of the element border
-- and its child. It’s the innermost spacing property of the padding\/border\/margin
-- series.
pattern STYLE_PROPERTY_PADDING = "padding" :: T.Text

-- | A property holding the rendered element’s margin as a t'GI.Gtk.Structs.Border.Border'. The
-- margin is defined as the spacing between the border of the element
-- and its surrounding elements. It is external to t'GI.Gtk.Objects.Widget.Widget'\'s
-- size allocations, and the most external spacing property of the
-- padding\/border\/margin series.
pattern STYLE_PROPERTY_MARGIN = "margin" :: T.Text

-- | A property holding the font properties used when rendering text
-- as a t'GI.Pango.Structs.FontDescription.FontDescription'.
pattern STYLE_PROPERTY_FONT = "font" :: T.Text

-- | A property holding the foreground color of rendered elements as a t'GI.Gdk.Structs.RGBA.RGBA'.
pattern STYLE_PROPERTY_COLOR = "color" :: T.Text

-- | A property holding the rendered element’s border width in pixels as
-- a t'GI.Gtk.Structs.Border.Border'. The border is the intermediary spacing property of the
-- padding\/border\/margin series.
-- 
-- 'GI.Gtk.Functions.renderFrame' uses this property to find out the frame line width,
-- so @/GtkWidgets/@ rendering frames may need to add up this padding when
-- requesting size
pattern STYLE_PROPERTY_BORDER_WIDTH = "border-width" :: T.Text

-- | A property holding the element’s border style as a t'GI.Gtk.Enums.BorderStyle'.
pattern STYLE_PROPERTY_BORDER_STYLE = "border-style" :: T.Text

-- | A property holding the rendered element’s border radius in pixels as a @/gint/@.
pattern STYLE_PROPERTY_BORDER_RADIUS = "border-radius" :: T.Text

-- | A property holding the element’s border color as a t'GI.Gdk.Structs.RGBA.RGBA'.
pattern STYLE_PROPERTY_BORDER_COLOR = "border-color" :: T.Text

-- | A property holding the element’s background as a t'GI.Cairo.Structs.Pattern.Pattern'.
pattern STYLE_PROPERTY_BACKGROUND_IMAGE = "background-image" :: T.Text

-- | A property holding the background color of rendered elements as a t'GI.Gdk.Structs.RGBA.RGBA'.
pattern STYLE_PROPERTY_BACKGROUND_COLOR = "background-color" :: T.Text

-- | A CSS class to indicate that a UI element should be \'wide\'.
-- Used by t'GI.Gtk.Objects.Paned.Paned'.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.16/
pattern STYLE_CLASS_WIDE = "wide" :: T.Text

-- | A CSS class for an area displaying a warning message,
-- such as those in infobars.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_WARNING = "warning" :: T.Text

-- | A CSS class defining a view, such as iconviews or treeviews.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_VIEW = "view" :: T.Text

-- | A CSS class for vertically layered widgets.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_VERTICAL = "vertical" :: T.Text

-- | A CSS class that is added on the visual hints that happen
-- where content is \'scrolled off\' and can be made visible
-- by scrolling.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.16/
pattern STYLE_CLASS_UNDERSHOOT = "undershoot" :: T.Text

-- | A CSS class to match troughs, as in scrollbars and progressbars.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_TROUGH = "trough" :: T.Text

-- | A CSS class for touch selection popups on entries
-- and text views.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.16/
pattern STYLE_CLASS_TOUCH_SELECTION = "touch-selection" :: T.Text

-- | A CSS class to indicate an area at the top of a widget.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_TOP = "top" :: T.Text

-- | A CSS class to match tooltip windows.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_TOOLTIP = "tooltip" :: T.Text

-- | A CSS class to match toolbars.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_TOOLBAR = "toolbar" :: T.Text

-- | A CSS class used when rendering a titlebar in a toplevel window.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_TITLEBAR = "titlebar" :: T.Text

-- | A CSS class used for the title label in a titlebar in
-- a toplevel window.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.14/
pattern STYLE_CLASS_TITLE = "title" :: T.Text

-- | A CSS class used when an action (usually a button) is the
-- primary suggested action in a specific context.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.12/
pattern STYLE_CLASS_SUGGESTED_ACTION = "suggested-action" :: T.Text

-- | A CSS class used for the subtitle label in a titlebar in
-- a toplevel window.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.14/
pattern STYLE_CLASS_SUBTITLE = "subtitle" :: T.Text

-- | A CSS class to match statusbars.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.16/
pattern STYLE_CLASS_STATUSBAR = "statusbar" :: T.Text

-- | A CSS class to use when rendering activity as a “spinner”.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_SPINNER = "spinner" :: T.Text

-- | A CSS class defining an spinbutton.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_SPINBUTTON = "spinbutton" :: T.Text

-- | A CSS class to match sliders.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_SLIDER = "slider" :: T.Text

-- | A CSS class defining a sidebar, such as the left side in
-- a file chooser.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_SIDEBAR = "sidebar" :: T.Text

-- | A CSS class for a separator.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_SEPARATOR = "separator" :: T.Text

-- | A CSS class to match the junction area between an horizontal
-- and vertical scrollbar, when they’re both shown.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_SCROLLBARS_JUNCTION = "scrollbars-junction" :: T.Text

-- | A CSS class to match scrollbars.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_SCROLLBAR = "scrollbar" :: T.Text

-- | A CSS class to match scale widgets with marks attached,
-- all the marks are below for horizontal t'GI.Gtk.Objects.Scale.Scale',
-- right for vertical t'GI.Gtk.Objects.Scale.Scale'.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_SCALE_HAS_MARKS_BELOW = "scale-has-marks-below" :: T.Text

-- | A CSS class to match scale widgets with marks attached,
-- all the marks are above for horizontal t'GI.Gtk.Objects.Scale.Scale'.
-- left for vertical t'GI.Gtk.Objects.Scale.Scale'.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_SCALE_HAS_MARKS_ABOVE = "scale-has-marks-above" :: T.Text

-- | A CSS class to match scale widgets.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_SCALE = "scale" :: T.Text

-- | A CSS class to match the rubberband selection rectangle.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_RUBBERBAND = "rubberband" :: T.Text

-- | A CSS class to indicate an area at the right of a widget.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_RIGHT = "right" :: T.Text

-- | A CSS class used to indicate a read-only state.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_READ_ONLY = "read-only" :: T.Text

-- | A CSS class to match a raised control, such as a raised
-- button on a toolbar.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_RAISED = "raised" :: T.Text

-- | A CSS class to match radio buttons.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_RADIO = "radio" :: T.Text

-- | A CSS class for an area displaying a question to the user,
-- such as those in infobars.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_QUESTION = "question" :: T.Text

-- | A CSS class to use when rendering a pulse in an indeterminate progress bar.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_PULSE = "pulse" :: T.Text

-- | A CSS class to use when rendering activity as a progressbar.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_PROGRESSBAR = "progressbar" :: T.Text

-- | A CSS class to match primary toolbars.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_PRIMARY_TOOLBAR = "primary-toolbar" :: T.Text

-- | A CSS class that is added to the toplevel windows used for menus.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.14/
pattern STYLE_CLASS_POPUP = "popup" :: T.Text

-- | A CSS class that matches popovers.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.14/
pattern STYLE_CLASS_POPOVER = "popover" :: T.Text

-- | A CSS class that is added to areas that should look like paper.
-- 
-- This is used in print previews and themes are encouraged to
-- style it as black text on white background.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.16/
pattern STYLE_CLASS_PAPER = "paper" :: T.Text

-- | A CSS class for a pane separator, such as those in t'GI.Gtk.Objects.Paned.Paned'.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_PANE_SEPARATOR = "pane-separator" :: T.Text

-- | A CSS class that is added on the visual hints that happen
-- when scrolling is attempted past the limits of a scrollable
-- area.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.14/
pattern STYLE_CLASS_OVERSHOOT = "overshoot" :: T.Text

-- | A CSS class used when rendering an OSD (On Screen Display) element,
-- on top of another container.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_OSD = "osd" :: T.Text

-- | A CSS class defining a notebook.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_NOTEBOOK = "notebook" :: T.Text

-- | A CSS class used when an element needs the user attention,
-- for instance a button in a stack switcher corresponding to
-- a hidden page that changed state.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.12/
pattern STYLE_CLASS_NEEDS_ATTENTION = "needs-attention" :: T.Text

-- | A CSS class that is added to text view that should use
-- a monospace font.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.16/
pattern STYLE_CLASS_MONOSPACE = "monospace" :: T.Text

-- | A CSS class that is added to message dialogs.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.14/
pattern STYLE_CLASS_MESSAGE_DIALOG = "message-dialog" :: T.Text

-- | A CSS class to match menu items.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_MENUITEM = "menuitem" :: T.Text

-- | A CSS class to menubars.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_MENUBAR = "menubar" :: T.Text

-- | A CSS class to match menus.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_MENU = "menu" :: T.Text

-- | A CSS class defining marks in a widget, such as in scales.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_MARK = "mark" :: T.Text

-- | A CSS class to match list rows.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_LIST_ROW = "list-row" :: T.Text

-- | A CSS class to match lists.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_LIST = "list" :: T.Text

-- | A CSS class to match a linked area, such as a box containing buttons
-- belonging to the same control.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_LINKED = "linked" :: T.Text

-- | A CSS class used when rendering a level indicator, such
-- as a battery charge level, or a password strength.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_LEVEL_BAR = "level-bar" :: T.Text

-- | A CSS class to indicate an area at the left of a widget.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_LEFT = "left" :: T.Text

-- | A CSS class to match labels.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.16/
pattern STYLE_CLASS_LABEL = "label" :: T.Text

-- | A CSS class used when rendering a drag handle for
-- the insertion cursor position.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_INSERTION_CURSOR = "insertion-cursor" :: T.Text

-- | A CSS class to match inline toolbars.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_INLINE_TOOLBAR = "inline-toolbar" :: T.Text

-- | A CSS class for an area displaying an informational message,
-- such as those in infobars.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_INFO = "info" :: T.Text

-- | A CSS class defining an image, such as the icon in an entry.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_IMAGE = "image" :: T.Text

-- | A CSS class for horizontally layered widgets.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_HORIZONTAL = "horizontal" :: T.Text

-- | A CSS class defining a highlighted area, such as headings in
-- assistants and calendars.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_HIGHLIGHT = "highlight" :: T.Text

-- | A CSS class to match a header element.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_HEADER = "header" :: T.Text

-- | A CSS class defining a resize grip.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_GRIP = "grip" :: T.Text

-- | A CSS class defining a frame delimiting content, such as
-- t'GI.Gtk.Objects.Frame.Frame' or the scrolled window frame around the
-- scrollable area.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_FRAME = "frame" :: T.Text

-- | A CSS class that is added when widgets that usually have
-- a frame or border (like buttons or entries) should appear
-- without it.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.14/
pattern STYLE_CLASS_FLAT = "flat" :: T.Text

-- | A CSS class defining an expander, such as those in treeviews.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_EXPANDER = "expander" :: T.Text

-- | A CSS class for an area displaying an error message,
-- such as those in infobars.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_ERROR = "error" :: T.Text

-- | A CSS class to match text entries.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_ENTRY = "entry" :: T.Text

-- | A CSS class defining a dock area.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_DOCK = "dock" :: T.Text

-- | A CSS class for a drag-and-drop indicator.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_DND = "dnd" :: T.Text

-- | A CSS class to match dimmed labels.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_DIM_LABEL = "dim-label" :: T.Text

-- | A CSS class used when an action (usually a button) is
-- one that is expected to remove or destroy something visible
-- to the user.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.12/
pattern STYLE_CLASS_DESTRUCTIVE_ACTION = "destructive-action" :: T.Text

-- | A CSS class to match the default widget.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_DEFAULT = "default" :: T.Text

-- | A CSS class used when rendering a drag handle for
-- text selection.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_CURSOR_HANDLE = "cursor-handle" :: T.Text

-- | A CSS class that gets added to windows which have client-side decorations.
-- 
-- Refer to individual widget documentation for used style classes.
-- 
-- /Since: 3.14/
pattern STYLE_CLASS_CSD = "csd" :: T.Text

-- | A CSS class to match context menus.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_CONTEXT_MENU = "context-menu" :: T.Text

-- | A CSS class to match combobox entries.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_COMBOBOX_ENTRY = "combobox-entry" :: T.Text

-- | A CSS class to match check boxes.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_CHECK = "check" :: T.Text

-- | A CSS class to match content rendered in cell views.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_CELL = "cell" :: T.Text

-- | A CSS class to match calendars.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_CALENDAR = "calendar" :: T.Text

-- | A CSS class to match buttons.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_BUTTON = "button" :: T.Text

-- | A CSS class to indicate an area at the bottom of a widget.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_BOTTOM = "bottom" :: T.Text

-- | A CSS class to match the window background.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_BACKGROUND = "background" :: T.Text

-- | A CSS class used when rendering an arrow element.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_ARROW = "arrow" :: T.Text

-- | A CSS class to match an accelerator.
-- 
-- Refer to individual widget documentation for used style classes.
pattern STYLE_CLASS_ACCELERATOR = "accelerator" :: T.Text

{-# DEPRECATED STOCK_ZOOM_OUT ["(Since version 3.10)","Use named icon &quot;zoom-out&quot; or the label &quot;Zoom _Out&quot;."] #-}
-- | The “Zoom Out” item and icon.
pattern STOCK_ZOOM_OUT = "gtk-zoom-out" :: T.Text

{-# DEPRECATED STOCK_ZOOM_IN ["(Since version 3.10)","Use named icon &quot;zoom-in&quot; or the label &quot;Zoom _In&quot;."] #-}
-- | The “Zoom In” item and icon.
pattern STOCK_ZOOM_IN = "gtk-zoom-in" :: T.Text

{-# DEPRECATED STOCK_ZOOM_FIT ["(Since version 3.10)","Use named icon &quot;zoom-fit-best&quot; or the label &quot;Best _Fit&quot;."] #-}
-- | The “Zoom to Fit” item and icon.
pattern STOCK_ZOOM_FIT = "gtk-zoom-fit" :: T.Text

{-# DEPRECATED STOCK_ZOOM_100 ["(Since version 3.10)","Use named icon &quot;zoom-original&quot; or the label &quot;_Normal Size&quot;."] #-}
-- | The “Zoom 100%” item and icon.
pattern STOCK_ZOOM_100 = "gtk-zoom-100" :: T.Text

{-# DEPRECATED STOCK_YES ["(Since version 3.10)"] #-}
-- | The “Yes” item and icon.
pattern STOCK_YES = "gtk-yes" :: T.Text

{-# DEPRECATED STOCK_UNINDENT ["(Since version 3.10)","Use named icon &quot;format-indent-less&quot;."] #-}
-- | The “Unindent” item and icon. The icon has an RTL variant.
-- 
-- /Since: 2.4/
pattern STOCK_UNINDENT = "gtk-unindent" :: T.Text

{-# DEPRECATED STOCK_UNDO ["(Since version 3.10)","Use named icon &quot;edit-undo&quot; or the label &quot;_Undo&quot;."] #-}
-- | The “Undo” item and icon. The icon has an RTL variant.
pattern STOCK_UNDO = "gtk-undo" :: T.Text

{-# DEPRECATED STOCK_UNDERLINE ["(Since version 3.10)","Use named icon &quot;format-text-underline&quot; or the label &quot;_Underline&quot;."] #-}
-- | The “Underline” item and icon.
pattern STOCK_UNDERLINE = "gtk-underline" :: T.Text

{-# DEPRECATED STOCK_UNDELETE ["(Since version 3.10)"] #-}
-- | The “Undelete” item and icon. The icon has an RTL variant.
pattern STOCK_UNDELETE = "gtk-undelete" :: T.Text

{-# DEPRECATED STOCK_STRIKETHROUGH ["(Since version 3.10)","Use named icon &quot;format-text-strikethrough&quot; or the label &quot;_Strikethrough&quot;."] #-}
-- | The “Strikethrough” item and icon.
pattern STOCK_STRIKETHROUGH = "gtk-strikethrough" :: T.Text

{-# DEPRECATED STOCK_STOP ["(Since version 3.10)","Use named icon &quot;process-stop&quot; or the label &quot;_Stop&quot;."] #-}
-- | The “Stop” item and icon.
pattern STOCK_STOP = "gtk-stop" :: T.Text

{-# DEPRECATED STOCK_SPELL_CHECK ["(Since version 3.10)","Use named icon &quot;tools-check-spelling&quot;."] #-}
-- | The “Spell Check” item and icon.
pattern STOCK_SPELL_CHECK = "gtk-spell-check" :: T.Text

{-# DEPRECATED STOCK_SORT_DESCENDING ["(Since version 3.10)","Use named icon &quot;view-sort-descending&quot;."] #-}
-- | The “Descending” item and icon.
pattern STOCK_SORT_DESCENDING = "gtk-sort-descending" :: T.Text

{-# DEPRECATED STOCK_SORT_ASCENDING ["(Since version 3.10)","Use named icon &quot;view-sort-ascending&quot;."] #-}
-- | The “Ascending” item and icon.
pattern STOCK_SORT_ASCENDING = "gtk-sort-ascending" :: T.Text

{-# DEPRECATED STOCK_SELECT_FONT ["(Since version 3.10)"] #-}
-- | The “Font” item and icon.
pattern STOCK_SELECT_FONT = "gtk-select-font" :: T.Text

{-# DEPRECATED STOCK_SELECT_COLOR ["(Since version 3.10)"] #-}
-- | The “Color” item and icon.
pattern STOCK_SELECT_COLOR = "gtk-select-color" :: T.Text

{-# DEPRECATED STOCK_SELECT_ALL ["(Since version 3.10)","Use named icon &quot;edit-select-all&quot; or the label &quot;Select _All&quot;."] #-}
-- | The “Select All” item and icon.
-- 
-- /Since: 2.10/
pattern STOCK_SELECT_ALL = "gtk-select-all" :: T.Text

{-# DEPRECATED STOCK_SAVE_AS ["(Since version 3.10)","Use named icon &quot;document-save-as&quot; or the label &quot;Save _As&quot;."] #-}
-- | The “Save As” item and icon.
pattern STOCK_SAVE_AS = "gtk-save-as" :: T.Text

{-# DEPRECATED STOCK_SAVE ["(Since version 3.10)","Use named icon &quot;document-save&quot; or the label &quot;_Save&quot;."] #-}
-- | The “Save” item and icon.
pattern STOCK_SAVE = "gtk-save" :: T.Text

{-# DEPRECATED STOCK_REVERT_TO_SAVED ["(Since version 3.10)","Use named icon &quot;document-revert&quot; or the label &quot;_Revert&quot;."] #-}
-- | The “Revert” item and icon. The icon has an RTL variant.
pattern STOCK_REVERT_TO_SAVED = "gtk-revert-to-saved" :: T.Text

{-# DEPRECATED STOCK_REMOVE ["(Since version 3.10)","Use named icon &quot;list-remove&quot; or the label &quot;_Remove&quot;."] #-}
-- | The “Remove” item and icon.
pattern STOCK_REMOVE = "gtk-remove" :: T.Text

{-# DEPRECATED STOCK_REFRESH ["(Since version 3.10)","Use named icon &quot;view-refresh&quot; or the label &quot;_Refresh&quot;."] #-}
-- | The “Refresh” item and icon.
pattern STOCK_REFRESH = "gtk-refresh" :: T.Text

{-# DEPRECATED STOCK_REDO ["(Since version 3.10)","Use named icon &quot;edit-redo&quot; or the label &quot;_Redo&quot;."] #-}
-- | The “Redo” item and icon. The icon has an RTL variant.
pattern STOCK_REDO = "gtk-redo" :: T.Text

{-# DEPRECATED STOCK_QUIT ["(Since version 3.10)","Use named icon &quot;application-exit&quot; or the label &quot;_Quit&quot;."] #-}
-- | The “Quit” item and icon.
pattern STOCK_QUIT = "gtk-quit" :: T.Text

{-# DEPRECATED STOCK_PROPERTIES ["(Since version 3.10)","Use named icon &quot;document-properties&quot; or the label &quot;_Properties&quot;."] #-}
-- | The “Properties” item and icon.
pattern STOCK_PROPERTIES = "gtk-properties" :: T.Text

{-# DEPRECATED STOCK_PRINT_WARNING ["(Since version 3.10)"] #-}
-- | The “Print Warning” icon.
-- 
-- /Since: 2.14/
pattern STOCK_PRINT_WARNING = "gtk-print-warning" :: T.Text

{-# DEPRECATED STOCK_PRINT_REPORT ["(Since version 3.10)"] #-}
-- | The “Print Report” icon.
-- 
-- /Since: 2.14/
pattern STOCK_PRINT_REPORT = "gtk-print-report" :: T.Text

{-# DEPRECATED STOCK_PRINT_PREVIEW ["(Since version 3.10)","Use label &quot;Pre_view&quot;."] #-}
-- | The “Print Preview” item and icon.
pattern STOCK_PRINT_PREVIEW = "gtk-print-preview" :: T.Text

{-# DEPRECATED STOCK_PRINT_PAUSED ["(Since version 3.10)"] #-}
-- | The “Print Paused” icon.
-- 
-- /Since: 2.14/
pattern STOCK_PRINT_PAUSED = "gtk-print-paused" :: T.Text

{-# DEPRECATED STOCK_PRINT_ERROR ["(Since version 3.10)","Use named icon &quot;printer-error&quot;."] #-}
-- | The “Print Error” icon.
-- 
-- /Since: 2.14/
pattern STOCK_PRINT_ERROR = "gtk-print-error" :: T.Text

{-# DEPRECATED STOCK_PRINT ["(Since version 3.10)","Use named icon &quot;document-print&quot; or the label &quot;_Print&quot;."] #-}
-- | The “Print” item and icon.
pattern STOCK_PRINT = "gtk-print" :: T.Text

{-# DEPRECATED STOCK_PREFERENCES ["(Since version 3.10)","Use named icon &quot;preferences-system&quot; or the label &quot;_Preferences&quot;."] #-}
-- | The “Preferences” item and icon.
pattern STOCK_PREFERENCES = "gtk-preferences" :: T.Text

{-# DEPRECATED STOCK_PASTE ["(Since version 3.10)","Use named icon &quot;edit-paste&quot; or the label &quot;_Paste&quot;."] #-}
-- | The “Paste” item and icon.
pattern STOCK_PASTE = "gtk-paste" :: T.Text

{-# DEPRECATED STOCK_PAGE_SETUP ["(Since version 3.10)","Use named icon &quot;document-page-setup&quot; or the label &quot;Page Set_up&quot;."] #-}
-- | The “Page Setup” item and icon.
-- 
-- /Since: 2.14/
pattern STOCK_PAGE_SETUP = "gtk-page-setup" :: T.Text

{-# DEPRECATED STOCK_ORIENTATION_REVERSE_PORTRAIT ["(Since version 3.10)"] #-}
-- | The “Reverse Portrait Orientation” item and icon.
-- 
-- /Since: 2.10/
pattern STOCK_ORIENTATION_REVERSE_PORTRAIT = "gtk-orientation-reverse-portrait" :: T.Text

{-# DEPRECATED STOCK_ORIENTATION_REVERSE_LANDSCAPE ["(Since version 3.10)"] #-}
-- | The “Reverse Landscape Orientation” item and icon.
-- 
-- /Since: 2.10/
pattern STOCK_ORIENTATION_REVERSE_LANDSCAPE = "gtk-orientation-reverse-landscape" :: T.Text

{-# DEPRECATED STOCK_ORIENTATION_PORTRAIT ["(Since version 3.10)"] #-}
-- | The “Portrait Orientation” item and icon.
-- 
-- /Since: 2.10/
pattern STOCK_ORIENTATION_PORTRAIT = "gtk-orientation-portrait" :: T.Text

{-# DEPRECATED STOCK_ORIENTATION_LANDSCAPE ["(Since version 3.10)"] #-}
-- | The “Landscape Orientation” item and icon.
-- 
-- /Since: 2.10/
pattern STOCK_ORIENTATION_LANDSCAPE = "gtk-orientation-landscape" :: T.Text

{-# DEPRECATED STOCK_OPEN ["(Since version 3.10)","Use named icon &quot;document-open&quot; or the label &quot;_Open&quot;."] #-}
-- | The “Open” item and icon.
pattern STOCK_OPEN = "gtk-open" :: T.Text

{-# DEPRECATED STOCK_OK ["(Since version 3.10)","Do not use an icon. Use label &quot;_OK&quot;."] #-}
-- | The “OK” item and icon.
pattern STOCK_OK = "gtk-ok" :: T.Text

{-# DEPRECATED STOCK_NO ["(Since version 3.10)"] #-}
-- | The “No” item and icon.
pattern STOCK_NO = "gtk-no" :: T.Text

{-# DEPRECATED STOCK_NEW ["(Since version 3.10)","Use named icon &quot;document-new&quot; or the label &quot;_New&quot;."] #-}
-- | The “New” item and icon.
pattern STOCK_NEW = "gtk-new" :: T.Text

{-# DEPRECATED STOCK_NETWORK ["(Since version 3.10)","Use named icon &quot;network-workgroup&quot;."] #-}
-- | The “Network” item and icon.
-- 
-- /Since: 2.4/
pattern STOCK_NETWORK = "gtk-network" :: T.Text

{-# DEPRECATED STOCK_MISSING_IMAGE ["(Since version 3.10)","Use named icon &quot;image-missing&quot;."] #-}
-- | The “Missing image” icon.
pattern STOCK_MISSING_IMAGE = "gtk-missing-image" :: T.Text

{-# DEPRECATED STOCK_MEDIA_STOP ["(Since version 3.10)","Use named icon &quot;media-playback-stop&quot; or the label &quot;_Stop&quot;."] #-}
-- | The “Media Stop” item and icon.
-- 
-- /Since: 2.6/
pattern STOCK_MEDIA_STOP = "gtk-media-stop" :: T.Text

{-# DEPRECATED STOCK_MEDIA_REWIND ["(Since version 3.10)","Use named icon &quot;media-seek-backward&quot; or the label &quot;R_ewind&quot;."] #-}
-- | The “Media Rewind” item and icon. The icon has an RTL variant.
-- 
-- /Since: 2.6/
pattern STOCK_MEDIA_REWIND = "gtk-media-rewind" :: T.Text

{-# DEPRECATED STOCK_MEDIA_RECORD ["(Since version 3.10)","Use named icon &quot;media-record&quot; or the label &quot;_Record&quot;."] #-}
-- | The “Media Record” item and icon.
-- 
-- /Since: 2.6/
pattern STOCK_MEDIA_RECORD = "gtk-media-record" :: T.Text

{-# DEPRECATED STOCK_MEDIA_PREVIOUS ["(Since version 3.10)","Use named icon &quot;media-skip-backward&quot; or the label &quot;Pre_vious&quot;."] #-}
-- | The “Media Previous” item and icon. The icon has an RTL variant.
-- 
-- /Since: 2.6/
pattern STOCK_MEDIA_PREVIOUS = "gtk-media-previous" :: T.Text

{-# DEPRECATED STOCK_MEDIA_PLAY ["(Since version 3.10)","Use named icon &quot;media-playback-start&quot; or the label &quot;_Play&quot;."] #-}
-- | The “Media Play” item and icon. The icon has an RTL variant.
-- 
-- /Since: 2.6/
pattern STOCK_MEDIA_PLAY = "gtk-media-play" :: T.Text

{-# DEPRECATED STOCK_MEDIA_PAUSE ["(Since version 3.10)","Use named icon &quot;media-playback-pause&quot; or the label &quot;P_ause&quot;."] #-}
-- | The “Media Pause” item and icon.
-- 
-- /Since: 2.6/
pattern STOCK_MEDIA_PAUSE = "gtk-media-pause" :: T.Text

{-# DEPRECATED STOCK_MEDIA_NEXT ["(Since version 3.10)","Use named icon &quot;media-skip-forward&quot; or the label &quot;_Next&quot;."] #-}
-- | The “Media Next” item and icon. The icon has an RTL variant.
-- 
-- /Since: 2.6/
pattern STOCK_MEDIA_NEXT = "gtk-media-next" :: T.Text

{-# DEPRECATED STOCK_MEDIA_FORWARD ["(Since version 3.10)","Use named icon &quot;media-seek-forward&quot; or the label &quot;_Forward&quot;."] #-}
-- | The “Media Forward” item and icon. The icon has an RTL variant.
-- 
-- /Since: 2.6/
pattern STOCK_MEDIA_FORWARD = "gtk-media-forward" :: T.Text

{-# DEPRECATED STOCK_LEAVE_FULLSCREEN ["(Since version 3.10)","Use named icon &quot;view-restore&quot;."] #-}
-- | The “Leave Fullscreen” item and icon.
-- 
-- /Since: 2.8/
pattern STOCK_LEAVE_FULLSCREEN = "gtk-leave-fullscreen" :: T.Text

{-# DEPRECATED STOCK_JUSTIFY_RIGHT ["(Since version 3.10)","Use named icon &quot;format-justify-right&quot;."] #-}
-- | The “Right” item and icon.
pattern STOCK_JUSTIFY_RIGHT = "gtk-justify-right" :: T.Text

{-# DEPRECATED STOCK_JUSTIFY_LEFT ["(Since version 3.10)","Use named icon &quot;format-justify-left&quot;."] #-}
-- | The “Left” item and icon.
pattern STOCK_JUSTIFY_LEFT = "gtk-justify-left" :: T.Text

{-# DEPRECATED STOCK_JUSTIFY_FILL ["(Since version 3.10)","Use named icon &quot;format-justify-fill&quot;."] #-}
-- | The “Fill” item and icon.
pattern STOCK_JUSTIFY_FILL = "gtk-justify-fill" :: T.Text

{-# DEPRECATED STOCK_JUSTIFY_CENTER ["(Since version 3.10)","Use named icon &quot;format-justify-center&quot;."] #-}
-- | The “Center” item and icon.
pattern STOCK_JUSTIFY_CENTER = "gtk-justify-center" :: T.Text

{-# DEPRECATED STOCK_JUMP_TO ["(Since version 3.10)","Use named icon &quot;go-jump&quot;."] #-}
-- | The “Jump to” item and icon. The icon has an RTL variant.
pattern STOCK_JUMP_TO = "gtk-jump-to" :: T.Text

{-# DEPRECATED STOCK_ITALIC ["(Since version 3.10)","Use named icon &quot;format-text-italic&quot;."] #-}
-- | The “Italic” item and icon.
pattern STOCK_ITALIC = "gtk-italic" :: T.Text

{-# DEPRECATED STOCK_INFO ["(Since version 3.10)","Use named icon &quot;dialog-information&quot;."] #-}
-- | The “Info” item and icon.
-- 
-- /Since: 2.8/
pattern STOCK_INFO = "gtk-info" :: T.Text

{-# DEPRECATED STOCK_INDEX ["(Since version 3.10)"] #-}
-- | The “Index” item and icon.
pattern STOCK_INDEX = "gtk-index" :: T.Text

{-# DEPRECATED STOCK_INDENT ["(Since version 3.10)","Use named icon &quot;format-indent-more&quot;."] #-}
-- | The “Indent” item and icon. The icon has an RTL variant.
-- 
-- /Since: 2.4/
pattern STOCK_INDENT = "gtk-indent" :: T.Text

{-# DEPRECATED STOCK_HOME ["(Since version 3.10)","Use named icon &quot;go-home&quot;."] #-}
-- | The “Home” item and icon.
pattern STOCK_HOME = "gtk-home" :: T.Text

{-# DEPRECATED STOCK_HELP ["(Since version 3.10)","Use named icon &quot;help-browser&quot;."] #-}
-- | The “Help” item and icon.
pattern STOCK_HELP = "gtk-help" :: T.Text

{-# DEPRECATED STOCK_HARDDISK ["(Since version 3.10)","Use named icon &quot;drive-harddisk&quot;."] #-}
-- | The “Harddisk” item and icon.
-- 
-- /Since: 2.4/
pattern STOCK_HARDDISK = "gtk-harddisk" :: T.Text

{-# DEPRECATED STOCK_GO_UP ["(Since version 3.10)","Use named icon &quot;go-up&quot;."] #-}
-- | The “Up” item and icon.
pattern STOCK_GO_UP = "gtk-go-up" :: T.Text

{-# DEPRECATED STOCK_GO_FORWARD ["(Since version 3.10)","Use named icon &quot;go-next&quot;."] #-}
-- | The “Forward” item and icon. The icon has an RTL variant.
pattern STOCK_GO_FORWARD = "gtk-go-forward" :: T.Text

{-# DEPRECATED STOCK_GO_DOWN ["(Since version 3.10)","Use named icon &quot;go-down&quot;."] #-}
-- | The “Down” item and icon.
pattern STOCK_GO_DOWN = "gtk-go-down" :: T.Text

{-# DEPRECATED STOCK_GO_BACK ["(Since version 3.10)","Use named icon &quot;go-previous&quot;."] #-}
-- | The “Back” item and icon. The icon has an RTL variant.
pattern STOCK_GO_BACK = "gtk-go-back" :: T.Text

{-# DEPRECATED STOCK_GOTO_TOP ["(Since version 3.10)","Use named icon &quot;go-top&quot;."] #-}
-- | The “Top” item and icon.
pattern STOCK_GOTO_TOP = "gtk-goto-top" :: T.Text

{-# DEPRECATED STOCK_GOTO_LAST ["(Since version 3.10)","Use named icon &quot;go-last&quot;."] #-}
-- | The “Last” item and icon. The icon has an RTL variant.
pattern STOCK_GOTO_LAST = "gtk-goto-last" :: T.Text

{-# DEPRECATED STOCK_GOTO_FIRST ["(Since version 3.10)","Use named icon &quot;go-first&quot;."] #-}
-- | The “First” item and icon. The icon has an RTL variant.
pattern STOCK_GOTO_FIRST = "gtk-goto-first" :: T.Text

{-# DEPRECATED STOCK_GOTO_BOTTOM ["(Since version 3.10)","Use named icon &quot;go-bottom&quot;."] #-}
-- | The “Bottom” item and icon.
pattern STOCK_GOTO_BOTTOM = "gtk-goto-bottom" :: T.Text

{-# DEPRECATED STOCK_FULLSCREEN ["(Since version 3.10)","Use named icon &quot;view-fullscreen&quot;."] #-}
-- | The “Fullscreen” item and icon.
-- 
-- /Since: 2.8/
pattern STOCK_FULLSCREEN = "gtk-fullscreen" :: T.Text

{-# DEPRECATED STOCK_FLOPPY ["(Since version 3.10)"] #-}
-- | The “Floppy” item and icon.
pattern STOCK_FLOPPY = "gtk-floppy" :: T.Text

{-# DEPRECATED STOCK_FIND_AND_REPLACE ["(Since version 3.10)","Use named icon &quot;edit-find-replace&quot;."] #-}
-- | The “Find and Replace” item and icon.
pattern STOCK_FIND_AND_REPLACE = "gtk-find-and-replace" :: T.Text

{-# DEPRECATED STOCK_FIND ["(Since version 3.10)","Use named icon &quot;edit-find&quot;."] #-}
-- | The “Find” item and icon.
pattern STOCK_FIND = "gtk-find" :: T.Text

{-# DEPRECATED STOCK_FILE ["(Since version 3.10)","Use named icon &quot;text-x-generic&quot;."] #-}
-- | The “File” item and icon.
-- 
-- Since 3.0, this item has a label, before it only had an icon.
-- 
-- /Since: 2.6/
pattern STOCK_FILE = "gtk-file" :: T.Text

{-# DEPRECATED STOCK_EXECUTE ["(Since version 3.10)","Use named icon &quot;system-run&quot;."] #-}
-- | The “Execute” item and icon.
pattern STOCK_EXECUTE = "gtk-execute" :: T.Text

{-# DEPRECATED STOCK_EDIT ["(Since version 3.10)"] #-}
-- | The “Edit” item and icon.
-- 
-- /Since: 2.6/
pattern STOCK_EDIT = "gtk-edit" :: T.Text

{-# DEPRECATED STOCK_DND_MULTIPLE ["(Since version 3.10)"] #-}
-- | The “Drag-And-Drop multiple” icon.
pattern STOCK_DND_MULTIPLE = "gtk-dnd-multiple" :: T.Text

{-# DEPRECATED STOCK_DND ["(Since version 3.10)"] #-}
-- | The “Drag-And-Drop” icon.
pattern STOCK_DND = "gtk-dnd" :: T.Text

{-# DEPRECATED STOCK_DISCONNECT ["(Since version 3.10)"] #-}
-- | The “Disconnect” icon.
-- 
-- /Since: 2.6/
pattern STOCK_DISCONNECT = "gtk-disconnect" :: T.Text

{-# DEPRECATED STOCK_DISCARD ["(Since version 3.10)"] #-}
-- | The “Discard” item.
-- 
-- /Since: 2.12/
pattern STOCK_DISCARD = "gtk-discard" :: T.Text

{-# DEPRECATED STOCK_DIRECTORY ["(Since version 3.10)","Use named icon &quot;folder&quot;."] #-}
-- | The “Directory” icon.
-- 
-- /Since: 2.6/
pattern STOCK_DIRECTORY = "gtk-directory" :: T.Text

{-# DEPRECATED STOCK_DIALOG_WARNING ["(Since version 3.10)","Use named icon &quot;dialog-warning&quot;."] #-}
-- | The “Warning” item and icon.
pattern STOCK_DIALOG_WARNING = "gtk-dialog-warning" :: T.Text

{-# DEPRECATED STOCK_DIALOG_QUESTION ["(Since version 3.10)","Use named icon &quot;dialog-question&quot;."] #-}
-- | The “Question” item and icon.
pattern STOCK_DIALOG_QUESTION = "gtk-dialog-question" :: T.Text

{-# DEPRECATED STOCK_DIALOG_INFO ["(Since version 3.10)","Use named icon &quot;dialog-information&quot;."] #-}
-- | The “Information” item and icon.
pattern STOCK_DIALOG_INFO = "gtk-dialog-info" :: T.Text

{-# DEPRECATED STOCK_DIALOG_ERROR ["(Since version 3.10)","Use named icon &quot;dialog-error&quot;."] #-}
-- | The “Error” item and icon.
pattern STOCK_DIALOG_ERROR = "gtk-dialog-error" :: T.Text

{-# DEPRECATED STOCK_DIALOG_AUTHENTICATION ["(Since version 3.10)","Use named icon &quot;dialog-password&quot;."] #-}
-- | The “Authentication” item and icon.
-- 
-- /Since: 2.4/
pattern STOCK_DIALOG_AUTHENTICATION = "gtk-dialog-authentication" :: T.Text

{-# DEPRECATED STOCK_DELETE ["(Since version 3.10)","Use the named icon &quot;edit-delete&quot; or the label &quot;_Delete&quot;."] #-}
-- | The “Delete” item and icon.
pattern STOCK_DELETE = "gtk-delete" :: T.Text

{-# DEPRECATED STOCK_CUT ["(Since version 3.10)","Use the named icon &quot;edit-cut&quot; or the label &quot;Cu_t&quot;."] #-}
-- | The “Cut” item and icon.
pattern STOCK_CUT = "gtk-cut" :: T.Text

{-# DEPRECATED STOCK_COPY ["(Since version 3.10)","Use the named icon &quot;edit-copy&quot; or the label &quot;_Copy&quot;."] #-}
-- | The “Copy” item and icon.
pattern STOCK_COPY = "gtk-copy" :: T.Text

{-# DEPRECATED STOCK_CONVERT ["(Since version 3.10)"] #-}
-- | The “Convert” item and icon.
pattern STOCK_CONVERT = "gtk-convert" :: T.Text

{-# DEPRECATED STOCK_CONNECT ["(Since version 3.10)"] #-}
-- | The “Connect” icon.
-- 
-- /Since: 2.6/
pattern STOCK_CONNECT = "gtk-connect" :: T.Text

{-# DEPRECATED STOCK_COLOR_PICKER ["(Since version 3.10)"] #-}
-- | The “Color Picker” item and icon.
-- 
-- /Since: 2.2/
pattern STOCK_COLOR_PICKER = "gtk-color-picker" :: T.Text

{-# DEPRECATED STOCK_CLOSE ["(Since version 3.10)","Use named icon &quot;window-close&quot; or the label &quot;_Close&quot;."] #-}
-- | The “Close” item and icon.
pattern STOCK_CLOSE = "gtk-close" :: T.Text

{-# DEPRECATED STOCK_CLEAR ["(Since version 3.10)","Use named icon &quot;edit-clear&quot;."] #-}
-- | The “Clear” item and icon.
pattern STOCK_CLEAR = "gtk-clear" :: T.Text

{-# DEPRECATED STOCK_CDROM ["(Since version 3.10)","Use named icon &quot;media-optical&quot;."] #-}
-- | The “CD-Rom” item and icon.
pattern STOCK_CDROM = "gtk-cdrom" :: T.Text

{-# DEPRECATED STOCK_CAPS_LOCK_WARNING ["(Since version 3.10)","Use named icon &quot;dialog-warning-symbolic&quot;."] #-}
-- | The “Caps Lock Warning” icon.
-- 
-- /Since: 2.16/
pattern STOCK_CAPS_LOCK_WARNING = "gtk-caps-lock-warning" :: T.Text

{-# DEPRECATED STOCK_CANCEL ["(Since version 3.10)","Do not use an icon. Use label &quot;_Cancel&quot;."] #-}
-- | The “Cancel” item and icon.
pattern STOCK_CANCEL = "gtk-cancel" :: T.Text

{-# DEPRECATED STOCK_BOLD ["(Since version 3.10)","Use named icon &quot;format-text-bold&quot;."] #-}
-- | The “Bold” item and icon.
pattern STOCK_BOLD = "gtk-bold" :: T.Text

{-# DEPRECATED STOCK_APPLY ["(Since version 3.10)","Do not use an icon. Use label &quot;_Apply&quot;."] #-}
-- | The “Apply” item and icon.
pattern STOCK_APPLY = "gtk-apply" :: T.Text

{-# DEPRECATED STOCK_ADD ["(Since version 3.10)","Use named icon &quot;list-add&quot; or the label &quot;_Add&quot;."] #-}
-- | The “Add” item and icon.
pattern STOCK_ADD = "gtk-add" :: T.Text

{-# DEPRECATED STOCK_ABOUT ["(Since version 3.10)","Use named icon &quot;help-about&quot; or the label &quot;_About&quot;."] #-}
-- | The “About” item.
-- <<https://developer.gnome.org/gtk3/stable/help-about.png>>
-- 
-- /Since: 2.6/
pattern STOCK_ABOUT = "gtk-about" :: T.Text

-- | Use this priority for functionality related to size allocation.
-- 
-- It is used internally by GTK+ to compute the sizes of widgets.
-- This priority is higher than 'GI.Gdk.Constants.PRIORITY_REDRAW' to avoid
-- resizing a widget which was just redrawn.
pattern PRIORITY_RESIZE = 110 :: Int32

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_WIN32_DRIVER_VERSION = "win32-driver-version" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_WIN32_DRIVER_EXTRA = "win32-driver-extra" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_USE_COLOR = "use-color" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_SCALE = "scale" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_REVERSE = "reverse" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_RESOLUTION_Y = "resolution-y" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_RESOLUTION_X = "resolution-x" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_RESOLUTION = "resolution" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_QUALITY = "quality" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_PRINT_PAGES = "print-pages" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_PRINTER_LPI = "printer-lpi" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_PRINTER = "printer" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_PAPER_WIDTH = "paper-width" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_PAPER_HEIGHT = "paper-height" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_PAPER_FORMAT = "paper-format" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_PAGE_SET = "page-set" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_PAGE_RANGES = "page-ranges" :: T.Text

-- | The key used by the “Print to file” printer to store the URI
-- to which the output should be written. GTK+ itself supports
-- only “file:\/\/” URIs.
pattern PRINT_SETTINGS_OUTPUT_URI = "output-uri" :: T.Text

-- | The key used by the “Print to file” printer to store the format
-- of the output. The supported values are “PS” and “PDF”.
pattern PRINT_SETTINGS_OUTPUT_FILE_FORMAT = "output-file-format" :: T.Text

-- | The key used by the “Print to file” printer to store the
-- directory to which the output should be written.
-- 
-- /Since: 3.6/
pattern PRINT_SETTINGS_OUTPUT_DIR = "output-dir" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_OUTPUT_BIN = "output-bin" :: T.Text

-- | The key used by the “Print to file” printer to store the file
-- name of the output without the path to the directory and the
-- file extension.
-- 
-- /Since: 3.6/
pattern PRINT_SETTINGS_OUTPUT_BASENAME = "output-basename" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_ORIENTATION = "orientation" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_N_COPIES = "n-copies" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_NUMBER_UP_LAYOUT = "number-up-layout" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_NUMBER_UP = "number-up" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_MEDIA_TYPE = "media-type" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_FINISHINGS = "finishings" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_DUPLEX = "duplex" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_DITHER = "dither" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_DEFAULT_SOURCE = "default-source" :: T.Text

-- | /No description available in the introspection data./
pattern PRINT_SETTINGS_COLLATE = "collate" :: T.Text

-- | /No description available in the introspection data./
pattern PATH_PRIO_MASK = 15 :: Int32

-- | Name for the Letter paper size.
pattern PAPER_NAME_LETTER = "na_letter" :: T.Text

-- | Name for the Legal paper size.
pattern PAPER_NAME_LEGAL = "na_legal" :: T.Text

-- | Name for the Executive paper size.
pattern PAPER_NAME_EXECUTIVE = "na_executive" :: T.Text

-- | Name for the B5 paper size.
pattern PAPER_NAME_B5 = "iso_b5" :: T.Text

-- | Name for the A5 paper size.
pattern PAPER_NAME_A5 = "iso_a5" :: T.Text

-- | Name for the A4 paper size.
pattern PAPER_NAME_A4 = "iso_a4" :: T.Text

-- | Name for the A3 paper size.
pattern PAPER_NAME_A3 = "iso_a3" :: T.Text

-- | Like 'GI.Gtk.Functions.getMinorVersion', but from the headers used at
-- application compile time, rather than from the library linked
-- against at application run time.
pattern MINOR_VERSION = 24 :: Int32

-- | Like 'GI.Gtk.Functions.getMicroVersion', but from the headers used at
-- application compile time, rather than from the library linked
-- against at application run time.
pattern MICRO_VERSION = 35 :: Int32

-- | The maximum length of sequences in compose tables.
pattern MAX_COMPOSE_LEN = 7 :: Int32

-- | Like 'GI.Gtk.Functions.getMajorVersion', but from the headers used at
-- application compile time, rather than from the library linked
-- against at application run time.
pattern MAJOR_VERSION = 3 :: Int32

-- | The name used for the stock low offset included by t'GI.Gtk.Objects.LevelBar.LevelBar'.
-- 
-- /Since: 3.6/
pattern LEVEL_BAR_OFFSET_LOW = "low" :: T.Text

-- | The name used for the stock high offset included by t'GI.Gtk.Objects.LevelBar.LevelBar'.
-- 
-- /Since: 3.6/
pattern LEVEL_BAR_OFFSET_HIGH = "high" :: T.Text

-- | The name used for the stock full offset included by t'GI.Gtk.Objects.LevelBar.LevelBar'.
-- 
-- /Since: 3.20/
pattern LEVEL_BAR_OFFSET_FULL = "full" :: T.Text

-- | Like 'GI.Gtk.Functions.getInterfaceAge', but from the headers used at
-- application compile time, rather than from the library linked
-- against at application run time.
pattern INTERFACE_AGE = 31 :: Int32

-- | Constant to return from a signal handler for the [SpinButton::input]("GI.Gtk.Objects.SpinButton#g:signal:input")
-- signal in case of conversion failure.
pattern INPUT_ERROR = -1 :: Int32

-- | Like 'GI.Gtk.Functions.getBinaryAge', but from the headers used at
-- application compile time, rather than from the library linked
-- against at application run time.
pattern BINARY_AGE = 2435 :: Int32


