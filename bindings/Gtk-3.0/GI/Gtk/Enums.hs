

-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Enums
    ( 

 -- * Enumerations


-- ** Align #enum:Align#

    Align(..)                               ,


-- ** ArrowPlacement #enum:ArrowPlacement#

    ArrowPlacement(..)                      ,


-- ** ArrowType #enum:ArrowType#

    ArrowType(..)                           ,


-- ** AssistantPageType #enum:AssistantPageType#

    AssistantPageType(..)                   ,


-- ** BaselinePosition #enum:BaselinePosition#

    BaselinePosition(..)                    ,


-- ** BorderStyle #enum:BorderStyle#

    BorderStyle(..)                         ,


-- ** BuilderError #enum:BuilderError#

    BuilderError(..)                        ,
    catchBuilderError                       ,
    handleBuilderError                      ,


-- ** ButtonBoxStyle #enum:ButtonBoxStyle#

    ButtonBoxStyle(..)                      ,


-- ** ButtonRole #enum:ButtonRole#

    ButtonRole(..)                          ,


-- ** ButtonsType #enum:ButtonsType#

    ButtonsType(..)                         ,


-- ** CellRendererAccelMode #enum:CellRendererAccelMode#

    CellRendererAccelMode(..)               ,


-- ** CellRendererMode #enum:CellRendererMode#

    CellRendererMode(..)                    ,


-- ** CornerType #enum:CornerType#

    CornerType(..)                          ,


-- ** CssProviderError #enum:CssProviderError#

    CssProviderError(..)                    ,
    catchCssProviderError                   ,
    handleCssProviderError                  ,


-- ** CssSectionType #enum:CssSectionType#

    CssSectionType(..)                      ,


-- ** DeleteType #enum:DeleteType#

    DeleteType(..)                          ,


-- ** DirectionType #enum:DirectionType#

    DirectionType(..)                       ,


-- ** DragResult #enum:DragResult#

    DragResult(..)                          ,


-- ** EntryIconPosition #enum:EntryIconPosition#

    EntryIconPosition(..)                   ,


-- ** EventSequenceState #enum:EventSequenceState#

    EventSequenceState(..)                  ,


-- ** ExpanderStyle #enum:ExpanderStyle#

    ExpanderStyle(..)                       ,


-- ** FileChooserAction #enum:FileChooserAction#

    FileChooserAction(..)                   ,


-- ** FileChooserConfirmation #enum:FileChooserConfirmation#

    FileChooserConfirmation(..)             ,


-- ** FileChooserError #enum:FileChooserError#

    FileChooserError(..)                    ,
    catchFileChooserError                   ,
    handleFileChooserError                  ,


-- ** IMPreeditStyle #enum:IMPreeditStyle#

    IMPreeditStyle(..)                      ,


-- ** IMStatusStyle #enum:IMStatusStyle#

    IMStatusStyle(..)                       ,


-- ** IconSize #enum:IconSize#

    IconSize(..)                            ,


-- ** IconThemeError #enum:IconThemeError#

    IconThemeError(..)                      ,
    catchIconThemeError                     ,
    handleIconThemeError                    ,


-- ** IconViewDropPosition #enum:IconViewDropPosition#

    IconViewDropPosition(..)                ,


-- ** ImageType #enum:ImageType#

    ImageType(..)                           ,


-- ** InputPurpose #enum:InputPurpose#

    InputPurpose(..)                        ,


-- ** Justification #enum:Justification#

    Justification(..)                       ,


-- ** LevelBarMode #enum:LevelBarMode#

    LevelBarMode(..)                        ,


-- ** License #enum:License#

    License(..)                             ,


-- ** MenuDirectionType #enum:MenuDirectionType#

    MenuDirectionType(..)                   ,


-- ** MessageType #enum:MessageType#

    MessageType(..)                         ,


-- ** MovementStep #enum:MovementStep#

    MovementStep(..)                        ,


-- ** NotebookTab #enum:NotebookTab#

    NotebookTab(..)                         ,


-- ** NumberUpLayout #enum:NumberUpLayout#

    NumberUpLayout(..)                      ,


-- ** Orientation #enum:Orientation#

    Orientation(..)                         ,


-- ** PackDirection #enum:PackDirection#

    PackDirection(..)                       ,


-- ** PackType #enum:PackType#

    PackType(..)                            ,


-- ** PadActionType #enum:PadActionType#

    PadActionType(..)                       ,


-- ** PageOrientation #enum:PageOrientation#

    PageOrientation(..)                     ,


-- ** PageSet #enum:PageSet#

    PageSet(..)                             ,


-- ** PanDirection #enum:PanDirection#

    PanDirection(..)                        ,


-- ** PathPriorityType #enum:PathPriorityType#

    PathPriorityType(..)                    ,


-- ** PathType #enum:PathType#

    PathType(..)                            ,


-- ** PolicyType #enum:PolicyType#

    PolicyType(..)                          ,


-- ** PopoverConstraint #enum:PopoverConstraint#

    PopoverConstraint(..)                   ,


-- ** PositionType #enum:PositionType#

    PositionType(..)                        ,


-- ** PrintDuplex #enum:PrintDuplex#

    PrintDuplex(..)                         ,


-- ** PrintError #enum:PrintError#

    PrintError(..)                          ,
    catchPrintError                         ,
    handlePrintError                        ,


-- ** PrintOperationAction #enum:PrintOperationAction#

    PrintOperationAction(..)                ,


-- ** PrintOperationResult #enum:PrintOperationResult#

    PrintOperationResult(..)                ,


-- ** PrintPages #enum:PrintPages#

    PrintPages(..)                          ,


-- ** PrintQuality #enum:PrintQuality#

    PrintQuality(..)                        ,


-- ** PrintStatus #enum:PrintStatus#

    PrintStatus(..)                         ,


-- ** PropagationPhase #enum:PropagationPhase#

    PropagationPhase(..)                    ,


-- ** RcTokenType #enum:RcTokenType#

    RcTokenType(..)                         ,


-- ** RecentChooserError #enum:RecentChooserError#

    RecentChooserError(..)                  ,
    catchRecentChooserError                 ,
    handleRecentChooserError                ,


-- ** RecentManagerError #enum:RecentManagerError#

    RecentManagerError(..)                  ,
    catchRecentManagerError                 ,
    handleRecentManagerError                ,


-- ** RecentSortType #enum:RecentSortType#

    RecentSortType(..)                      ,


-- ** ReliefStyle #enum:ReliefStyle#

    ReliefStyle(..)                         ,


-- ** ResizeMode #enum:ResizeMode#

    ResizeMode(..)                          ,


-- ** ResponseType #enum:ResponseType#

    ResponseType(..)                        ,


-- ** RevealerTransitionType #enum:RevealerTransitionType#

    RevealerTransitionType(..)              ,


-- ** ScrollStep #enum:ScrollStep#

    ScrollStep(..)                          ,


-- ** ScrollType #enum:ScrollType#

    ScrollType(..)                          ,


-- ** ScrollablePolicy #enum:ScrollablePolicy#

    ScrollablePolicy(..)                    ,


-- ** SelectionMode #enum:SelectionMode#

    SelectionMode(..)                       ,


-- ** SensitivityType #enum:SensitivityType#

    SensitivityType(..)                     ,


-- ** ShadowType #enum:ShadowType#

    ShadowType(..)                          ,


-- ** ShortcutType #enum:ShortcutType#

    ShortcutType(..)                        ,


-- ** SizeGroupMode #enum:SizeGroupMode#

    SizeGroupMode(..)                       ,


-- ** SizeRequestMode #enum:SizeRequestMode#

    SizeRequestMode(..)                     ,


-- ** SortType #enum:SortType#

    SortType(..)                            ,


-- ** SpinButtonUpdatePolicy #enum:SpinButtonUpdatePolicy#

    SpinButtonUpdatePolicy(..)              ,


-- ** SpinType #enum:SpinType#

    SpinType(..)                            ,


-- ** StackTransitionType #enum:StackTransitionType#

    StackTransitionType(..)                 ,


-- ** StateType #enum:StateType#

    StateType(..)                           ,


-- ** TextBufferTargetInfo #enum:TextBufferTargetInfo#

    TextBufferTargetInfo(..)                ,


-- ** TextDirection #enum:TextDirection#

    TextDirection(..)                       ,


-- ** TextExtendSelection #enum:TextExtendSelection#

    TextExtendSelection(..)                 ,


-- ** TextViewLayer #enum:TextViewLayer#

    TextViewLayer(..)                       ,


-- ** TextWindowType #enum:TextWindowType#

    TextWindowType(..)                      ,


-- ** ToolbarSpaceStyle #enum:ToolbarSpaceStyle#

    ToolbarSpaceStyle(..)                   ,


-- ** ToolbarStyle #enum:ToolbarStyle#

    ToolbarStyle(..)                        ,


-- ** TreeViewColumnSizing #enum:TreeViewColumnSizing#

    TreeViewColumnSizing(..)                ,


-- ** TreeViewDropPosition #enum:TreeViewDropPosition#

    TreeViewDropPosition(..)                ,


-- ** TreeViewGridLines #enum:TreeViewGridLines#

    TreeViewGridLines(..)                   ,


-- ** Unit #enum:Unit#

    Unit(..)                                ,


-- ** WidgetHelpType #enum:WidgetHelpType#

    WidgetHelpType(..)                      ,


-- ** WindowPosition #enum:WindowPosition#

    WindowPosition(..)                      ,


-- ** WindowType #enum:WindowType#

    WindowType(..)                          ,


-- ** WrapMode #enum:WrapMode#

    WrapMode(..)                            ,




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


-- Enum WrapMode
-- | Describes a type of line wrapping.
data WrapMode = 
      WrapModeNone
    -- ^ do not wrap lines; just make the text area wider
    | WrapModeChar
    -- ^ wrap text, breaking lines anywhere the cursor can
    --     appear (between characters, usually - if you want to be technical,
    --     between graphemes, see 'GI.Pango.Functions.getLogAttrs')
    | WrapModeWord
    -- ^ wrap text, breaking lines in between words
    | WrapModeWordChar
    -- ^ wrap text, breaking lines in between words, or if
    --     that is not enough, also between graphemes
    | AnotherWrapMode Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum WrapMode where
    fromEnum WrapModeNone = 0
    fromEnum WrapModeChar = 1
    fromEnum WrapModeWord = 2
    fromEnum WrapModeWordChar = 3
    fromEnum (AnotherWrapMode k) = k

    toEnum 0 = WrapModeNone
    toEnum 1 = WrapModeChar
    toEnum 2 = WrapModeWord
    toEnum 3 = WrapModeWordChar
    toEnum k = AnotherWrapMode k

instance P.Ord WrapMode where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes WrapMode = '[]
instance O.HasParentTypes WrapMode

foreign import ccall "gtk_wrap_mode_get_type" c_gtk_wrap_mode_get_type :: 
    IO GType

instance B.Types.TypedObject WrapMode where
    glibType = c_gtk_wrap_mode_get_type

instance B.Types.BoxedEnum WrapMode

-- Enum WindowType
-- | A t'GI.Gtk.Objects.Window.Window' can be one of these types. Most things you’d consider a
-- “window” should have type @/GTK_WINDOW_TOPLEVEL/@; windows with this type
-- are managed by the window manager and have a frame by default (call
-- 'GI.Gtk.Objects.Window.windowSetDecorated' to toggle the frame).  Windows with type
-- @/GTK_WINDOW_POPUP/@ are ignored by the window manager; window manager
-- keybindings won’t work on them, the window manager won’t decorate the
-- window with a frame, many GTK+ features that rely on the window
-- manager will not work (e.g. resize grips and
-- maximization\/minimization). @/GTK_WINDOW_POPUP/@ is used to implement
-- widgets such as t'GI.Gtk.Objects.Menu.Menu' or tooltips that you normally don’t think of
-- as windows per se. Nearly all windows should be @/GTK_WINDOW_TOPLEVEL/@.
-- In particular, do not use @/GTK_WINDOW_POPUP/@ just to turn off
-- the window borders; use 'GI.Gtk.Objects.Window.windowSetDecorated' for that.
data WindowType = 
      WindowTypeToplevel
    -- ^ A regular window, such as a dialog.
    | WindowTypePopup
    -- ^ A special window such as a tooltip.
    | AnotherWindowType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum WindowType where
    fromEnum WindowTypeToplevel = 0
    fromEnum WindowTypePopup = 1
    fromEnum (AnotherWindowType k) = k

    toEnum 0 = WindowTypeToplevel
    toEnum 1 = WindowTypePopup
    toEnum k = AnotherWindowType k

instance P.Ord WindowType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes WindowType = '[]
instance O.HasParentTypes WindowType

foreign import ccall "gtk_window_type_get_type" c_gtk_window_type_get_type :: 
    IO GType

instance B.Types.TypedObject WindowType where
    glibType = c_gtk_window_type_get_type

instance B.Types.BoxedEnum WindowType

-- Enum WindowPosition
-- | Window placement can be influenced using this enumeration. Note that
-- using @/GTK_WIN_POS_CENTER_ALWAYS/@ is almost always a bad idea.
-- It won’t necessarily work well with all window managers or on all windowing systems.
data WindowPosition = 
      WindowPositionNone
    -- ^ No influence is made on placement.
    | WindowPositionCenter
    -- ^ Windows should be placed in the center of the screen.
    | WindowPositionMouse
    -- ^ Windows should be placed at the current mouse position.
    | WindowPositionCenterAlways
    -- ^ Keep window centered as it changes size, etc.
    | WindowPositionCenterOnParent
    -- ^ Center the window on its transient
    --  parent (see 'GI.Gtk.Objects.Window.windowSetTransientFor').
    | AnotherWindowPosition Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum WindowPosition where
    fromEnum WindowPositionNone = 0
    fromEnum WindowPositionCenter = 1
    fromEnum WindowPositionMouse = 2
    fromEnum WindowPositionCenterAlways = 3
    fromEnum WindowPositionCenterOnParent = 4
    fromEnum (AnotherWindowPosition k) = k

    toEnum 0 = WindowPositionNone
    toEnum 1 = WindowPositionCenter
    toEnum 2 = WindowPositionMouse
    toEnum 3 = WindowPositionCenterAlways
    toEnum 4 = WindowPositionCenterOnParent
    toEnum k = AnotherWindowPosition k

instance P.Ord WindowPosition where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes WindowPosition = '[]
instance O.HasParentTypes WindowPosition

foreign import ccall "gtk_window_position_get_type" c_gtk_window_position_get_type :: 
    IO GType

instance B.Types.TypedObject WindowPosition where
    glibType = c_gtk_window_position_get_type

instance B.Types.BoxedEnum WindowPosition

-- Enum WidgetHelpType
-- | Kinds of widget-specific help. Used by the [showHelp](#g:signal:showHelp) signal.
data WidgetHelpType = 
      WidgetHelpTypeTooltip
    -- ^ Tooltip.
    | WidgetHelpTypeWhatsThis
    -- ^ What’s this.
    | AnotherWidgetHelpType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum WidgetHelpType where
    fromEnum WidgetHelpTypeTooltip = 0
    fromEnum WidgetHelpTypeWhatsThis = 1
    fromEnum (AnotherWidgetHelpType k) = k

    toEnum 0 = WidgetHelpTypeTooltip
    toEnum 1 = WidgetHelpTypeWhatsThis
    toEnum k = AnotherWidgetHelpType k

instance P.Ord WidgetHelpType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes WidgetHelpType = '[]
instance O.HasParentTypes WidgetHelpType

foreign import ccall "gtk_widget_help_type_get_type" c_gtk_widget_help_type_get_type :: 
    IO GType

instance B.Types.TypedObject WidgetHelpType where
    glibType = c_gtk_widget_help_type_get_type

instance B.Types.BoxedEnum WidgetHelpType

-- Enum Unit
-- | See also 'GI.Gtk.Objects.PrintSettings.printSettingsSetPaperWidth'.
data Unit = 
      UnitNone
    -- ^ No units.
    | UnitPoints
    -- ^ Dimensions in points.
    | UnitInch
    -- ^ Dimensions in inches.
    | UnitMm
    -- ^ Dimensions in millimeters
    | AnotherUnit Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum Unit where
    fromEnum UnitNone = 0
    fromEnum UnitPoints = 1
    fromEnum UnitInch = 2
    fromEnum UnitMm = 3
    fromEnum (AnotherUnit k) = k

    toEnum 0 = UnitNone
    toEnum 1 = UnitPoints
    toEnum 2 = UnitInch
    toEnum 3 = UnitMm
    toEnum k = AnotherUnit k

instance P.Ord Unit where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes Unit = '[]
instance O.HasParentTypes Unit

foreign import ccall "gtk_unit_get_type" c_gtk_unit_get_type :: 
    IO GType

instance B.Types.TypedObject Unit where
    glibType = c_gtk_unit_get_type

instance B.Types.BoxedEnum Unit

-- Enum TreeViewGridLines
-- | Used to indicate which grid lines to draw in a tree view.
data TreeViewGridLines = 
      TreeViewGridLinesNone
    -- ^ No grid lines.
    | TreeViewGridLinesHorizontal
    -- ^ Horizontal grid lines.
    | TreeViewGridLinesVertical
    -- ^ Vertical grid lines.
    | TreeViewGridLinesBoth
    -- ^ Horizontal and vertical grid lines.
    | AnotherTreeViewGridLines Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum TreeViewGridLines where
    fromEnum TreeViewGridLinesNone = 0
    fromEnum TreeViewGridLinesHorizontal = 1
    fromEnum TreeViewGridLinesVertical = 2
    fromEnum TreeViewGridLinesBoth = 3
    fromEnum (AnotherTreeViewGridLines k) = k

    toEnum 0 = TreeViewGridLinesNone
    toEnum 1 = TreeViewGridLinesHorizontal
    toEnum 2 = TreeViewGridLinesVertical
    toEnum 3 = TreeViewGridLinesBoth
    toEnum k = AnotherTreeViewGridLines k

instance P.Ord TreeViewGridLines where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes TreeViewGridLines = '[]
instance O.HasParentTypes TreeViewGridLines

foreign import ccall "gtk_tree_view_grid_lines_get_type" c_gtk_tree_view_grid_lines_get_type :: 
    IO GType

instance B.Types.TypedObject TreeViewGridLines where
    glibType = c_gtk_tree_view_grid_lines_get_type

instance B.Types.BoxedEnum TreeViewGridLines

-- Enum TreeViewDropPosition
-- | An enum for determining where a dropped row goes.
data TreeViewDropPosition = 
      TreeViewDropPositionBefore
    -- ^ dropped row is inserted before
    | TreeViewDropPositionAfter
    -- ^ dropped row is inserted after
    | TreeViewDropPositionIntoOrBefore
    -- ^ dropped row becomes a child or is inserted before
    | TreeViewDropPositionIntoOrAfter
    -- ^ dropped row becomes a child or is inserted after
    | AnotherTreeViewDropPosition Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum TreeViewDropPosition where
    fromEnum TreeViewDropPositionBefore = 0
    fromEnum TreeViewDropPositionAfter = 1
    fromEnum TreeViewDropPositionIntoOrBefore = 2
    fromEnum TreeViewDropPositionIntoOrAfter = 3
    fromEnum (AnotherTreeViewDropPosition k) = k

    toEnum 0 = TreeViewDropPositionBefore
    toEnum 1 = TreeViewDropPositionAfter
    toEnum 2 = TreeViewDropPositionIntoOrBefore
    toEnum 3 = TreeViewDropPositionIntoOrAfter
    toEnum k = AnotherTreeViewDropPosition k

instance P.Ord TreeViewDropPosition where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes TreeViewDropPosition = '[]
instance O.HasParentTypes TreeViewDropPosition

foreign import ccall "gtk_tree_view_drop_position_get_type" c_gtk_tree_view_drop_position_get_type :: 
    IO GType

instance B.Types.TypedObject TreeViewDropPosition where
    glibType = c_gtk_tree_view_drop_position_get_type

instance B.Types.BoxedEnum TreeViewDropPosition

-- Enum TreeViewColumnSizing
-- | The sizing method the column uses to determine its width.  Please note
-- that /@gTKTREEVIEWCOLUMNAUTOSIZE@/ are inefficient for large views, and
-- can make columns appear choppy.
data TreeViewColumnSizing = 
      TreeViewColumnSizingGrowOnly
    -- ^ Columns only get bigger in reaction to changes in the model
    | TreeViewColumnSizingAutosize
    -- ^ Columns resize to be the optimal size everytime the model changes.
    | TreeViewColumnSizingFixed
    -- ^ Columns are a fixed numbers of pixels wide.
    | AnotherTreeViewColumnSizing Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum TreeViewColumnSizing where
    fromEnum TreeViewColumnSizingGrowOnly = 0
    fromEnum TreeViewColumnSizingAutosize = 1
    fromEnum TreeViewColumnSizingFixed = 2
    fromEnum (AnotherTreeViewColumnSizing k) = k

    toEnum 0 = TreeViewColumnSizingGrowOnly
    toEnum 1 = TreeViewColumnSizingAutosize
    toEnum 2 = TreeViewColumnSizingFixed
    toEnum k = AnotherTreeViewColumnSizing k

instance P.Ord TreeViewColumnSizing where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes TreeViewColumnSizing = '[]
instance O.HasParentTypes TreeViewColumnSizing

foreign import ccall "gtk_tree_view_column_sizing_get_type" c_gtk_tree_view_column_sizing_get_type :: 
    IO GType

instance B.Types.TypedObject TreeViewColumnSizing where
    glibType = c_gtk_tree_view_column_sizing_get_type

instance B.Types.BoxedEnum TreeViewColumnSizing

-- Enum ToolbarStyle
-- | Used to customize the appearance of a t'GI.Gtk.Objects.Toolbar.Toolbar'. Note that
-- setting the toolbar style overrides the user’s preferences
-- for the default toolbar style.  Note that if the button has only
-- a label set and GTK_TOOLBAR_ICONS is used, the label will be
-- visible, and vice versa.
data ToolbarStyle = 
      ToolbarStyleIcons
    -- ^ Buttons display only icons in the toolbar.
    | ToolbarStyleText
    -- ^ Buttons display only text labels in the toolbar.
    | ToolbarStyleBoth
    -- ^ Buttons display text and icons in the toolbar.
    | ToolbarStyleBothHoriz
    -- ^ Buttons display icons and text alongside each
    --  other, rather than vertically stacked
    | AnotherToolbarStyle Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ToolbarStyle where
    fromEnum ToolbarStyleIcons = 0
    fromEnum ToolbarStyleText = 1
    fromEnum ToolbarStyleBoth = 2
    fromEnum ToolbarStyleBothHoriz = 3
    fromEnum (AnotherToolbarStyle k) = k

    toEnum 0 = ToolbarStyleIcons
    toEnum 1 = ToolbarStyleText
    toEnum 2 = ToolbarStyleBoth
    toEnum 3 = ToolbarStyleBothHoriz
    toEnum k = AnotherToolbarStyle k

instance P.Ord ToolbarStyle where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ToolbarStyle = '[]
instance O.HasParentTypes ToolbarStyle

foreign import ccall "gtk_toolbar_style_get_type" c_gtk_toolbar_style_get_type :: 
    IO GType

instance B.Types.TypedObject ToolbarStyle where
    glibType = c_gtk_toolbar_style_get_type

instance B.Types.BoxedEnum ToolbarStyle

-- Enum ToolbarSpaceStyle
{-# DEPRECATED ToolbarSpaceStyle ["(Since version 3.20)"] #-}
-- | Whether spacers are vertical lines or just blank.
data ToolbarSpaceStyle = 
      ToolbarSpaceStyleEmpty
    -- ^ Use blank spacers.
    | ToolbarSpaceStyleLine
    -- ^ Use vertical lines for spacers.
    | AnotherToolbarSpaceStyle Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ToolbarSpaceStyle where
    fromEnum ToolbarSpaceStyleEmpty = 0
    fromEnum ToolbarSpaceStyleLine = 1
    fromEnum (AnotherToolbarSpaceStyle k) = k

    toEnum 0 = ToolbarSpaceStyleEmpty
    toEnum 1 = ToolbarSpaceStyleLine
    toEnum k = AnotherToolbarSpaceStyle k

instance P.Ord ToolbarSpaceStyle where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ToolbarSpaceStyle = '[]
instance O.HasParentTypes ToolbarSpaceStyle

foreign import ccall "gtk_toolbar_space_style_get_type" c_gtk_toolbar_space_style_get_type :: 
    IO GType

instance B.Types.TypedObject ToolbarSpaceStyle where
    glibType = c_gtk_toolbar_space_style_get_type

instance B.Types.BoxedEnum ToolbarSpaceStyle

-- Enum TextWindowType
-- | Used to reference the parts of t'GI.Gtk.Objects.TextView.TextView'.
data TextWindowType = 
      TextWindowTypePrivate
    -- ^ Invalid value, used as a marker
    | TextWindowTypeWidget
    -- ^ Window that floats over scrolling areas.
    | TextWindowTypeText
    -- ^ Scrollable text window.
    | TextWindowTypeLeft
    -- ^ Left side border window.
    | TextWindowTypeRight
    -- ^ Right side border window.
    | TextWindowTypeTop
    -- ^ Top border window.
    | TextWindowTypeBottom
    -- ^ Bottom border window.
    | AnotherTextWindowType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum TextWindowType where
    fromEnum TextWindowTypePrivate = 0
    fromEnum TextWindowTypeWidget = 1
    fromEnum TextWindowTypeText = 2
    fromEnum TextWindowTypeLeft = 3
    fromEnum TextWindowTypeRight = 4
    fromEnum TextWindowTypeTop = 5
    fromEnum TextWindowTypeBottom = 6
    fromEnum (AnotherTextWindowType k) = k

    toEnum 0 = TextWindowTypePrivate
    toEnum 1 = TextWindowTypeWidget
    toEnum 2 = TextWindowTypeText
    toEnum 3 = TextWindowTypeLeft
    toEnum 4 = TextWindowTypeRight
    toEnum 5 = TextWindowTypeTop
    toEnum 6 = TextWindowTypeBottom
    toEnum k = AnotherTextWindowType k

instance P.Ord TextWindowType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes TextWindowType = '[]
instance O.HasParentTypes TextWindowType

foreign import ccall "gtk_text_window_type_get_type" c_gtk_text_window_type_get_type :: 
    IO GType

instance B.Types.TypedObject TextWindowType where
    glibType = c_gtk_text_window_type_get_type

instance B.Types.BoxedEnum TextWindowType

-- Enum TextViewLayer
-- | Used to reference the layers of t'GI.Gtk.Objects.TextView.TextView' for the purpose of customized
-- drawing with the [draw_layer](#g:signal:draw_layer) vfunc.
data TextViewLayer = 
      TextViewLayerBelow
    -- ^ Old deprecated layer, use 'GI.Gtk.Enums.TextViewLayerBelowText' instead
    | TextViewLayerAbove
    -- ^ Old deprecated layer, use 'GI.Gtk.Enums.TextViewLayerAboveText' instead
    | TextViewLayerBelowText
    -- ^ The layer rendered below the text (but above the background).  Since: 3.20
    | TextViewLayerAboveText
    -- ^ The layer rendered above the text.  Since: 3.20
    | AnotherTextViewLayer Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum TextViewLayer where
    fromEnum TextViewLayerBelow = 0
    fromEnum TextViewLayerAbove = 1
    fromEnum TextViewLayerBelowText = 2
    fromEnum TextViewLayerAboveText = 3
    fromEnum (AnotherTextViewLayer k) = k

    toEnum 0 = TextViewLayerBelow
    toEnum 1 = TextViewLayerAbove
    toEnum 2 = TextViewLayerBelowText
    toEnum 3 = TextViewLayerAboveText
    toEnum k = AnotherTextViewLayer k

instance P.Ord TextViewLayer where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes TextViewLayer = '[]
instance O.HasParentTypes TextViewLayer

foreign import ccall "gtk_text_view_layer_get_type" c_gtk_text_view_layer_get_type :: 
    IO GType

instance B.Types.TypedObject TextViewLayer where
    glibType = c_gtk_text_view_layer_get_type

instance B.Types.BoxedEnum TextViewLayer

-- Enum TextExtendSelection
-- | Granularity types that extend the text selection. Use the
-- [TextView::extendSelection]("GI.Gtk.Objects.TextView#g:signal:extendSelection") signal to customize the selection.
-- 
-- /Since: 3.16/
data TextExtendSelection = 
      TextExtendSelectionWord
    -- ^ Selects the current word. It is triggered by
    --   a double-click for example.
    | TextExtendSelectionLine
    -- ^ Selects the current line. It is triggered by
    --   a triple-click for example.
    | AnotherTextExtendSelection Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum TextExtendSelection where
    fromEnum TextExtendSelectionWord = 0
    fromEnum TextExtendSelectionLine = 1
    fromEnum (AnotherTextExtendSelection k) = k

    toEnum 0 = TextExtendSelectionWord
    toEnum 1 = TextExtendSelectionLine
    toEnum k = AnotherTextExtendSelection k

instance P.Ord TextExtendSelection where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes TextExtendSelection = '[]
instance O.HasParentTypes TextExtendSelection

foreign import ccall "gtk_text_extend_selection_get_type" c_gtk_text_extend_selection_get_type :: 
    IO GType

instance B.Types.TypedObject TextExtendSelection where
    glibType = c_gtk_text_extend_selection_get_type

instance B.Types.BoxedEnum TextExtendSelection

-- Enum TextDirection
-- | Reading directions for text.
data TextDirection = 
      TextDirectionNone
    -- ^ No direction.
    | TextDirectionLtr
    -- ^ Left to right text direction.
    | TextDirectionRtl
    -- ^ Right to left text direction.
    | AnotherTextDirection Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum TextDirection where
    fromEnum TextDirectionNone = 0
    fromEnum TextDirectionLtr = 1
    fromEnum TextDirectionRtl = 2
    fromEnum (AnotherTextDirection k) = k

    toEnum 0 = TextDirectionNone
    toEnum 1 = TextDirectionLtr
    toEnum 2 = TextDirectionRtl
    toEnum k = AnotherTextDirection k

instance P.Ord TextDirection where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes TextDirection = '[]
instance O.HasParentTypes TextDirection

foreign import ccall "gtk_text_direction_get_type" c_gtk_text_direction_get_type :: 
    IO GType

instance B.Types.TypedObject TextDirection where
    glibType = c_gtk_text_direction_get_type

instance B.Types.BoxedEnum TextDirection

-- Enum TextBufferTargetInfo
-- | These values are used as “info” for the targets contained in the
-- lists returned by 'GI.Gtk.Objects.TextBuffer.textBufferGetCopyTargetList' and
-- 'GI.Gtk.Objects.TextBuffer.textBufferGetPasteTargetList'.
-- 
-- The values counts down from @-1@ to avoid clashes
-- with application added drag destinations which usually start at 0.
data TextBufferTargetInfo = 
      TextBufferTargetInfoBufferContents
    -- ^ Buffer contents
    | TextBufferTargetInfoRichText
    -- ^ Rich text
    | TextBufferTargetInfoText
    -- ^ Text
    | AnotherTextBufferTargetInfo Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum TextBufferTargetInfo where
    fromEnum TextBufferTargetInfoBufferContents = -1
    fromEnum TextBufferTargetInfoRichText = -2
    fromEnum TextBufferTargetInfoText = -3
    fromEnum (AnotherTextBufferTargetInfo k) = k

    toEnum -1 = TextBufferTargetInfoBufferContents
    toEnum -2 = TextBufferTargetInfoRichText
    toEnum -3 = TextBufferTargetInfoText
    toEnum k = AnotherTextBufferTargetInfo k

instance P.Ord TextBufferTargetInfo where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes TextBufferTargetInfo = '[]
instance O.HasParentTypes TextBufferTargetInfo

foreign import ccall "gtk_text_buffer_target_info_get_type" c_gtk_text_buffer_target_info_get_type :: 
    IO GType

instance B.Types.TypedObject TextBufferTargetInfo where
    glibType = c_gtk_text_buffer_target_info_get_type

instance B.Types.BoxedEnum TextBufferTargetInfo

-- Enum StateType
{-# DEPRECATED StateType ["(Since version 3.14)","All APIs that are using this enumeration have been deprecated","    in favor of alternatives using t'GI.Gtk.Flags.StateFlags'."] #-}
-- | This type indicates the current state of a widget; the state determines how
-- the widget is drawn. The t'GI.Gtk.Enums.StateType' enumeration is also used to
-- identify different colors in a t'GI.Gtk.Objects.Style.Style' for drawing, so states can be
-- used for subparts of a widget as well as entire widgets.
data StateType = 
      StateTypeNormal
    -- ^ State during normal operation.
    | StateTypeActive
    -- ^ State of a currently active widget, such as a depressed button.
    | StateTypePrelight
    -- ^ State indicating that the mouse pointer is over
    --                      the widget and the widget will respond to mouse clicks.
    | StateTypeSelected
    -- ^ State of a selected item, such the selected row in a list.
    | StateTypeInsensitive
    -- ^ State indicating that the widget is
    --                         unresponsive to user actions.
    | StateTypeInconsistent
    -- ^ The widget is inconsistent, such as checkbuttons
    --                          or radiobuttons that aren’t either set to 'P.True' nor 'P.False',
    --                          or buttons requiring the user attention.
    | StateTypeFocused
    -- ^ The widget has the keyboard focus.
    | AnotherStateType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum StateType where
    fromEnum StateTypeNormal = 0
    fromEnum StateTypeActive = 1
    fromEnum StateTypePrelight = 2
    fromEnum StateTypeSelected = 3
    fromEnum StateTypeInsensitive = 4
    fromEnum StateTypeInconsistent = 5
    fromEnum StateTypeFocused = 6
    fromEnum (AnotherStateType k) = k

    toEnum 0 = StateTypeNormal
    toEnum 1 = StateTypeActive
    toEnum 2 = StateTypePrelight
    toEnum 3 = StateTypeSelected
    toEnum 4 = StateTypeInsensitive
    toEnum 5 = StateTypeInconsistent
    toEnum 6 = StateTypeFocused
    toEnum k = AnotherStateType k

instance P.Ord StateType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes StateType = '[]
instance O.HasParentTypes StateType

foreign import ccall "gtk_state_type_get_type" c_gtk_state_type_get_type :: 
    IO GType

instance B.Types.TypedObject StateType where
    glibType = c_gtk_state_type_get_type

instance B.Types.BoxedEnum StateType

-- Enum StackTransitionType
-- | These enumeration values describe the possible transitions
-- between pages in a t'GI.Gtk.Objects.Stack.Stack' widget.
-- 
-- New values may be added to this enumeration over time.
data StackTransitionType = 
      StackTransitionTypeNone
    -- ^ No transition
    | StackTransitionTypeCrossfade
    -- ^ A cross-fade
    | StackTransitionTypeSlideRight
    -- ^ Slide from left to right
    | StackTransitionTypeSlideLeft
    -- ^ Slide from right to left
    | StackTransitionTypeSlideUp
    -- ^ Slide from bottom up
    | StackTransitionTypeSlideDown
    -- ^ Slide from top down
    | StackTransitionTypeSlideLeftRight
    -- ^ Slide from left or right according to the children order
    | StackTransitionTypeSlideUpDown
    -- ^ Slide from top down or bottom up according to the order
    | StackTransitionTypeOverUp
    -- ^ Cover the old page by sliding up. Since 3.12
    | StackTransitionTypeOverDown
    -- ^ Cover the old page by sliding down. Since: 3.12
    | StackTransitionTypeOverLeft
    -- ^ Cover the old page by sliding to the left. Since: 3.12
    | StackTransitionTypeOverRight
    -- ^ Cover the old page by sliding to the right. Since: 3.12
    | StackTransitionTypeUnderUp
    -- ^ Uncover the new page by sliding up. Since 3.12
    | StackTransitionTypeUnderDown
    -- ^ Uncover the new page by sliding down. Since: 3.12
    | StackTransitionTypeUnderLeft
    -- ^ Uncover the new page by sliding to the left. Since: 3.12
    | StackTransitionTypeUnderRight
    -- ^ Uncover the new page by sliding to the right. Since: 3.12
    | StackTransitionTypeOverUpDown
    -- ^ Cover the old page sliding up or uncover the new page sliding down, according to order. Since: 3.12
    | StackTransitionTypeOverDownUp
    -- ^ Cover the old page sliding down or uncover the new page sliding up, according to order. Since: 3.14
    | StackTransitionTypeOverLeftRight
    -- ^ Cover the old page sliding left or uncover the new page sliding right, according to order. Since: 3.14
    | StackTransitionTypeOverRightLeft
    -- ^ Cover the old page sliding right or uncover the new page sliding left, according to order. Since: 3.14
    | AnotherStackTransitionType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum StackTransitionType where
    fromEnum StackTransitionTypeNone = 0
    fromEnum StackTransitionTypeCrossfade = 1
    fromEnum StackTransitionTypeSlideRight = 2
    fromEnum StackTransitionTypeSlideLeft = 3
    fromEnum StackTransitionTypeSlideUp = 4
    fromEnum StackTransitionTypeSlideDown = 5
    fromEnum StackTransitionTypeSlideLeftRight = 6
    fromEnum StackTransitionTypeSlideUpDown = 7
    fromEnum StackTransitionTypeOverUp = 8
    fromEnum StackTransitionTypeOverDown = 9
    fromEnum StackTransitionTypeOverLeft = 10
    fromEnum StackTransitionTypeOverRight = 11
    fromEnum StackTransitionTypeUnderUp = 12
    fromEnum StackTransitionTypeUnderDown = 13
    fromEnum StackTransitionTypeUnderLeft = 14
    fromEnum StackTransitionTypeUnderRight = 15
    fromEnum StackTransitionTypeOverUpDown = 16
    fromEnum StackTransitionTypeOverDownUp = 17
    fromEnum StackTransitionTypeOverLeftRight = 18
    fromEnum StackTransitionTypeOverRightLeft = 19
    fromEnum (AnotherStackTransitionType k) = k

    toEnum 0 = StackTransitionTypeNone
    toEnum 1 = StackTransitionTypeCrossfade
    toEnum 2 = StackTransitionTypeSlideRight
    toEnum 3 = StackTransitionTypeSlideLeft
    toEnum 4 = StackTransitionTypeSlideUp
    toEnum 5 = StackTransitionTypeSlideDown
    toEnum 6 = StackTransitionTypeSlideLeftRight
    toEnum 7 = StackTransitionTypeSlideUpDown
    toEnum 8 = StackTransitionTypeOverUp
    toEnum 9 = StackTransitionTypeOverDown
    toEnum 10 = StackTransitionTypeOverLeft
    toEnum 11 = StackTransitionTypeOverRight
    toEnum 12 = StackTransitionTypeUnderUp
    toEnum 13 = StackTransitionTypeUnderDown
    toEnum 14 = StackTransitionTypeUnderLeft
    toEnum 15 = StackTransitionTypeUnderRight
    toEnum 16 = StackTransitionTypeOverUpDown
    toEnum 17 = StackTransitionTypeOverDownUp
    toEnum 18 = StackTransitionTypeOverLeftRight
    toEnum 19 = StackTransitionTypeOverRightLeft
    toEnum k = AnotherStackTransitionType k

instance P.Ord StackTransitionType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes StackTransitionType = '[]
instance O.HasParentTypes StackTransitionType

foreign import ccall "gtk_stack_transition_type_get_type" c_gtk_stack_transition_type_get_type :: 
    IO GType

instance B.Types.TypedObject StackTransitionType where
    glibType = c_gtk_stack_transition_type_get_type

instance B.Types.BoxedEnum StackTransitionType

-- Enum SpinType
-- | The values of the GtkSpinType enumeration are used to specify the
-- change to make in 'GI.Gtk.Objects.SpinButton.spinButtonSpin'.
data SpinType = 
      SpinTypeStepForward
    -- ^ Increment by the adjustments step increment.
    | SpinTypeStepBackward
    -- ^ Decrement by the adjustments step increment.
    | SpinTypePageForward
    -- ^ Increment by the adjustments page increment.
    | SpinTypePageBackward
    -- ^ Decrement by the adjustments page increment.
    | SpinTypeHome
    -- ^ Go to the adjustments lower bound.
    | SpinTypeEnd
    -- ^ Go to the adjustments upper bound.
    | SpinTypeUserDefined
    -- ^ Change by a specified amount.
    | AnotherSpinType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum SpinType where
    fromEnum SpinTypeStepForward = 0
    fromEnum SpinTypeStepBackward = 1
    fromEnum SpinTypePageForward = 2
    fromEnum SpinTypePageBackward = 3
    fromEnum SpinTypeHome = 4
    fromEnum SpinTypeEnd = 5
    fromEnum SpinTypeUserDefined = 6
    fromEnum (AnotherSpinType k) = k

    toEnum 0 = SpinTypeStepForward
    toEnum 1 = SpinTypeStepBackward
    toEnum 2 = SpinTypePageForward
    toEnum 3 = SpinTypePageBackward
    toEnum 4 = SpinTypeHome
    toEnum 5 = SpinTypeEnd
    toEnum 6 = SpinTypeUserDefined
    toEnum k = AnotherSpinType k

instance P.Ord SpinType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes SpinType = '[]
instance O.HasParentTypes SpinType

foreign import ccall "gtk_spin_type_get_type" c_gtk_spin_type_get_type :: 
    IO GType

instance B.Types.TypedObject SpinType where
    glibType = c_gtk_spin_type_get_type

instance B.Types.BoxedEnum SpinType

-- Enum SpinButtonUpdatePolicy
-- | The spin button update policy determines whether the spin button displays
-- values even if they are outside the bounds of its adjustment.
-- See 'GI.Gtk.Objects.SpinButton.spinButtonSetUpdatePolicy'.
data SpinButtonUpdatePolicy = 
      SpinButtonUpdatePolicyAlways
    -- ^ When refreshing your t'GI.Gtk.Objects.SpinButton.SpinButton', the value is
    --     always displayed
    | SpinButtonUpdatePolicyIfValid
    -- ^ When refreshing your t'GI.Gtk.Objects.SpinButton.SpinButton', the value is
    --     only displayed if it is valid within the bounds of the spin button\'s
    --     adjustment
    | AnotherSpinButtonUpdatePolicy Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum SpinButtonUpdatePolicy where
    fromEnum SpinButtonUpdatePolicyAlways = 0
    fromEnum SpinButtonUpdatePolicyIfValid = 1
    fromEnum (AnotherSpinButtonUpdatePolicy k) = k

    toEnum 0 = SpinButtonUpdatePolicyAlways
    toEnum 1 = SpinButtonUpdatePolicyIfValid
    toEnum k = AnotherSpinButtonUpdatePolicy k

instance P.Ord SpinButtonUpdatePolicy where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes SpinButtonUpdatePolicy = '[]
instance O.HasParentTypes SpinButtonUpdatePolicy

foreign import ccall "gtk_spin_button_update_policy_get_type" c_gtk_spin_button_update_policy_get_type :: 
    IO GType

instance B.Types.TypedObject SpinButtonUpdatePolicy where
    glibType = c_gtk_spin_button_update_policy_get_type

instance B.Types.BoxedEnum SpinButtonUpdatePolicy

-- Enum SortType
-- | Determines the direction of a sort.
data SortType = 
      SortTypeAscending
    -- ^ Sorting is in ascending order.
    | SortTypeDescending
    -- ^ Sorting is in descending order.
    | AnotherSortType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum SortType where
    fromEnum SortTypeAscending = 0
    fromEnum SortTypeDescending = 1
    fromEnum (AnotherSortType k) = k

    toEnum 0 = SortTypeAscending
    toEnum 1 = SortTypeDescending
    toEnum k = AnotherSortType k

instance P.Ord SortType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes SortType = '[]
instance O.HasParentTypes SortType

foreign import ccall "gtk_sort_type_get_type" c_gtk_sort_type_get_type :: 
    IO GType

instance B.Types.TypedObject SortType where
    glibType = c_gtk_sort_type_get_type

instance B.Types.BoxedEnum SortType

-- Enum SizeRequestMode
-- | Specifies a preference for height-for-width or
-- width-for-height geometry management.
data SizeRequestMode = 
      SizeRequestModeHeightForWidth
    -- ^ Prefer height-for-width geometry management
    | SizeRequestModeWidthForHeight
    -- ^ Prefer width-for-height geometry management
    | SizeRequestModeConstantSize
    -- ^ Don’t trade height-for-width or width-for-height
    | AnotherSizeRequestMode Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum SizeRequestMode where
    fromEnum SizeRequestModeHeightForWidth = 0
    fromEnum SizeRequestModeWidthForHeight = 1
    fromEnum SizeRequestModeConstantSize = 2
    fromEnum (AnotherSizeRequestMode k) = k

    toEnum 0 = SizeRequestModeHeightForWidth
    toEnum 1 = SizeRequestModeWidthForHeight
    toEnum 2 = SizeRequestModeConstantSize
    toEnum k = AnotherSizeRequestMode k

instance P.Ord SizeRequestMode where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes SizeRequestMode = '[]
instance O.HasParentTypes SizeRequestMode

foreign import ccall "gtk_size_request_mode_get_type" c_gtk_size_request_mode_get_type :: 
    IO GType

instance B.Types.TypedObject SizeRequestMode where
    glibType = c_gtk_size_request_mode_get_type

instance B.Types.BoxedEnum SizeRequestMode

-- Enum SizeGroupMode
-- | The mode of the size group determines the directions in which the size
-- group affects the requested sizes of its component widgets.
data SizeGroupMode = 
      SizeGroupModeNone
    -- ^ group has no effect
    | SizeGroupModeHorizontal
    -- ^ group affects horizontal requisition
    | SizeGroupModeVertical
    -- ^ group affects vertical requisition
    | SizeGroupModeBoth
    -- ^ group affects both horizontal and vertical requisition
    | AnotherSizeGroupMode Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum SizeGroupMode where
    fromEnum SizeGroupModeNone = 0
    fromEnum SizeGroupModeHorizontal = 1
    fromEnum SizeGroupModeVertical = 2
    fromEnum SizeGroupModeBoth = 3
    fromEnum (AnotherSizeGroupMode k) = k

    toEnum 0 = SizeGroupModeNone
    toEnum 1 = SizeGroupModeHorizontal
    toEnum 2 = SizeGroupModeVertical
    toEnum 3 = SizeGroupModeBoth
    toEnum k = AnotherSizeGroupMode k

instance P.Ord SizeGroupMode where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes SizeGroupMode = '[]
instance O.HasParentTypes SizeGroupMode

foreign import ccall "gtk_size_group_mode_get_type" c_gtk_size_group_mode_get_type :: 
    IO GType

instance B.Types.TypedObject SizeGroupMode where
    glibType = c_gtk_size_group_mode_get_type

instance B.Types.BoxedEnum SizeGroupMode

-- Enum ShortcutType
-- | GtkShortcutType specifies the kind of shortcut that is being described.
-- More values may be added to this enumeration over time.
-- 
-- /Since: 3.20/
data ShortcutType = 
      ShortcutTypeAccelerator
    -- ^ The shortcut is a keyboard accelerator. The [ShortcutsShortcut:accelerator]("GI.Gtk.Objects.ShortcutsShortcut#g:attr:accelerator")
    --   property will be used.
    | ShortcutTypeGesturePinch
    -- ^ The shortcut is a pinch gesture. GTK+ provides an icon and subtitle.
    | ShortcutTypeGestureStretch
    -- ^ The shortcut is a stretch gesture. GTK+ provides an icon and subtitle.
    | ShortcutTypeGestureRotateClockwise
    -- ^ The shortcut is a clockwise rotation gesture. GTK+ provides an icon and subtitle.
    | ShortcutTypeGestureRotateCounterclockwise
    -- ^ The shortcut is a counterclockwise rotation gesture. GTK+ provides an icon and subtitle.
    | ShortcutTypeGestureTwoFingerSwipeLeft
    -- ^ The shortcut is a two-finger swipe gesture. GTK+ provides an icon and subtitle.
    | ShortcutTypeGestureTwoFingerSwipeRight
    -- ^ The shortcut is a two-finger swipe gesture. GTK+ provides an icon and subtitle.
    | ShortcutTypeGesture
    -- ^ The shortcut is a gesture. The [ShortcutsShortcut:icon]("GI.Gtk.Objects.ShortcutsShortcut#g:attr:icon") property will be
    --   used.
    | AnotherShortcutType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ShortcutType where
    fromEnum ShortcutTypeAccelerator = 0
    fromEnum ShortcutTypeGesturePinch = 1
    fromEnum ShortcutTypeGestureStretch = 2
    fromEnum ShortcutTypeGestureRotateClockwise = 3
    fromEnum ShortcutTypeGestureRotateCounterclockwise = 4
    fromEnum ShortcutTypeGestureTwoFingerSwipeLeft = 5
    fromEnum ShortcutTypeGestureTwoFingerSwipeRight = 6
    fromEnum ShortcutTypeGesture = 7
    fromEnum (AnotherShortcutType k) = k

    toEnum 0 = ShortcutTypeAccelerator
    toEnum 1 = ShortcutTypeGesturePinch
    toEnum 2 = ShortcutTypeGestureStretch
    toEnum 3 = ShortcutTypeGestureRotateClockwise
    toEnum 4 = ShortcutTypeGestureRotateCounterclockwise
    toEnum 5 = ShortcutTypeGestureTwoFingerSwipeLeft
    toEnum 6 = ShortcutTypeGestureTwoFingerSwipeRight
    toEnum 7 = ShortcutTypeGesture
    toEnum k = AnotherShortcutType k

instance P.Ord ShortcutType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ShortcutType = '[]
instance O.HasParentTypes ShortcutType

foreign import ccall "gtk_shortcut_type_get_type" c_gtk_shortcut_type_get_type :: 
    IO GType

instance B.Types.TypedObject ShortcutType where
    glibType = c_gtk_shortcut_type_get_type

instance B.Types.BoxedEnum ShortcutType

-- Enum ShadowType
-- | Used to change the appearance of an outline typically provided by a t'GI.Gtk.Objects.Frame.Frame'.
-- 
-- Note that many themes do not differentiate the appearance of the
-- various shadow types: Either their is no visible shadow (/@gTKSHADOWNONE@/),
-- or there is (any other value).
data ShadowType = 
      ShadowTypeNone
    -- ^ No outline.
    | ShadowTypeIn
    -- ^ The outline is bevelled inwards.
    | ShadowTypeOut
    -- ^ The outline is bevelled outwards like a button.
    | ShadowTypeEtchedIn
    -- ^ The outline has a sunken 3d appearance.
    | ShadowTypeEtchedOut
    -- ^ The outline has a raised 3d appearance.
    | AnotherShadowType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ShadowType where
    fromEnum ShadowTypeNone = 0
    fromEnum ShadowTypeIn = 1
    fromEnum ShadowTypeOut = 2
    fromEnum ShadowTypeEtchedIn = 3
    fromEnum ShadowTypeEtchedOut = 4
    fromEnum (AnotherShadowType k) = k

    toEnum 0 = ShadowTypeNone
    toEnum 1 = ShadowTypeIn
    toEnum 2 = ShadowTypeOut
    toEnum 3 = ShadowTypeEtchedIn
    toEnum 4 = ShadowTypeEtchedOut
    toEnum k = AnotherShadowType k

instance P.Ord ShadowType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ShadowType = '[]
instance O.HasParentTypes ShadowType

foreign import ccall "gtk_shadow_type_get_type" c_gtk_shadow_type_get_type :: 
    IO GType

instance B.Types.TypedObject ShadowType where
    glibType = c_gtk_shadow_type_get_type

instance B.Types.BoxedEnum ShadowType

-- Enum SensitivityType
-- | Determines how GTK+ handles the sensitivity of stepper arrows
-- at the end of range widgets.
data SensitivityType = 
      SensitivityTypeAuto
    -- ^ The arrow is made insensitive if the
    --   thumb is at the end
    | SensitivityTypeOn
    -- ^ The arrow is always sensitive
    | SensitivityTypeOff
    -- ^ The arrow is always insensitive
    | AnotherSensitivityType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum SensitivityType where
    fromEnum SensitivityTypeAuto = 0
    fromEnum SensitivityTypeOn = 1
    fromEnum SensitivityTypeOff = 2
    fromEnum (AnotherSensitivityType k) = k

    toEnum 0 = SensitivityTypeAuto
    toEnum 1 = SensitivityTypeOn
    toEnum 2 = SensitivityTypeOff
    toEnum k = AnotherSensitivityType k

instance P.Ord SensitivityType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes SensitivityType = '[]
instance O.HasParentTypes SensitivityType

foreign import ccall "gtk_sensitivity_type_get_type" c_gtk_sensitivity_type_get_type :: 
    IO GType

instance B.Types.TypedObject SensitivityType where
    glibType = c_gtk_sensitivity_type_get_type

instance B.Types.BoxedEnum SensitivityType

-- Enum SelectionMode
-- | Used to control what selections users are allowed to make.
data SelectionMode = 
      SelectionModeNone
    -- ^ No selection is possible.
    | SelectionModeSingle
    -- ^ Zero or one element may be selected.
    | SelectionModeBrowse
    -- ^ Exactly one element is selected.
    --     In some circumstances, such as initially or during a search
    --     operation, it’s possible for no element to be selected with
    --     'GI.Gtk.Enums.SelectionModeBrowse'. What is really enforced is that the user
    --     can’t deselect a currently selected element except by selecting
    --     another element.
    | SelectionModeMultiple
    -- ^ Any number of elements may be selected.
    --      The Ctrl key may be used to enlarge the selection, and Shift
    --      key to select between the focus and the child pointed to.
    --      Some widgets may also allow Click-drag to select a range of elements.
    | AnotherSelectionMode Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum SelectionMode where
    fromEnum SelectionModeNone = 0
    fromEnum SelectionModeSingle = 1
    fromEnum SelectionModeBrowse = 2
    fromEnum SelectionModeMultiple = 3
    fromEnum (AnotherSelectionMode k) = k

    toEnum 0 = SelectionModeNone
    toEnum 1 = SelectionModeSingle
    toEnum 2 = SelectionModeBrowse
    toEnum 3 = SelectionModeMultiple
    toEnum k = AnotherSelectionMode k

instance P.Ord SelectionMode where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes SelectionMode = '[]
instance O.HasParentTypes SelectionMode

foreign import ccall "gtk_selection_mode_get_type" c_gtk_selection_mode_get_type :: 
    IO GType

instance B.Types.TypedObject SelectionMode where
    glibType = c_gtk_selection_mode_get_type

instance B.Types.BoxedEnum SelectionMode

-- Enum ScrollablePolicy
-- | Defines the policy to be used in a scrollable widget when updating
-- the scrolled window adjustments in a given orientation.
data ScrollablePolicy = 
      ScrollablePolicyMinimum
    -- ^ Scrollable adjustments are based on the minimum size
    | ScrollablePolicyNatural
    -- ^ Scrollable adjustments are based on the natural size
    | AnotherScrollablePolicy Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ScrollablePolicy where
    fromEnum ScrollablePolicyMinimum = 0
    fromEnum ScrollablePolicyNatural = 1
    fromEnum (AnotherScrollablePolicy k) = k

    toEnum 0 = ScrollablePolicyMinimum
    toEnum 1 = ScrollablePolicyNatural
    toEnum k = AnotherScrollablePolicy k

instance P.Ord ScrollablePolicy where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ScrollablePolicy = '[]
instance O.HasParentTypes ScrollablePolicy

foreign import ccall "gtk_scrollable_policy_get_type" c_gtk_scrollable_policy_get_type :: 
    IO GType

instance B.Types.TypedObject ScrollablePolicy where
    glibType = c_gtk_scrollable_policy_get_type

instance B.Types.BoxedEnum ScrollablePolicy

-- Enum ScrollType
-- | Scrolling types.
data ScrollType = 
      ScrollTypeNone
    -- ^ No scrolling.
    | ScrollTypeJump
    -- ^ Jump to new location.
    | ScrollTypeStepBackward
    -- ^ Step backward.
    | ScrollTypeStepForward
    -- ^ Step forward.
    | ScrollTypePageBackward
    -- ^ Page backward.
    | ScrollTypePageForward
    -- ^ Page forward.
    | ScrollTypeStepUp
    -- ^ Step up.
    | ScrollTypeStepDown
    -- ^ Step down.
    | ScrollTypePageUp
    -- ^ Page up.
    | ScrollTypePageDown
    -- ^ Page down.
    | ScrollTypeStepLeft
    -- ^ Step to the left.
    | ScrollTypeStepRight
    -- ^ Step to the right.
    | ScrollTypePageLeft
    -- ^ Page to the left.
    | ScrollTypePageRight
    -- ^ Page to the right.
    | ScrollTypeStart
    -- ^ Scroll to start.
    | ScrollTypeEnd
    -- ^ Scroll to end.
    | AnotherScrollType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ScrollType where
    fromEnum ScrollTypeNone = 0
    fromEnum ScrollTypeJump = 1
    fromEnum ScrollTypeStepBackward = 2
    fromEnum ScrollTypeStepForward = 3
    fromEnum ScrollTypePageBackward = 4
    fromEnum ScrollTypePageForward = 5
    fromEnum ScrollTypeStepUp = 6
    fromEnum ScrollTypeStepDown = 7
    fromEnum ScrollTypePageUp = 8
    fromEnum ScrollTypePageDown = 9
    fromEnum ScrollTypeStepLeft = 10
    fromEnum ScrollTypeStepRight = 11
    fromEnum ScrollTypePageLeft = 12
    fromEnum ScrollTypePageRight = 13
    fromEnum ScrollTypeStart = 14
    fromEnum ScrollTypeEnd = 15
    fromEnum (AnotherScrollType k) = k

    toEnum 0 = ScrollTypeNone
    toEnum 1 = ScrollTypeJump
    toEnum 2 = ScrollTypeStepBackward
    toEnum 3 = ScrollTypeStepForward
    toEnum 4 = ScrollTypePageBackward
    toEnum 5 = ScrollTypePageForward
    toEnum 6 = ScrollTypeStepUp
    toEnum 7 = ScrollTypeStepDown
    toEnum 8 = ScrollTypePageUp
    toEnum 9 = ScrollTypePageDown
    toEnum 10 = ScrollTypeStepLeft
    toEnum 11 = ScrollTypeStepRight
    toEnum 12 = ScrollTypePageLeft
    toEnum 13 = ScrollTypePageRight
    toEnum 14 = ScrollTypeStart
    toEnum 15 = ScrollTypeEnd
    toEnum k = AnotherScrollType k

instance P.Ord ScrollType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ScrollType = '[]
instance O.HasParentTypes ScrollType

foreign import ccall "gtk_scroll_type_get_type" c_gtk_scroll_type_get_type :: 
    IO GType

instance B.Types.TypedObject ScrollType where
    glibType = c_gtk_scroll_type_get_type

instance B.Types.BoxedEnum ScrollType

-- Enum ScrollStep
-- | /No description available in the introspection data./
data ScrollStep = 
      ScrollStepSteps
    -- ^ Scroll in steps.
    | ScrollStepPages
    -- ^ Scroll by pages.
    | ScrollStepEnds
    -- ^ Scroll to ends.
    | ScrollStepHorizontalSteps
    -- ^ Scroll in horizontal steps.
    | ScrollStepHorizontalPages
    -- ^ Scroll by horizontal pages.
    | ScrollStepHorizontalEnds
    -- ^ Scroll to the horizontal ends.
    | AnotherScrollStep Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ScrollStep where
    fromEnum ScrollStepSteps = 0
    fromEnum ScrollStepPages = 1
    fromEnum ScrollStepEnds = 2
    fromEnum ScrollStepHorizontalSteps = 3
    fromEnum ScrollStepHorizontalPages = 4
    fromEnum ScrollStepHorizontalEnds = 5
    fromEnum (AnotherScrollStep k) = k

    toEnum 0 = ScrollStepSteps
    toEnum 1 = ScrollStepPages
    toEnum 2 = ScrollStepEnds
    toEnum 3 = ScrollStepHorizontalSteps
    toEnum 4 = ScrollStepHorizontalPages
    toEnum 5 = ScrollStepHorizontalEnds
    toEnum k = AnotherScrollStep k

instance P.Ord ScrollStep where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ScrollStep = '[]
instance O.HasParentTypes ScrollStep

foreign import ccall "gtk_scroll_step_get_type" c_gtk_scroll_step_get_type :: 
    IO GType

instance B.Types.TypedObject ScrollStep where
    glibType = c_gtk_scroll_step_get_type

instance B.Types.BoxedEnum ScrollStep

-- Enum RevealerTransitionType
-- | These enumeration values describe the possible transitions
-- when the child of a t'GI.Gtk.Objects.Revealer.Revealer' widget is shown or hidden.
data RevealerTransitionType = 
      RevealerTransitionTypeNone
    -- ^ No transition
    | RevealerTransitionTypeCrossfade
    -- ^ Fade in
    | RevealerTransitionTypeSlideRight
    -- ^ Slide in from the left
    | RevealerTransitionTypeSlideLeft
    -- ^ Slide in from the right
    | RevealerTransitionTypeSlideUp
    -- ^ Slide in from the bottom
    | RevealerTransitionTypeSlideDown
    -- ^ Slide in from the top
    | AnotherRevealerTransitionType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum RevealerTransitionType where
    fromEnum RevealerTransitionTypeNone = 0
    fromEnum RevealerTransitionTypeCrossfade = 1
    fromEnum RevealerTransitionTypeSlideRight = 2
    fromEnum RevealerTransitionTypeSlideLeft = 3
    fromEnum RevealerTransitionTypeSlideUp = 4
    fromEnum RevealerTransitionTypeSlideDown = 5
    fromEnum (AnotherRevealerTransitionType k) = k

    toEnum 0 = RevealerTransitionTypeNone
    toEnum 1 = RevealerTransitionTypeCrossfade
    toEnum 2 = RevealerTransitionTypeSlideRight
    toEnum 3 = RevealerTransitionTypeSlideLeft
    toEnum 4 = RevealerTransitionTypeSlideUp
    toEnum 5 = RevealerTransitionTypeSlideDown
    toEnum k = AnotherRevealerTransitionType k

instance P.Ord RevealerTransitionType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes RevealerTransitionType = '[]
instance O.HasParentTypes RevealerTransitionType

foreign import ccall "gtk_revealer_transition_type_get_type" c_gtk_revealer_transition_type_get_type :: 
    IO GType

instance B.Types.TypedObject RevealerTransitionType where
    glibType = c_gtk_revealer_transition_type_get_type

instance B.Types.BoxedEnum RevealerTransitionType

-- Enum ResponseType
-- | Predefined values for use as response ids in 'GI.Gtk.Objects.Dialog.dialogAddButton'.
-- All predefined values are negative; GTK+ leaves values of 0 or greater for
-- application-defined response ids.
data ResponseType = 
      ResponseTypeNone
    -- ^ Returned if an action widget has no response id,
    --     or if the dialog gets programmatically hidden or destroyed
    | ResponseTypeReject
    -- ^ Generic response id, not used by GTK+ dialogs
    | ResponseTypeAccept
    -- ^ Generic response id, not used by GTK+ dialogs
    | ResponseTypeDeleteEvent
    -- ^ Returned if the dialog is deleted
    | ResponseTypeOk
    -- ^ Returned by OK buttons in GTK+ dialogs
    | ResponseTypeCancel
    -- ^ Returned by Cancel buttons in GTK+ dialogs
    | ResponseTypeClose
    -- ^ Returned by Close buttons in GTK+ dialogs
    | ResponseTypeYes
    -- ^ Returned by Yes buttons in GTK+ dialogs
    | ResponseTypeNo
    -- ^ Returned by No buttons in GTK+ dialogs
    | ResponseTypeApply
    -- ^ Returned by Apply buttons in GTK+ dialogs
    | ResponseTypeHelp
    -- ^ Returned by Help buttons in GTK+ dialogs
    | AnotherResponseType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ResponseType where
    fromEnum ResponseTypeNone = -1
    fromEnum ResponseTypeReject = -2
    fromEnum ResponseTypeAccept = -3
    fromEnum ResponseTypeDeleteEvent = -4
    fromEnum ResponseTypeOk = -5
    fromEnum ResponseTypeCancel = -6
    fromEnum ResponseTypeClose = -7
    fromEnum ResponseTypeYes = -8
    fromEnum ResponseTypeNo = -9
    fromEnum ResponseTypeApply = -10
    fromEnum ResponseTypeHelp = -11
    fromEnum (AnotherResponseType k) = k

    toEnum -1 = ResponseTypeNone
    toEnum -2 = ResponseTypeReject
    toEnum -3 = ResponseTypeAccept
    toEnum -4 = ResponseTypeDeleteEvent
    toEnum -5 = ResponseTypeOk
    toEnum -6 = ResponseTypeCancel
    toEnum -7 = ResponseTypeClose
    toEnum -8 = ResponseTypeYes
    toEnum -9 = ResponseTypeNo
    toEnum -10 = ResponseTypeApply
    toEnum -11 = ResponseTypeHelp
    toEnum k = AnotherResponseType k

instance P.Ord ResponseType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ResponseType = '[]
instance O.HasParentTypes ResponseType

foreign import ccall "gtk_response_type_get_type" c_gtk_response_type_get_type :: 
    IO GType

instance B.Types.TypedObject ResponseType where
    glibType = c_gtk_response_type_get_type

instance B.Types.BoxedEnum ResponseType

-- Enum ResizeMode
-- | /No description available in the introspection data./
data ResizeMode = 
      ResizeModeParent
    -- ^ Pass resize request to the parent
    | ResizeModeQueue
    -- ^ Queue resizes on this widget
    | ResizeModeImmediate
    -- ^ Resize immediately. Deprecated.
    | AnotherResizeMode Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ResizeMode where
    fromEnum ResizeModeParent = 0
    fromEnum ResizeModeQueue = 1
    fromEnum ResizeModeImmediate = 2
    fromEnum (AnotherResizeMode k) = k

    toEnum 0 = ResizeModeParent
    toEnum 1 = ResizeModeQueue
    toEnum 2 = ResizeModeImmediate
    toEnum k = AnotherResizeMode k

instance P.Ord ResizeMode where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ResizeMode = '[]
instance O.HasParentTypes ResizeMode

foreign import ccall "gtk_resize_mode_get_type" c_gtk_resize_mode_get_type :: 
    IO GType

instance B.Types.TypedObject ResizeMode where
    glibType = c_gtk_resize_mode_get_type

instance B.Types.BoxedEnum ResizeMode

-- Enum ReliefStyle
-- | Indicated the relief to be drawn around a t'GI.Gtk.Objects.Button.Button'.
data ReliefStyle = 
      ReliefStyleNormal
    -- ^ Draw a normal relief.
    | ReliefStyleHalf
    -- ^ A half relief. Deprecated in 3.14, does the same as /@gTKRELIEFNORMAL@/
    | ReliefStyleNone
    -- ^ No relief.
    | AnotherReliefStyle Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ReliefStyle where
    fromEnum ReliefStyleNormal = 0
    fromEnum ReliefStyleHalf = 1
    fromEnum ReliefStyleNone = 2
    fromEnum (AnotherReliefStyle k) = k

    toEnum 0 = ReliefStyleNormal
    toEnum 1 = ReliefStyleHalf
    toEnum 2 = ReliefStyleNone
    toEnum k = AnotherReliefStyle k

instance P.Ord ReliefStyle where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ReliefStyle = '[]
instance O.HasParentTypes ReliefStyle

foreign import ccall "gtk_relief_style_get_type" c_gtk_relief_style_get_type :: 
    IO GType

instance B.Types.TypedObject ReliefStyle where
    glibType = c_gtk_relief_style_get_type

instance B.Types.BoxedEnum ReliefStyle

-- Enum RecentSortType
-- | Used to specify the sorting method to be applyed to the recently
-- used resource list.
-- 
-- /Since: 2.10/
data RecentSortType = 
      RecentSortTypeNone
    -- ^ Do not sort the returned list of recently used
    --   resources.
    | RecentSortTypeMru
    -- ^ Sort the returned list with the most recently used
    --   items first.
    | RecentSortTypeLru
    -- ^ Sort the returned list with the least recently used
    --   items first.
    | RecentSortTypeCustom
    -- ^ Sort the returned list using a custom sorting
    --   function passed using 'GI.Gtk.Interfaces.RecentChooser.recentChooserSetSortFunc'.
    | AnotherRecentSortType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum RecentSortType where
    fromEnum RecentSortTypeNone = 0
    fromEnum RecentSortTypeMru = 1
    fromEnum RecentSortTypeLru = 2
    fromEnum RecentSortTypeCustom = 3
    fromEnum (AnotherRecentSortType k) = k

    toEnum 0 = RecentSortTypeNone
    toEnum 1 = RecentSortTypeMru
    toEnum 2 = RecentSortTypeLru
    toEnum 3 = RecentSortTypeCustom
    toEnum k = AnotherRecentSortType k

instance P.Ord RecentSortType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes RecentSortType = '[]
instance O.HasParentTypes RecentSortType

foreign import ccall "gtk_recent_sort_type_get_type" c_gtk_recent_sort_type_get_type :: 
    IO GType

instance B.Types.TypedObject RecentSortType where
    glibType = c_gtk_recent_sort_type_get_type

instance B.Types.BoxedEnum RecentSortType

-- Enum RecentManagerError
-- | Error codes for t'GI.Gtk.Objects.RecentManager.RecentManager' operations
-- 
-- /Since: 2.10/
data RecentManagerError = 
      RecentManagerErrorNotFound
    -- ^ the URI specified does not exists in
    --   the recently used resources list.
    | RecentManagerErrorInvalidUri
    -- ^ the URI specified is not valid.
    | RecentManagerErrorInvalidEncoding
    -- ^ the supplied string is not
    --   UTF-8 encoded.
    | RecentManagerErrorNotRegistered
    -- ^ no application has registered
    --   the specified item.
    | RecentManagerErrorRead
    -- ^ failure while reading the recently used
    --   resources file.
    | RecentManagerErrorWrite
    -- ^ failure while writing the recently used
    --   resources file.
    | RecentManagerErrorUnknown
    -- ^ unspecified error.
    | AnotherRecentManagerError Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum RecentManagerError where
    fromEnum RecentManagerErrorNotFound = 0
    fromEnum RecentManagerErrorInvalidUri = 1
    fromEnum RecentManagerErrorInvalidEncoding = 2
    fromEnum RecentManagerErrorNotRegistered = 3
    fromEnum RecentManagerErrorRead = 4
    fromEnum RecentManagerErrorWrite = 5
    fromEnum RecentManagerErrorUnknown = 6
    fromEnum (AnotherRecentManagerError k) = k

    toEnum 0 = RecentManagerErrorNotFound
    toEnum 1 = RecentManagerErrorInvalidUri
    toEnum 2 = RecentManagerErrorInvalidEncoding
    toEnum 3 = RecentManagerErrorNotRegistered
    toEnum 4 = RecentManagerErrorRead
    toEnum 5 = RecentManagerErrorWrite
    toEnum 6 = RecentManagerErrorUnknown
    toEnum k = AnotherRecentManagerError k

instance P.Ord RecentManagerError where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

instance GErrorClass RecentManagerError where
    gerrorClassDomain _ = "gtk-recent-manager-error-quark"

-- | Catch exceptions of type `RecentManagerError`. This is a specialized version of `Data.GI.Base.GError.catchGErrorJustDomain`.
catchRecentManagerError ::
    IO a ->
    (RecentManagerError -> GErrorMessage -> IO a) ->
    IO a
catchRecentManagerError = catchGErrorJustDomain

-- | Handle exceptions of type `RecentManagerError`. This is a specialized version of `Data.GI.Base.GError.handleGErrorJustDomain`.
handleRecentManagerError ::
    (RecentManagerError -> GErrorMessage -> IO a) ->
    IO a ->
    IO a
handleRecentManagerError = handleGErrorJustDomain

type instance O.ParentTypes RecentManagerError = '[]
instance O.HasParentTypes RecentManagerError

foreign import ccall "gtk_recent_manager_error_get_type" c_gtk_recent_manager_error_get_type :: 
    IO GType

instance B.Types.TypedObject RecentManagerError where
    glibType = c_gtk_recent_manager_error_get_type

instance B.Types.BoxedEnum RecentManagerError

-- Enum RecentChooserError
-- | These identify the various errors that can occur while calling
-- t'GI.Gtk.Interfaces.RecentChooser.RecentChooser' functions.
-- 
-- /Since: 2.10/
data RecentChooserError = 
      RecentChooserErrorNotFound
    -- ^ Indicates that a file does not exist
    | RecentChooserErrorInvalidUri
    -- ^ Indicates a malformed URI
    | AnotherRecentChooserError Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum RecentChooserError where
    fromEnum RecentChooserErrorNotFound = 0
    fromEnum RecentChooserErrorInvalidUri = 1
    fromEnum (AnotherRecentChooserError k) = k

    toEnum 0 = RecentChooserErrorNotFound
    toEnum 1 = RecentChooserErrorInvalidUri
    toEnum k = AnotherRecentChooserError k

instance P.Ord RecentChooserError where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

instance GErrorClass RecentChooserError where
    gerrorClassDomain _ = "gtk-recent-chooser-error-quark"

-- | Catch exceptions of type `RecentChooserError`. This is a specialized version of `Data.GI.Base.GError.catchGErrorJustDomain`.
catchRecentChooserError ::
    IO a ->
    (RecentChooserError -> GErrorMessage -> IO a) ->
    IO a
catchRecentChooserError = catchGErrorJustDomain

-- | Handle exceptions of type `RecentChooserError`. This is a specialized version of `Data.GI.Base.GError.handleGErrorJustDomain`.
handleRecentChooserError ::
    (RecentChooserError -> GErrorMessage -> IO a) ->
    IO a ->
    IO a
handleRecentChooserError = handleGErrorJustDomain

type instance O.ParentTypes RecentChooserError = '[]
instance O.HasParentTypes RecentChooserError

foreign import ccall "gtk_recent_chooser_error_get_type" c_gtk_recent_chooser_error_get_type :: 
    IO GType

instance B.Types.TypedObject RecentChooserError where
    glibType = c_gtk_recent_chooser_error_get_type

instance B.Types.BoxedEnum RecentChooserError

-- Enum RcTokenType
{-# DEPRECATED RcTokenType ["(Since version 3.0)","Use t'GI.Gtk.Objects.CssProvider.CssProvider' instead."] #-}
-- | The t'GI.Gtk.Enums.RcTokenType' enumeration represents the tokens
-- in the RC file. It is exposed so that theme engines
-- can reuse these tokens when parsing the theme-engine
-- specific portions of a RC file.
data RcTokenType = 
      RcTokenTypeInvalid
    -- ^ Deprecated
    | RcTokenTypeInclude
    -- ^ Deprecated
    | RcTokenTypeNormal
    -- ^ Deprecated
    | RcTokenTypeActive
    -- ^ Deprecated
    | RcTokenTypePrelight
    -- ^ Deprecated
    | RcTokenTypeSelected
    -- ^ Deprecated
    | RcTokenTypeInsensitive
    -- ^ Deprecated
    | RcTokenTypeFg
    -- ^ Deprecated
    | RcTokenTypeBg
    -- ^ Deprecated
    | RcTokenTypeText
    -- ^ Deprecated
    | RcTokenTypeBase
    -- ^ Deprecated
    | RcTokenTypeXthickness
    -- ^ Deprecated
    | RcTokenTypeYthickness
    -- ^ Deprecated
    | RcTokenTypeFont
    -- ^ Deprecated
    | RcTokenTypeFontset
    -- ^ Deprecated
    | RcTokenTypeFontName
    -- ^ Deprecated
    | RcTokenTypeBgPixmap
    -- ^ Deprecated
    | RcTokenTypePixmapPath
    -- ^ Deprecated
    | RcTokenTypeStyle
    -- ^ Deprecated
    | RcTokenTypeBinding
    -- ^ Deprecated
    | RcTokenTypeBind
    -- ^ Deprecated
    | RcTokenTypeWidget
    -- ^ Deprecated
    | RcTokenTypeWidgetClass
    -- ^ Deprecated
    | RcTokenTypeClass
    -- ^ Deprecated
    | RcTokenTypeLowest
    -- ^ Deprecated
    | RcTokenTypeGtk
    -- ^ Deprecated
    | RcTokenTypeApplication
    -- ^ Deprecated
    | RcTokenTypeTheme
    -- ^ Deprecated
    | RcTokenTypeRc
    -- ^ Deprecated
    | RcTokenTypeHighest
    -- ^ Deprecated
    | RcTokenTypeEngine
    -- ^ Deprecated
    | RcTokenTypeModulePath
    -- ^ Deprecated
    | RcTokenTypeImModulePath
    -- ^ Deprecated
    | RcTokenTypeImModuleFile
    -- ^ Deprecated
    | RcTokenTypeStock
    -- ^ Deprecated
    | RcTokenTypeLtr
    -- ^ Deprecated
    | RcTokenTypeRtl
    -- ^ Deprecated
    | RcTokenTypeColor
    -- ^ Deprecated
    | RcTokenTypeUnbind
    -- ^ Deprecated
    | RcTokenTypeLast
    -- ^ Deprecated
    | AnotherRcTokenType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum RcTokenType where
    fromEnum RcTokenTypeInvalid = 270
    fromEnum RcTokenTypeInclude = 271
    fromEnum RcTokenTypeNormal = 272
    fromEnum RcTokenTypeActive = 273
    fromEnum RcTokenTypePrelight = 274
    fromEnum RcTokenTypeSelected = 275
    fromEnum RcTokenTypeInsensitive = 276
    fromEnum RcTokenTypeFg = 277
    fromEnum RcTokenTypeBg = 278
    fromEnum RcTokenTypeText = 279
    fromEnum RcTokenTypeBase = 280
    fromEnum RcTokenTypeXthickness = 281
    fromEnum RcTokenTypeYthickness = 282
    fromEnum RcTokenTypeFont = 283
    fromEnum RcTokenTypeFontset = 284
    fromEnum RcTokenTypeFontName = 285
    fromEnum RcTokenTypeBgPixmap = 286
    fromEnum RcTokenTypePixmapPath = 287
    fromEnum RcTokenTypeStyle = 288
    fromEnum RcTokenTypeBinding = 289
    fromEnum RcTokenTypeBind = 290
    fromEnum RcTokenTypeWidget = 291
    fromEnum RcTokenTypeWidgetClass = 292
    fromEnum RcTokenTypeClass = 293
    fromEnum RcTokenTypeLowest = 294
    fromEnum RcTokenTypeGtk = 295
    fromEnum RcTokenTypeApplication = 296
    fromEnum RcTokenTypeTheme = 297
    fromEnum RcTokenTypeRc = 298
    fromEnum RcTokenTypeHighest = 299
    fromEnum RcTokenTypeEngine = 300
    fromEnum RcTokenTypeModulePath = 301
    fromEnum RcTokenTypeImModulePath = 302
    fromEnum RcTokenTypeImModuleFile = 303
    fromEnum RcTokenTypeStock = 304
    fromEnum RcTokenTypeLtr = 305
    fromEnum RcTokenTypeRtl = 306
    fromEnum RcTokenTypeColor = 307
    fromEnum RcTokenTypeUnbind = 308
    fromEnum RcTokenTypeLast = 309
    fromEnum (AnotherRcTokenType k) = k

    toEnum 270 = RcTokenTypeInvalid
    toEnum 271 = RcTokenTypeInclude
    toEnum 272 = RcTokenTypeNormal
    toEnum 273 = RcTokenTypeActive
    toEnum 274 = RcTokenTypePrelight
    toEnum 275 = RcTokenTypeSelected
    toEnum 276 = RcTokenTypeInsensitive
    toEnum 277 = RcTokenTypeFg
    toEnum 278 = RcTokenTypeBg
    toEnum 279 = RcTokenTypeText
    toEnum 280 = RcTokenTypeBase
    toEnum 281 = RcTokenTypeXthickness
    toEnum 282 = RcTokenTypeYthickness
    toEnum 283 = RcTokenTypeFont
    toEnum 284 = RcTokenTypeFontset
    toEnum 285 = RcTokenTypeFontName
    toEnum 286 = RcTokenTypeBgPixmap
    toEnum 287 = RcTokenTypePixmapPath
    toEnum 288 = RcTokenTypeStyle
    toEnum 289 = RcTokenTypeBinding
    toEnum 290 = RcTokenTypeBind
    toEnum 291 = RcTokenTypeWidget
    toEnum 292 = RcTokenTypeWidgetClass
    toEnum 293 = RcTokenTypeClass
    toEnum 294 = RcTokenTypeLowest
    toEnum 295 = RcTokenTypeGtk
    toEnum 296 = RcTokenTypeApplication
    toEnum 297 = RcTokenTypeTheme
    toEnum 298 = RcTokenTypeRc
    toEnum 299 = RcTokenTypeHighest
    toEnum 300 = RcTokenTypeEngine
    toEnum 301 = RcTokenTypeModulePath
    toEnum 302 = RcTokenTypeImModulePath
    toEnum 303 = RcTokenTypeImModuleFile
    toEnum 304 = RcTokenTypeStock
    toEnum 305 = RcTokenTypeLtr
    toEnum 306 = RcTokenTypeRtl
    toEnum 307 = RcTokenTypeColor
    toEnum 308 = RcTokenTypeUnbind
    toEnum 309 = RcTokenTypeLast
    toEnum k = AnotherRcTokenType k

instance P.Ord RcTokenType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes RcTokenType = '[]
instance O.HasParentTypes RcTokenType

foreign import ccall "gtk_rc_token_type_get_type" c_gtk_rc_token_type_get_type :: 
    IO GType

instance B.Types.TypedObject RcTokenType where
    glibType = c_gtk_rc_token_type_get_type

instance B.Types.BoxedEnum RcTokenType

-- Enum PropagationPhase
-- | Describes the stage at which events are fed into a t'GI.Gtk.Objects.EventController.EventController'.
-- 
-- /Since: 3.14/
data PropagationPhase = 
      PropagationPhaseNone
    -- ^ Events are not delivered automatically. Those can be
    --   manually fed through 'GI.Gtk.Objects.EventController.eventControllerHandleEvent'. This should
    --   only be used when full control about when, or whether the controller
    --   handles the event is needed.
    | PropagationPhaseCapture
    -- ^ Events are delivered in the capture phase. The
    --   capture phase happens before the bubble phase, runs from the toplevel down
    --   to the event widget. This option should only be used on containers that
    --   might possibly handle events before their children do.
    | PropagationPhaseBubble
    -- ^ Events are delivered in the bubble phase. The bubble
    --   phase happens after the capture phase, and before the default handlers
    --   are run. This phase runs from the event widget, up to the toplevel.
    | PropagationPhaseTarget
    -- ^ Events are delivered in the default widget event handlers,
    --   note that widget implementations must chain up on button, motion, touch and
    --   grab broken handlers for controllers in this phase to be run.
    | AnotherPropagationPhase Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PropagationPhase where
    fromEnum PropagationPhaseNone = 0
    fromEnum PropagationPhaseCapture = 1
    fromEnum PropagationPhaseBubble = 2
    fromEnum PropagationPhaseTarget = 3
    fromEnum (AnotherPropagationPhase k) = k

    toEnum 0 = PropagationPhaseNone
    toEnum 1 = PropagationPhaseCapture
    toEnum 2 = PropagationPhaseBubble
    toEnum 3 = PropagationPhaseTarget
    toEnum k = AnotherPropagationPhase k

instance P.Ord PropagationPhase where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PropagationPhase = '[]
instance O.HasParentTypes PropagationPhase

foreign import ccall "gtk_propagation_phase_get_type" c_gtk_propagation_phase_get_type :: 
    IO GType

instance B.Types.TypedObject PropagationPhase where
    glibType = c_gtk_propagation_phase_get_type

instance B.Types.BoxedEnum PropagationPhase

-- Enum PrintStatus
-- | The status gives a rough indication of the completion of a running
-- print operation.
data PrintStatus = 
      PrintStatusInitial
    -- ^ The printing has not started yet; this
    --     status is set initially, and while the print dialog is shown.
    | PrintStatusPreparing
    -- ^ This status is set while the begin-print
    --     signal is emitted and during pagination.
    | PrintStatusGeneratingData
    -- ^ This status is set while the
    --     pages are being rendered.
    | PrintStatusSendingData
    -- ^ The print job is being sent off to the
    --     printer.
    | PrintStatusPending
    -- ^ The print job has been sent to the printer,
    --     but is not printed for some reason, e.g. the printer may be stopped.
    | PrintStatusPendingIssue
    -- ^ Some problem has occurred during
    --     printing, e.g. a paper jam.
    | PrintStatusPrinting
    -- ^ The printer is processing the print job.
    | PrintStatusFinished
    -- ^ The printing has been completed successfully.
    | PrintStatusFinishedAborted
    -- ^ The printing has been aborted.
    | AnotherPrintStatus Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PrintStatus where
    fromEnum PrintStatusInitial = 0
    fromEnum PrintStatusPreparing = 1
    fromEnum PrintStatusGeneratingData = 2
    fromEnum PrintStatusSendingData = 3
    fromEnum PrintStatusPending = 4
    fromEnum PrintStatusPendingIssue = 5
    fromEnum PrintStatusPrinting = 6
    fromEnum PrintStatusFinished = 7
    fromEnum PrintStatusFinishedAborted = 8
    fromEnum (AnotherPrintStatus k) = k

    toEnum 0 = PrintStatusInitial
    toEnum 1 = PrintStatusPreparing
    toEnum 2 = PrintStatusGeneratingData
    toEnum 3 = PrintStatusSendingData
    toEnum 4 = PrintStatusPending
    toEnum 5 = PrintStatusPendingIssue
    toEnum 6 = PrintStatusPrinting
    toEnum 7 = PrintStatusFinished
    toEnum 8 = PrintStatusFinishedAborted
    toEnum k = AnotherPrintStatus k

instance P.Ord PrintStatus where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PrintStatus = '[]
instance O.HasParentTypes PrintStatus

foreign import ccall "gtk_print_status_get_type" c_gtk_print_status_get_type :: 
    IO GType

instance B.Types.TypedObject PrintStatus where
    glibType = c_gtk_print_status_get_type

instance B.Types.BoxedEnum PrintStatus

-- Enum PrintQuality
-- | See also 'GI.Gtk.Objects.PrintSettings.printSettingsSetQuality'.
data PrintQuality = 
      PrintQualityLow
    -- ^ Low quality.
    | PrintQualityNormal
    -- ^ Normal quality.
    | PrintQualityHigh
    -- ^ High quality.
    | PrintQualityDraft
    -- ^ Draft quality.
    | AnotherPrintQuality Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PrintQuality where
    fromEnum PrintQualityLow = 0
    fromEnum PrintQualityNormal = 1
    fromEnum PrintQualityHigh = 2
    fromEnum PrintQualityDraft = 3
    fromEnum (AnotherPrintQuality k) = k

    toEnum 0 = PrintQualityLow
    toEnum 1 = PrintQualityNormal
    toEnum 2 = PrintQualityHigh
    toEnum 3 = PrintQualityDraft
    toEnum k = AnotherPrintQuality k

instance P.Ord PrintQuality where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PrintQuality = '[]
instance O.HasParentTypes PrintQuality

foreign import ccall "gtk_print_quality_get_type" c_gtk_print_quality_get_type :: 
    IO GType

instance B.Types.TypedObject PrintQuality where
    glibType = c_gtk_print_quality_get_type

instance B.Types.BoxedEnum PrintQuality

-- Enum PrintPages
-- | See also @/gtk_print_job_set_pages()/@
data PrintPages = 
      PrintPagesAll
    -- ^ All pages.
    | PrintPagesCurrent
    -- ^ Current page.
    | PrintPagesRanges
    -- ^ Range of pages.
    | PrintPagesSelection
    -- ^ Selected pages.
    | AnotherPrintPages Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PrintPages where
    fromEnum PrintPagesAll = 0
    fromEnum PrintPagesCurrent = 1
    fromEnum PrintPagesRanges = 2
    fromEnum PrintPagesSelection = 3
    fromEnum (AnotherPrintPages k) = k

    toEnum 0 = PrintPagesAll
    toEnum 1 = PrintPagesCurrent
    toEnum 2 = PrintPagesRanges
    toEnum 3 = PrintPagesSelection
    toEnum k = AnotherPrintPages k

instance P.Ord PrintPages where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PrintPages = '[]
instance O.HasParentTypes PrintPages

foreign import ccall "gtk_print_pages_get_type" c_gtk_print_pages_get_type :: 
    IO GType

instance B.Types.TypedObject PrintPages where
    glibType = c_gtk_print_pages_get_type

instance B.Types.BoxedEnum PrintPages

-- Enum PrintOperationResult
-- | A value of this type is returned by 'GI.Gtk.Objects.PrintOperation.printOperationRun'.
data PrintOperationResult = 
      PrintOperationResultError
    -- ^ An error has occurred.
    | PrintOperationResultApply
    -- ^ The print settings should be stored.
    | PrintOperationResultCancel
    -- ^ The print operation has been canceled,
    --     the print settings should not be stored.
    | PrintOperationResultInProgress
    -- ^ The print operation is not complete
    --     yet. This value will only be returned when running asynchronously.
    | AnotherPrintOperationResult Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PrintOperationResult where
    fromEnum PrintOperationResultError = 0
    fromEnum PrintOperationResultApply = 1
    fromEnum PrintOperationResultCancel = 2
    fromEnum PrintOperationResultInProgress = 3
    fromEnum (AnotherPrintOperationResult k) = k

    toEnum 0 = PrintOperationResultError
    toEnum 1 = PrintOperationResultApply
    toEnum 2 = PrintOperationResultCancel
    toEnum 3 = PrintOperationResultInProgress
    toEnum k = AnotherPrintOperationResult k

instance P.Ord PrintOperationResult where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PrintOperationResult = '[]
instance O.HasParentTypes PrintOperationResult

foreign import ccall "gtk_print_operation_result_get_type" c_gtk_print_operation_result_get_type :: 
    IO GType

instance B.Types.TypedObject PrintOperationResult where
    glibType = c_gtk_print_operation_result_get_type

instance B.Types.BoxedEnum PrintOperationResult

-- Enum PrintOperationAction
-- | The /@action@/ parameter to 'GI.Gtk.Objects.PrintOperation.printOperationRun'
-- determines what action the print operation should perform.
data PrintOperationAction = 
      PrintOperationActionPrintDialog
    -- ^ Show the print dialog.
    | PrintOperationActionPrint
    -- ^ Start to print without showing
    --     the print dialog, based on the current print settings.
    | PrintOperationActionPreview
    -- ^ Show the print preview.
    | PrintOperationActionExport
    -- ^ Export to a file. This requires
    --     the export-filename property to be set.
    | AnotherPrintOperationAction Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PrintOperationAction where
    fromEnum PrintOperationActionPrintDialog = 0
    fromEnum PrintOperationActionPrint = 1
    fromEnum PrintOperationActionPreview = 2
    fromEnum PrintOperationActionExport = 3
    fromEnum (AnotherPrintOperationAction k) = k

    toEnum 0 = PrintOperationActionPrintDialog
    toEnum 1 = PrintOperationActionPrint
    toEnum 2 = PrintOperationActionPreview
    toEnum 3 = PrintOperationActionExport
    toEnum k = AnotherPrintOperationAction k

instance P.Ord PrintOperationAction where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PrintOperationAction = '[]
instance O.HasParentTypes PrintOperationAction

foreign import ccall "gtk_print_operation_action_get_type" c_gtk_print_operation_action_get_type :: 
    IO GType

instance B.Types.TypedObject PrintOperationAction where
    glibType = c_gtk_print_operation_action_get_type

instance B.Types.BoxedEnum PrintOperationAction

-- Enum PrintError
-- | Error codes that identify various errors that can occur while
-- using the GTK+ printing support.
data PrintError = 
      PrintErrorGeneral
    -- ^ An unspecified error occurred.
    | PrintErrorInternalError
    -- ^ An internal error occurred.
    | PrintErrorNomem
    -- ^ A memory allocation failed.
    | PrintErrorInvalidFile
    -- ^ An error occurred while loading a page setup
    --     or paper size from a key file.
    | AnotherPrintError Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PrintError where
    fromEnum PrintErrorGeneral = 0
    fromEnum PrintErrorInternalError = 1
    fromEnum PrintErrorNomem = 2
    fromEnum PrintErrorInvalidFile = 3
    fromEnum (AnotherPrintError k) = k

    toEnum 0 = PrintErrorGeneral
    toEnum 1 = PrintErrorInternalError
    toEnum 2 = PrintErrorNomem
    toEnum 3 = PrintErrorInvalidFile
    toEnum k = AnotherPrintError k

instance P.Ord PrintError where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

instance GErrorClass PrintError where
    gerrorClassDomain _ = "gtk-print-error-quark"

-- | Catch exceptions of type `PrintError`. This is a specialized version of `Data.GI.Base.GError.catchGErrorJustDomain`.
catchPrintError ::
    IO a ->
    (PrintError -> GErrorMessage -> IO a) ->
    IO a
catchPrintError = catchGErrorJustDomain

-- | Handle exceptions of type `PrintError`. This is a specialized version of `Data.GI.Base.GError.handleGErrorJustDomain`.
handlePrintError ::
    (PrintError -> GErrorMessage -> IO a) ->
    IO a ->
    IO a
handlePrintError = handleGErrorJustDomain

type instance O.ParentTypes PrintError = '[]
instance O.HasParentTypes PrintError

foreign import ccall "gtk_print_error_get_type" c_gtk_print_error_get_type :: 
    IO GType

instance B.Types.TypedObject PrintError where
    glibType = c_gtk_print_error_get_type

instance B.Types.BoxedEnum PrintError

-- Enum PrintDuplex
-- | See also 'GI.Gtk.Objects.PrintSettings.printSettingsSetDuplex'.
data PrintDuplex = 
      PrintDuplexSimplex
    -- ^ No duplex.
    | PrintDuplexHorizontal
    -- ^ Horizontal duplex.
    | PrintDuplexVertical
    -- ^ Vertical duplex.
    | AnotherPrintDuplex Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PrintDuplex where
    fromEnum PrintDuplexSimplex = 0
    fromEnum PrintDuplexHorizontal = 1
    fromEnum PrintDuplexVertical = 2
    fromEnum (AnotherPrintDuplex k) = k

    toEnum 0 = PrintDuplexSimplex
    toEnum 1 = PrintDuplexHorizontal
    toEnum 2 = PrintDuplexVertical
    toEnum k = AnotherPrintDuplex k

instance P.Ord PrintDuplex where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PrintDuplex = '[]
instance O.HasParentTypes PrintDuplex

foreign import ccall "gtk_print_duplex_get_type" c_gtk_print_duplex_get_type :: 
    IO GType

instance B.Types.TypedObject PrintDuplex where
    glibType = c_gtk_print_duplex_get_type

instance B.Types.BoxedEnum PrintDuplex

-- Enum PositionType
-- | Describes which edge of a widget a certain feature is positioned at, e.g. the
-- tabs of a t'GI.Gtk.Objects.Notebook.Notebook', the handle of a t'GI.Gtk.Objects.HandleBox.HandleBox' or the label of a
-- t'GI.Gtk.Objects.Scale.Scale'.
data PositionType = 
      PositionTypeLeft
    -- ^ The feature is at the left edge.
    | PositionTypeRight
    -- ^ The feature is at the right edge.
    | PositionTypeTop
    -- ^ The feature is at the top edge.
    | PositionTypeBottom
    -- ^ The feature is at the bottom edge.
    | AnotherPositionType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PositionType where
    fromEnum PositionTypeLeft = 0
    fromEnum PositionTypeRight = 1
    fromEnum PositionTypeTop = 2
    fromEnum PositionTypeBottom = 3
    fromEnum (AnotherPositionType k) = k

    toEnum 0 = PositionTypeLeft
    toEnum 1 = PositionTypeRight
    toEnum 2 = PositionTypeTop
    toEnum 3 = PositionTypeBottom
    toEnum k = AnotherPositionType k

instance P.Ord PositionType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PositionType = '[]
instance O.HasParentTypes PositionType

foreign import ccall "gtk_position_type_get_type" c_gtk_position_type_get_type :: 
    IO GType

instance B.Types.TypedObject PositionType where
    glibType = c_gtk_position_type_get_type

instance B.Types.BoxedEnum PositionType

-- Enum PopoverConstraint
-- | Describes constraints to positioning of popovers. More values
-- may be added to this enumeration in the future.
-- 
-- /Since: 3.20/
data PopoverConstraint = 
      PopoverConstraintNone
    -- ^ Don\'t constrain the popover position
    --   beyond what is imposed by the implementation
    | PopoverConstraintWindow
    -- ^ Constrain the popover to the boundaries
    --   of the window that it is attached to
    | AnotherPopoverConstraint Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PopoverConstraint where
    fromEnum PopoverConstraintNone = 0
    fromEnum PopoverConstraintWindow = 1
    fromEnum (AnotherPopoverConstraint k) = k

    toEnum 0 = PopoverConstraintNone
    toEnum 1 = PopoverConstraintWindow
    toEnum k = AnotherPopoverConstraint k

instance P.Ord PopoverConstraint where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PopoverConstraint = '[]
instance O.HasParentTypes PopoverConstraint

foreign import ccall "gtk_popover_constraint_get_type" c_gtk_popover_constraint_get_type :: 
    IO GType

instance B.Types.TypedObject PopoverConstraint where
    glibType = c_gtk_popover_constraint_get_type

instance B.Types.BoxedEnum PopoverConstraint

-- Enum PolicyType
-- | Determines how the size should be computed to achieve the one of the
-- visibility mode for the scrollbars.
data PolicyType = 
      PolicyTypeAlways
    -- ^ The scrollbar is always visible. The view size is
    --  independent of the content.
    | PolicyTypeAutomatic
    -- ^ The scrollbar will appear and disappear as necessary.
    --  For example, when all of a t'GI.Gtk.Objects.TreeView.TreeView' can not be seen.
    | PolicyTypeNever
    -- ^ The scrollbar should never appear. In this mode the
    --  content determines the size.
    | PolicyTypeExternal
    -- ^ Don\'t show a scrollbar, but don\'t force the
    --  size to follow the content. This can be used e.g. to make multiple
    --  scrolled windows share a scrollbar. Since: 3.16
    | AnotherPolicyType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PolicyType where
    fromEnum PolicyTypeAlways = 0
    fromEnum PolicyTypeAutomatic = 1
    fromEnum PolicyTypeNever = 2
    fromEnum PolicyTypeExternal = 3
    fromEnum (AnotherPolicyType k) = k

    toEnum 0 = PolicyTypeAlways
    toEnum 1 = PolicyTypeAutomatic
    toEnum 2 = PolicyTypeNever
    toEnum 3 = PolicyTypeExternal
    toEnum k = AnotherPolicyType k

instance P.Ord PolicyType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PolicyType = '[]
instance O.HasParentTypes PolicyType

foreign import ccall "gtk_policy_type_get_type" c_gtk_policy_type_get_type :: 
    IO GType

instance B.Types.TypedObject PolicyType where
    glibType = c_gtk_policy_type_get_type

instance B.Types.BoxedEnum PolicyType

-- Enum PathType
{-# DEPRECATED PathType ["(Since version 3.0)"] #-}
-- | Widget path types.
-- See also 'GI.Gtk.Structs.BindingSet.bindingSetAddPath'.
data PathType = 
      PathTypeWidget
    -- ^ Deprecated
    | PathTypeWidgetClass
    -- ^ Deprecated
    | PathTypeClass
    -- ^ Deprecated
    | AnotherPathType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PathType where
    fromEnum PathTypeWidget = 0
    fromEnum PathTypeWidgetClass = 1
    fromEnum PathTypeClass = 2
    fromEnum (AnotherPathType k) = k

    toEnum 0 = PathTypeWidget
    toEnum 1 = PathTypeWidgetClass
    toEnum 2 = PathTypeClass
    toEnum k = AnotherPathType k

instance P.Ord PathType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PathType = '[]
instance O.HasParentTypes PathType

foreign import ccall "gtk_path_type_get_type" c_gtk_path_type_get_type :: 
    IO GType

instance B.Types.TypedObject PathType where
    glibType = c_gtk_path_type_get_type

instance B.Types.BoxedEnum PathType

-- Enum PathPriorityType
{-# DEPRECATED PathPriorityType ["(Since version 3.0)"] #-}
-- | Priorities for path lookups.
-- See also 'GI.Gtk.Structs.BindingSet.bindingSetAddPath'.
data PathPriorityType = 
      PathPriorityTypeLowest
    -- ^ Deprecated
    | PathPriorityTypeGtk
    -- ^ Deprecated
    | PathPriorityTypeApplication
    -- ^ Deprecated
    | PathPriorityTypeTheme
    -- ^ Deprecated
    | PathPriorityTypeRc
    -- ^ Deprecated
    | PathPriorityTypeHighest
    -- ^ Deprecated
    | AnotherPathPriorityType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PathPriorityType where
    fromEnum PathPriorityTypeLowest = 0
    fromEnum PathPriorityTypeGtk = 4
    fromEnum PathPriorityTypeApplication = 8
    fromEnum PathPriorityTypeTheme = 10
    fromEnum PathPriorityTypeRc = 12
    fromEnum PathPriorityTypeHighest = 15
    fromEnum (AnotherPathPriorityType k) = k

    toEnum 0 = PathPriorityTypeLowest
    toEnum 4 = PathPriorityTypeGtk
    toEnum 8 = PathPriorityTypeApplication
    toEnum 10 = PathPriorityTypeTheme
    toEnum 12 = PathPriorityTypeRc
    toEnum 15 = PathPriorityTypeHighest
    toEnum k = AnotherPathPriorityType k

instance P.Ord PathPriorityType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PathPriorityType = '[]
instance O.HasParentTypes PathPriorityType

foreign import ccall "gtk_path_priority_type_get_type" c_gtk_path_priority_type_get_type :: 
    IO GType

instance B.Types.TypedObject PathPriorityType where
    glibType = c_gtk_path_priority_type_get_type

instance B.Types.BoxedEnum PathPriorityType

-- Enum PanDirection
-- | Describes the panning direction of a t'GI.Gtk.Objects.GesturePan.GesturePan'
-- 
-- /Since: 3.14/
data PanDirection = 
      PanDirectionLeft
    -- ^ panned towards the left
    | PanDirectionRight
    -- ^ panned towards the right
    | PanDirectionUp
    -- ^ panned upwards
    | PanDirectionDown
    -- ^ panned downwards
    | AnotherPanDirection Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PanDirection where
    fromEnum PanDirectionLeft = 0
    fromEnum PanDirectionRight = 1
    fromEnum PanDirectionUp = 2
    fromEnum PanDirectionDown = 3
    fromEnum (AnotherPanDirection k) = k

    toEnum 0 = PanDirectionLeft
    toEnum 1 = PanDirectionRight
    toEnum 2 = PanDirectionUp
    toEnum 3 = PanDirectionDown
    toEnum k = AnotherPanDirection k

instance P.Ord PanDirection where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PanDirection = '[]
instance O.HasParentTypes PanDirection

foreign import ccall "gtk_pan_direction_get_type" c_gtk_pan_direction_get_type :: 
    IO GType

instance B.Types.TypedObject PanDirection where
    glibType = c_gtk_pan_direction_get_type

instance B.Types.BoxedEnum PanDirection

-- Enum PageSet
-- | See also @/gtk_print_job_set_page_set()/@.
data PageSet = 
      PageSetAll
    -- ^ All pages.
    | PageSetEven
    -- ^ Even pages.
    | PageSetOdd
    -- ^ Odd pages.
    | AnotherPageSet Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PageSet where
    fromEnum PageSetAll = 0
    fromEnum PageSetEven = 1
    fromEnum PageSetOdd = 2
    fromEnum (AnotherPageSet k) = k

    toEnum 0 = PageSetAll
    toEnum 1 = PageSetEven
    toEnum 2 = PageSetOdd
    toEnum k = AnotherPageSet k

instance P.Ord PageSet where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PageSet = '[]
instance O.HasParentTypes PageSet

foreign import ccall "gtk_page_set_get_type" c_gtk_page_set_get_type :: 
    IO GType

instance B.Types.TypedObject PageSet where
    glibType = c_gtk_page_set_get_type

instance B.Types.BoxedEnum PageSet

-- Enum PageOrientation
-- | See also 'GI.Gtk.Objects.PrintSettings.printSettingsSetOrientation'.
data PageOrientation = 
      PageOrientationPortrait
    -- ^ Portrait mode.
    | PageOrientationLandscape
    -- ^ Landscape mode.
    | PageOrientationReversePortrait
    -- ^ Reverse portrait mode.
    | PageOrientationReverseLandscape
    -- ^ Reverse landscape mode.
    | AnotherPageOrientation Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PageOrientation where
    fromEnum PageOrientationPortrait = 0
    fromEnum PageOrientationLandscape = 1
    fromEnum PageOrientationReversePortrait = 2
    fromEnum PageOrientationReverseLandscape = 3
    fromEnum (AnotherPageOrientation k) = k

    toEnum 0 = PageOrientationPortrait
    toEnum 1 = PageOrientationLandscape
    toEnum 2 = PageOrientationReversePortrait
    toEnum 3 = PageOrientationReverseLandscape
    toEnum k = AnotherPageOrientation k

instance P.Ord PageOrientation where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PageOrientation = '[]
instance O.HasParentTypes PageOrientation

foreign import ccall "gtk_page_orientation_get_type" c_gtk_page_orientation_get_type :: 
    IO GType

instance B.Types.TypedObject PageOrientation where
    glibType = c_gtk_page_orientation_get_type

instance B.Types.BoxedEnum PageOrientation

-- Enum PadActionType
-- | The type of a pad action.
data PadActionType = 
      PadActionTypeButton
    -- ^ Action is triggered by a pad button
    | PadActionTypeRing
    -- ^ Action is triggered by a pad ring
    | PadActionTypeStrip
    -- ^ Action is triggered by a pad strip
    | AnotherPadActionType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PadActionType where
    fromEnum PadActionTypeButton = 0
    fromEnum PadActionTypeRing = 1
    fromEnum PadActionTypeStrip = 2
    fromEnum (AnotherPadActionType k) = k

    toEnum 0 = PadActionTypeButton
    toEnum 1 = PadActionTypeRing
    toEnum 2 = PadActionTypeStrip
    toEnum k = AnotherPadActionType k

instance P.Ord PadActionType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PadActionType = '[]
instance O.HasParentTypes PadActionType

foreign import ccall "gtk_pad_action_type_get_type" c_gtk_pad_action_type_get_type :: 
    IO GType

instance B.Types.TypedObject PadActionType where
    glibType = c_gtk_pad_action_type_get_type

instance B.Types.BoxedEnum PadActionType

-- Enum PackType
-- | Represents the packing location t'GI.Gtk.Objects.Box.Box' children. (See: t'GI.Gtk.Objects.VBox.VBox',
-- t'GI.Gtk.Objects.HBox.HBox', and t'GI.Gtk.Objects.ButtonBox.ButtonBox').
data PackType = 
      PackTypeStart
    -- ^ The child is packed into the start of the box
    | PackTypeEnd
    -- ^ The child is packed into the end of the box
    | AnotherPackType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PackType where
    fromEnum PackTypeStart = 0
    fromEnum PackTypeEnd = 1
    fromEnum (AnotherPackType k) = k

    toEnum 0 = PackTypeStart
    toEnum 1 = PackTypeEnd
    toEnum k = AnotherPackType k

instance P.Ord PackType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PackType = '[]
instance O.HasParentTypes PackType

foreign import ccall "gtk_pack_type_get_type" c_gtk_pack_type_get_type :: 
    IO GType

instance B.Types.TypedObject PackType where
    glibType = c_gtk_pack_type_get_type

instance B.Types.BoxedEnum PackType

-- Enum PackDirection
-- | Determines how widgets should be packed inside menubars
-- and menuitems contained in menubars.
data PackDirection = 
      PackDirectionLtr
    -- ^ Widgets are packed left-to-right
    | PackDirectionRtl
    -- ^ Widgets are packed right-to-left
    | PackDirectionTtb
    -- ^ Widgets are packed top-to-bottom
    | PackDirectionBtt
    -- ^ Widgets are packed bottom-to-top
    | AnotherPackDirection Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PackDirection where
    fromEnum PackDirectionLtr = 0
    fromEnum PackDirectionRtl = 1
    fromEnum PackDirectionTtb = 2
    fromEnum PackDirectionBtt = 3
    fromEnum (AnotherPackDirection k) = k

    toEnum 0 = PackDirectionLtr
    toEnum 1 = PackDirectionRtl
    toEnum 2 = PackDirectionTtb
    toEnum 3 = PackDirectionBtt
    toEnum k = AnotherPackDirection k

instance P.Ord PackDirection where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PackDirection = '[]
instance O.HasParentTypes PackDirection

foreign import ccall "gtk_pack_direction_get_type" c_gtk_pack_direction_get_type :: 
    IO GType

instance B.Types.TypedObject PackDirection where
    glibType = c_gtk_pack_direction_get_type

instance B.Types.BoxedEnum PackDirection

-- Enum Orientation
-- | Represents the orientation of widgets and other objects which can be switched
-- between horizontal and vertical orientation on the fly, like t'GI.Gtk.Objects.Toolbar.Toolbar' or
-- t'GI.Gtk.Objects.GesturePan.GesturePan'.
data Orientation = 
      OrientationHorizontal
    -- ^ The element is in horizontal orientation.
    | OrientationVertical
    -- ^ The element is in vertical orientation.
    | AnotherOrientation Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum Orientation where
    fromEnum OrientationHorizontal = 0
    fromEnum OrientationVertical = 1
    fromEnum (AnotherOrientation k) = k

    toEnum 0 = OrientationHorizontal
    toEnum 1 = OrientationVertical
    toEnum k = AnotherOrientation k

instance P.Ord Orientation where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes Orientation = '[]
instance O.HasParentTypes Orientation

foreign import ccall "gtk_orientation_get_type" c_gtk_orientation_get_type :: 
    IO GType

instance B.Types.TypedObject Orientation where
    glibType = c_gtk_orientation_get_type

instance B.Types.BoxedEnum Orientation

-- Enum NumberUpLayout
-- | Used to determine the layout of pages on a sheet when printing
-- multiple pages per sheet.
data NumberUpLayout = 
      NumberUpLayoutLrtb
    -- ^ <<https://developer.gnome.org/gtk3/stable/layout-lrtb.png>>
    | NumberUpLayoutLrbt
    -- ^ <<https://developer.gnome.org/gtk3/stable/layout-lrbt.png>>
    | NumberUpLayoutRltb
    -- ^ <<https://developer.gnome.org/gtk3/stable/layout-rltb.png>>
    | NumberUpLayoutRlbt
    -- ^ <<https://developer.gnome.org/gtk3/stable/layout-rlbt.png>>
    | NumberUpLayoutTblr
    -- ^ <<https://developer.gnome.org/gtk3/stable/layout-tblr.png>>
    | NumberUpLayoutTbrl
    -- ^ <<https://developer.gnome.org/gtk3/stable/layout-tbrl.png>>
    | NumberUpLayoutBtlr
    -- ^ <<https://developer.gnome.org/gtk3/stable/layout-btlr.png>>
    | NumberUpLayoutBtrl
    -- ^ <<https://developer.gnome.org/gtk3/stable/layout-btrl.png>>
    | AnotherNumberUpLayout Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum NumberUpLayout where
    fromEnum NumberUpLayoutLrtb = 0
    fromEnum NumberUpLayoutLrbt = 1
    fromEnum NumberUpLayoutRltb = 2
    fromEnum NumberUpLayoutRlbt = 3
    fromEnum NumberUpLayoutTblr = 4
    fromEnum NumberUpLayoutTbrl = 5
    fromEnum NumberUpLayoutBtlr = 6
    fromEnum NumberUpLayoutBtrl = 7
    fromEnum (AnotherNumberUpLayout k) = k

    toEnum 0 = NumberUpLayoutLrtb
    toEnum 1 = NumberUpLayoutLrbt
    toEnum 2 = NumberUpLayoutRltb
    toEnum 3 = NumberUpLayoutRlbt
    toEnum 4 = NumberUpLayoutTblr
    toEnum 5 = NumberUpLayoutTbrl
    toEnum 6 = NumberUpLayoutBtlr
    toEnum 7 = NumberUpLayoutBtrl
    toEnum k = AnotherNumberUpLayout k

instance P.Ord NumberUpLayout where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes NumberUpLayout = '[]
instance O.HasParentTypes NumberUpLayout

foreign import ccall "gtk_number_up_layout_get_type" c_gtk_number_up_layout_get_type :: 
    IO GType

instance B.Types.TypedObject NumberUpLayout where
    glibType = c_gtk_number_up_layout_get_type

instance B.Types.BoxedEnum NumberUpLayout

-- Enum NotebookTab
-- | /No description available in the introspection data./
data NotebookTab = 
      NotebookTabFirst
    -- ^ /No description available in the introspection data./
    | NotebookTabLast
    -- ^ /No description available in the introspection data./
    | AnotherNotebookTab Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum NotebookTab where
    fromEnum NotebookTabFirst = 0
    fromEnum NotebookTabLast = 1
    fromEnum (AnotherNotebookTab k) = k

    toEnum 0 = NotebookTabFirst
    toEnum 1 = NotebookTabLast
    toEnum k = AnotherNotebookTab k

instance P.Ord NotebookTab where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes NotebookTab = '[]
instance O.HasParentTypes NotebookTab

foreign import ccall "gtk_notebook_tab_get_type" c_gtk_notebook_tab_get_type :: 
    IO GType

instance B.Types.TypedObject NotebookTab where
    glibType = c_gtk_notebook_tab_get_type

instance B.Types.BoxedEnum NotebookTab

-- Enum MovementStep
-- | /No description available in the introspection data./
data MovementStep = 
      MovementStepLogicalPositions
    -- ^ Move forward or back by graphemes
    | MovementStepVisualPositions
    -- ^ Move left or right by graphemes
    | MovementStepWords
    -- ^ Move forward or back by words
    | MovementStepDisplayLines
    -- ^ Move up or down lines (wrapped lines)
    | MovementStepDisplayLineEnds
    -- ^ Move to either end of a line
    | MovementStepParagraphs
    -- ^ Move up or down paragraphs (newline-ended lines)
    | MovementStepParagraphEnds
    -- ^ Move to either end of a paragraph
    | MovementStepPages
    -- ^ Move by pages
    | MovementStepBufferEnds
    -- ^ Move to ends of the buffer
    | MovementStepHorizontalPages
    -- ^ Move horizontally by pages
    | AnotherMovementStep Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum MovementStep where
    fromEnum MovementStepLogicalPositions = 0
    fromEnum MovementStepVisualPositions = 1
    fromEnum MovementStepWords = 2
    fromEnum MovementStepDisplayLines = 3
    fromEnum MovementStepDisplayLineEnds = 4
    fromEnum MovementStepParagraphs = 5
    fromEnum MovementStepParagraphEnds = 6
    fromEnum MovementStepPages = 7
    fromEnum MovementStepBufferEnds = 8
    fromEnum MovementStepHorizontalPages = 9
    fromEnum (AnotherMovementStep k) = k

    toEnum 0 = MovementStepLogicalPositions
    toEnum 1 = MovementStepVisualPositions
    toEnum 2 = MovementStepWords
    toEnum 3 = MovementStepDisplayLines
    toEnum 4 = MovementStepDisplayLineEnds
    toEnum 5 = MovementStepParagraphs
    toEnum 6 = MovementStepParagraphEnds
    toEnum 7 = MovementStepPages
    toEnum 8 = MovementStepBufferEnds
    toEnum 9 = MovementStepHorizontalPages
    toEnum k = AnotherMovementStep k

instance P.Ord MovementStep where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes MovementStep = '[]
instance O.HasParentTypes MovementStep

foreign import ccall "gtk_movement_step_get_type" c_gtk_movement_step_get_type :: 
    IO GType

instance B.Types.TypedObject MovementStep where
    glibType = c_gtk_movement_step_get_type

instance B.Types.BoxedEnum MovementStep

-- Enum MessageType
-- | The type of message being displayed in the dialog.
data MessageType = 
      MessageTypeInfo
    -- ^ Informational message
    | MessageTypeWarning
    -- ^ Non-fatal warning message
    | MessageTypeQuestion
    -- ^ Question requiring a choice
    | MessageTypeError
    -- ^ Fatal error message
    | MessageTypeOther
    -- ^ None of the above
    | AnotherMessageType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum MessageType where
    fromEnum MessageTypeInfo = 0
    fromEnum MessageTypeWarning = 1
    fromEnum MessageTypeQuestion = 2
    fromEnum MessageTypeError = 3
    fromEnum MessageTypeOther = 4
    fromEnum (AnotherMessageType k) = k

    toEnum 0 = MessageTypeInfo
    toEnum 1 = MessageTypeWarning
    toEnum 2 = MessageTypeQuestion
    toEnum 3 = MessageTypeError
    toEnum 4 = MessageTypeOther
    toEnum k = AnotherMessageType k

instance P.Ord MessageType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes MessageType = '[]
instance O.HasParentTypes MessageType

foreign import ccall "gtk_message_type_get_type" c_gtk_message_type_get_type :: 
    IO GType

instance B.Types.TypedObject MessageType where
    glibType = c_gtk_message_type_get_type

instance B.Types.BoxedEnum MessageType

-- Enum MenuDirectionType
-- | An enumeration representing directional movements within a menu.
data MenuDirectionType = 
      MenuDirectionTypeParent
    -- ^ To the parent menu shell
    | MenuDirectionTypeChild
    -- ^ To the submenu, if any, associated with the item
    | MenuDirectionTypeNext
    -- ^ To the next menu item
    | MenuDirectionTypePrev
    -- ^ To the previous menu item
    | AnotherMenuDirectionType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum MenuDirectionType where
    fromEnum MenuDirectionTypeParent = 0
    fromEnum MenuDirectionTypeChild = 1
    fromEnum MenuDirectionTypeNext = 2
    fromEnum MenuDirectionTypePrev = 3
    fromEnum (AnotherMenuDirectionType k) = k

    toEnum 0 = MenuDirectionTypeParent
    toEnum 1 = MenuDirectionTypeChild
    toEnum 2 = MenuDirectionTypeNext
    toEnum 3 = MenuDirectionTypePrev
    toEnum k = AnotherMenuDirectionType k

instance P.Ord MenuDirectionType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes MenuDirectionType = '[]
instance O.HasParentTypes MenuDirectionType

foreign import ccall "gtk_menu_direction_type_get_type" c_gtk_menu_direction_type_get_type :: 
    IO GType

instance B.Types.TypedObject MenuDirectionType where
    glibType = c_gtk_menu_direction_type_get_type

instance B.Types.BoxedEnum MenuDirectionType

-- Enum License
-- | The type of license for an application.
-- 
-- This enumeration can be expanded at later date.
-- 
-- /Since: 3.0/
data License = 
      LicenseUnknown
    -- ^ No license specified
    | LicenseCustom
    -- ^ A license text is going to be specified by the
    --   developer
    | LicenseGpl20
    -- ^ The GNU General Public License, version 2.0 or later
    | LicenseGpl30
    -- ^ The GNU General Public License, version 3.0 or later
    | LicenseLgpl21
    -- ^ The GNU Lesser General Public License, version 2.1 or later
    | LicenseLgpl30
    -- ^ The GNU Lesser General Public License, version 3.0 or later
    | LicenseBsd
    -- ^ The BSD standard license
    | LicenseMitX11
    -- ^ The MIT\/X11 standard license
    | LicenseArtistic
    -- ^ The Artistic License, version 2.0
    | LicenseGpl20Only
    -- ^ The GNU General Public License, version 2.0 only. Since 3.12.
    | LicenseGpl30Only
    -- ^ The GNU General Public License, version 3.0 only. Since 3.12.
    | LicenseLgpl21Only
    -- ^ The GNU Lesser General Public License, version 2.1 only. Since 3.12.
    | LicenseLgpl30Only
    -- ^ The GNU Lesser General Public License, version 3.0 only. Since 3.12.
    | LicenseAgpl30
    -- ^ The GNU Affero General Public License, version 3.0 or later. Since: 3.22.
    | LicenseAgpl30Only
    -- ^ The GNU Affero General Public License, version 3.0 only. Since: 3.22.27.
    | LicenseBsd3
    -- ^ The 3-clause BSD licence. Since: 3.24.20.
    | LicenseApache20
    -- ^ The Apache License, version 2.0. Since: 3.24.20.
    | LicenseMpl20
    -- ^ The Mozilla Public License, version 2.0. Since: 3.24.20.
    | AnotherLicense Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum License where
    fromEnum LicenseUnknown = 0
    fromEnum LicenseCustom = 1
    fromEnum LicenseGpl20 = 2
    fromEnum LicenseGpl30 = 3
    fromEnum LicenseLgpl21 = 4
    fromEnum LicenseLgpl30 = 5
    fromEnum LicenseBsd = 6
    fromEnum LicenseMitX11 = 7
    fromEnum LicenseArtistic = 8
    fromEnum LicenseGpl20Only = 9
    fromEnum LicenseGpl30Only = 10
    fromEnum LicenseLgpl21Only = 11
    fromEnum LicenseLgpl30Only = 12
    fromEnum LicenseAgpl30 = 13
    fromEnum LicenseAgpl30Only = 14
    fromEnum LicenseBsd3 = 15
    fromEnum LicenseApache20 = 16
    fromEnum LicenseMpl20 = 17
    fromEnum (AnotherLicense k) = k

    toEnum 0 = LicenseUnknown
    toEnum 1 = LicenseCustom
    toEnum 2 = LicenseGpl20
    toEnum 3 = LicenseGpl30
    toEnum 4 = LicenseLgpl21
    toEnum 5 = LicenseLgpl30
    toEnum 6 = LicenseBsd
    toEnum 7 = LicenseMitX11
    toEnum 8 = LicenseArtistic
    toEnum 9 = LicenseGpl20Only
    toEnum 10 = LicenseGpl30Only
    toEnum 11 = LicenseLgpl21Only
    toEnum 12 = LicenseLgpl30Only
    toEnum 13 = LicenseAgpl30
    toEnum 14 = LicenseAgpl30Only
    toEnum 15 = LicenseBsd3
    toEnum 16 = LicenseApache20
    toEnum 17 = LicenseMpl20
    toEnum k = AnotherLicense k

instance P.Ord License where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes License = '[]
instance O.HasParentTypes License

foreign import ccall "gtk_license_get_type" c_gtk_license_get_type :: 
    IO GType

instance B.Types.TypedObject License where
    glibType = c_gtk_license_get_type

instance B.Types.BoxedEnum License

-- Enum LevelBarMode
-- | Describes how t'GI.Gtk.Objects.LevelBar.LevelBar' contents should be rendered.
-- Note that this enumeration could be extended with additional modes
-- in the future.
-- 
-- /Since: 3.6/
data LevelBarMode = 
      LevelBarModeContinuous
    -- ^ the bar has a continuous mode
    | LevelBarModeDiscrete
    -- ^ the bar has a discrete mode
    | AnotherLevelBarMode Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum LevelBarMode where
    fromEnum LevelBarModeContinuous = 0
    fromEnum LevelBarModeDiscrete = 1
    fromEnum (AnotherLevelBarMode k) = k

    toEnum 0 = LevelBarModeContinuous
    toEnum 1 = LevelBarModeDiscrete
    toEnum k = AnotherLevelBarMode k

instance P.Ord LevelBarMode where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes LevelBarMode = '[]
instance O.HasParentTypes LevelBarMode

foreign import ccall "gtk_level_bar_mode_get_type" c_gtk_level_bar_mode_get_type :: 
    IO GType

instance B.Types.TypedObject LevelBarMode where
    glibType = c_gtk_level_bar_mode_get_type

instance B.Types.BoxedEnum LevelBarMode

-- Enum Justification
-- | Used for justifying the text inside a t'GI.Gtk.Objects.Label.Label' widget. (See also
-- t'GI.Gtk.Objects.Alignment.Alignment').
data Justification = 
      JustificationLeft
    -- ^ The text is placed at the left edge of the label.
    | JustificationRight
    -- ^ The text is placed at the right edge of the label.
    | JustificationCenter
    -- ^ The text is placed in the center of the label.
    | JustificationFill
    -- ^ The text is placed is distributed across the label.
    | AnotherJustification Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum Justification where
    fromEnum JustificationLeft = 0
    fromEnum JustificationRight = 1
    fromEnum JustificationCenter = 2
    fromEnum JustificationFill = 3
    fromEnum (AnotherJustification k) = k

    toEnum 0 = JustificationLeft
    toEnum 1 = JustificationRight
    toEnum 2 = JustificationCenter
    toEnum 3 = JustificationFill
    toEnum k = AnotherJustification k

instance P.Ord Justification where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes Justification = '[]
instance O.HasParentTypes Justification

foreign import ccall "gtk_justification_get_type" c_gtk_justification_get_type :: 
    IO GType

instance B.Types.TypedObject Justification where
    glibType = c_gtk_justification_get_type

instance B.Types.BoxedEnum Justification

-- Enum InputPurpose
-- | Describes primary purpose of the input widget. This information is
-- useful for on-screen keyboards and similar input methods to decide
-- which keys should be presented to the user.
-- 
-- Note that the purpose is not meant to impose a totally strict rule
-- about allowed characters, and does not replace input validation.
-- It is fine for an on-screen keyboard to let the user override the
-- character set restriction that is expressed by the purpose. The
-- application is expected to validate the entry contents, even if
-- it specified a purpose.
-- 
-- The difference between /@gTKINPUTPURPOSEDIGITS@/ and
-- /@gTKINPUTPURPOSENUMBER@/ is that the former accepts only digits
-- while the latter also some punctuation (like commas or points, plus,
-- minus) and “e” or “E” as in 3.14E+000.
-- 
-- This enumeration may be extended in the future; input methods should
-- interpret unknown values as “free form”.
-- 
-- /Since: 3.6/
data InputPurpose = 
      InputPurposeFreeForm
    -- ^ Allow any character
    | InputPurposeAlpha
    -- ^ Allow only alphabetic characters
    | InputPurposeDigits
    -- ^ Allow only digits
    | InputPurposeNumber
    -- ^ Edited field expects numbers
    | InputPurposePhone
    -- ^ Edited field expects phone number
    | InputPurposeUrl
    -- ^ Edited field expects URL
    | InputPurposeEmail
    -- ^ Edited field expects email address
    | InputPurposeName
    -- ^ Edited field expects the name of a person
    | InputPurposePassword
    -- ^ Like /@gTKINPUTPURPOSEFREEFORM@/, but characters are hidden
    | InputPurposePin
    -- ^ Like /@gTKINPUTPURPOSEDIGITS@/, but characters are hidden
    | InputPurposeTerminal
    -- ^ Allow any character, in addition to control codes
    | AnotherInputPurpose Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum InputPurpose where
    fromEnum InputPurposeFreeForm = 0
    fromEnum InputPurposeAlpha = 1
    fromEnum InputPurposeDigits = 2
    fromEnum InputPurposeNumber = 3
    fromEnum InputPurposePhone = 4
    fromEnum InputPurposeUrl = 5
    fromEnum InputPurposeEmail = 6
    fromEnum InputPurposeName = 7
    fromEnum InputPurposePassword = 8
    fromEnum InputPurposePin = 9
    fromEnum InputPurposeTerminal = 10
    fromEnum (AnotherInputPurpose k) = k

    toEnum 0 = InputPurposeFreeForm
    toEnum 1 = InputPurposeAlpha
    toEnum 2 = InputPurposeDigits
    toEnum 3 = InputPurposeNumber
    toEnum 4 = InputPurposePhone
    toEnum 5 = InputPurposeUrl
    toEnum 6 = InputPurposeEmail
    toEnum 7 = InputPurposeName
    toEnum 8 = InputPurposePassword
    toEnum 9 = InputPurposePin
    toEnum 10 = InputPurposeTerminal
    toEnum k = AnotherInputPurpose k

instance P.Ord InputPurpose where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes InputPurpose = '[]
instance O.HasParentTypes InputPurpose

foreign import ccall "gtk_input_purpose_get_type" c_gtk_input_purpose_get_type :: 
    IO GType

instance B.Types.TypedObject InputPurpose where
    glibType = c_gtk_input_purpose_get_type

instance B.Types.BoxedEnum InputPurpose

-- Enum ImageType
-- | Describes the image data representation used by a t'GI.Gtk.Objects.Image.Image'. If you
-- want to get the image from the widget, you can only get the
-- currently-stored representation. e.g.  if the
-- 'GI.Gtk.Objects.Image.imageGetStorageType' returns @/GTK_IMAGE_PIXBUF/@, then you can
-- call 'GI.Gtk.Objects.Image.imageGetPixbuf' but not 'GI.Gtk.Objects.Image.imageGetStock'.  For empty
-- images, you can request any storage type (call any of the \"get\"
-- functions), but they will all return 'P.Nothing' values.
data ImageType = 
      ImageTypeEmpty
    -- ^ there is no image displayed by the widget
    | ImageTypePixbuf
    -- ^ the widget contains a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf'
    | ImageTypeStock
    -- ^ the widget contains a [stock item name][gtkstock]
    | ImageTypeIconSet
    -- ^ the widget contains a t'GI.Gtk.Structs.IconSet.IconSet'
    | ImageTypeAnimation
    -- ^ the widget contains a t'GI.GdkPixbuf.Objects.PixbufAnimation.PixbufAnimation'
    | ImageTypeIconName
    -- ^ the widget contains a named icon.
    --  This image type was added in GTK+ 2.6
    | ImageTypeGicon
    -- ^ the widget contains a t'GI.Gio.Interfaces.Icon.Icon'.
    --  This image type was added in GTK+ 2.14
    | ImageTypeSurface
    -- ^ the widget contains a t'GI.Cairo.Structs.Surface.Surface'.
    --  This image type was added in GTK+ 3.10
    | AnotherImageType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ImageType where
    fromEnum ImageTypeEmpty = 0
    fromEnum ImageTypePixbuf = 1
    fromEnum ImageTypeStock = 2
    fromEnum ImageTypeIconSet = 3
    fromEnum ImageTypeAnimation = 4
    fromEnum ImageTypeIconName = 5
    fromEnum ImageTypeGicon = 6
    fromEnum ImageTypeSurface = 7
    fromEnum (AnotherImageType k) = k

    toEnum 0 = ImageTypeEmpty
    toEnum 1 = ImageTypePixbuf
    toEnum 2 = ImageTypeStock
    toEnum 3 = ImageTypeIconSet
    toEnum 4 = ImageTypeAnimation
    toEnum 5 = ImageTypeIconName
    toEnum 6 = ImageTypeGicon
    toEnum 7 = ImageTypeSurface
    toEnum k = AnotherImageType k

instance P.Ord ImageType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ImageType = '[]
instance O.HasParentTypes ImageType

foreign import ccall "gtk_image_type_get_type" c_gtk_image_type_get_type :: 
    IO GType

instance B.Types.TypedObject ImageType where
    glibType = c_gtk_image_type_get_type

instance B.Types.BoxedEnum ImageType

-- Enum IconViewDropPosition
-- | An enum for determining where a dropped item goes.
data IconViewDropPosition = 
      IconViewDropPositionNoDrop
    -- ^ no drop possible
    | IconViewDropPositionDropInto
    -- ^ dropped item replaces the item
    | IconViewDropPositionDropLeft
    -- ^ droppped item is inserted to the left
    | IconViewDropPositionDropRight
    -- ^ dropped item is inserted to the right
    | IconViewDropPositionDropAbove
    -- ^ dropped item is inserted above
    | IconViewDropPositionDropBelow
    -- ^ dropped item is inserted below
    | AnotherIconViewDropPosition Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum IconViewDropPosition where
    fromEnum IconViewDropPositionNoDrop = 0
    fromEnum IconViewDropPositionDropInto = 1
    fromEnum IconViewDropPositionDropLeft = 2
    fromEnum IconViewDropPositionDropRight = 3
    fromEnum IconViewDropPositionDropAbove = 4
    fromEnum IconViewDropPositionDropBelow = 5
    fromEnum (AnotherIconViewDropPosition k) = k

    toEnum 0 = IconViewDropPositionNoDrop
    toEnum 1 = IconViewDropPositionDropInto
    toEnum 2 = IconViewDropPositionDropLeft
    toEnum 3 = IconViewDropPositionDropRight
    toEnum 4 = IconViewDropPositionDropAbove
    toEnum 5 = IconViewDropPositionDropBelow
    toEnum k = AnotherIconViewDropPosition k

instance P.Ord IconViewDropPosition where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes IconViewDropPosition = '[]
instance O.HasParentTypes IconViewDropPosition

foreign import ccall "gtk_icon_view_drop_position_get_type" c_gtk_icon_view_drop_position_get_type :: 
    IO GType

instance B.Types.TypedObject IconViewDropPosition where
    glibType = c_gtk_icon_view_drop_position_get_type

instance B.Types.BoxedEnum IconViewDropPosition

-- Enum IconThemeError
-- | Error codes for GtkIconTheme operations.
data IconThemeError = 
      IconThemeErrorNotFound
    -- ^ The icon specified does not exist in the theme
    | IconThemeErrorFailed
    -- ^ An unspecified error occurred.
    | AnotherIconThemeError Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum IconThemeError where
    fromEnum IconThemeErrorNotFound = 0
    fromEnum IconThemeErrorFailed = 1
    fromEnum (AnotherIconThemeError k) = k

    toEnum 0 = IconThemeErrorNotFound
    toEnum 1 = IconThemeErrorFailed
    toEnum k = AnotherIconThemeError k

instance P.Ord IconThemeError where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

instance GErrorClass IconThemeError where
    gerrorClassDomain _ = "gtk-icon-theme-error-quark"

-- | Catch exceptions of type `IconThemeError`. This is a specialized version of `Data.GI.Base.GError.catchGErrorJustDomain`.
catchIconThemeError ::
    IO a ->
    (IconThemeError -> GErrorMessage -> IO a) ->
    IO a
catchIconThemeError = catchGErrorJustDomain

-- | Handle exceptions of type `IconThemeError`. This is a specialized version of `Data.GI.Base.GError.handleGErrorJustDomain`.
handleIconThemeError ::
    (IconThemeError -> GErrorMessage -> IO a) ->
    IO a ->
    IO a
handleIconThemeError = handleGErrorJustDomain

type instance O.ParentTypes IconThemeError = '[]
instance O.HasParentTypes IconThemeError

foreign import ccall "gtk_icon_theme_error_get_type" c_gtk_icon_theme_error_get_type :: 
    IO GType

instance B.Types.TypedObject IconThemeError where
    glibType = c_gtk_icon_theme_error_get_type

instance B.Types.BoxedEnum IconThemeError

-- Enum IconSize
-- | Built-in stock icon sizes.
data IconSize = 
      IconSizeInvalid
    -- ^ Invalid size.
    | IconSizeMenu
    -- ^ Size appropriate for menus (16px).
    | IconSizeSmallToolbar
    -- ^ Size appropriate for small toolbars (16px).
    | IconSizeLargeToolbar
    -- ^ Size appropriate for large toolbars (24px)
    | IconSizeButton
    -- ^ Size appropriate for buttons (16px)
    | IconSizeDnd
    -- ^ Size appropriate for drag and drop (32px)
    | IconSizeDialog
    -- ^ Size appropriate for dialogs (48px)
    | AnotherIconSize Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum IconSize where
    fromEnum IconSizeInvalid = 0
    fromEnum IconSizeMenu = 1
    fromEnum IconSizeSmallToolbar = 2
    fromEnum IconSizeLargeToolbar = 3
    fromEnum IconSizeButton = 4
    fromEnum IconSizeDnd = 5
    fromEnum IconSizeDialog = 6
    fromEnum (AnotherIconSize k) = k

    toEnum 0 = IconSizeInvalid
    toEnum 1 = IconSizeMenu
    toEnum 2 = IconSizeSmallToolbar
    toEnum 3 = IconSizeLargeToolbar
    toEnum 4 = IconSizeButton
    toEnum 5 = IconSizeDnd
    toEnum 6 = IconSizeDialog
    toEnum k = AnotherIconSize k

instance P.Ord IconSize where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes IconSize = '[]
instance O.HasParentTypes IconSize

foreign import ccall "gtk_icon_size_get_type" c_gtk_icon_size_get_type :: 
    IO GType

instance B.Types.TypedObject IconSize where
    glibType = c_gtk_icon_size_get_type

instance B.Types.BoxedEnum IconSize

-- Enum IMStatusStyle
{-# DEPRECATED IMStatusStyle ["(Since version 3.10)"] #-}
-- | Style for input method status. See also
-- [Settings:gtkImStatusStyle]("GI.Gtk.Objects.Settings#g:attr:gtkImStatusStyle")
data IMStatusStyle = 
      IMStatusStyleNothing
    -- ^ Deprecated
    | IMStatusStyleCallback
    -- ^ Deprecated
    | IMStatusStyleNone
    -- ^ Deprecated
    | AnotherIMStatusStyle Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum IMStatusStyle where
    fromEnum IMStatusStyleNothing = 0
    fromEnum IMStatusStyleCallback = 1
    fromEnum IMStatusStyleNone = 2
    fromEnum (AnotherIMStatusStyle k) = k

    toEnum 0 = IMStatusStyleNothing
    toEnum 1 = IMStatusStyleCallback
    toEnum 2 = IMStatusStyleNone
    toEnum k = AnotherIMStatusStyle k

instance P.Ord IMStatusStyle where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes IMStatusStyle = '[]
instance O.HasParentTypes IMStatusStyle

foreign import ccall "gtk_im_status_style_get_type" c_gtk_im_status_style_get_type :: 
    IO GType

instance B.Types.TypedObject IMStatusStyle where
    glibType = c_gtk_im_status_style_get_type

instance B.Types.BoxedEnum IMStatusStyle

-- Enum IMPreeditStyle
{-# DEPRECATED IMPreeditStyle ["(Since version 3.10)"] #-}
-- | Style for input method preedit. See also
-- [Settings:gtkImPreeditStyle]("GI.Gtk.Objects.Settings#g:attr:gtkImPreeditStyle")
data IMPreeditStyle = 
      IMPreeditStyleNothing
    -- ^ Deprecated
    | IMPreeditStyleCallback
    -- ^ Deprecated
    | IMPreeditStyleNone
    -- ^ Deprecated
    | AnotherIMPreeditStyle Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum IMPreeditStyle where
    fromEnum IMPreeditStyleNothing = 0
    fromEnum IMPreeditStyleCallback = 1
    fromEnum IMPreeditStyleNone = 2
    fromEnum (AnotherIMPreeditStyle k) = k

    toEnum 0 = IMPreeditStyleNothing
    toEnum 1 = IMPreeditStyleCallback
    toEnum 2 = IMPreeditStyleNone
    toEnum k = AnotherIMPreeditStyle k

instance P.Ord IMPreeditStyle where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes IMPreeditStyle = '[]
instance O.HasParentTypes IMPreeditStyle

foreign import ccall "gtk_im_preedit_style_get_type" c_gtk_im_preedit_style_get_type :: 
    IO GType

instance B.Types.TypedObject IMPreeditStyle where
    glibType = c_gtk_im_preedit_style_get_type

instance B.Types.BoxedEnum IMPreeditStyle

-- Enum FileChooserError
-- | These identify the various errors that can occur while calling
-- t'GI.Gtk.Interfaces.FileChooser.FileChooser' functions.
data FileChooserError = 
      FileChooserErrorNonexistent
    -- ^ Indicates that a file does not exist.
    | FileChooserErrorBadFilename
    -- ^ Indicates a malformed filename.
    | FileChooserErrorAlreadyExists
    -- ^ Indicates a duplicate path (e.g. when
    --  adding a bookmark).
    | FileChooserErrorIncompleteHostname
    -- ^ Indicates an incomplete hostname (e.g. \"http:\/\/foo\" without a slash after that).
    | AnotherFileChooserError Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum FileChooserError where
    fromEnum FileChooserErrorNonexistent = 0
    fromEnum FileChooserErrorBadFilename = 1
    fromEnum FileChooserErrorAlreadyExists = 2
    fromEnum FileChooserErrorIncompleteHostname = 3
    fromEnum (AnotherFileChooserError k) = k

    toEnum 0 = FileChooserErrorNonexistent
    toEnum 1 = FileChooserErrorBadFilename
    toEnum 2 = FileChooserErrorAlreadyExists
    toEnum 3 = FileChooserErrorIncompleteHostname
    toEnum k = AnotherFileChooserError k

instance P.Ord FileChooserError where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

instance GErrorClass FileChooserError where
    gerrorClassDomain _ = "gtk-file-chooser-error-quark"

-- | Catch exceptions of type `FileChooserError`. This is a specialized version of `Data.GI.Base.GError.catchGErrorJustDomain`.
catchFileChooserError ::
    IO a ->
    (FileChooserError -> GErrorMessage -> IO a) ->
    IO a
catchFileChooserError = catchGErrorJustDomain

-- | Handle exceptions of type `FileChooserError`. This is a specialized version of `Data.GI.Base.GError.handleGErrorJustDomain`.
handleFileChooserError ::
    (FileChooserError -> GErrorMessage -> IO a) ->
    IO a ->
    IO a
handleFileChooserError = handleGErrorJustDomain

type instance O.ParentTypes FileChooserError = '[]
instance O.HasParentTypes FileChooserError

foreign import ccall "gtk_file_chooser_error_get_type" c_gtk_file_chooser_error_get_type :: 
    IO GType

instance B.Types.TypedObject FileChooserError where
    glibType = c_gtk_file_chooser_error_get_type

instance B.Types.BoxedEnum FileChooserError

-- Enum FileChooserConfirmation
-- | Used as a return value of handlers for the
-- [FileChooser::confirmOverwrite]("GI.Gtk.Interfaces.FileChooser#g:signal:confirmOverwrite") signal of a t'GI.Gtk.Interfaces.FileChooser.FileChooser'. This
-- value determines whether the file chooser will present the stock
-- confirmation dialog, accept the user’s choice of a filename, or
-- let the user choose another filename.
-- 
-- /Since: 2.8/
data FileChooserConfirmation = 
      FileChooserConfirmationConfirm
    -- ^ The file chooser will present
    --  its stock dialog to confirm about overwriting an existing file.
    | FileChooserConfirmationAcceptFilename
    -- ^ The file chooser will
    --  terminate and accept the user’s choice of a file name.
    | FileChooserConfirmationSelectAgain
    -- ^ The file chooser will
    --  continue running, so as to let the user select another file name.
    | AnotherFileChooserConfirmation Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum FileChooserConfirmation where
    fromEnum FileChooserConfirmationConfirm = 0
    fromEnum FileChooserConfirmationAcceptFilename = 1
    fromEnum FileChooserConfirmationSelectAgain = 2
    fromEnum (AnotherFileChooserConfirmation k) = k

    toEnum 0 = FileChooserConfirmationConfirm
    toEnum 1 = FileChooserConfirmationAcceptFilename
    toEnum 2 = FileChooserConfirmationSelectAgain
    toEnum k = AnotherFileChooserConfirmation k

instance P.Ord FileChooserConfirmation where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes FileChooserConfirmation = '[]
instance O.HasParentTypes FileChooserConfirmation

foreign import ccall "gtk_file_chooser_confirmation_get_type" c_gtk_file_chooser_confirmation_get_type :: 
    IO GType

instance B.Types.TypedObject FileChooserConfirmation where
    glibType = c_gtk_file_chooser_confirmation_get_type

instance B.Types.BoxedEnum FileChooserConfirmation

-- Enum FileChooserAction
-- | Describes whether a t'GI.Gtk.Interfaces.FileChooser.FileChooser' is being used to open existing files
-- or to save to a possibly new file.
data FileChooserAction = 
      FileChooserActionOpen
    -- ^ Indicates open mode.  The file chooser
    --  will only let the user pick an existing file.
    | FileChooserActionSave
    -- ^ Indicates save mode.  The file chooser
    --  will let the user pick an existing file, or type in a new
    --  filename.
    | FileChooserActionSelectFolder
    -- ^ Indicates an Open mode for
    --  selecting folders.  The file chooser will let the user pick an
    --  existing folder.
    | FileChooserActionCreateFolder
    -- ^ Indicates a mode for creating a
    --  new folder.  The file chooser will let the user name an existing or
    --  new folder.
    | AnotherFileChooserAction Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum FileChooserAction where
    fromEnum FileChooserActionOpen = 0
    fromEnum FileChooserActionSave = 1
    fromEnum FileChooserActionSelectFolder = 2
    fromEnum FileChooserActionCreateFolder = 3
    fromEnum (AnotherFileChooserAction k) = k

    toEnum 0 = FileChooserActionOpen
    toEnum 1 = FileChooserActionSave
    toEnum 2 = FileChooserActionSelectFolder
    toEnum 3 = FileChooserActionCreateFolder
    toEnum k = AnotherFileChooserAction k

instance P.Ord FileChooserAction where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes FileChooserAction = '[]
instance O.HasParentTypes FileChooserAction

foreign import ccall "gtk_file_chooser_action_get_type" c_gtk_file_chooser_action_get_type :: 
    IO GType

instance B.Types.TypedObject FileChooserAction where
    glibType = c_gtk_file_chooser_action_get_type

instance B.Types.BoxedEnum FileChooserAction

-- Enum ExpanderStyle
-- | Used to specify the style of the expanders drawn by a t'GI.Gtk.Objects.TreeView.TreeView'.
data ExpanderStyle = 
      ExpanderStyleCollapsed
    -- ^ The style used for a collapsed subtree.
    | ExpanderStyleSemiCollapsed
    -- ^ Intermediate style used during animation.
    | ExpanderStyleSemiExpanded
    -- ^ Intermediate style used during animation.
    | ExpanderStyleExpanded
    -- ^ The style used for an expanded subtree.
    | AnotherExpanderStyle Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ExpanderStyle where
    fromEnum ExpanderStyleCollapsed = 0
    fromEnum ExpanderStyleSemiCollapsed = 1
    fromEnum ExpanderStyleSemiExpanded = 2
    fromEnum ExpanderStyleExpanded = 3
    fromEnum (AnotherExpanderStyle k) = k

    toEnum 0 = ExpanderStyleCollapsed
    toEnum 1 = ExpanderStyleSemiCollapsed
    toEnum 2 = ExpanderStyleSemiExpanded
    toEnum 3 = ExpanderStyleExpanded
    toEnum k = AnotherExpanderStyle k

instance P.Ord ExpanderStyle where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ExpanderStyle = '[]
instance O.HasParentTypes ExpanderStyle

foreign import ccall "gtk_expander_style_get_type" c_gtk_expander_style_get_type :: 
    IO GType

instance B.Types.TypedObject ExpanderStyle where
    glibType = c_gtk_expander_style_get_type

instance B.Types.BoxedEnum ExpanderStyle

-- Enum EventSequenceState
-- | Describes the state of a t'GI.Gdk.Structs.EventSequence.EventSequence' in a t'GI.Gtk.Objects.Gesture.Gesture'.
-- 
-- /Since: 3.14/
data EventSequenceState = 
      EventSequenceStateNone
    -- ^ The sequence is handled, but not grabbed.
    | EventSequenceStateClaimed
    -- ^ The sequence is handled and grabbed.
    | EventSequenceStateDenied
    -- ^ The sequence is denied.
    | AnotherEventSequenceState Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum EventSequenceState where
    fromEnum EventSequenceStateNone = 0
    fromEnum EventSequenceStateClaimed = 1
    fromEnum EventSequenceStateDenied = 2
    fromEnum (AnotherEventSequenceState k) = k

    toEnum 0 = EventSequenceStateNone
    toEnum 1 = EventSequenceStateClaimed
    toEnum 2 = EventSequenceStateDenied
    toEnum k = AnotherEventSequenceState k

instance P.Ord EventSequenceState where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes EventSequenceState = '[]
instance O.HasParentTypes EventSequenceState

foreign import ccall "gtk_event_sequence_state_get_type" c_gtk_event_sequence_state_get_type :: 
    IO GType

instance B.Types.TypedObject EventSequenceState where
    glibType = c_gtk_event_sequence_state_get_type

instance B.Types.BoxedEnum EventSequenceState

-- Enum EntryIconPosition
-- | Specifies the side of the entry at which an icon is placed.
-- 
-- /Since: 2.16/
data EntryIconPosition = 
      EntryIconPositionPrimary
    -- ^ At the beginning of the entry (depending on the text direction).
    | EntryIconPositionSecondary
    -- ^ At the end of the entry (depending on the text direction).
    | AnotherEntryIconPosition Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum EntryIconPosition where
    fromEnum EntryIconPositionPrimary = 0
    fromEnum EntryIconPositionSecondary = 1
    fromEnum (AnotherEntryIconPosition k) = k

    toEnum 0 = EntryIconPositionPrimary
    toEnum 1 = EntryIconPositionSecondary
    toEnum k = AnotherEntryIconPosition k

instance P.Ord EntryIconPosition where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes EntryIconPosition = '[]
instance O.HasParentTypes EntryIconPosition

foreign import ccall "gtk_entry_icon_position_get_type" c_gtk_entry_icon_position_get_type :: 
    IO GType

instance B.Types.TypedObject EntryIconPosition where
    glibType = c_gtk_entry_icon_position_get_type

instance B.Types.BoxedEnum EntryIconPosition

-- Enum DragResult
-- | Gives an indication why a drag operation failed.
-- The value can by obtained by connecting to the
-- [Widget::dragFailed]("GI.Gtk.Objects.Widget#g:signal:dragFailed") signal.
data DragResult = 
      DragResultSuccess
    -- ^ The drag operation was successful.
    | DragResultNoTarget
    -- ^ No suitable drag target.
    | DragResultUserCancelled
    -- ^ The user cancelled the drag operation.
    | DragResultTimeoutExpired
    -- ^ The drag operation timed out.
    | DragResultGrabBroken
    -- ^ The pointer or keyboard grab used
    --  for the drag operation was broken.
    | DragResultError
    -- ^ The drag operation failed due to some
    --  unspecified error.
    | AnotherDragResult Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum DragResult where
    fromEnum DragResultSuccess = 0
    fromEnum DragResultNoTarget = 1
    fromEnum DragResultUserCancelled = 2
    fromEnum DragResultTimeoutExpired = 3
    fromEnum DragResultGrabBroken = 4
    fromEnum DragResultError = 5
    fromEnum (AnotherDragResult k) = k

    toEnum 0 = DragResultSuccess
    toEnum 1 = DragResultNoTarget
    toEnum 2 = DragResultUserCancelled
    toEnum 3 = DragResultTimeoutExpired
    toEnum 4 = DragResultGrabBroken
    toEnum 5 = DragResultError
    toEnum k = AnotherDragResult k

instance P.Ord DragResult where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes DragResult = '[]
instance O.HasParentTypes DragResult

foreign import ccall "gtk_drag_result_get_type" c_gtk_drag_result_get_type :: 
    IO GType

instance B.Types.TypedObject DragResult where
    glibType = c_gtk_drag_result_get_type

instance B.Types.BoxedEnum DragResult

-- Enum DirectionType
-- | Focus movement types.
data DirectionType = 
      DirectionTypeTabForward
    -- ^ Move forward.
    | DirectionTypeTabBackward
    -- ^ Move backward.
    | DirectionTypeUp
    -- ^ Move up.
    | DirectionTypeDown
    -- ^ Move down.
    | DirectionTypeLeft
    -- ^ Move left.
    | DirectionTypeRight
    -- ^ Move right.
    | AnotherDirectionType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum DirectionType where
    fromEnum DirectionTypeTabForward = 0
    fromEnum DirectionTypeTabBackward = 1
    fromEnum DirectionTypeUp = 2
    fromEnum DirectionTypeDown = 3
    fromEnum DirectionTypeLeft = 4
    fromEnum DirectionTypeRight = 5
    fromEnum (AnotherDirectionType k) = k

    toEnum 0 = DirectionTypeTabForward
    toEnum 1 = DirectionTypeTabBackward
    toEnum 2 = DirectionTypeUp
    toEnum 3 = DirectionTypeDown
    toEnum 4 = DirectionTypeLeft
    toEnum 5 = DirectionTypeRight
    toEnum k = AnotherDirectionType k

instance P.Ord DirectionType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes DirectionType = '[]
instance O.HasParentTypes DirectionType

foreign import ccall "gtk_direction_type_get_type" c_gtk_direction_type_get_type :: 
    IO GType

instance B.Types.TypedObject DirectionType where
    glibType = c_gtk_direction_type_get_type

instance B.Types.BoxedEnum DirectionType

-- Enum DeleteType
-- | See also: [Entry::deleteFromCursor]("GI.Gtk.Objects.Entry#g:signal:deleteFromCursor").
data DeleteType = 
      DeleteTypeChars
    -- ^ Delete characters.
    | DeleteTypeWordEnds
    -- ^ Delete only the portion of the word to the
    --   left\/right of cursor if we’re in the middle of a word.
    | DeleteTypeWords
    -- ^ Delete words.
    | DeleteTypeDisplayLines
    -- ^ Delete display-lines. Display-lines
    --   refers to the visible lines, with respect to to the current line
    --   breaks. As opposed to paragraphs, which are defined by line
    --   breaks in the input.
    | DeleteTypeDisplayLineEnds
    -- ^ Delete only the portion of the
    --   display-line to the left\/right of cursor.
    | DeleteTypeParagraphEnds
    -- ^ Delete to the end of the
    --   paragraph. Like C-k in Emacs (or its reverse).
    | DeleteTypeParagraphs
    -- ^ Delete entire line. Like C-k in pico.
    | DeleteTypeWhitespace
    -- ^ Delete only whitespace. Like M-\\ in Emacs.
    | AnotherDeleteType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum DeleteType where
    fromEnum DeleteTypeChars = 0
    fromEnum DeleteTypeWordEnds = 1
    fromEnum DeleteTypeWords = 2
    fromEnum DeleteTypeDisplayLines = 3
    fromEnum DeleteTypeDisplayLineEnds = 4
    fromEnum DeleteTypeParagraphEnds = 5
    fromEnum DeleteTypeParagraphs = 6
    fromEnum DeleteTypeWhitespace = 7
    fromEnum (AnotherDeleteType k) = k

    toEnum 0 = DeleteTypeChars
    toEnum 1 = DeleteTypeWordEnds
    toEnum 2 = DeleteTypeWords
    toEnum 3 = DeleteTypeDisplayLines
    toEnum 4 = DeleteTypeDisplayLineEnds
    toEnum 5 = DeleteTypeParagraphEnds
    toEnum 6 = DeleteTypeParagraphs
    toEnum 7 = DeleteTypeWhitespace
    toEnum k = AnotherDeleteType k

instance P.Ord DeleteType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes DeleteType = '[]
instance O.HasParentTypes DeleteType

foreign import ccall "gtk_delete_type_get_type" c_gtk_delete_type_get_type :: 
    IO GType

instance B.Types.TypedObject DeleteType where
    glibType = c_gtk_delete_type_get_type

instance B.Types.BoxedEnum DeleteType

-- Enum CssSectionType
-- | The different types of sections indicate parts of a CSS document as
-- parsed by GTK’s CSS parser. They are oriented towards the
-- <http://www.w3.org/TR/CSS21/grammar.html CSS Grammar>,
-- but may contain extensions.
-- 
-- More types might be added in the future as the parser incorporates
-- more features.
-- 
-- /Since: 3.2/
data CssSectionType = 
      CssSectionTypeDocument
    -- ^ The section describes a complete document.
    --   This section time is the only one where 'GI.Gtk.Structs.CssSection.cssSectionGetParent'
    --   might return 'P.Nothing'.
    | CssSectionTypeImport
    -- ^ The section defines an import rule.
    | CssSectionTypeColorDefinition
    -- ^ The section defines a color. This
    --   is a GTK extension to CSS.
    | CssSectionTypeBindingSet
    -- ^ The section defines a binding set. This
    --   is a GTK extension to CSS.
    | CssSectionTypeRuleset
    -- ^ The section defines a CSS ruleset.
    | CssSectionTypeSelector
    -- ^ The section defines a CSS selector.
    | CssSectionTypeDeclaration
    -- ^ The section defines the declaration of
    --   a CSS variable.
    | CssSectionTypeValue
    -- ^ The section defines the value of a CSS declaration.
    | CssSectionTypeKeyframes
    -- ^ The section defines keyframes. See <http://dev.w3.org/csswg/css3-animations/#keyframes CSS
    --   Animations> for details. Since 3.6
    | AnotherCssSectionType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum CssSectionType where
    fromEnum CssSectionTypeDocument = 0
    fromEnum CssSectionTypeImport = 1
    fromEnum CssSectionTypeColorDefinition = 2
    fromEnum CssSectionTypeBindingSet = 3
    fromEnum CssSectionTypeRuleset = 4
    fromEnum CssSectionTypeSelector = 5
    fromEnum CssSectionTypeDeclaration = 6
    fromEnum CssSectionTypeValue = 7
    fromEnum CssSectionTypeKeyframes = 8
    fromEnum (AnotherCssSectionType k) = k

    toEnum 0 = CssSectionTypeDocument
    toEnum 1 = CssSectionTypeImport
    toEnum 2 = CssSectionTypeColorDefinition
    toEnum 3 = CssSectionTypeBindingSet
    toEnum 4 = CssSectionTypeRuleset
    toEnum 5 = CssSectionTypeSelector
    toEnum 6 = CssSectionTypeDeclaration
    toEnum 7 = CssSectionTypeValue
    toEnum 8 = CssSectionTypeKeyframes
    toEnum k = AnotherCssSectionType k

instance P.Ord CssSectionType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes CssSectionType = '[]
instance O.HasParentTypes CssSectionType

foreign import ccall "gtk_css_section_type_get_type" c_gtk_css_section_type_get_type :: 
    IO GType

instance B.Types.TypedObject CssSectionType where
    glibType = c_gtk_css_section_type_get_type

instance B.Types.BoxedEnum CssSectionType

-- Enum CssProviderError
-- | Error codes for @/GTK_CSS_PROVIDER_ERROR/@.
data CssProviderError = 
      CssProviderErrorFailed
    -- ^ Failed.
    | CssProviderErrorSyntax
    -- ^ Syntax error.
    | CssProviderErrorImport
    -- ^ Import error.
    | CssProviderErrorName
    -- ^ Name error.
    | CssProviderErrorDeprecated
    -- ^ Deprecation error.
    | CssProviderErrorUnknownValue
    -- ^ Unknown value.
    | AnotherCssProviderError Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum CssProviderError where
    fromEnum CssProviderErrorFailed = 0
    fromEnum CssProviderErrorSyntax = 1
    fromEnum CssProviderErrorImport = 2
    fromEnum CssProviderErrorName = 3
    fromEnum CssProviderErrorDeprecated = 4
    fromEnum CssProviderErrorUnknownValue = 5
    fromEnum (AnotherCssProviderError k) = k

    toEnum 0 = CssProviderErrorFailed
    toEnum 1 = CssProviderErrorSyntax
    toEnum 2 = CssProviderErrorImport
    toEnum 3 = CssProviderErrorName
    toEnum 4 = CssProviderErrorDeprecated
    toEnum 5 = CssProviderErrorUnknownValue
    toEnum k = AnotherCssProviderError k

instance P.Ord CssProviderError where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

instance GErrorClass CssProviderError where
    gerrorClassDomain _ = "gtk-css-provider-error-quark"

-- | Catch exceptions of type `CssProviderError`. This is a specialized version of `Data.GI.Base.GError.catchGErrorJustDomain`.
catchCssProviderError ::
    IO a ->
    (CssProviderError -> GErrorMessage -> IO a) ->
    IO a
catchCssProviderError = catchGErrorJustDomain

-- | Handle exceptions of type `CssProviderError`. This is a specialized version of `Data.GI.Base.GError.handleGErrorJustDomain`.
handleCssProviderError ::
    (CssProviderError -> GErrorMessage -> IO a) ->
    IO a ->
    IO a
handleCssProviderError = handleGErrorJustDomain

type instance O.ParentTypes CssProviderError = '[]
instance O.HasParentTypes CssProviderError

foreign import ccall "gtk_css_provider_error_get_type" c_gtk_css_provider_error_get_type :: 
    IO GType

instance B.Types.TypedObject CssProviderError where
    glibType = c_gtk_css_provider_error_get_type

instance B.Types.BoxedEnum CssProviderError

-- Enum CornerType
-- | Specifies which corner a child widget should be placed in when packed into
-- a t'GI.Gtk.Objects.ScrolledWindow.ScrolledWindow'. This is effectively the opposite of where the scroll
-- bars are placed.
data CornerType = 
      CornerTypeTopLeft
    -- ^ Place the scrollbars on the right and bottom of the
    --  widget (default behaviour).
    | CornerTypeBottomLeft
    -- ^ Place the scrollbars on the top and right of the
    --  widget.
    | CornerTypeTopRight
    -- ^ Place the scrollbars on the left and bottom of the
    --  widget.
    | CornerTypeBottomRight
    -- ^ Place the scrollbars on the top and left of the
    --  widget.
    | AnotherCornerType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum CornerType where
    fromEnum CornerTypeTopLeft = 0
    fromEnum CornerTypeBottomLeft = 1
    fromEnum CornerTypeTopRight = 2
    fromEnum CornerTypeBottomRight = 3
    fromEnum (AnotherCornerType k) = k

    toEnum 0 = CornerTypeTopLeft
    toEnum 1 = CornerTypeBottomLeft
    toEnum 2 = CornerTypeTopRight
    toEnum 3 = CornerTypeBottomRight
    toEnum k = AnotherCornerType k

instance P.Ord CornerType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes CornerType = '[]
instance O.HasParentTypes CornerType

foreign import ccall "gtk_corner_type_get_type" c_gtk_corner_type_get_type :: 
    IO GType

instance B.Types.TypedObject CornerType where
    glibType = c_gtk_corner_type_get_type

instance B.Types.BoxedEnum CornerType

-- Enum CellRendererMode
-- | Identifies how the user can interact with a particular cell.
data CellRendererMode = 
      CellRendererModeInert
    -- ^ The cell is just for display
    --  and cannot be interacted with.  Note that this doesn’t mean that eg. the
    --  row being drawn can’t be selected -- just that a particular element of
    --  it cannot be individually modified.
    | CellRendererModeActivatable
    -- ^ The cell can be clicked.
    | CellRendererModeEditable
    -- ^ The cell can be edited or otherwise modified.
    | AnotherCellRendererMode Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum CellRendererMode where
    fromEnum CellRendererModeInert = 0
    fromEnum CellRendererModeActivatable = 1
    fromEnum CellRendererModeEditable = 2
    fromEnum (AnotherCellRendererMode k) = k

    toEnum 0 = CellRendererModeInert
    toEnum 1 = CellRendererModeActivatable
    toEnum 2 = CellRendererModeEditable
    toEnum k = AnotherCellRendererMode k

instance P.Ord CellRendererMode where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes CellRendererMode = '[]
instance O.HasParentTypes CellRendererMode

foreign import ccall "gtk_cell_renderer_mode_get_type" c_gtk_cell_renderer_mode_get_type :: 
    IO GType

instance B.Types.TypedObject CellRendererMode where
    glibType = c_gtk_cell_renderer_mode_get_type

instance B.Types.BoxedEnum CellRendererMode

-- Enum CellRendererAccelMode
-- | Determines if the edited accelerators are GTK+ accelerators. If
-- they are, consumed modifiers are suppressed, only accelerators
-- accepted by GTK+ are allowed, and the accelerators are rendered
-- in the same way as they are in menus.
data CellRendererAccelMode = 
      CellRendererAccelModeGtk
    -- ^ GTK+ accelerators mode
    | CellRendererAccelModeOther
    -- ^ Other accelerator mode
    | AnotherCellRendererAccelMode Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum CellRendererAccelMode where
    fromEnum CellRendererAccelModeGtk = 0
    fromEnum CellRendererAccelModeOther = 1
    fromEnum (AnotherCellRendererAccelMode k) = k

    toEnum 0 = CellRendererAccelModeGtk
    toEnum 1 = CellRendererAccelModeOther
    toEnum k = AnotherCellRendererAccelMode k

instance P.Ord CellRendererAccelMode where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes CellRendererAccelMode = '[]
instance O.HasParentTypes CellRendererAccelMode

foreign import ccall "gtk_cell_renderer_accel_mode_get_type" c_gtk_cell_renderer_accel_mode_get_type :: 
    IO GType

instance B.Types.TypedObject CellRendererAccelMode where
    glibType = c_gtk_cell_renderer_accel_mode_get_type

instance B.Types.BoxedEnum CellRendererAccelMode

-- Enum ButtonsType
-- | Prebuilt sets of buttons for the dialog. If
-- none of these choices are appropriate, simply use 'GI.Gtk.Enums.ButtonsTypeNone'
-- then call @/gtk_dialog_add_buttons()/@.
-- 
-- > Please note that 'GI.Gtk.Enums.ButtonsTypeOk', 'GI.Gtk.Enums.ButtonsTypeYesNo'
-- > and 'GI.Gtk.Enums.ButtonsTypeOkCancel' are discouraged by the
-- > <http://library.gnome.org/devel/hig-book/stable/ GNOME Human Interface Guidelines>.
data ButtonsType = 
      ButtonsTypeNone
    -- ^ no buttons at all
    | ButtonsTypeOk
    -- ^ an OK button
    | ButtonsTypeClose
    -- ^ a Close button
    | ButtonsTypeCancel
    -- ^ a Cancel button
    | ButtonsTypeYesNo
    -- ^ Yes and No buttons
    | ButtonsTypeOkCancel
    -- ^ OK and Cancel buttons
    | AnotherButtonsType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ButtonsType where
    fromEnum ButtonsTypeNone = 0
    fromEnum ButtonsTypeOk = 1
    fromEnum ButtonsTypeClose = 2
    fromEnum ButtonsTypeCancel = 3
    fromEnum ButtonsTypeYesNo = 4
    fromEnum ButtonsTypeOkCancel = 5
    fromEnum (AnotherButtonsType k) = k

    toEnum 0 = ButtonsTypeNone
    toEnum 1 = ButtonsTypeOk
    toEnum 2 = ButtonsTypeClose
    toEnum 3 = ButtonsTypeCancel
    toEnum 4 = ButtonsTypeYesNo
    toEnum 5 = ButtonsTypeOkCancel
    toEnum k = AnotherButtonsType k

instance P.Ord ButtonsType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ButtonsType = '[]
instance O.HasParentTypes ButtonsType

foreign import ccall "gtk_buttons_type_get_type" c_gtk_buttons_type_get_type :: 
    IO GType

instance B.Types.TypedObject ButtonsType where
    glibType = c_gtk_buttons_type_get_type

instance B.Types.BoxedEnum ButtonsType

-- Enum ButtonRole
-- | The role specifies the desired appearance of a t'GI.Gtk.Objects.ModelButton.ModelButton'.
data ButtonRole = 
      ButtonRoleNormal
    -- ^ A plain button
    | ButtonRoleCheck
    -- ^ A check button
    | ButtonRoleRadio
    -- ^ A radio button
    | AnotherButtonRole Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ButtonRole where
    fromEnum ButtonRoleNormal = 0
    fromEnum ButtonRoleCheck = 1
    fromEnum ButtonRoleRadio = 2
    fromEnum (AnotherButtonRole k) = k

    toEnum 0 = ButtonRoleNormal
    toEnum 1 = ButtonRoleCheck
    toEnum 2 = ButtonRoleRadio
    toEnum k = AnotherButtonRole k

instance P.Ord ButtonRole where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ButtonRole = '[]
instance O.HasParentTypes ButtonRole

foreign import ccall "gtk_button_role_get_type" c_gtk_button_role_get_type :: 
    IO GType

instance B.Types.TypedObject ButtonRole where
    glibType = c_gtk_button_role_get_type

instance B.Types.BoxedEnum ButtonRole

-- Enum ButtonBoxStyle
-- | Used to dictate the style that a t'GI.Gtk.Objects.ButtonBox.ButtonBox' uses to layout the buttons it
-- contains.
data ButtonBoxStyle = 
      ButtonBoxStyleSpread
    -- ^ Buttons are evenly spread across the box.
    | ButtonBoxStyleEdge
    -- ^ Buttons are placed at the edges of the box.
    | ButtonBoxStyleStart
    -- ^ Buttons are grouped towards the start of the box,
    --   (on the left for a HBox, or the top for a VBox).
    | ButtonBoxStyleEnd
    -- ^ Buttons are grouped towards the end of the box,
    --   (on the right for a HBox, or the bottom for a VBox).
    | ButtonBoxStyleCenter
    -- ^ Buttons are centered in the box. Since 2.12.
    | ButtonBoxStyleExpand
    -- ^ Buttons expand to fill the box. This entails giving
    --   buttons a \"linked\" appearance, making button sizes homogeneous, and
    --   setting spacing to 0 (same as calling 'GI.Gtk.Objects.Box.boxSetHomogeneous' and
    --   'GI.Gtk.Objects.Box.boxSetSpacing' manually). Since 3.12.
    | AnotherButtonBoxStyle Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ButtonBoxStyle where
    fromEnum ButtonBoxStyleSpread = 1
    fromEnum ButtonBoxStyleEdge = 2
    fromEnum ButtonBoxStyleStart = 3
    fromEnum ButtonBoxStyleEnd = 4
    fromEnum ButtonBoxStyleCenter = 5
    fromEnum ButtonBoxStyleExpand = 6
    fromEnum (AnotherButtonBoxStyle k) = k

    toEnum 1 = ButtonBoxStyleSpread
    toEnum 2 = ButtonBoxStyleEdge
    toEnum 3 = ButtonBoxStyleStart
    toEnum 4 = ButtonBoxStyleEnd
    toEnum 5 = ButtonBoxStyleCenter
    toEnum 6 = ButtonBoxStyleExpand
    toEnum k = AnotherButtonBoxStyle k

instance P.Ord ButtonBoxStyle where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ButtonBoxStyle = '[]
instance O.HasParentTypes ButtonBoxStyle

foreign import ccall "gtk_button_box_style_get_type" c_gtk_button_box_style_get_type :: 
    IO GType

instance B.Types.TypedObject ButtonBoxStyle where
    glibType = c_gtk_button_box_style_get_type

instance B.Types.BoxedEnum ButtonBoxStyle

-- Enum BuilderError
-- | Error codes that identify various errors that can occur while using
-- t'GI.Gtk.Objects.Builder.Builder'.
data BuilderError = 
      BuilderErrorInvalidTypeFunction
    -- ^ A type-func attribute didn’t name
    --  a function that returns a t'GType'.
    | BuilderErrorUnhandledTag
    -- ^ The input contained a tag that t'GI.Gtk.Objects.Builder.Builder'
    --  can’t handle.
    | BuilderErrorMissingAttribute
    -- ^ An attribute that is required by
    --  t'GI.Gtk.Objects.Builder.Builder' was missing.
    | BuilderErrorInvalidAttribute
    -- ^ t'GI.Gtk.Objects.Builder.Builder' found an attribute that
    --  it doesn’t understand.
    | BuilderErrorInvalidTag
    -- ^ t'GI.Gtk.Objects.Builder.Builder' found a tag that
    --  it doesn’t understand.
    | BuilderErrorMissingPropertyValue
    -- ^ A required property value was
    --  missing.
    | BuilderErrorInvalidValue
    -- ^ t'GI.Gtk.Objects.Builder.Builder' couldn’t parse
    --  some attribute value.
    | BuilderErrorVersionMismatch
    -- ^ The input file requires a newer version
    --  of GTK+.
    | BuilderErrorDuplicateId
    -- ^ An object id occurred twice.
    | BuilderErrorObjectTypeRefused
    -- ^ A specified object type is of the same type or
    --  derived from the type of the composite class being extended with builder XML.
    | BuilderErrorTemplateMismatch
    -- ^ The wrong type was specified in a composite class’s template XML
    | BuilderErrorInvalidProperty
    -- ^ The specified property is unknown for the object class.
    | BuilderErrorInvalidSignal
    -- ^ The specified signal is unknown for the object class.
    | BuilderErrorInvalidId
    -- ^ An object id is unknown
    | AnotherBuilderError Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum BuilderError where
    fromEnum BuilderErrorInvalidTypeFunction = 0
    fromEnum BuilderErrorUnhandledTag = 1
    fromEnum BuilderErrorMissingAttribute = 2
    fromEnum BuilderErrorInvalidAttribute = 3
    fromEnum BuilderErrorInvalidTag = 4
    fromEnum BuilderErrorMissingPropertyValue = 5
    fromEnum BuilderErrorInvalidValue = 6
    fromEnum BuilderErrorVersionMismatch = 7
    fromEnum BuilderErrorDuplicateId = 8
    fromEnum BuilderErrorObjectTypeRefused = 9
    fromEnum BuilderErrorTemplateMismatch = 10
    fromEnum BuilderErrorInvalidProperty = 11
    fromEnum BuilderErrorInvalidSignal = 12
    fromEnum BuilderErrorInvalidId = 13
    fromEnum (AnotherBuilderError k) = k

    toEnum 0 = BuilderErrorInvalidTypeFunction
    toEnum 1 = BuilderErrorUnhandledTag
    toEnum 2 = BuilderErrorMissingAttribute
    toEnum 3 = BuilderErrorInvalidAttribute
    toEnum 4 = BuilderErrorInvalidTag
    toEnum 5 = BuilderErrorMissingPropertyValue
    toEnum 6 = BuilderErrorInvalidValue
    toEnum 7 = BuilderErrorVersionMismatch
    toEnum 8 = BuilderErrorDuplicateId
    toEnum 9 = BuilderErrorObjectTypeRefused
    toEnum 10 = BuilderErrorTemplateMismatch
    toEnum 11 = BuilderErrorInvalidProperty
    toEnum 12 = BuilderErrorInvalidSignal
    toEnum 13 = BuilderErrorInvalidId
    toEnum k = AnotherBuilderError k

instance P.Ord BuilderError where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

instance GErrorClass BuilderError where
    gerrorClassDomain _ = "gtk-builder-error-quark"

-- | Catch exceptions of type `BuilderError`. This is a specialized version of `Data.GI.Base.GError.catchGErrorJustDomain`.
catchBuilderError ::
    IO a ->
    (BuilderError -> GErrorMessage -> IO a) ->
    IO a
catchBuilderError = catchGErrorJustDomain

-- | Handle exceptions of type `BuilderError`. This is a specialized version of `Data.GI.Base.GError.handleGErrorJustDomain`.
handleBuilderError ::
    (BuilderError -> GErrorMessage -> IO a) ->
    IO a ->
    IO a
handleBuilderError = handleGErrorJustDomain

type instance O.ParentTypes BuilderError = '[]
instance O.HasParentTypes BuilderError

foreign import ccall "gtk_builder_error_get_type" c_gtk_builder_error_get_type :: 
    IO GType

instance B.Types.TypedObject BuilderError where
    glibType = c_gtk_builder_error_get_type

instance B.Types.BoxedEnum BuilderError

-- Enum BorderStyle
-- | Describes how the border of a UI element should be rendered.
data BorderStyle = 
      BorderStyleNone
    -- ^ No visible border
    | BorderStyleSolid
    -- ^ A single line segment
    | BorderStyleInset
    -- ^ Looks as if the content is sunken into the canvas
    | BorderStyleOutset
    -- ^ Looks as if the content is coming out of the canvas
    | BorderStyleHidden
    -- ^ Same as /@gTKBORDERSTYLENONE@/
    | BorderStyleDotted
    -- ^ A series of round dots
    | BorderStyleDashed
    -- ^ A series of square-ended dashes
    | BorderStyleDouble
    -- ^ Two parallel lines with some space between them
    | BorderStyleGroove
    -- ^ Looks as if it were carved in the canvas
    | BorderStyleRidge
    -- ^ Looks as if it were coming out of the canvas
    | AnotherBorderStyle Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum BorderStyle where
    fromEnum BorderStyleNone = 0
    fromEnum BorderStyleSolid = 1
    fromEnum BorderStyleInset = 2
    fromEnum BorderStyleOutset = 3
    fromEnum BorderStyleHidden = 4
    fromEnum BorderStyleDotted = 5
    fromEnum BorderStyleDashed = 6
    fromEnum BorderStyleDouble = 7
    fromEnum BorderStyleGroove = 8
    fromEnum BorderStyleRidge = 9
    fromEnum (AnotherBorderStyle k) = k

    toEnum 0 = BorderStyleNone
    toEnum 1 = BorderStyleSolid
    toEnum 2 = BorderStyleInset
    toEnum 3 = BorderStyleOutset
    toEnum 4 = BorderStyleHidden
    toEnum 5 = BorderStyleDotted
    toEnum 6 = BorderStyleDashed
    toEnum 7 = BorderStyleDouble
    toEnum 8 = BorderStyleGroove
    toEnum 9 = BorderStyleRidge
    toEnum k = AnotherBorderStyle k

instance P.Ord BorderStyle where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes BorderStyle = '[]
instance O.HasParentTypes BorderStyle

foreign import ccall "gtk_border_style_get_type" c_gtk_border_style_get_type :: 
    IO GType

instance B.Types.TypedObject BorderStyle where
    glibType = c_gtk_border_style_get_type

instance B.Types.BoxedEnum BorderStyle

-- Enum BaselinePosition
-- | Whenever a container has some form of natural row it may align
-- children in that row along a common typographical baseline. If
-- the amount of verical space in the row is taller than the total
-- requested height of the baseline-aligned children then it can use a
-- t'GI.Gtk.Enums.BaselinePosition' to select where to put the baseline inside the
-- extra availible space.
-- 
-- /Since: 3.10/
data BaselinePosition = 
      BaselinePositionTop
    -- ^ Align the baseline at the top
    | BaselinePositionCenter
    -- ^ Center the baseline
    | BaselinePositionBottom
    -- ^ Align the baseline at the bottom
    | AnotherBaselinePosition Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum BaselinePosition where
    fromEnum BaselinePositionTop = 0
    fromEnum BaselinePositionCenter = 1
    fromEnum BaselinePositionBottom = 2
    fromEnum (AnotherBaselinePosition k) = k

    toEnum 0 = BaselinePositionTop
    toEnum 1 = BaselinePositionCenter
    toEnum 2 = BaselinePositionBottom
    toEnum k = AnotherBaselinePosition k

instance P.Ord BaselinePosition where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes BaselinePosition = '[]
instance O.HasParentTypes BaselinePosition

foreign import ccall "gtk_baseline_position_get_type" c_gtk_baseline_position_get_type :: 
    IO GType

instance B.Types.TypedObject BaselinePosition where
    glibType = c_gtk_baseline_position_get_type

instance B.Types.BoxedEnum BaselinePosition

-- Enum AssistantPageType
-- | An enum for determining the page role inside the t'GI.Gtk.Objects.Assistant.Assistant'. It\'s
-- used to handle buttons sensitivity and visibility.
-- 
-- Note that an assistant needs to end its page flow with a page of type
-- 'GI.Gtk.Enums.AssistantPageTypeConfirm', 'GI.Gtk.Enums.AssistantPageTypeSummary' or
-- 'GI.Gtk.Enums.AssistantPageTypeProgress' to be correct.
-- 
-- The Cancel button will only be shown if the page isn’t “committed”.
-- See 'GI.Gtk.Objects.Assistant.assistantCommit' for details.
data AssistantPageType = 
      AssistantPageTypeContent
    -- ^ The page has regular contents. Both the
    --  Back and forward buttons will be shown.
    | AssistantPageTypeIntro
    -- ^ The page contains an introduction to the
    --  assistant task. Only the Forward button will be shown if there is a
    --   next page.
    | AssistantPageTypeConfirm
    -- ^ The page lets the user confirm or deny the
    --  changes. The Back and Apply buttons will be shown.
    | AssistantPageTypeSummary
    -- ^ The page informs the user of the changes
    --  done. Only the Close button will be shown.
    | AssistantPageTypeProgress
    -- ^ Used for tasks that take a long time to
    --  complete, blocks the assistant until the page is marked as complete.
    --   Only the back button will be shown.
    | AssistantPageTypeCustom
    -- ^ Used for when other page types are not
    --  appropriate. No buttons will be shown, and the application must
    --  add its own buttons through 'GI.Gtk.Objects.Assistant.assistantAddActionWidget'.
    | AnotherAssistantPageType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum AssistantPageType where
    fromEnum AssistantPageTypeContent = 0
    fromEnum AssistantPageTypeIntro = 1
    fromEnum AssistantPageTypeConfirm = 2
    fromEnum AssistantPageTypeSummary = 3
    fromEnum AssistantPageTypeProgress = 4
    fromEnum AssistantPageTypeCustom = 5
    fromEnum (AnotherAssistantPageType k) = k

    toEnum 0 = AssistantPageTypeContent
    toEnum 1 = AssistantPageTypeIntro
    toEnum 2 = AssistantPageTypeConfirm
    toEnum 3 = AssistantPageTypeSummary
    toEnum 4 = AssistantPageTypeProgress
    toEnum 5 = AssistantPageTypeCustom
    toEnum k = AnotherAssistantPageType k

instance P.Ord AssistantPageType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes AssistantPageType = '[]
instance O.HasParentTypes AssistantPageType

foreign import ccall "gtk_assistant_page_type_get_type" c_gtk_assistant_page_type_get_type :: 
    IO GType

instance B.Types.TypedObject AssistantPageType where
    glibType = c_gtk_assistant_page_type_get_type

instance B.Types.BoxedEnum AssistantPageType

-- Enum ArrowType
-- | Used to indicate the direction in which an arrow should point.
data ArrowType = 
      ArrowTypeUp
    -- ^ Represents an upward pointing arrow.
    | ArrowTypeDown
    -- ^ Represents a downward pointing arrow.
    | ArrowTypeLeft
    -- ^ Represents a left pointing arrow.
    | ArrowTypeRight
    -- ^ Represents a right pointing arrow.
    | ArrowTypeNone
    -- ^ No arrow. Since 2.10.
    | AnotherArrowType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ArrowType where
    fromEnum ArrowTypeUp = 0
    fromEnum ArrowTypeDown = 1
    fromEnum ArrowTypeLeft = 2
    fromEnum ArrowTypeRight = 3
    fromEnum ArrowTypeNone = 4
    fromEnum (AnotherArrowType k) = k

    toEnum 0 = ArrowTypeUp
    toEnum 1 = ArrowTypeDown
    toEnum 2 = ArrowTypeLeft
    toEnum 3 = ArrowTypeRight
    toEnum 4 = ArrowTypeNone
    toEnum k = AnotherArrowType k

instance P.Ord ArrowType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ArrowType = '[]
instance O.HasParentTypes ArrowType

foreign import ccall "gtk_arrow_type_get_type" c_gtk_arrow_type_get_type :: 
    IO GType

instance B.Types.TypedObject ArrowType where
    glibType = c_gtk_arrow_type_get_type

instance B.Types.BoxedEnum ArrowType

-- Enum ArrowPlacement
-- | Used to specify the placement of scroll arrows in scrolling menus.
data ArrowPlacement = 
      ArrowPlacementBoth
    -- ^ Place one arrow on each end of the menu.
    | ArrowPlacementStart
    -- ^ Place both arrows at the top of the menu.
    | ArrowPlacementEnd
    -- ^ Place both arrows at the bottom of the menu.
    | AnotherArrowPlacement Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ArrowPlacement where
    fromEnum ArrowPlacementBoth = 0
    fromEnum ArrowPlacementStart = 1
    fromEnum ArrowPlacementEnd = 2
    fromEnum (AnotherArrowPlacement k) = k

    toEnum 0 = ArrowPlacementBoth
    toEnum 1 = ArrowPlacementStart
    toEnum 2 = ArrowPlacementEnd
    toEnum k = AnotherArrowPlacement k

instance P.Ord ArrowPlacement where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ArrowPlacement = '[]
instance O.HasParentTypes ArrowPlacement

foreign import ccall "gtk_arrow_placement_get_type" c_gtk_arrow_placement_get_type :: 
    IO GType

instance B.Types.TypedObject ArrowPlacement where
    glibType = c_gtk_arrow_placement_get_type

instance B.Types.BoxedEnum ArrowPlacement

-- Enum Align
-- | Controls how a widget deals with extra space in a single (x or y)
-- dimension.
-- 
-- Alignment only matters if the widget receives a “too large” allocation,
-- for example if you packed the widget with the [Widget:expand]("GI.Gtk.Objects.Widget#g:attr:expand")
-- flag inside a t'GI.Gtk.Objects.Box.Box', then the widget might get extra space.  If
-- you have for example a 16x16 icon inside a 32x32 space, the icon
-- could be scaled and stretched, it could be centered, or it could be
-- positioned to one side of the space.
-- 
-- Note that in horizontal context /@gTKALIGNSTART@/ and /@gTKALIGNEND@/
-- are interpreted relative to text direction.
-- 
-- GTK_ALIGN_BASELINE support for it is optional for containers and widgets, and
-- it is only supported for vertical alignment.  When its not supported by
-- a child or a container it is treated as /@gTKALIGNFILL@/.
data Align = 
      AlignFill
    -- ^ stretch to fill all space if possible, center if
    --     no meaningful way to stretch
    | AlignStart
    -- ^ snap to left or top side, leaving space on right
    --     or bottom
    | AlignEnd
    -- ^ snap to right or bottom side, leaving space on left
    --     or top
    | AlignCenter
    -- ^ center natural width of widget inside the
    --     allocation
    | AlignBaseline
    -- ^ align the widget according to the baseline. Since 3.10.
    | AnotherAlign Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum Align where
    fromEnum AlignFill = 0
    fromEnum AlignStart = 1
    fromEnum AlignEnd = 2
    fromEnum AlignCenter = 3
    fromEnum AlignBaseline = 4
    fromEnum (AnotherAlign k) = k

    toEnum 0 = AlignFill
    toEnum 1 = AlignStart
    toEnum 2 = AlignEnd
    toEnum 3 = AlignCenter
    toEnum 4 = AlignBaseline
    toEnum k = AnotherAlign k

instance P.Ord Align where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes Align = '[]
instance O.HasParentTypes Align

foreign import ccall "gtk_align_get_type" c_gtk_align_get_type :: 
    IO GType

instance B.Types.TypedObject Align where
    glibType = c_gtk_align_get_type

instance B.Types.BoxedEnum Align


