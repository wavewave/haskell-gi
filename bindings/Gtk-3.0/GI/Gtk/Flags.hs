

-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Flags
    ( 

 -- * Flags


-- ** AccelFlags #flag:AccelFlags#

    AccelFlags(..)                          ,


-- ** ApplicationInhibitFlags #flag:ApplicationInhibitFlags#

    ApplicationInhibitFlags(..)             ,


-- ** AttachOptions #flag:AttachOptions#

    AttachOptions(..)                       ,


-- ** CalendarDisplayOptions #flag:CalendarDisplayOptions#

    CalendarDisplayOptions(..)              ,


-- ** CellRendererState #flag:CellRendererState#

    CellRendererState(..)                   ,


-- ** DebugFlag #flag:DebugFlag#

    DebugFlag(..)                           ,


-- ** DestDefaults #flag:DestDefaults#

    DestDefaults(..)                        ,


-- ** DialogFlags #flag:DialogFlags#

    DialogFlags(..)                         ,


-- ** EventControllerScrollFlags #flag:EventControllerScrollFlags#

    EventControllerScrollFlags(..)          ,


-- ** FileFilterFlags #flag:FileFilterFlags#

    FileFilterFlags(..)                     ,


-- ** FontChooserLevel #flag:FontChooserLevel#

    FontChooserLevel(..)                    ,


-- ** IconLookupFlags #flag:IconLookupFlags#

    IconLookupFlags(..)                     ,


-- ** InputHints #flag:InputHints#

    InputHints(..)                          ,


-- ** JunctionSides #flag:JunctionSides#

    JunctionSides(..)                       ,


-- ** PlacesOpenFlags #flag:PlacesOpenFlags#

    PlacesOpenFlags(..)                     ,


-- ** RcFlags #flag:RcFlags#

    RcFlags(..)                             ,


-- ** RecentFilterFlags #flag:RecentFilterFlags#

    RecentFilterFlags(..)                   ,


-- ** RegionFlags #flag:RegionFlags#

    RegionFlags(..)                         ,


-- ** StateFlags #flag:StateFlags#

    StateFlags(..)                          ,


-- ** StyleContextPrintFlags #flag:StyleContextPrintFlags#

    StyleContextPrintFlags(..)              ,


-- ** TargetFlags #flag:TargetFlags#

    TargetFlags(..)                         ,


-- ** TextSearchFlags #flag:TextSearchFlags#

    TextSearchFlags(..)                     ,


-- ** ToolPaletteDragTargets #flag:ToolPaletteDragTargets#

    ToolPaletteDragTargets(..)              ,


-- ** TreeModelFlags #flag:TreeModelFlags#

    TreeModelFlags(..)                      ,


-- ** UIManagerItemType #flag:UIManagerItemType#

    UIManagerItemType(..)                   ,




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


-- Flags UIManagerItemType
{-# DEPRECATED UIManagerItemType ["(Since version 3.10)"] #-}
-- | These enumeration values are used by 'GI.Gtk.Objects.UIManager.uIManagerAddUi' to determine
-- what UI element to create.
data UIManagerItemType = 
      UIManagerItemTypeAuto
    -- ^ Pick the type of the UI element according to context.
    | UIManagerItemTypeMenubar
    -- ^ Create a menubar.
    | UIManagerItemTypeMenu
    -- ^ Create a menu.
    | UIManagerItemTypeToolbar
    -- ^ Create a toolbar.
    | UIManagerItemTypePlaceholder
    -- ^ Insert a placeholder.
    | UIManagerItemTypePopup
    -- ^ Create a popup menu.
    | UIManagerItemTypeMenuitem
    -- ^ Create a menuitem.
    | UIManagerItemTypeToolitem
    -- ^ Create a toolitem.
    | UIManagerItemTypeSeparator
    -- ^ Create a separator.
    | UIManagerItemTypeAccelerator
    -- ^ Install an accelerator.
    | UIManagerItemTypePopupWithAccels
    -- ^ Same as 'GI.Gtk.Flags.UIManagerItemTypePopup', but the
    --   actions’ accelerators are shown.
    | AnotherUIManagerItemType Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum UIManagerItemType where
    fromEnum UIManagerItemTypeAuto = 0
    fromEnum UIManagerItemTypeMenubar = 1
    fromEnum UIManagerItemTypeMenu = 2
    fromEnum UIManagerItemTypeToolbar = 4
    fromEnum UIManagerItemTypePlaceholder = 8
    fromEnum UIManagerItemTypePopup = 16
    fromEnum UIManagerItemTypeMenuitem = 32
    fromEnum UIManagerItemTypeToolitem = 64
    fromEnum UIManagerItemTypeSeparator = 128
    fromEnum UIManagerItemTypeAccelerator = 256
    fromEnum UIManagerItemTypePopupWithAccels = 512
    fromEnum (AnotherUIManagerItemType k) = k

    toEnum 0 = UIManagerItemTypeAuto
    toEnum 1 = UIManagerItemTypeMenubar
    toEnum 2 = UIManagerItemTypeMenu
    toEnum 4 = UIManagerItemTypeToolbar
    toEnum 8 = UIManagerItemTypePlaceholder
    toEnum 16 = UIManagerItemTypePopup
    toEnum 32 = UIManagerItemTypeMenuitem
    toEnum 64 = UIManagerItemTypeToolitem
    toEnum 128 = UIManagerItemTypeSeparator
    toEnum 256 = UIManagerItemTypeAccelerator
    toEnum 512 = UIManagerItemTypePopupWithAccels
    toEnum k = AnotherUIManagerItemType k

instance P.Ord UIManagerItemType where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes UIManagerItemType = '[]
instance O.HasParentTypes UIManagerItemType

foreign import ccall "gtk_ui_manager_item_type_get_type" c_gtk_ui_manager_item_type_get_type :: 
    IO GType

instance B.Types.TypedObject UIManagerItemType where
    glibType = c_gtk_ui_manager_item_type_get_type

instance B.Types.BoxedFlags UIManagerItemType

instance IsGFlag UIManagerItemType

-- Flags TreeModelFlags
-- | These flags indicate various properties of a t'GI.Gtk.Interfaces.TreeModel.TreeModel'.
-- 
-- They are returned by 'GI.Gtk.Interfaces.TreeModel.treeModelGetFlags', and must be
-- static for the lifetime of the object. A more complete description
-- of @/GTK_TREE_MODEL_ITERS_PERSIST/@ can be found in the overview of
-- this section.
data TreeModelFlags = 
      TreeModelFlagsItersPersist
    -- ^ iterators survive all signals
    --     emitted by the tree
    | TreeModelFlagsListOnly
    -- ^ the model is a list only, and never
    --     has children
    | AnotherTreeModelFlags Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum TreeModelFlags where
    fromEnum TreeModelFlagsItersPersist = 1
    fromEnum TreeModelFlagsListOnly = 2
    fromEnum (AnotherTreeModelFlags k) = k

    toEnum 1 = TreeModelFlagsItersPersist
    toEnum 2 = TreeModelFlagsListOnly
    toEnum k = AnotherTreeModelFlags k

instance P.Ord TreeModelFlags where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes TreeModelFlags = '[]
instance O.HasParentTypes TreeModelFlags

foreign import ccall "gtk_tree_model_flags_get_type" c_gtk_tree_model_flags_get_type :: 
    IO GType

instance B.Types.TypedObject TreeModelFlags where
    glibType = c_gtk_tree_model_flags_get_type

instance B.Types.BoxedFlags TreeModelFlags

instance IsGFlag TreeModelFlags

-- Flags ToolPaletteDragTargets
-- | Flags used to specify the supported drag targets.
data ToolPaletteDragTargets = 
      ToolPaletteDragTargetsItems
    -- ^ Support drag of items.
    | ToolPaletteDragTargetsGroups
    -- ^ Support drag of groups.
    | AnotherToolPaletteDragTargets Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ToolPaletteDragTargets where
    fromEnum ToolPaletteDragTargetsItems = 1
    fromEnum ToolPaletteDragTargetsGroups = 2
    fromEnum (AnotherToolPaletteDragTargets k) = k

    toEnum 1 = ToolPaletteDragTargetsItems
    toEnum 2 = ToolPaletteDragTargetsGroups
    toEnum k = AnotherToolPaletteDragTargets k

instance P.Ord ToolPaletteDragTargets where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ToolPaletteDragTargets = '[]
instance O.HasParentTypes ToolPaletteDragTargets

foreign import ccall "gtk_tool_palette_drag_targets_get_type" c_gtk_tool_palette_drag_targets_get_type :: 
    IO GType

instance B.Types.TypedObject ToolPaletteDragTargets where
    glibType = c_gtk_tool_palette_drag_targets_get_type

instance B.Types.BoxedFlags ToolPaletteDragTargets

instance IsGFlag ToolPaletteDragTargets

-- Flags TextSearchFlags
-- | Flags affecting how a search is done.
-- 
-- If neither @/GTK_TEXT_SEARCH_VISIBLE_ONLY/@ nor @/GTK_TEXT_SEARCH_TEXT_ONLY/@ are
-- enabled, the match must be exact; the special 0xFFFC character will match
-- embedded pixbufs or child widgets.
data TextSearchFlags = 
      TextSearchFlagsVisibleOnly
    -- ^ Search only visible data. A search match may
    -- have invisible text interspersed.
    | TextSearchFlagsTextOnly
    -- ^ Search only text. A match may have pixbufs or
    -- child widgets mixed inside the matched range.
    | TextSearchFlagsCaseInsensitive
    -- ^ The text will be matched regardless of
    -- what case it is in.
    | AnotherTextSearchFlags Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum TextSearchFlags where
    fromEnum TextSearchFlagsVisibleOnly = 1
    fromEnum TextSearchFlagsTextOnly = 2
    fromEnum TextSearchFlagsCaseInsensitive = 4
    fromEnum (AnotherTextSearchFlags k) = k

    toEnum 1 = TextSearchFlagsVisibleOnly
    toEnum 2 = TextSearchFlagsTextOnly
    toEnum 4 = TextSearchFlagsCaseInsensitive
    toEnum k = AnotherTextSearchFlags k

instance P.Ord TextSearchFlags where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes TextSearchFlags = '[]
instance O.HasParentTypes TextSearchFlags

foreign import ccall "gtk_text_search_flags_get_type" c_gtk_text_search_flags_get_type :: 
    IO GType

instance B.Types.TypedObject TextSearchFlags where
    glibType = c_gtk_text_search_flags_get_type

instance B.Types.BoxedFlags TextSearchFlags

instance IsGFlag TextSearchFlags

-- Flags TargetFlags
-- | The t'GI.Gtk.Flags.TargetFlags' enumeration is used to specify
-- constraints on a t'GI.Gtk.Structs.TargetEntry.TargetEntry'.
data TargetFlags = 
      TargetFlagsSameApp
    -- ^ If this is set, the target will only be selected
    --   for drags within a single application.
    | TargetFlagsSameWidget
    -- ^ If this is set, the target will only be selected
    --   for drags within a single widget.
    | TargetFlagsOtherApp
    -- ^ If this is set, the target will not be selected
    --   for drags within a single application.
    | TargetFlagsOtherWidget
    -- ^ If this is set, the target will not be selected
    --   for drags withing a single widget.
    | AnotherTargetFlags Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum TargetFlags where
    fromEnum TargetFlagsSameApp = 1
    fromEnum TargetFlagsSameWidget = 2
    fromEnum TargetFlagsOtherApp = 4
    fromEnum TargetFlagsOtherWidget = 8
    fromEnum (AnotherTargetFlags k) = k

    toEnum 1 = TargetFlagsSameApp
    toEnum 2 = TargetFlagsSameWidget
    toEnum 4 = TargetFlagsOtherApp
    toEnum 8 = TargetFlagsOtherWidget
    toEnum k = AnotherTargetFlags k

instance P.Ord TargetFlags where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes TargetFlags = '[]
instance O.HasParentTypes TargetFlags

foreign import ccall "gtk_target_flags_get_type" c_gtk_target_flags_get_type :: 
    IO GType

instance B.Types.TypedObject TargetFlags where
    glibType = c_gtk_target_flags_get_type

instance B.Types.BoxedFlags TargetFlags

instance IsGFlag TargetFlags

-- Flags StyleContextPrintFlags
-- | Flags that modify the behavior of 'GI.Gtk.Objects.StyleContext.styleContextToString'.
-- New values may be added to this enumeration.
data StyleContextPrintFlags = 
      StyleContextPrintFlagsNone
    -- ^ /No description available in the introspection data./
    | StyleContextPrintFlagsRecurse
    -- ^ Print the entire tree of
    --     CSS nodes starting at the style context\'s node
    | StyleContextPrintFlagsShowStyle
    -- ^ Show the values of the
    --     CSS properties for each node
    | AnotherStyleContextPrintFlags Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum StyleContextPrintFlags where
    fromEnum StyleContextPrintFlagsNone = 0
    fromEnum StyleContextPrintFlagsRecurse = 1
    fromEnum StyleContextPrintFlagsShowStyle = 2
    fromEnum (AnotherStyleContextPrintFlags k) = k

    toEnum 0 = StyleContextPrintFlagsNone
    toEnum 1 = StyleContextPrintFlagsRecurse
    toEnum 2 = StyleContextPrintFlagsShowStyle
    toEnum k = AnotherStyleContextPrintFlags k

instance P.Ord StyleContextPrintFlags where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes StyleContextPrintFlags = '[]
instance O.HasParentTypes StyleContextPrintFlags

foreign import ccall "gtk_style_context_print_flags_get_type" c_gtk_style_context_print_flags_get_type :: 
    IO GType

instance B.Types.TypedObject StyleContextPrintFlags where
    glibType = c_gtk_style_context_print_flags_get_type

instance B.Types.BoxedFlags StyleContextPrintFlags

instance IsGFlag StyleContextPrintFlags

-- Flags StateFlags
-- | Describes a widget state. Widget states are used to match the widget
-- against CSS pseudo-classes. Note that GTK extends the regular CSS
-- classes and sometimes uses different names.
data StateFlags = 
      StateFlagsNormal
    -- ^ State during normal operation.
    | StateFlagsActive
    -- ^ Widget is active.
    | StateFlagsPrelight
    -- ^ Widget has a mouse pointer over it.
    | StateFlagsSelected
    -- ^ Widget is selected.
    | StateFlagsInsensitive
    -- ^ Widget is insensitive.
    | StateFlagsInconsistent
    -- ^ Widget is inconsistent.
    | StateFlagsFocused
    -- ^ Widget has the keyboard focus.
    | StateFlagsBackdrop
    -- ^ Widget is in a background toplevel window.
    | StateFlagsDirLtr
    -- ^ Widget is in left-to-right text direction. Since 3.8
    | StateFlagsDirRtl
    -- ^ Widget is in right-to-left text direction. Since 3.8
    | StateFlagsLink
    -- ^ Widget is a link. Since 3.12
    | StateFlagsVisited
    -- ^ The location the widget points to has already been visited. Since 3.12
    | StateFlagsChecked
    -- ^ Widget is checked. Since 3.14
    | StateFlagsDropActive
    -- ^ Widget is highlighted as a drop target for DND. Since 3.20
    | AnotherStateFlags Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum StateFlags where
    fromEnum StateFlagsNormal = 0
    fromEnum StateFlagsActive = 1
    fromEnum StateFlagsPrelight = 2
    fromEnum StateFlagsSelected = 4
    fromEnum StateFlagsInsensitive = 8
    fromEnum StateFlagsInconsistent = 16
    fromEnum StateFlagsFocused = 32
    fromEnum StateFlagsBackdrop = 64
    fromEnum StateFlagsDirLtr = 128
    fromEnum StateFlagsDirRtl = 256
    fromEnum StateFlagsLink = 512
    fromEnum StateFlagsVisited = 1024
    fromEnum StateFlagsChecked = 2048
    fromEnum StateFlagsDropActive = 4096
    fromEnum (AnotherStateFlags k) = k

    toEnum 0 = StateFlagsNormal
    toEnum 1 = StateFlagsActive
    toEnum 2 = StateFlagsPrelight
    toEnum 4 = StateFlagsSelected
    toEnum 8 = StateFlagsInsensitive
    toEnum 16 = StateFlagsInconsistent
    toEnum 32 = StateFlagsFocused
    toEnum 64 = StateFlagsBackdrop
    toEnum 128 = StateFlagsDirLtr
    toEnum 256 = StateFlagsDirRtl
    toEnum 512 = StateFlagsLink
    toEnum 1024 = StateFlagsVisited
    toEnum 2048 = StateFlagsChecked
    toEnum 4096 = StateFlagsDropActive
    toEnum k = AnotherStateFlags k

instance P.Ord StateFlags where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes StateFlags = '[]
instance O.HasParentTypes StateFlags

foreign import ccall "gtk_state_flags_get_type" c_gtk_state_flags_get_type :: 
    IO GType

instance B.Types.TypedObject StateFlags where
    glibType = c_gtk_state_flags_get_type

instance B.Types.BoxedFlags StateFlags

instance IsGFlag StateFlags

-- Flags RegionFlags
-- | Describes a region within a widget.
data RegionFlags = 
      RegionFlagsEven
    -- ^ Region has an even number within a set.
    | RegionFlagsOdd
    -- ^ Region has an odd number within a set.
    | RegionFlagsFirst
    -- ^ Region is the first one within a set.
    | RegionFlagsLast
    -- ^ Region is the last one within a set.
    | RegionFlagsOnly
    -- ^ Region is the only one within a set.
    | RegionFlagsSorted
    -- ^ Region is part of a sorted area.
    | AnotherRegionFlags Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum RegionFlags where
    fromEnum RegionFlagsEven = 1
    fromEnum RegionFlagsOdd = 2
    fromEnum RegionFlagsFirst = 4
    fromEnum RegionFlagsLast = 8
    fromEnum RegionFlagsOnly = 16
    fromEnum RegionFlagsSorted = 32
    fromEnum (AnotherRegionFlags k) = k

    toEnum 1 = RegionFlagsEven
    toEnum 2 = RegionFlagsOdd
    toEnum 4 = RegionFlagsFirst
    toEnum 8 = RegionFlagsLast
    toEnum 16 = RegionFlagsOnly
    toEnum 32 = RegionFlagsSorted
    toEnum k = AnotherRegionFlags k

instance P.Ord RegionFlags where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes RegionFlags = '[]
instance O.HasParentTypes RegionFlags

foreign import ccall "gtk_region_flags_get_type" c_gtk_region_flags_get_type :: 
    IO GType

instance B.Types.TypedObject RegionFlags where
    glibType = c_gtk_region_flags_get_type

instance B.Types.BoxedFlags RegionFlags

instance IsGFlag RegionFlags

-- Flags RecentFilterFlags
-- | These flags indicate what parts of a t'GI.Gtk.Structs.RecentFilterInfo.RecentFilterInfo' struct
-- are filled or need to be filled.
data RecentFilterFlags = 
      RecentFilterFlagsUri
    -- ^ the URI of the file being tested
    | RecentFilterFlagsDisplayName
    -- ^ the string that will be used to
    --  display the file in the recent chooser
    | RecentFilterFlagsMimeType
    -- ^ the mime type of the file
    | RecentFilterFlagsApplication
    -- ^ the list of applications that have
    --  registered the file
    | RecentFilterFlagsGroup
    -- ^ the groups to which the file belongs to
    | RecentFilterFlagsAge
    -- ^ the number of days elapsed since the file
    --  has been registered
    | AnotherRecentFilterFlags Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum RecentFilterFlags where
    fromEnum RecentFilterFlagsUri = 1
    fromEnum RecentFilterFlagsDisplayName = 2
    fromEnum RecentFilterFlagsMimeType = 4
    fromEnum RecentFilterFlagsApplication = 8
    fromEnum RecentFilterFlagsGroup = 16
    fromEnum RecentFilterFlagsAge = 32
    fromEnum (AnotherRecentFilterFlags k) = k

    toEnum 1 = RecentFilterFlagsUri
    toEnum 2 = RecentFilterFlagsDisplayName
    toEnum 4 = RecentFilterFlagsMimeType
    toEnum 8 = RecentFilterFlagsApplication
    toEnum 16 = RecentFilterFlagsGroup
    toEnum 32 = RecentFilterFlagsAge
    toEnum k = AnotherRecentFilterFlags k

instance P.Ord RecentFilterFlags where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes RecentFilterFlags = '[]
instance O.HasParentTypes RecentFilterFlags

foreign import ccall "gtk_recent_filter_flags_get_type" c_gtk_recent_filter_flags_get_type :: 
    IO GType

instance B.Types.TypedObject RecentFilterFlags where
    glibType = c_gtk_recent_filter_flags_get_type

instance B.Types.BoxedFlags RecentFilterFlags

instance IsGFlag RecentFilterFlags

-- Flags RcFlags
-- | Deprecated
data RcFlags = 
      RcFlagsFg
    -- ^ Deprecated
    | RcFlagsBg
    -- ^ Deprecated
    | RcFlagsText
    -- ^ Deprecated
    | RcFlagsBase
    -- ^ Deprecated
    | AnotherRcFlags Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum RcFlags where
    fromEnum RcFlagsFg = 1
    fromEnum RcFlagsBg = 2
    fromEnum RcFlagsText = 4
    fromEnum RcFlagsBase = 8
    fromEnum (AnotherRcFlags k) = k

    toEnum 1 = RcFlagsFg
    toEnum 2 = RcFlagsBg
    toEnum 4 = RcFlagsText
    toEnum 8 = RcFlagsBase
    toEnum k = AnotherRcFlags k

instance P.Ord RcFlags where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes RcFlags = '[]
instance O.HasParentTypes RcFlags

foreign import ccall "gtk_rc_flags_get_type" c_gtk_rc_flags_get_type :: 
    IO GType

instance B.Types.TypedObject RcFlags where
    glibType = c_gtk_rc_flags_get_type

instance B.Types.BoxedFlags RcFlags

instance IsGFlag RcFlags

-- Flags PlacesOpenFlags
-- | These flags serve two purposes.  First, the application can call 'GI.Gtk.Objects.PlacesSidebar.placesSidebarSetOpenFlags'
-- using these flags as a bitmask.  This tells the sidebar that the application is able to open
-- folders selected from the sidebar in various ways, for example, in new tabs or in new windows in
-- addition to the normal mode.
-- 
-- Second, when one of these values gets passed back to the application in the
-- [PlacesSidebar::openLocation]("GI.Gtk.Objects.PlacesSidebar#g:signal:openLocation") signal, it means that the application should
-- open the selected location in the normal way, in a new tab, or in a new
-- window.  The sidebar takes care of determining the desired way to open the location,
-- based on the modifier keys that the user is pressing at the time the selection is made.
-- 
-- If the application never calls 'GI.Gtk.Objects.PlacesSidebar.placesSidebarSetOpenFlags', then the sidebar will only
-- use @/GTK_PLACES_OPEN_NORMAL/@ in the [PlacesSidebar::openLocation]("GI.Gtk.Objects.PlacesSidebar#g:signal:openLocation") signal.  This is the
-- default mode of operation.
data PlacesOpenFlags = 
      PlacesOpenFlagsNormal
    -- ^ This is the default mode that t'GI.Gtk.Objects.PlacesSidebar.PlacesSidebar' uses if no other flags
    --  are specified.  It indicates that the calling application should open the selected location
    --  in the normal way, for example, in the folder view beside the sidebar.
    | PlacesOpenFlagsNewTab
    -- ^ When passed to 'GI.Gtk.Objects.PlacesSidebar.placesSidebarSetOpenFlags', this indicates
    --  that the application can open folders selected from the sidebar in new tabs.  This value
    --  will be passed to the [PlacesSidebar::openLocation]("GI.Gtk.Objects.PlacesSidebar#g:signal:openLocation") signal when the user selects
    --  that a location be opened in a new tab instead of in the standard fashion.
    | PlacesOpenFlagsNewWindow
    -- ^ Similar to /@gTKPLACESOPENNEWTAB@/, but indicates that the application
    --  can open folders in new windows.
    | AnotherPlacesOpenFlags Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum PlacesOpenFlags where
    fromEnum PlacesOpenFlagsNormal = 1
    fromEnum PlacesOpenFlagsNewTab = 2
    fromEnum PlacesOpenFlagsNewWindow = 4
    fromEnum (AnotherPlacesOpenFlags k) = k

    toEnum 1 = PlacesOpenFlagsNormal
    toEnum 2 = PlacesOpenFlagsNewTab
    toEnum 4 = PlacesOpenFlagsNewWindow
    toEnum k = AnotherPlacesOpenFlags k

instance P.Ord PlacesOpenFlags where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes PlacesOpenFlags = '[]
instance O.HasParentTypes PlacesOpenFlags

foreign import ccall "gtk_places_open_flags_get_type" c_gtk_places_open_flags_get_type :: 
    IO GType

instance B.Types.TypedObject PlacesOpenFlags where
    glibType = c_gtk_places_open_flags_get_type

instance B.Types.BoxedFlags PlacesOpenFlags

instance IsGFlag PlacesOpenFlags

-- Flags JunctionSides
-- | Describes how a rendered element connects to adjacent elements.
data JunctionSides = 
      JunctionSidesNone
    -- ^ No junctions.
    | JunctionSidesCornerTopleft
    -- ^ Element connects on the top-left corner.
    | JunctionSidesCornerTopright
    -- ^ Element connects on the top-right corner.
    | JunctionSidesCornerBottomleft
    -- ^ Element connects on the bottom-left corner.
    | JunctionSidesCornerBottomright
    -- ^ Element connects on the bottom-right corner.
    | JunctionSidesTop
    -- ^ Element connects on the top side.
    | JunctionSidesBottom
    -- ^ Element connects on the bottom side.
    | JunctionSidesLeft
    -- ^ Element connects on the left side.
    | JunctionSidesRight
    -- ^ Element connects on the right side.
    | AnotherJunctionSides Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum JunctionSides where
    fromEnum JunctionSidesNone = 0
    fromEnum JunctionSidesCornerTopleft = 1
    fromEnum JunctionSidesCornerTopright = 2
    fromEnum JunctionSidesCornerBottomleft = 4
    fromEnum JunctionSidesCornerBottomright = 8
    fromEnum JunctionSidesTop = 3
    fromEnum JunctionSidesBottom = 12
    fromEnum JunctionSidesLeft = 5
    fromEnum JunctionSidesRight = 10
    fromEnum (AnotherJunctionSides k) = k

    toEnum 0 = JunctionSidesNone
    toEnum 1 = JunctionSidesCornerTopleft
    toEnum 2 = JunctionSidesCornerTopright
    toEnum 4 = JunctionSidesCornerBottomleft
    toEnum 8 = JunctionSidesCornerBottomright
    toEnum 3 = JunctionSidesTop
    toEnum 12 = JunctionSidesBottom
    toEnum 5 = JunctionSidesLeft
    toEnum 10 = JunctionSidesRight
    toEnum k = AnotherJunctionSides k

instance P.Ord JunctionSides where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes JunctionSides = '[]
instance O.HasParentTypes JunctionSides

foreign import ccall "gtk_junction_sides_get_type" c_gtk_junction_sides_get_type :: 
    IO GType

instance B.Types.TypedObject JunctionSides where
    glibType = c_gtk_junction_sides_get_type

instance B.Types.BoxedFlags JunctionSides

instance IsGFlag JunctionSides

-- Flags InputHints
-- | Describes hints that might be taken into account by input methods
-- or applications. Note that input methods may already tailor their
-- behaviour according to the t'GI.Gtk.Enums.InputPurpose' of the entry.
-- 
-- Some common sense is expected when using these flags - mixing
-- /@gTKINPUTHINTLOWERCASE@/ with any of the uppercase hints makes no sense.
-- 
-- This enumeration may be extended in the future; input methods should
-- ignore unknown values.
-- 
-- /Since: 3.6/
data InputHints = 
      InputHintsNone
    -- ^ No special behaviour suggested
    | InputHintsSpellcheck
    -- ^ Suggest checking for typos
    | InputHintsNoSpellcheck
    -- ^ Suggest not checking for typos
    | InputHintsWordCompletion
    -- ^ Suggest word completion
    | InputHintsLowercase
    -- ^ Suggest to convert all text to lowercase
    | InputHintsUppercaseChars
    -- ^ Suggest to capitalize all text
    | InputHintsUppercaseWords
    -- ^ Suggest to capitalize the first
    --     character of each word
    | InputHintsUppercaseSentences
    -- ^ Suggest to capitalize the
    --     first word of each sentence
    | InputHintsInhibitOsk
    -- ^ Suggest to not show an onscreen keyboard
    --     (e.g for a calculator that already has all the keys).
    | InputHintsVerticalWriting
    -- ^ The text is vertical. Since 3.18
    | InputHintsEmoji
    -- ^ Suggest offering Emoji support. Since 3.22.20
    | InputHintsNoEmoji
    -- ^ Suggest not offering Emoji support. Since 3.22.20
    | AnotherInputHints Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum InputHints where
    fromEnum InputHintsNone = 0
    fromEnum InputHintsSpellcheck = 1
    fromEnum InputHintsNoSpellcheck = 2
    fromEnum InputHintsWordCompletion = 4
    fromEnum InputHintsLowercase = 8
    fromEnum InputHintsUppercaseChars = 16
    fromEnum InputHintsUppercaseWords = 32
    fromEnum InputHintsUppercaseSentences = 64
    fromEnum InputHintsInhibitOsk = 128
    fromEnum InputHintsVerticalWriting = 256
    fromEnum InputHintsEmoji = 512
    fromEnum InputHintsNoEmoji = 1024
    fromEnum (AnotherInputHints k) = k

    toEnum 0 = InputHintsNone
    toEnum 1 = InputHintsSpellcheck
    toEnum 2 = InputHintsNoSpellcheck
    toEnum 4 = InputHintsWordCompletion
    toEnum 8 = InputHintsLowercase
    toEnum 16 = InputHintsUppercaseChars
    toEnum 32 = InputHintsUppercaseWords
    toEnum 64 = InputHintsUppercaseSentences
    toEnum 128 = InputHintsInhibitOsk
    toEnum 256 = InputHintsVerticalWriting
    toEnum 512 = InputHintsEmoji
    toEnum 1024 = InputHintsNoEmoji
    toEnum k = AnotherInputHints k

instance P.Ord InputHints where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes InputHints = '[]
instance O.HasParentTypes InputHints

foreign import ccall "gtk_input_hints_get_type" c_gtk_input_hints_get_type :: 
    IO GType

instance B.Types.TypedObject InputHints where
    glibType = c_gtk_input_hints_get_type

instance B.Types.BoxedFlags InputHints

instance IsGFlag InputHints

-- Flags IconLookupFlags
-- | Used to specify options for 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'
data IconLookupFlags = 
      IconLookupFlagsNoSvg
    -- ^ Never get SVG icons, even if gdk-pixbuf
    --   supports them. Cannot be used together with 'GI.Gtk.Flags.IconLookupFlagsForceSvg'.
    | IconLookupFlagsForceSvg
    -- ^ Get SVG icons, even if gdk-pixbuf
    --   doesn’t support them.
    --   Cannot be used together with 'GI.Gtk.Flags.IconLookupFlagsNoSvg'.
    | IconLookupFlagsUseBuiltin
    -- ^ When passed to
    --   'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon' includes builtin icons
    --   as well as files. For a builtin icon, 'GI.Gtk.Objects.IconInfo.iconInfoGetFilename'
    --   is 'P.Nothing' and you need to call 'GI.Gtk.Objects.IconInfo.iconInfoGetBuiltinPixbuf'.
    | IconLookupFlagsGenericFallback
    -- ^ Try to shorten icon name at \'-\'
    --   characters before looking at inherited themes. This flag is only
    --   supported in functions that take a single icon name. For more general
    --   fallback, see 'GI.Gtk.Objects.IconTheme.iconThemeChooseIcon'. Since 2.12.
    | IconLookupFlagsForceSize
    -- ^ Always get the icon scaled to the
    --   requested size. Since 2.14.
    | IconLookupFlagsForceRegular
    -- ^ Try to always load regular icons, even
    --   when symbolic icon names are given. Since 3.14.
    | IconLookupFlagsForceSymbolic
    -- ^ Try to always load symbolic icons, even
    --   when regular icon names are given. Since 3.14.
    | IconLookupFlagsDirLtr
    -- ^ Try to load a variant of the icon for left-to-right
    --   text direction. Since 3.14.
    | IconLookupFlagsDirRtl
    -- ^ Try to load a variant of the icon for right-to-left
    --   text direction. Since 3.14.
    | AnotherIconLookupFlags Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum IconLookupFlags where
    fromEnum IconLookupFlagsNoSvg = 1
    fromEnum IconLookupFlagsForceSvg = 2
    fromEnum IconLookupFlagsUseBuiltin = 4
    fromEnum IconLookupFlagsGenericFallback = 8
    fromEnum IconLookupFlagsForceSize = 16
    fromEnum IconLookupFlagsForceRegular = 32
    fromEnum IconLookupFlagsForceSymbolic = 64
    fromEnum IconLookupFlagsDirLtr = 128
    fromEnum IconLookupFlagsDirRtl = 256
    fromEnum (AnotherIconLookupFlags k) = k

    toEnum 1 = IconLookupFlagsNoSvg
    toEnum 2 = IconLookupFlagsForceSvg
    toEnum 4 = IconLookupFlagsUseBuiltin
    toEnum 8 = IconLookupFlagsGenericFallback
    toEnum 16 = IconLookupFlagsForceSize
    toEnum 32 = IconLookupFlagsForceRegular
    toEnum 64 = IconLookupFlagsForceSymbolic
    toEnum 128 = IconLookupFlagsDirLtr
    toEnum 256 = IconLookupFlagsDirRtl
    toEnum k = AnotherIconLookupFlags k

instance P.Ord IconLookupFlags where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes IconLookupFlags = '[]
instance O.HasParentTypes IconLookupFlags

foreign import ccall "gtk_icon_lookup_flags_get_type" c_gtk_icon_lookup_flags_get_type :: 
    IO GType

instance B.Types.TypedObject IconLookupFlags where
    glibType = c_gtk_icon_lookup_flags_get_type

instance B.Types.BoxedFlags IconLookupFlags

instance IsGFlag IconLookupFlags

-- Flags FontChooserLevel
-- | This enumeration specifies the granularity of font selection
-- that is desired in a font chooser.
-- 
-- This enumeration may be extended in the future; applications should
-- ignore unknown values.
data FontChooserLevel = 
      FontChooserLevelFamily
    -- ^ Allow selecting a font family
    | FontChooserLevelStyle
    -- ^ Allow selecting a specific font face
    | FontChooserLevelSize
    -- ^ Allow selecting a specific font size
    | FontChooserLevelVariations
    -- ^ /No description available in the introspection data./
    | FontChooserLevelFeatures
    -- ^ Allow selecting specific OpenType font features
    | AnotherFontChooserLevel Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum FontChooserLevel where
    fromEnum FontChooserLevelFamily = 0
    fromEnum FontChooserLevelStyle = 1
    fromEnum FontChooserLevelSize = 2
    fromEnum FontChooserLevelVariations = 4
    fromEnum FontChooserLevelFeatures = 8
    fromEnum (AnotherFontChooserLevel k) = k

    toEnum 0 = FontChooserLevelFamily
    toEnum 1 = FontChooserLevelStyle
    toEnum 2 = FontChooserLevelSize
    toEnum 4 = FontChooserLevelVariations
    toEnum 8 = FontChooserLevelFeatures
    toEnum k = AnotherFontChooserLevel k

instance P.Ord FontChooserLevel where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes FontChooserLevel = '[]
instance O.HasParentTypes FontChooserLevel

foreign import ccall "gtk_font_chooser_level_get_type" c_gtk_font_chooser_level_get_type :: 
    IO GType

instance B.Types.TypedObject FontChooserLevel where
    glibType = c_gtk_font_chooser_level_get_type

instance B.Types.BoxedFlags FontChooserLevel

instance IsGFlag FontChooserLevel

-- Flags FileFilterFlags
-- | These flags indicate what parts of a t'GI.Gtk.Structs.FileFilterInfo.FileFilterInfo' struct
-- are filled or need to be filled.
data FileFilterFlags = 
      FileFilterFlagsFilename
    -- ^ the filename of the file being tested
    | FileFilterFlagsUri
    -- ^ the URI for the file being tested
    | FileFilterFlagsDisplayName
    -- ^ the string that will be used to
    --   display the file in the file chooser
    | FileFilterFlagsMimeType
    -- ^ the mime type of the file
    | AnotherFileFilterFlags Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum FileFilterFlags where
    fromEnum FileFilterFlagsFilename = 1
    fromEnum FileFilterFlagsUri = 2
    fromEnum FileFilterFlagsDisplayName = 4
    fromEnum FileFilterFlagsMimeType = 8
    fromEnum (AnotherFileFilterFlags k) = k

    toEnum 1 = FileFilterFlagsFilename
    toEnum 2 = FileFilterFlagsUri
    toEnum 4 = FileFilterFlagsDisplayName
    toEnum 8 = FileFilterFlagsMimeType
    toEnum k = AnotherFileFilterFlags k

instance P.Ord FileFilterFlags where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes FileFilterFlags = '[]
instance O.HasParentTypes FileFilterFlags

foreign import ccall "gtk_file_filter_flags_get_type" c_gtk_file_filter_flags_get_type :: 
    IO GType

instance B.Types.TypedObject FileFilterFlags where
    glibType = c_gtk_file_filter_flags_get_type

instance B.Types.BoxedFlags FileFilterFlags

instance IsGFlag FileFilterFlags

-- Flags EventControllerScrollFlags
-- | Describes the behavior of a t'GI.Gtk.Objects.EventControllerScroll.EventControllerScroll'.
-- 
-- /Since: 3.24/
data EventControllerScrollFlags = 
      EventControllerScrollFlagsNone
    -- ^ Don\'t emit scroll.
    | EventControllerScrollFlagsVertical
    -- ^ Emit scroll with vertical deltas.
    | EventControllerScrollFlagsHorizontal
    -- ^ Emit scroll with horizontal deltas.
    | EventControllerScrollFlagsDiscrete
    -- ^ Only emit deltas that are multiples of 1.
    | EventControllerScrollFlagsKinetic
    -- ^ Emit [EventControllerScroll::decelerate]("GI.Gtk.Objects.EventControllerScroll#g:signal:decelerate")
    --   after continuous scroll finishes.
    | EventControllerScrollFlagsBothAxes
    -- ^ Emit scroll on both axes.
    | AnotherEventControllerScrollFlags Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum EventControllerScrollFlags where
    fromEnum EventControllerScrollFlagsNone = 0
    fromEnum EventControllerScrollFlagsVertical = 1
    fromEnum EventControllerScrollFlagsHorizontal = 2
    fromEnum EventControllerScrollFlagsDiscrete = 4
    fromEnum EventControllerScrollFlagsKinetic = 8
    fromEnum EventControllerScrollFlagsBothAxes = 3
    fromEnum (AnotherEventControllerScrollFlags k) = k

    toEnum 0 = EventControllerScrollFlagsNone
    toEnum 1 = EventControllerScrollFlagsVertical
    toEnum 2 = EventControllerScrollFlagsHorizontal
    toEnum 4 = EventControllerScrollFlagsDiscrete
    toEnum 8 = EventControllerScrollFlagsKinetic
    toEnum 3 = EventControllerScrollFlagsBothAxes
    toEnum k = AnotherEventControllerScrollFlags k

instance P.Ord EventControllerScrollFlags where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes EventControllerScrollFlags = '[]
instance O.HasParentTypes EventControllerScrollFlags

foreign import ccall "gtk_event_controller_scroll_flags_get_type" c_gtk_event_controller_scroll_flags_get_type :: 
    IO GType

instance B.Types.TypedObject EventControllerScrollFlags where
    glibType = c_gtk_event_controller_scroll_flags_get_type

instance B.Types.BoxedFlags EventControllerScrollFlags

instance IsGFlag EventControllerScrollFlags

-- Flags DialogFlags
-- | Flags used to influence dialog construction.
data DialogFlags = 
      DialogFlagsModal
    -- ^ Make the constructed dialog modal,
    --     see 'GI.Gtk.Objects.Window.windowSetModal'
    | DialogFlagsDestroyWithParent
    -- ^ Destroy the dialog when its
    --     parent is destroyed, see 'GI.Gtk.Objects.Window.windowSetDestroyWithParent'
    | DialogFlagsUseHeaderBar
    -- ^ Create dialog with actions in header
    --     bar instead of action area. Since 3.12.
    | AnotherDialogFlags Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum DialogFlags where
    fromEnum DialogFlagsModal = 1
    fromEnum DialogFlagsDestroyWithParent = 2
    fromEnum DialogFlagsUseHeaderBar = 4
    fromEnum (AnotherDialogFlags k) = k

    toEnum 1 = DialogFlagsModal
    toEnum 2 = DialogFlagsDestroyWithParent
    toEnum 4 = DialogFlagsUseHeaderBar
    toEnum k = AnotherDialogFlags k

instance P.Ord DialogFlags where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes DialogFlags = '[]
instance O.HasParentTypes DialogFlags

foreign import ccall "gtk_dialog_flags_get_type" c_gtk_dialog_flags_get_type :: 
    IO GType

instance B.Types.TypedObject DialogFlags where
    glibType = c_gtk_dialog_flags_get_type

instance B.Types.BoxedFlags DialogFlags

instance IsGFlag DialogFlags

-- Flags DestDefaults
-- | The t'GI.Gtk.Flags.DestDefaults' enumeration specifies the various
-- types of action that will be taken on behalf
-- of the user for a drag destination site.
data DestDefaults = 
      DestDefaultsMotion
    -- ^ If set for a widget, GTK+, during a drag over this
    --   widget will check if the drag matches this widget’s list of possible targets
    --   and actions.
    --   GTK+ will then call 'GI.Gdk.Functions.dragStatus' as appropriate.
    | DestDefaultsHighlight
    -- ^ If set for a widget, GTK+ will draw a highlight on
    --   this widget as long as a drag is over this widget and the widget drag format
    --   and action are acceptable.
    | DestDefaultsDrop
    -- ^ If set for a widget, when a drop occurs, GTK+ will
    --   will check if the drag matches this widget’s list of possible targets and
    --   actions. If so, GTK+ will call 'GI.Gtk.Objects.Widget.widgetDragGetData' on behalf of the widget.
    --   Whether or not the drop is successful, GTK+ will call 'GI.Gtk.Functions.dragFinish'. If
    --   the action was a move, then if the drag was successful, then 'P.True' will be
    --   passed for the /@delete@/ parameter to 'GI.Gtk.Functions.dragFinish'.
    | DestDefaultsAll
    -- ^ If set, specifies that all default actions should
    --   be taken.
    | AnotherDestDefaults Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum DestDefaults where
    fromEnum DestDefaultsMotion = 1
    fromEnum DestDefaultsHighlight = 2
    fromEnum DestDefaultsDrop = 4
    fromEnum DestDefaultsAll = 7
    fromEnum (AnotherDestDefaults k) = k

    toEnum 1 = DestDefaultsMotion
    toEnum 2 = DestDefaultsHighlight
    toEnum 4 = DestDefaultsDrop
    toEnum 7 = DestDefaultsAll
    toEnum k = AnotherDestDefaults k

instance P.Ord DestDefaults where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes DestDefaults = '[]
instance O.HasParentTypes DestDefaults

foreign import ccall "gtk_dest_defaults_get_type" c_gtk_dest_defaults_get_type :: 
    IO GType

instance B.Types.TypedObject DestDefaults where
    glibType = c_gtk_dest_defaults_get_type

instance B.Types.BoxedFlags DestDefaults

instance IsGFlag DestDefaults

-- Flags DebugFlag
-- | /No description available in the introspection data./
data DebugFlag = 
      DebugFlagMisc
    -- ^ /No description available in the introspection data./
    | DebugFlagPlugsocket
    -- ^ /No description available in the introspection data./
    | DebugFlagText
    -- ^ /No description available in the introspection data./
    | DebugFlagTree
    -- ^ /No description available in the introspection data./
    | DebugFlagUpdates
    -- ^ /No description available in the introspection data./
    | DebugFlagKeybindings
    -- ^ /No description available in the introspection data./
    | DebugFlagMultihead
    -- ^ /No description available in the introspection data./
    | DebugFlagModules
    -- ^ /No description available in the introspection data./
    | DebugFlagGeometry
    -- ^ /No description available in the introspection data./
    | DebugFlagIcontheme
    -- ^ /No description available in the introspection data./
    | DebugFlagPrinting
    -- ^ /No description available in the introspection data./
    | DebugFlagBuilder
    -- ^ /No description available in the introspection data./
    | DebugFlagSizeRequest
    -- ^ /No description available in the introspection data./
    | DebugFlagNoCssCache
    -- ^ /No description available in the introspection data./
    | DebugFlagBaselines
    -- ^ /No description available in the introspection data./
    | DebugFlagPixelCache
    -- ^ /No description available in the introspection data./
    | DebugFlagNoPixelCache
    -- ^ /No description available in the introspection data./
    | DebugFlagInteractive
    -- ^ /No description available in the introspection data./
    | DebugFlagTouchscreen
    -- ^ /No description available in the introspection data./
    | DebugFlagActions
    -- ^ /No description available in the introspection data./
    | DebugFlagResize
    -- ^ /No description available in the introspection data./
    | DebugFlagLayout
    -- ^ /No description available in the introspection data./
    | AnotherDebugFlag Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum DebugFlag where
    fromEnum DebugFlagMisc = 1
    fromEnum DebugFlagPlugsocket = 2
    fromEnum DebugFlagText = 4
    fromEnum DebugFlagTree = 8
    fromEnum DebugFlagUpdates = 16
    fromEnum DebugFlagKeybindings = 32
    fromEnum DebugFlagMultihead = 64
    fromEnum DebugFlagModules = 128
    fromEnum DebugFlagGeometry = 256
    fromEnum DebugFlagIcontheme = 512
    fromEnum DebugFlagPrinting = 1024
    fromEnum DebugFlagBuilder = 2048
    fromEnum DebugFlagSizeRequest = 4096
    fromEnum DebugFlagNoCssCache = 8192
    fromEnum DebugFlagBaselines = 16384
    fromEnum DebugFlagPixelCache = 32768
    fromEnum DebugFlagNoPixelCache = 65536
    fromEnum DebugFlagInteractive = 131072
    fromEnum DebugFlagTouchscreen = 262144
    fromEnum DebugFlagActions = 524288
    fromEnum DebugFlagResize = 1048576
    fromEnum DebugFlagLayout = 2097152
    fromEnum (AnotherDebugFlag k) = k

    toEnum 1 = DebugFlagMisc
    toEnum 2 = DebugFlagPlugsocket
    toEnum 4 = DebugFlagText
    toEnum 8 = DebugFlagTree
    toEnum 16 = DebugFlagUpdates
    toEnum 32 = DebugFlagKeybindings
    toEnum 64 = DebugFlagMultihead
    toEnum 128 = DebugFlagModules
    toEnum 256 = DebugFlagGeometry
    toEnum 512 = DebugFlagIcontheme
    toEnum 1024 = DebugFlagPrinting
    toEnum 2048 = DebugFlagBuilder
    toEnum 4096 = DebugFlagSizeRequest
    toEnum 8192 = DebugFlagNoCssCache
    toEnum 16384 = DebugFlagBaselines
    toEnum 32768 = DebugFlagPixelCache
    toEnum 65536 = DebugFlagNoPixelCache
    toEnum 131072 = DebugFlagInteractive
    toEnum 262144 = DebugFlagTouchscreen
    toEnum 524288 = DebugFlagActions
    toEnum 1048576 = DebugFlagResize
    toEnum 2097152 = DebugFlagLayout
    toEnum k = AnotherDebugFlag k

instance P.Ord DebugFlag where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes DebugFlag = '[]
instance O.HasParentTypes DebugFlag

foreign import ccall "gtk_debug_flag_get_type" c_gtk_debug_flag_get_type :: 
    IO GType

instance B.Types.TypedObject DebugFlag where
    glibType = c_gtk_debug_flag_get_type

instance B.Types.BoxedFlags DebugFlag

instance IsGFlag DebugFlag

-- Flags CellRendererState
-- | Tells how a cell is to be rendered.
data CellRendererState = 
      CellRendererStateSelected
    -- ^ The cell is currently selected, and
    --  probably has a selection colored background to render to.
    | CellRendererStatePrelit
    -- ^ The mouse is hovering over the cell.
    | CellRendererStateInsensitive
    -- ^ The cell is drawn in an insensitive manner
    | CellRendererStateSorted
    -- ^ The cell is in a sorted row
    | CellRendererStateFocused
    -- ^ The cell is in the focus row.
    | CellRendererStateExpandable
    -- ^ The cell is in a row that can be expanded. Since 3.4
    | CellRendererStateExpanded
    -- ^ The cell is in a row that is expanded. Since 3.4
    | AnotherCellRendererState Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum CellRendererState where
    fromEnum CellRendererStateSelected = 1
    fromEnum CellRendererStatePrelit = 2
    fromEnum CellRendererStateInsensitive = 4
    fromEnum CellRendererStateSorted = 8
    fromEnum CellRendererStateFocused = 16
    fromEnum CellRendererStateExpandable = 32
    fromEnum CellRendererStateExpanded = 64
    fromEnum (AnotherCellRendererState k) = k

    toEnum 1 = CellRendererStateSelected
    toEnum 2 = CellRendererStatePrelit
    toEnum 4 = CellRendererStateInsensitive
    toEnum 8 = CellRendererStateSorted
    toEnum 16 = CellRendererStateFocused
    toEnum 32 = CellRendererStateExpandable
    toEnum 64 = CellRendererStateExpanded
    toEnum k = AnotherCellRendererState k

instance P.Ord CellRendererState where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes CellRendererState = '[]
instance O.HasParentTypes CellRendererState

foreign import ccall "gtk_cell_renderer_state_get_type" c_gtk_cell_renderer_state_get_type :: 
    IO GType

instance B.Types.TypedObject CellRendererState where
    glibType = c_gtk_cell_renderer_state_get_type

instance B.Types.BoxedFlags CellRendererState

instance IsGFlag CellRendererState

-- Flags CalendarDisplayOptions
-- | These options can be used to influence the display and behaviour of a t'GI.Gtk.Objects.Calendar.Calendar'.
data CalendarDisplayOptions = 
      CalendarDisplayOptionsShowHeading
    -- ^ Specifies that the month and year should be displayed.
    | CalendarDisplayOptionsShowDayNames
    -- ^ Specifies that three letter day descriptions should be present.
    | CalendarDisplayOptionsNoMonthChange
    -- ^ Prevents the user from switching months with the calendar.
    | CalendarDisplayOptionsShowWeekNumbers
    -- ^ Displays each week numbers of the current year, down the
    -- left side of the calendar.
    | CalendarDisplayOptionsShowDetails
    -- ^ Just show an indicator, not the full details
    -- text when details are provided. See 'GI.Gtk.Objects.Calendar.calendarSetDetailFunc'.
    | AnotherCalendarDisplayOptions Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum CalendarDisplayOptions where
    fromEnum CalendarDisplayOptionsShowHeading = 1
    fromEnum CalendarDisplayOptionsShowDayNames = 2
    fromEnum CalendarDisplayOptionsNoMonthChange = 4
    fromEnum CalendarDisplayOptionsShowWeekNumbers = 8
    fromEnum CalendarDisplayOptionsShowDetails = 32
    fromEnum (AnotherCalendarDisplayOptions k) = k

    toEnum 1 = CalendarDisplayOptionsShowHeading
    toEnum 2 = CalendarDisplayOptionsShowDayNames
    toEnum 4 = CalendarDisplayOptionsNoMonthChange
    toEnum 8 = CalendarDisplayOptionsShowWeekNumbers
    toEnum 32 = CalendarDisplayOptionsShowDetails
    toEnum k = AnotherCalendarDisplayOptions k

instance P.Ord CalendarDisplayOptions where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes CalendarDisplayOptions = '[]
instance O.HasParentTypes CalendarDisplayOptions

foreign import ccall "gtk_calendar_display_options_get_type" c_gtk_calendar_display_options_get_type :: 
    IO GType

instance B.Types.TypedObject CalendarDisplayOptions where
    glibType = c_gtk_calendar_display_options_get_type

instance B.Types.BoxedFlags CalendarDisplayOptions

instance IsGFlag CalendarDisplayOptions

-- Flags AttachOptions
-- | Denotes the expansion properties that a widget will have when it (or its
-- parent) is resized.
data AttachOptions = 
      AttachOptionsExpand
    -- ^ the widget should expand to take up any extra space in its
    -- container that has been allocated.
    | AttachOptionsShrink
    -- ^ the widget should shrink as and when possible.
    | AttachOptionsFill
    -- ^ the widget should fill the space allocated to it.
    | AnotherAttachOptions Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum AttachOptions where
    fromEnum AttachOptionsExpand = 1
    fromEnum AttachOptionsShrink = 2
    fromEnum AttachOptionsFill = 4
    fromEnum (AnotherAttachOptions k) = k

    toEnum 1 = AttachOptionsExpand
    toEnum 2 = AttachOptionsShrink
    toEnum 4 = AttachOptionsFill
    toEnum k = AnotherAttachOptions k

instance P.Ord AttachOptions where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes AttachOptions = '[]
instance O.HasParentTypes AttachOptions

foreign import ccall "gtk_attach_options_get_type" c_gtk_attach_options_get_type :: 
    IO GType

instance B.Types.TypedObject AttachOptions where
    glibType = c_gtk_attach_options_get_type

instance B.Types.BoxedFlags AttachOptions

instance IsGFlag AttachOptions

-- Flags ApplicationInhibitFlags
-- | Types of user actions that may be blocked by 'GI.Gtk.Objects.Application.applicationInhibit'.
-- 
-- /Since: 3.4/
data ApplicationInhibitFlags = 
      ApplicationInhibitFlagsLogout
    -- ^ Inhibit ending the user session
    --     by logging out or by shutting down the computer
    | ApplicationInhibitFlagsSwitch
    -- ^ Inhibit user switching
    | ApplicationInhibitFlagsSuspend
    -- ^ Inhibit suspending the
    --     session or computer
    | ApplicationInhibitFlagsIdle
    -- ^ Inhibit the session being
    --     marked as idle (and possibly locked)
    | AnotherApplicationInhibitFlags Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum ApplicationInhibitFlags where
    fromEnum ApplicationInhibitFlagsLogout = 1
    fromEnum ApplicationInhibitFlagsSwitch = 2
    fromEnum ApplicationInhibitFlagsSuspend = 4
    fromEnum ApplicationInhibitFlagsIdle = 8
    fromEnum (AnotherApplicationInhibitFlags k) = k

    toEnum 1 = ApplicationInhibitFlagsLogout
    toEnum 2 = ApplicationInhibitFlagsSwitch
    toEnum 4 = ApplicationInhibitFlagsSuspend
    toEnum 8 = ApplicationInhibitFlagsIdle
    toEnum k = AnotherApplicationInhibitFlags k

instance P.Ord ApplicationInhibitFlags where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes ApplicationInhibitFlags = '[]
instance O.HasParentTypes ApplicationInhibitFlags

foreign import ccall "gtk_application_inhibit_flags_get_type" c_gtk_application_inhibit_flags_get_type :: 
    IO GType

instance B.Types.TypedObject ApplicationInhibitFlags where
    glibType = c_gtk_application_inhibit_flags_get_type

instance B.Types.BoxedFlags ApplicationInhibitFlags

instance IsGFlag ApplicationInhibitFlags

-- Flags AccelFlags
-- | Accelerator flags used with 'GI.Gtk.Objects.AccelGroup.accelGroupConnect'.
data AccelFlags = 
      AccelFlagsVisible
    -- ^ Accelerator is visible
    | AccelFlagsLocked
    -- ^ Accelerator not removable
    | AccelFlagsMask
    -- ^ Mask
    | AnotherAccelFlags Int
    -- ^ Catch-all for unknown values
    deriving (Show, Eq)

instance P.Enum AccelFlags where
    fromEnum AccelFlagsVisible = 1
    fromEnum AccelFlagsLocked = 2
    fromEnum AccelFlagsMask = 7
    fromEnum (AnotherAccelFlags k) = k

    toEnum 1 = AccelFlagsVisible
    toEnum 2 = AccelFlagsLocked
    toEnum 7 = AccelFlagsMask
    toEnum k = AnotherAccelFlags k

instance P.Ord AccelFlags where
    compare a b = P.compare (P.fromEnum a) (P.fromEnum b)

type instance O.ParentTypes AccelFlags = '[]
instance O.HasParentTypes AccelFlags

foreign import ccall "gtk_accel_flags_get_type" c_gtk_accel_flags_get_type :: 
    IO GType

instance B.Types.TypedObject AccelFlags where
    glibType = c_gtk_accel_flags_get_type

instance B.Types.BoxedFlags AccelFlags

instance IsGFlag AccelFlags


