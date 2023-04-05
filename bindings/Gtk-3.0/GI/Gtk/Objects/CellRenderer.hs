{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.CellRenderer.CellRenderer' is a base class of a set of objects used for
-- rendering a cell to a t'GI.Cairo.Structs.Context.Context'.  These objects are used primarily by
-- the t'GI.Gtk.Objects.TreeView.TreeView' widget, though they aren’t tied to them in any
-- specific way.  It is worth noting that t'GI.Gtk.Objects.CellRenderer.CellRenderer' is not a
-- t'GI.Gtk.Objects.Widget.Widget' and cannot be treated as such.
-- 
-- The primary use of a t'GI.Gtk.Objects.CellRenderer.CellRenderer' is for drawing a certain graphical
-- elements on a t'GI.Cairo.Structs.Context.Context'. Typically, one cell renderer is used to
-- draw many cells on the screen.  To this extent, it isn’t expected that a
-- CellRenderer keep any permanent state around.  Instead, any state is set
-- just prior to use using @/GObjects/@ property system.  Then, the
-- cell is measured using 'GI.Gtk.Objects.CellRenderer.cellRendererGetSize'. Finally, the cell
-- is rendered in the correct location using 'GI.Gtk.Objects.CellRenderer.cellRendererRender'.
-- 
-- There are a number of rules that must be followed when writing a new
-- t'GI.Gtk.Objects.CellRenderer.CellRenderer'.  First and foremost, it’s important that a certain set
-- of properties will always yield a cell renderer of the same size,
-- barring a t'GI.Gtk.Objects.Style.Style' change.  The t'GI.Gtk.Objects.CellRenderer.CellRenderer' also has a number of
-- generic properties that are expected to be honored by all children.
-- 
-- Beyond merely rendering a cell, cell renderers can optionally
-- provide active user interface elements. A cell renderer can be
-- “activatable” like t'GI.Gtk.Objects.CellRendererToggle.CellRendererToggle',
-- which toggles when it gets activated by a mouse click, or it can be
-- “editable” like t'GI.Gtk.Objects.CellRendererText.CellRendererText', which
-- allows the user to edit the text using a widget implementing the
-- t'GI.Gtk.Interfaces.CellEditable.CellEditable' interface, e.g. t'GI.Gtk.Objects.Entry.Entry'.
-- To make a cell renderer activatable or editable, you have to
-- implement the t'GI.Gtk.Structs.CellRendererClass.CellRendererClass'.@/activate/@ or
-- t'GI.Gtk.Structs.CellRendererClass.CellRendererClass'.@/start_editing/@ virtual functions, respectively.
-- 
-- Many properties of t'GI.Gtk.Objects.CellRenderer.CellRenderer' and its subclasses have a
-- corresponding “set” property, e.g. “cell-background-set” corresponds
-- to “cell-background”. These “set” properties reflect whether a property
-- has been set or not. You should not set them independently.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.CellRenderer
    ( 

-- * Exported types
    CellRenderer(..)                        ,
    IsCellRenderer                          ,
    toCellRenderer                          ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [activate]("GI.Gtk.Objects.CellRenderer#g:method:activate"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isActivatable]("GI.Gtk.Objects.CellRenderer#g:method:isActivatable"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [render]("GI.Gtk.Objects.CellRenderer#g:method:render"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [startEditing]("GI.Gtk.Objects.CellRenderer#g:method:startEditing"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [stopEditing]("GI.Gtk.Objects.CellRenderer#g:method:stopEditing"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAlignedArea]("GI.Gtk.Objects.CellRenderer#g:method:getAlignedArea"), [getAlignment]("GI.Gtk.Objects.CellRenderer#g:method:getAlignment"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getFixedSize]("GI.Gtk.Objects.CellRenderer#g:method:getFixedSize"), [getPadding]("GI.Gtk.Objects.CellRenderer#g:method:getPadding"), [getPreferredHeight]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredHeight"), [getPreferredHeightForWidth]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredHeightForWidth"), [getPreferredSize]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredSize"), [getPreferredWidth]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredWidth"), [getPreferredWidthForHeight]("GI.Gtk.Objects.CellRenderer#g:method:getPreferredWidthForHeight"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getRequestMode]("GI.Gtk.Objects.CellRenderer#g:method:getRequestMode"), [getSensitive]("GI.Gtk.Objects.CellRenderer#g:method:getSensitive"), [getSize]("GI.Gtk.Objects.CellRenderer#g:method:getSize"), [getState]("GI.Gtk.Objects.CellRenderer#g:method:getState"), [getVisible]("GI.Gtk.Objects.CellRenderer#g:method:getVisible").
-- 
-- ==== Setters
-- [setAlignment]("GI.Gtk.Objects.CellRenderer#g:method:setAlignment"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setFixedSize]("GI.Gtk.Objects.CellRenderer#g:method:setFixedSize"), [setPadding]("GI.Gtk.Objects.CellRenderer#g:method:setPadding"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSensitive]("GI.Gtk.Objects.CellRenderer#g:method:setSensitive"), [setVisible]("GI.Gtk.Objects.CellRenderer#g:method:setVisible").

#if defined(ENABLE_OVERLOADING)
    ResolveCellRendererMethod               ,
#endif

-- ** activate #method:activate#

#if defined(ENABLE_OVERLOADING)
    CellRendererActivateMethodInfo          ,
#endif
    cellRendererActivate                    ,


-- ** getAlignedArea #method:getAlignedArea#

#if defined(ENABLE_OVERLOADING)
    CellRendererGetAlignedAreaMethodInfo    ,
#endif
    cellRendererGetAlignedArea              ,


-- ** getAlignment #method:getAlignment#

#if defined(ENABLE_OVERLOADING)
    CellRendererGetAlignmentMethodInfo      ,
#endif
    cellRendererGetAlignment                ,


-- ** getFixedSize #method:getFixedSize#

#if defined(ENABLE_OVERLOADING)
    CellRendererGetFixedSizeMethodInfo      ,
#endif
    cellRendererGetFixedSize                ,


-- ** getPadding #method:getPadding#

#if defined(ENABLE_OVERLOADING)
    CellRendererGetPaddingMethodInfo        ,
#endif
    cellRendererGetPadding                  ,


-- ** getPreferredHeight #method:getPreferredHeight#

#if defined(ENABLE_OVERLOADING)
    CellRendererGetPreferredHeightMethodInfo,
#endif
    cellRendererGetPreferredHeight          ,


-- ** getPreferredHeightForWidth #method:getPreferredHeightForWidth#

#if defined(ENABLE_OVERLOADING)
    CellRendererGetPreferredHeightForWidthMethodInfo,
#endif
    cellRendererGetPreferredHeightForWidth  ,


-- ** getPreferredSize #method:getPreferredSize#

#if defined(ENABLE_OVERLOADING)
    CellRendererGetPreferredSizeMethodInfo  ,
#endif
    cellRendererGetPreferredSize            ,


-- ** getPreferredWidth #method:getPreferredWidth#

#if defined(ENABLE_OVERLOADING)
    CellRendererGetPreferredWidthMethodInfo ,
#endif
    cellRendererGetPreferredWidth           ,


-- ** getPreferredWidthForHeight #method:getPreferredWidthForHeight#

#if defined(ENABLE_OVERLOADING)
    CellRendererGetPreferredWidthForHeightMethodInfo,
#endif
    cellRendererGetPreferredWidthForHeight  ,


-- ** getRequestMode #method:getRequestMode#

#if defined(ENABLE_OVERLOADING)
    CellRendererGetRequestModeMethodInfo    ,
#endif
    cellRendererGetRequestMode              ,


-- ** getSensitive #method:getSensitive#

#if defined(ENABLE_OVERLOADING)
    CellRendererGetSensitiveMethodInfo      ,
#endif
    cellRendererGetSensitive                ,


-- ** getSize #method:getSize#

#if defined(ENABLE_OVERLOADING)
    CellRendererGetSizeMethodInfo           ,
#endif
    cellRendererGetSize                     ,


-- ** getState #method:getState#

#if defined(ENABLE_OVERLOADING)
    CellRendererGetStateMethodInfo          ,
#endif
    cellRendererGetState                    ,


-- ** getVisible #method:getVisible#

#if defined(ENABLE_OVERLOADING)
    CellRendererGetVisibleMethodInfo        ,
#endif
    cellRendererGetVisible                  ,


-- ** isActivatable #method:isActivatable#

#if defined(ENABLE_OVERLOADING)
    CellRendererIsActivatableMethodInfo     ,
#endif
    cellRendererIsActivatable               ,


-- ** render #method:render#

#if defined(ENABLE_OVERLOADING)
    CellRendererRenderMethodInfo            ,
#endif
    cellRendererRender                      ,


-- ** setAlignment #method:setAlignment#

#if defined(ENABLE_OVERLOADING)
    CellRendererSetAlignmentMethodInfo      ,
#endif
    cellRendererSetAlignment                ,


-- ** setFixedSize #method:setFixedSize#

#if defined(ENABLE_OVERLOADING)
    CellRendererSetFixedSizeMethodInfo      ,
#endif
    cellRendererSetFixedSize                ,


-- ** setPadding #method:setPadding#

#if defined(ENABLE_OVERLOADING)
    CellRendererSetPaddingMethodInfo        ,
#endif
    cellRendererSetPadding                  ,


-- ** setSensitive #method:setSensitive#

#if defined(ENABLE_OVERLOADING)
    CellRendererSetSensitiveMethodInfo      ,
#endif
    cellRendererSetSensitive                ,


-- ** setVisible #method:setVisible#

#if defined(ENABLE_OVERLOADING)
    CellRendererSetVisibleMethodInfo        ,
#endif
    cellRendererSetVisible                  ,


-- ** startEditing #method:startEditing#

#if defined(ENABLE_OVERLOADING)
    CellRendererStartEditingMethodInfo      ,
#endif
    cellRendererStartEditing                ,


-- ** stopEditing #method:stopEditing#

#if defined(ENABLE_OVERLOADING)
    CellRendererStopEditingMethodInfo       ,
#endif
    cellRendererStopEditing                 ,




 -- * Properties


-- ** cellBackground #attr:cellBackground#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererCellBackgroundPropertyInfo  ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererCellBackground              ,
#endif
    clearCellRendererCellBackground         ,
    constructCellRendererCellBackground     ,
    setCellRendererCellBackground           ,


-- ** cellBackgroundGdk #attr:cellBackgroundGdk#
-- | Cell background as a t'GI.Gdk.Structs.Color.Color'

#if defined(ENABLE_OVERLOADING)
    CellRendererCellBackgroundGdkPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererCellBackgroundGdk           ,
#endif
    clearCellRendererCellBackgroundGdk      ,
    constructCellRendererCellBackgroundGdk  ,
    getCellRendererCellBackgroundGdk        ,
    setCellRendererCellBackgroundGdk        ,


-- ** cellBackgroundRgba #attr:cellBackgroundRgba#
-- | Cell background as a t'GI.Gdk.Structs.RGBA.RGBA'
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    CellRendererCellBackgroundRgbaPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererCellBackgroundRgba          ,
#endif
    clearCellRendererCellBackgroundRgba     ,
    constructCellRendererCellBackgroundRgba ,
    getCellRendererCellBackgroundRgba       ,
    setCellRendererCellBackgroundRgba       ,


-- ** cellBackgroundSet #attr:cellBackgroundSet#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererCellBackgroundSetPropertyInfo,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererCellBackgroundSet           ,
#endif
    constructCellRendererCellBackgroundSet  ,
    getCellRendererCellBackgroundSet        ,
    setCellRendererCellBackgroundSet        ,


-- ** editing #attr:editing#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererEditingPropertyInfo         ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererEditing                     ,
#endif
    getCellRendererEditing                  ,


-- ** height #attr:height#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererHeightPropertyInfo          ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererHeight                      ,
#endif
    constructCellRendererHeight             ,
    getCellRendererHeight                   ,
    setCellRendererHeight                   ,


-- ** isExpanded #attr:isExpanded#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererIsExpandedPropertyInfo      ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererIsExpanded                  ,
#endif
    constructCellRendererIsExpanded         ,
    getCellRendererIsExpanded               ,
    setCellRendererIsExpanded               ,


-- ** isExpander #attr:isExpander#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererIsExpanderPropertyInfo      ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererIsExpander                  ,
#endif
    constructCellRendererIsExpander         ,
    getCellRendererIsExpander               ,
    setCellRendererIsExpander               ,


-- ** mode #attr:mode#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererModePropertyInfo            ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererMode                        ,
#endif
    constructCellRendererMode               ,
    getCellRendererMode                     ,
    setCellRendererMode                     ,


-- ** sensitive #attr:sensitive#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererSensitivePropertyInfo       ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererSensitive                   ,
#endif
    constructCellRendererSensitive          ,
    getCellRendererSensitive                ,
    setCellRendererSensitive                ,


-- ** visible #attr:visible#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererVisiblePropertyInfo         ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererVisible                     ,
#endif
    constructCellRendererVisible            ,
    getCellRendererVisible                  ,
    setCellRendererVisible                  ,


-- ** width #attr:width#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererWidthPropertyInfo           ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererWidth                       ,
#endif
    constructCellRendererWidth              ,
    getCellRendererWidth                    ,
    setCellRendererWidth                    ,


-- ** xalign #attr:xalign#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererXalignPropertyInfo          ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererXalign                      ,
#endif
    constructCellRendererXalign             ,
    getCellRendererXalign                   ,
    setCellRendererXalign                   ,


-- ** xpad #attr:xpad#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererXpadPropertyInfo            ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererXpad                        ,
#endif
    constructCellRendererXpad               ,
    getCellRendererXpad                     ,
    setCellRendererXpad                     ,


-- ** yalign #attr:yalign#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererYalignPropertyInfo          ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererYalign                      ,
#endif
    constructCellRendererYalign             ,
    getCellRendererYalign                   ,
    setCellRendererYalign                   ,


-- ** ypad #attr:ypad#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    CellRendererYpadPropertyInfo            ,
#endif
#if defined(ENABLE_OVERLOADING)
    cellRendererYpad                        ,
#endif
    constructCellRendererYpad               ,
    getCellRendererYpad                     ,
    setCellRendererYpad                     ,




 -- * Signals


-- ** editingCanceled #signal:editingCanceled#

    CellRendererEditingCanceledCallback     ,
#if defined(ENABLE_OVERLOADING)
    CellRendererEditingCanceledSignalInfo   ,
#endif
    afterCellRendererEditingCanceled        ,
    onCellRendererEditingCanceled           ,


-- ** editingStarted #signal:editingStarted#

    CellRendererEditingStartedCallback      ,
#if defined(ENABLE_OVERLOADING)
    CellRendererEditingStartedSignalInfo    ,
#endif
    afterCellRendererEditingStarted         ,
    onCellRendererEditingStarted            ,




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

import qualified GI.Cairo.Structs.Context as Cairo.Context
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gdk.Structs.Color as Gdk.Color
import qualified GI.Gdk.Structs.RGBA as Gdk.RGBA
import qualified GI.Gdk.Structs.Rectangle as Gdk.Rectangle
import qualified GI.Gdk.Unions.Event as Gdk.Event
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.CellEditable as Gtk.CellEditable
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Structs.Requisition as Gtk.Requisition

-- | Memory-managed wrapper type.
newtype CellRenderer = CellRenderer (SP.ManagedPtr CellRenderer)
    deriving (Eq)

instance SP.ManagedPtrNewtype CellRenderer where
    toManagedPtr (CellRenderer p) = p

foreign import ccall "gtk_cell_renderer_get_type"
    c_gtk_cell_renderer_get_type :: IO B.Types.GType

instance B.Types.TypedObject CellRenderer where
    glibType = c_gtk_cell_renderer_get_type

instance B.Types.GObject CellRenderer

-- | Type class for types which can be safely cast to `CellRenderer`, for instance with `toCellRenderer`.
class (SP.GObject o, O.IsDescendantOf CellRenderer o) => IsCellRenderer o
instance (SP.GObject o, O.IsDescendantOf CellRenderer o) => IsCellRenderer o

instance O.HasParentTypes CellRenderer
type instance O.ParentTypes CellRenderer = '[GObject.Object.Object]

-- | Cast to `CellRenderer`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toCellRenderer :: (MIO.MonadIO m, IsCellRenderer o) => o -> m CellRenderer
toCellRenderer = MIO.liftIO . B.ManagedPtr.unsafeCastTo CellRenderer

-- | Convert 'CellRenderer' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe CellRenderer) where
    gvalueGType_ = c_gtk_cell_renderer_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr CellRenderer)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr CellRenderer)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject CellRenderer ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveCellRendererMethod (t :: Symbol) (o :: *) :: * where
    ResolveCellRendererMethod "activate" o = CellRendererActivateMethodInfo
    ResolveCellRendererMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveCellRendererMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveCellRendererMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveCellRendererMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveCellRendererMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveCellRendererMethod "isActivatable" o = CellRendererIsActivatableMethodInfo
    ResolveCellRendererMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveCellRendererMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveCellRendererMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveCellRendererMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveCellRendererMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveCellRendererMethod "render" o = CellRendererRenderMethodInfo
    ResolveCellRendererMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveCellRendererMethod "startEditing" o = CellRendererStartEditingMethodInfo
    ResolveCellRendererMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveCellRendererMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveCellRendererMethod "stopEditing" o = CellRendererStopEditingMethodInfo
    ResolveCellRendererMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveCellRendererMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveCellRendererMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveCellRendererMethod "getAlignedArea" o = CellRendererGetAlignedAreaMethodInfo
    ResolveCellRendererMethod "getAlignment" o = CellRendererGetAlignmentMethodInfo
    ResolveCellRendererMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveCellRendererMethod "getFixedSize" o = CellRendererGetFixedSizeMethodInfo
    ResolveCellRendererMethod "getPadding" o = CellRendererGetPaddingMethodInfo
    ResolveCellRendererMethod "getPreferredHeight" o = CellRendererGetPreferredHeightMethodInfo
    ResolveCellRendererMethod "getPreferredHeightForWidth" o = CellRendererGetPreferredHeightForWidthMethodInfo
    ResolveCellRendererMethod "getPreferredSize" o = CellRendererGetPreferredSizeMethodInfo
    ResolveCellRendererMethod "getPreferredWidth" o = CellRendererGetPreferredWidthMethodInfo
    ResolveCellRendererMethod "getPreferredWidthForHeight" o = CellRendererGetPreferredWidthForHeightMethodInfo
    ResolveCellRendererMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveCellRendererMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveCellRendererMethod "getRequestMode" o = CellRendererGetRequestModeMethodInfo
    ResolveCellRendererMethod "getSensitive" o = CellRendererGetSensitiveMethodInfo
    ResolveCellRendererMethod "getSize" o = CellRendererGetSizeMethodInfo
    ResolveCellRendererMethod "getState" o = CellRendererGetStateMethodInfo
    ResolveCellRendererMethod "getVisible" o = CellRendererGetVisibleMethodInfo
    ResolveCellRendererMethod "setAlignment" o = CellRendererSetAlignmentMethodInfo
    ResolveCellRendererMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveCellRendererMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveCellRendererMethod "setFixedSize" o = CellRendererSetFixedSizeMethodInfo
    ResolveCellRendererMethod "setPadding" o = CellRendererSetPaddingMethodInfo
    ResolveCellRendererMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveCellRendererMethod "setSensitive" o = CellRendererSetSensitiveMethodInfo
    ResolveCellRendererMethod "setVisible" o = CellRendererSetVisibleMethodInfo
    ResolveCellRendererMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveCellRendererMethod t CellRenderer, O.OverloadedMethod info CellRenderer p) => OL.IsLabel t (CellRenderer -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveCellRendererMethod t CellRenderer, O.OverloadedMethod info CellRenderer p, R.HasField t CellRenderer p) => R.HasField t CellRenderer p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveCellRendererMethod t CellRenderer, O.OverloadedMethodInfo info CellRenderer) => OL.IsLabel t (O.MethodProxy info CellRenderer) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal CellRenderer::editing-canceled
-- | This signal gets emitted when the user cancels the process of editing a
-- cell.  For example, an editable cell renderer could be written to cancel
-- editing when the user presses Escape.
-- 
-- See also: 'GI.Gtk.Objects.CellRenderer.cellRendererStopEditing'.
-- 
-- /Since: 2.4/
type CellRendererEditingCanceledCallback =
    IO ()

type C_CellRendererEditingCanceledCallback =
    Ptr CellRenderer ->                     -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CellRendererEditingCanceledCallback`.
foreign import ccall "wrapper"
    mk_CellRendererEditingCanceledCallback :: C_CellRendererEditingCanceledCallback -> IO (FunPtr C_CellRendererEditingCanceledCallback)

wrap_CellRendererEditingCanceledCallback :: 
    GObject a => (a -> CellRendererEditingCanceledCallback) ->
    C_CellRendererEditingCanceledCallback
wrap_CellRendererEditingCanceledCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [editingCanceled](#signal:editingCanceled) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' cellRenderer #editingCanceled callback
-- @
-- 
-- 
onCellRendererEditingCanceled :: (IsCellRenderer a, MonadIO m) => a -> ((?self :: a) => CellRendererEditingCanceledCallback) -> m SignalHandlerId
onCellRendererEditingCanceled obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellRendererEditingCanceledCallback wrapped
    wrapped'' <- mk_CellRendererEditingCanceledCallback wrapped'
    connectSignalFunPtr obj "editing-canceled" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [editingCanceled](#signal:editingCanceled) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' cellRenderer #editingCanceled callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCellRendererEditingCanceled :: (IsCellRenderer a, MonadIO m) => a -> ((?self :: a) => CellRendererEditingCanceledCallback) -> m SignalHandlerId
afterCellRendererEditingCanceled obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellRendererEditingCanceledCallback wrapped
    wrapped'' <- mk_CellRendererEditingCanceledCallback wrapped'
    connectSignalFunPtr obj "editing-canceled" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CellRendererEditingCanceledSignalInfo
instance SignalInfo CellRendererEditingCanceledSignalInfo where
    type HaskellCallbackType CellRendererEditingCanceledSignalInfo = CellRendererEditingCanceledCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CellRendererEditingCanceledCallback cb
        cb'' <- mk_CellRendererEditingCanceledCallback cb'
        connectSignalFunPtr obj "editing-canceled" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer::editing-canceled"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:signal:editingCanceled"})

#endif

-- signal CellRenderer::editing-started
-- | This signal gets emitted when a cell starts to be edited.
-- The intended use of this signal is to do special setup
-- on /@editable@/, e.g. adding a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion' or setting
-- up additional columns in a t'GI.Gtk.Objects.ComboBox.ComboBox'.
-- 
-- See 'GI.Gtk.Interfaces.CellEditable.cellEditableStartEditing' for information on the lifecycle of
-- the /@editable@/ and a way to do setup that doesn’t depend on the /@renderer@/.
-- 
-- Note that GTK+ doesn\'t guarantee that cell renderers will
-- continue to use the same kind of widget for editing in future
-- releases, therefore you should check the type of /@editable@/
-- before doing any specific setup, as in the following example:
-- 
-- === /C code/
-- >
-- >static void
-- >text_editing_started (GtkCellRenderer *cell,
-- >                      GtkCellEditable *editable,
-- >                      const gchar     *path,
-- >                      gpointer         data)
-- >{
-- >  if (GTK_IS_ENTRY (editable))
-- >    {
-- >      GtkEntry *entry = GTK_ENTRY (editable);
-- >      
-- >      // ... create a GtkEntryCompletion
-- >      
-- >      gtk_entry_set_completion (entry, completion);
-- >    }
-- >}
-- 
-- 
-- /Since: 2.6/
type CellRendererEditingStartedCallback =
    Gtk.CellEditable.CellEditable
    -- ^ /@editable@/: the t'GI.Gtk.Interfaces.CellEditable.CellEditable'
    -> T.Text
    -- ^ /@path@/: the path identifying the edited cell
    -> IO ()

type C_CellRendererEditingStartedCallback =
    Ptr CellRenderer ->                     -- object
    Ptr Gtk.CellEditable.CellEditable ->
    CString ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CellRendererEditingStartedCallback`.
foreign import ccall "wrapper"
    mk_CellRendererEditingStartedCallback :: C_CellRendererEditingStartedCallback -> IO (FunPtr C_CellRendererEditingStartedCallback)

wrap_CellRendererEditingStartedCallback :: 
    GObject a => (a -> CellRendererEditingStartedCallback) ->
    C_CellRendererEditingStartedCallback
wrap_CellRendererEditingStartedCallback gi'cb gi'selfPtr editable path _ = do
    editable' <- (newObject Gtk.CellEditable.CellEditable) editable
    path' <- cstringToText path
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  editable' path'


-- | Connect a signal handler for the [editingStarted](#signal:editingStarted) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' cellRenderer #editingStarted callback
-- @
-- 
-- 
onCellRendererEditingStarted :: (IsCellRenderer a, MonadIO m) => a -> ((?self :: a) => CellRendererEditingStartedCallback) -> m SignalHandlerId
onCellRendererEditingStarted obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellRendererEditingStartedCallback wrapped
    wrapped'' <- mk_CellRendererEditingStartedCallback wrapped'
    connectSignalFunPtr obj "editing-started" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [editingStarted](#signal:editingStarted) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' cellRenderer #editingStarted callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCellRendererEditingStarted :: (IsCellRenderer a, MonadIO m) => a -> ((?self :: a) => CellRendererEditingStartedCallback) -> m SignalHandlerId
afterCellRendererEditingStarted obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CellRendererEditingStartedCallback wrapped
    wrapped'' <- mk_CellRendererEditingStartedCallback wrapped'
    connectSignalFunPtr obj "editing-started" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CellRendererEditingStartedSignalInfo
instance SignalInfo CellRendererEditingStartedSignalInfo where
    type HaskellCallbackType CellRendererEditingStartedSignalInfo = CellRendererEditingStartedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CellRendererEditingStartedCallback cb
        cb'' <- mk_CellRendererEditingStartedCallback cb'
        connectSignalFunPtr obj "editing-started" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer::editing-started"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:signal:editingStarted"})

#endif

-- VVV Prop "cell-background"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Set the value of the “@cell-background@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRenderer [ #cellBackground 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererCellBackground :: (MonadIO m, IsCellRenderer o) => o -> T.Text -> m ()
setCellRendererCellBackground obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyString obj "cell-background" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@cell-background@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererCellBackground :: (IsCellRenderer o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructCellRendererCellBackground val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "cell-background" (P.Just val)

-- | Set the value of the “@cell-background@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #cellBackground
-- @
clearCellRendererCellBackground :: (MonadIO m, IsCellRenderer o) => o -> m ()
clearCellRendererCellBackground obj = liftIO $ B.Properties.setObjectPropertyString obj "cell-background" (Nothing :: Maybe T.Text)

#if defined(ENABLE_OVERLOADING)
data CellRendererCellBackgroundPropertyInfo
instance AttrInfo CellRendererCellBackgroundPropertyInfo where
    type AttrAllowedOps CellRendererCellBackgroundPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererCellBackgroundPropertyInfo = IsCellRenderer
    type AttrSetTypeConstraint CellRendererCellBackgroundPropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint CellRendererCellBackgroundPropertyInfo = (~) T.Text
    type AttrTransferType CellRendererCellBackgroundPropertyInfo = T.Text
    type AttrGetType CellRendererCellBackgroundPropertyInfo = ()
    type AttrLabel CellRendererCellBackgroundPropertyInfo = "cell-background"
    type AttrOrigin CellRendererCellBackgroundPropertyInfo = CellRenderer
    attrGet = undefined
    attrSet = setCellRendererCellBackground
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererCellBackground
    attrClear = clearCellRendererCellBackground
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellBackground"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:attr:cellBackground"
        })
#endif

-- VVV Prop "cell-background-gdk"
   -- Type: TInterface (Name {namespace = "Gdk", name = "Color"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@cell-background-gdk@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRenderer #cellBackgroundGdk
-- @
getCellRendererCellBackgroundGdk :: (MonadIO m, IsCellRenderer o) => o -> m (Maybe Gdk.Color.Color)
getCellRendererCellBackgroundGdk obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "cell-background-gdk" Gdk.Color.Color

-- | Set the value of the “@cell-background-gdk@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRenderer [ #cellBackgroundGdk 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererCellBackgroundGdk :: (MonadIO m, IsCellRenderer o) => o -> Gdk.Color.Color -> m ()
setCellRendererCellBackgroundGdk obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "cell-background-gdk" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@cell-background-gdk@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererCellBackgroundGdk :: (IsCellRenderer o, MIO.MonadIO m) => Gdk.Color.Color -> m (GValueConstruct o)
constructCellRendererCellBackgroundGdk val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "cell-background-gdk" (P.Just val)

-- | Set the value of the “@cell-background-gdk@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #cellBackgroundGdk
-- @
clearCellRendererCellBackgroundGdk :: (MonadIO m, IsCellRenderer o) => o -> m ()
clearCellRendererCellBackgroundGdk obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "cell-background-gdk" (Nothing :: Maybe Gdk.Color.Color)

#if defined(ENABLE_OVERLOADING)
data CellRendererCellBackgroundGdkPropertyInfo
instance AttrInfo CellRendererCellBackgroundGdkPropertyInfo where
    type AttrAllowedOps CellRendererCellBackgroundGdkPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererCellBackgroundGdkPropertyInfo = IsCellRenderer
    type AttrSetTypeConstraint CellRendererCellBackgroundGdkPropertyInfo = (~) Gdk.Color.Color
    type AttrTransferTypeConstraint CellRendererCellBackgroundGdkPropertyInfo = (~) Gdk.Color.Color
    type AttrTransferType CellRendererCellBackgroundGdkPropertyInfo = Gdk.Color.Color
    type AttrGetType CellRendererCellBackgroundGdkPropertyInfo = (Maybe Gdk.Color.Color)
    type AttrLabel CellRendererCellBackgroundGdkPropertyInfo = "cell-background-gdk"
    type AttrOrigin CellRendererCellBackgroundGdkPropertyInfo = CellRenderer
    attrGet = getCellRendererCellBackgroundGdk
    attrSet = setCellRendererCellBackgroundGdk
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererCellBackgroundGdk
    attrClear = clearCellRendererCellBackgroundGdk
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellBackgroundGdk"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:attr:cellBackgroundGdk"
        })
#endif

-- VVV Prop "cell-background-rgba"
   -- Type: TInterface (Name {namespace = "Gdk", name = "RGBA"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@cell-background-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRenderer #cellBackgroundRgba
-- @
getCellRendererCellBackgroundRgba :: (MonadIO m, IsCellRenderer o) => o -> m (Maybe Gdk.RGBA.RGBA)
getCellRendererCellBackgroundRgba obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "cell-background-rgba" Gdk.RGBA.RGBA

-- | Set the value of the “@cell-background-rgba@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRenderer [ #cellBackgroundRgba 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererCellBackgroundRgba :: (MonadIO m, IsCellRenderer o) => o -> Gdk.RGBA.RGBA -> m ()
setCellRendererCellBackgroundRgba obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBoxed obj "cell-background-rgba" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@cell-background-rgba@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererCellBackgroundRgba :: (IsCellRenderer o, MIO.MonadIO m) => Gdk.RGBA.RGBA -> m (GValueConstruct o)
constructCellRendererCellBackgroundRgba val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "cell-background-rgba" (P.Just val)

-- | Set the value of the “@cell-background-rgba@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #cellBackgroundRgba
-- @
clearCellRendererCellBackgroundRgba :: (MonadIO m, IsCellRenderer o) => o -> m ()
clearCellRendererCellBackgroundRgba obj = liftIO $ B.Properties.setObjectPropertyBoxed obj "cell-background-rgba" (Nothing :: Maybe Gdk.RGBA.RGBA)

#if defined(ENABLE_OVERLOADING)
data CellRendererCellBackgroundRgbaPropertyInfo
instance AttrInfo CellRendererCellBackgroundRgbaPropertyInfo where
    type AttrAllowedOps CellRendererCellBackgroundRgbaPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint CellRendererCellBackgroundRgbaPropertyInfo = IsCellRenderer
    type AttrSetTypeConstraint CellRendererCellBackgroundRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferTypeConstraint CellRendererCellBackgroundRgbaPropertyInfo = (~) Gdk.RGBA.RGBA
    type AttrTransferType CellRendererCellBackgroundRgbaPropertyInfo = Gdk.RGBA.RGBA
    type AttrGetType CellRendererCellBackgroundRgbaPropertyInfo = (Maybe Gdk.RGBA.RGBA)
    type AttrLabel CellRendererCellBackgroundRgbaPropertyInfo = "cell-background-rgba"
    type AttrOrigin CellRendererCellBackgroundRgbaPropertyInfo = CellRenderer
    attrGet = getCellRendererCellBackgroundRgba
    attrSet = setCellRendererCellBackgroundRgba
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererCellBackgroundRgba
    attrClear = clearCellRendererCellBackgroundRgba
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellBackgroundRgba"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:attr:cellBackgroundRgba"
        })
#endif

-- VVV Prop "cell-background-set"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@cell-background-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRenderer #cellBackgroundSet
-- @
getCellRendererCellBackgroundSet :: (MonadIO m, IsCellRenderer o) => o -> m Bool
getCellRendererCellBackgroundSet obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "cell-background-set"

-- | Set the value of the “@cell-background-set@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRenderer [ #cellBackgroundSet 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererCellBackgroundSet :: (MonadIO m, IsCellRenderer o) => o -> Bool -> m ()
setCellRendererCellBackgroundSet obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "cell-background-set" val

-- | Construct a `GValueConstruct` with valid value for the “@cell-background-set@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererCellBackgroundSet :: (IsCellRenderer o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererCellBackgroundSet val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "cell-background-set" val

#if defined(ENABLE_OVERLOADING)
data CellRendererCellBackgroundSetPropertyInfo
instance AttrInfo CellRendererCellBackgroundSetPropertyInfo where
    type AttrAllowedOps CellRendererCellBackgroundSetPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererCellBackgroundSetPropertyInfo = IsCellRenderer
    type AttrSetTypeConstraint CellRendererCellBackgroundSetPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererCellBackgroundSetPropertyInfo = (~) Bool
    type AttrTransferType CellRendererCellBackgroundSetPropertyInfo = Bool
    type AttrGetType CellRendererCellBackgroundSetPropertyInfo = Bool
    type AttrLabel CellRendererCellBackgroundSetPropertyInfo = "cell-background-set"
    type AttrOrigin CellRendererCellBackgroundSetPropertyInfo = CellRenderer
    attrGet = getCellRendererCellBackgroundSet
    attrSet = setCellRendererCellBackgroundSet
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererCellBackgroundSet
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellBackgroundSet"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:attr:cellBackgroundSet"
        })
#endif

-- VVV Prop "editing"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@editing@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRenderer #editing
-- @
getCellRendererEditing :: (MonadIO m, IsCellRenderer o) => o -> m Bool
getCellRendererEditing obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "editing"

#if defined(ENABLE_OVERLOADING)
data CellRendererEditingPropertyInfo
instance AttrInfo CellRendererEditingPropertyInfo where
    type AttrAllowedOps CellRendererEditingPropertyInfo = '[ 'AttrGet]
    type AttrBaseTypeConstraint CellRendererEditingPropertyInfo = IsCellRenderer
    type AttrSetTypeConstraint CellRendererEditingPropertyInfo = (~) ()
    type AttrTransferTypeConstraint CellRendererEditingPropertyInfo = (~) ()
    type AttrTransferType CellRendererEditingPropertyInfo = ()
    type AttrGetType CellRendererEditingPropertyInfo = Bool
    type AttrLabel CellRendererEditingPropertyInfo = "editing"
    type AttrOrigin CellRendererEditingPropertyInfo = CellRenderer
    attrGet = getCellRendererEditing
    attrSet = undefined
    attrTransfer _ = undefined
    attrConstruct = undefined
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.editing"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:attr:editing"
        })
#endif

-- VVV Prop "height"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@height@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRenderer #height
-- @
getCellRendererHeight :: (MonadIO m, IsCellRenderer o) => o -> m Int32
getCellRendererHeight obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "height"

-- | Set the value of the “@height@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRenderer [ #height 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererHeight :: (MonadIO m, IsCellRenderer o) => o -> Int32 -> m ()
setCellRendererHeight obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "height" val

-- | Construct a `GValueConstruct` with valid value for the “@height@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererHeight :: (IsCellRenderer o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructCellRendererHeight val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "height" val

#if defined(ENABLE_OVERLOADING)
data CellRendererHeightPropertyInfo
instance AttrInfo CellRendererHeightPropertyInfo where
    type AttrAllowedOps CellRendererHeightPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererHeightPropertyInfo = IsCellRenderer
    type AttrSetTypeConstraint CellRendererHeightPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint CellRendererHeightPropertyInfo = (~) Int32
    type AttrTransferType CellRendererHeightPropertyInfo = Int32
    type AttrGetType CellRendererHeightPropertyInfo = Int32
    type AttrLabel CellRendererHeightPropertyInfo = "height"
    type AttrOrigin CellRendererHeightPropertyInfo = CellRenderer
    attrGet = getCellRendererHeight
    attrSet = setCellRendererHeight
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererHeight
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.height"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:attr:height"
        })
#endif

-- VVV Prop "is-expanded"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@is-expanded@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRenderer #isExpanded
-- @
getCellRendererIsExpanded :: (MonadIO m, IsCellRenderer o) => o -> m Bool
getCellRendererIsExpanded obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "is-expanded"

-- | Set the value of the “@is-expanded@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRenderer [ #isExpanded 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererIsExpanded :: (MonadIO m, IsCellRenderer o) => o -> Bool -> m ()
setCellRendererIsExpanded obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "is-expanded" val

-- | Construct a `GValueConstruct` with valid value for the “@is-expanded@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererIsExpanded :: (IsCellRenderer o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererIsExpanded val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "is-expanded" val

#if defined(ENABLE_OVERLOADING)
data CellRendererIsExpandedPropertyInfo
instance AttrInfo CellRendererIsExpandedPropertyInfo where
    type AttrAllowedOps CellRendererIsExpandedPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererIsExpandedPropertyInfo = IsCellRenderer
    type AttrSetTypeConstraint CellRendererIsExpandedPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererIsExpandedPropertyInfo = (~) Bool
    type AttrTransferType CellRendererIsExpandedPropertyInfo = Bool
    type AttrGetType CellRendererIsExpandedPropertyInfo = Bool
    type AttrLabel CellRendererIsExpandedPropertyInfo = "is-expanded"
    type AttrOrigin CellRendererIsExpandedPropertyInfo = CellRenderer
    attrGet = getCellRendererIsExpanded
    attrSet = setCellRendererIsExpanded
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererIsExpanded
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.isExpanded"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:attr:isExpanded"
        })
#endif

-- VVV Prop "is-expander"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@is-expander@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRenderer #isExpander
-- @
getCellRendererIsExpander :: (MonadIO m, IsCellRenderer o) => o -> m Bool
getCellRendererIsExpander obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "is-expander"

-- | Set the value of the “@is-expander@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRenderer [ #isExpander 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererIsExpander :: (MonadIO m, IsCellRenderer o) => o -> Bool -> m ()
setCellRendererIsExpander obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "is-expander" val

-- | Construct a `GValueConstruct` with valid value for the “@is-expander@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererIsExpander :: (IsCellRenderer o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererIsExpander val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "is-expander" val

#if defined(ENABLE_OVERLOADING)
data CellRendererIsExpanderPropertyInfo
instance AttrInfo CellRendererIsExpanderPropertyInfo where
    type AttrAllowedOps CellRendererIsExpanderPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererIsExpanderPropertyInfo = IsCellRenderer
    type AttrSetTypeConstraint CellRendererIsExpanderPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererIsExpanderPropertyInfo = (~) Bool
    type AttrTransferType CellRendererIsExpanderPropertyInfo = Bool
    type AttrGetType CellRendererIsExpanderPropertyInfo = Bool
    type AttrLabel CellRendererIsExpanderPropertyInfo = "is-expander"
    type AttrOrigin CellRendererIsExpanderPropertyInfo = CellRenderer
    attrGet = getCellRendererIsExpander
    attrSet = setCellRendererIsExpander
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererIsExpander
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.isExpander"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:attr:isExpander"
        })
#endif

-- VVV Prop "mode"
   -- Type: TInterface (Name {namespace = "Gtk", name = "CellRendererMode"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRenderer #mode
-- @
getCellRendererMode :: (MonadIO m, IsCellRenderer o) => o -> m Gtk.Enums.CellRendererMode
getCellRendererMode obj = MIO.liftIO $ B.Properties.getObjectPropertyEnum obj "mode"

-- | Set the value of the “@mode@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRenderer [ #mode 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererMode :: (MonadIO m, IsCellRenderer o) => o -> Gtk.Enums.CellRendererMode -> m ()
setCellRendererMode obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyEnum obj "mode" val

-- | Construct a `GValueConstruct` with valid value for the “@mode@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererMode :: (IsCellRenderer o, MIO.MonadIO m) => Gtk.Enums.CellRendererMode -> m (GValueConstruct o)
constructCellRendererMode val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyEnum "mode" val

#if defined(ENABLE_OVERLOADING)
data CellRendererModePropertyInfo
instance AttrInfo CellRendererModePropertyInfo where
    type AttrAllowedOps CellRendererModePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererModePropertyInfo = IsCellRenderer
    type AttrSetTypeConstraint CellRendererModePropertyInfo = (~) Gtk.Enums.CellRendererMode
    type AttrTransferTypeConstraint CellRendererModePropertyInfo = (~) Gtk.Enums.CellRendererMode
    type AttrTransferType CellRendererModePropertyInfo = Gtk.Enums.CellRendererMode
    type AttrGetType CellRendererModePropertyInfo = Gtk.Enums.CellRendererMode
    type AttrLabel CellRendererModePropertyInfo = "mode"
    type AttrOrigin CellRendererModePropertyInfo = CellRenderer
    attrGet = getCellRendererMode
    attrSet = setCellRendererMode
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererMode
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.mode"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:attr:mode"
        })
#endif

-- VVV Prop "sensitive"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@sensitive@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRenderer #sensitive
-- @
getCellRendererSensitive :: (MonadIO m, IsCellRenderer o) => o -> m Bool
getCellRendererSensitive obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "sensitive"

-- | Set the value of the “@sensitive@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRenderer [ #sensitive 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererSensitive :: (MonadIO m, IsCellRenderer o) => o -> Bool -> m ()
setCellRendererSensitive obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "sensitive" val

-- | Construct a `GValueConstruct` with valid value for the “@sensitive@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererSensitive :: (IsCellRenderer o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererSensitive val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "sensitive" val

#if defined(ENABLE_OVERLOADING)
data CellRendererSensitivePropertyInfo
instance AttrInfo CellRendererSensitivePropertyInfo where
    type AttrAllowedOps CellRendererSensitivePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererSensitivePropertyInfo = IsCellRenderer
    type AttrSetTypeConstraint CellRendererSensitivePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererSensitivePropertyInfo = (~) Bool
    type AttrTransferType CellRendererSensitivePropertyInfo = Bool
    type AttrGetType CellRendererSensitivePropertyInfo = Bool
    type AttrLabel CellRendererSensitivePropertyInfo = "sensitive"
    type AttrOrigin CellRendererSensitivePropertyInfo = CellRenderer
    attrGet = getCellRendererSensitive
    attrSet = setCellRendererSensitive
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererSensitive
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.sensitive"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:attr:sensitive"
        })
#endif

-- VVV Prop "visible"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRenderer #visible
-- @
getCellRendererVisible :: (MonadIO m, IsCellRenderer o) => o -> m Bool
getCellRendererVisible obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "visible"

-- | Set the value of the “@visible@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRenderer [ #visible 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererVisible :: (MonadIO m, IsCellRenderer o) => o -> Bool -> m ()
setCellRendererVisible obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "visible" val

-- | Construct a `GValueConstruct` with valid value for the “@visible@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererVisible :: (IsCellRenderer o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructCellRendererVisible val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "visible" val

#if defined(ENABLE_OVERLOADING)
data CellRendererVisiblePropertyInfo
instance AttrInfo CellRendererVisiblePropertyInfo where
    type AttrAllowedOps CellRendererVisiblePropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererVisiblePropertyInfo = IsCellRenderer
    type AttrSetTypeConstraint CellRendererVisiblePropertyInfo = (~) Bool
    type AttrTransferTypeConstraint CellRendererVisiblePropertyInfo = (~) Bool
    type AttrTransferType CellRendererVisiblePropertyInfo = Bool
    type AttrGetType CellRendererVisiblePropertyInfo = Bool
    type AttrLabel CellRendererVisiblePropertyInfo = "visible"
    type AttrOrigin CellRendererVisiblePropertyInfo = CellRenderer
    attrGet = getCellRendererVisible
    attrSet = setCellRendererVisible
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererVisible
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.visible"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:attr:visible"
        })
#endif

-- VVV Prop "width"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRenderer #width
-- @
getCellRendererWidth :: (MonadIO m, IsCellRenderer o) => o -> m Int32
getCellRendererWidth obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "width"

-- | Set the value of the “@width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRenderer [ #width 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererWidth :: (MonadIO m, IsCellRenderer o) => o -> Int32 -> m ()
setCellRendererWidth obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "width" val

-- | Construct a `GValueConstruct` with valid value for the “@width@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererWidth :: (IsCellRenderer o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructCellRendererWidth val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "width" val

#if defined(ENABLE_OVERLOADING)
data CellRendererWidthPropertyInfo
instance AttrInfo CellRendererWidthPropertyInfo where
    type AttrAllowedOps CellRendererWidthPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererWidthPropertyInfo = IsCellRenderer
    type AttrSetTypeConstraint CellRendererWidthPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint CellRendererWidthPropertyInfo = (~) Int32
    type AttrTransferType CellRendererWidthPropertyInfo = Int32
    type AttrGetType CellRendererWidthPropertyInfo = Int32
    type AttrLabel CellRendererWidthPropertyInfo = "width"
    type AttrOrigin CellRendererWidthPropertyInfo = CellRenderer
    attrGet = getCellRendererWidth
    attrSet = setCellRendererWidth
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererWidth
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.width"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:attr:width"
        })
#endif

-- VVV Prop "xalign"
   -- Type: TBasicType TFloat
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@xalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRenderer #xalign
-- @
getCellRendererXalign :: (MonadIO m, IsCellRenderer o) => o -> m Float
getCellRendererXalign obj = MIO.liftIO $ B.Properties.getObjectPropertyFloat obj "xalign"

-- | Set the value of the “@xalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRenderer [ #xalign 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererXalign :: (MonadIO m, IsCellRenderer o) => o -> Float -> m ()
setCellRendererXalign obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFloat obj "xalign" val

-- | Construct a `GValueConstruct` with valid value for the “@xalign@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererXalign :: (IsCellRenderer o, MIO.MonadIO m) => Float -> m (GValueConstruct o)
constructCellRendererXalign val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFloat "xalign" val

#if defined(ENABLE_OVERLOADING)
data CellRendererXalignPropertyInfo
instance AttrInfo CellRendererXalignPropertyInfo where
    type AttrAllowedOps CellRendererXalignPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererXalignPropertyInfo = IsCellRenderer
    type AttrSetTypeConstraint CellRendererXalignPropertyInfo = (~) Float
    type AttrTransferTypeConstraint CellRendererXalignPropertyInfo = (~) Float
    type AttrTransferType CellRendererXalignPropertyInfo = Float
    type AttrGetType CellRendererXalignPropertyInfo = Float
    type AttrLabel CellRendererXalignPropertyInfo = "xalign"
    type AttrOrigin CellRendererXalignPropertyInfo = CellRenderer
    attrGet = getCellRendererXalign
    attrSet = setCellRendererXalign
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererXalign
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.xalign"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:attr:xalign"
        })
#endif

-- VVV Prop "xpad"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@xpad@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRenderer #xpad
-- @
getCellRendererXpad :: (MonadIO m, IsCellRenderer o) => o -> m Word32
getCellRendererXpad obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "xpad"

-- | Set the value of the “@xpad@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRenderer [ #xpad 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererXpad :: (MonadIO m, IsCellRenderer o) => o -> Word32 -> m ()
setCellRendererXpad obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "xpad" val

-- | Construct a `GValueConstruct` with valid value for the “@xpad@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererXpad :: (IsCellRenderer o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructCellRendererXpad val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "xpad" val

#if defined(ENABLE_OVERLOADING)
data CellRendererXpadPropertyInfo
instance AttrInfo CellRendererXpadPropertyInfo where
    type AttrAllowedOps CellRendererXpadPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererXpadPropertyInfo = IsCellRenderer
    type AttrSetTypeConstraint CellRendererXpadPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint CellRendererXpadPropertyInfo = (~) Word32
    type AttrTransferType CellRendererXpadPropertyInfo = Word32
    type AttrGetType CellRendererXpadPropertyInfo = Word32
    type AttrLabel CellRendererXpadPropertyInfo = "xpad"
    type AttrOrigin CellRendererXpadPropertyInfo = CellRenderer
    attrGet = getCellRendererXpad
    attrSet = setCellRendererXpad
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererXpad
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.xpad"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:attr:xpad"
        })
#endif

-- VVV Prop "yalign"
   -- Type: TBasicType TFloat
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@yalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRenderer #yalign
-- @
getCellRendererYalign :: (MonadIO m, IsCellRenderer o) => o -> m Float
getCellRendererYalign obj = MIO.liftIO $ B.Properties.getObjectPropertyFloat obj "yalign"

-- | Set the value of the “@yalign@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRenderer [ #yalign 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererYalign :: (MonadIO m, IsCellRenderer o) => o -> Float -> m ()
setCellRendererYalign obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFloat obj "yalign" val

-- | Construct a `GValueConstruct` with valid value for the “@yalign@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererYalign :: (IsCellRenderer o, MIO.MonadIO m) => Float -> m (GValueConstruct o)
constructCellRendererYalign val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFloat "yalign" val

#if defined(ENABLE_OVERLOADING)
data CellRendererYalignPropertyInfo
instance AttrInfo CellRendererYalignPropertyInfo where
    type AttrAllowedOps CellRendererYalignPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererYalignPropertyInfo = IsCellRenderer
    type AttrSetTypeConstraint CellRendererYalignPropertyInfo = (~) Float
    type AttrTransferTypeConstraint CellRendererYalignPropertyInfo = (~) Float
    type AttrTransferType CellRendererYalignPropertyInfo = Float
    type AttrGetType CellRendererYalignPropertyInfo = Float
    type AttrLabel CellRendererYalignPropertyInfo = "yalign"
    type AttrOrigin CellRendererYalignPropertyInfo = CellRenderer
    attrGet = getCellRendererYalign
    attrSet = setCellRendererYalign
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererYalign
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.yalign"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:attr:yalign"
        })
#endif

-- VVV Prop "ypad"
   -- Type: TBasicType TUInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@ypad@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' cellRenderer #ypad
-- @
getCellRendererYpad :: (MonadIO m, IsCellRenderer o) => o -> m Word32
getCellRendererYpad obj = MIO.liftIO $ B.Properties.getObjectPropertyUInt32 obj "ypad"

-- | Set the value of the “@ypad@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' cellRenderer [ #ypad 'Data.GI.Base.Attributes.:=' value ]
-- @
setCellRendererYpad :: (MonadIO m, IsCellRenderer o) => o -> Word32 -> m ()
setCellRendererYpad obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyUInt32 obj "ypad" val

-- | Construct a `GValueConstruct` with valid value for the “@ypad@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructCellRendererYpad :: (IsCellRenderer o, MIO.MonadIO m) => Word32 -> m (GValueConstruct o)
constructCellRendererYpad val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyUInt32 "ypad" val

#if defined(ENABLE_OVERLOADING)
data CellRendererYpadPropertyInfo
instance AttrInfo CellRendererYpadPropertyInfo where
    type AttrAllowedOps CellRendererYpadPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint CellRendererYpadPropertyInfo = IsCellRenderer
    type AttrSetTypeConstraint CellRendererYpadPropertyInfo = (~) Word32
    type AttrTransferTypeConstraint CellRendererYpadPropertyInfo = (~) Word32
    type AttrTransferType CellRendererYpadPropertyInfo = Word32
    type AttrGetType CellRendererYpadPropertyInfo = Word32
    type AttrLabel CellRendererYpadPropertyInfo = "ypad"
    type AttrOrigin CellRendererYpadPropertyInfo = CellRenderer
    attrGet = getCellRendererYpad
    attrSet = setCellRendererYpad
    attrTransfer _ v = do
        return v
    attrConstruct = constructCellRendererYpad
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.ypad"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#g:attr:ypad"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList CellRenderer
type instance O.AttributeList CellRenderer = CellRendererAttributeList
type CellRendererAttributeList = ('[ '("cellBackground", CellRendererCellBackgroundPropertyInfo), '("cellBackgroundGdk", CellRendererCellBackgroundGdkPropertyInfo), '("cellBackgroundRgba", CellRendererCellBackgroundRgbaPropertyInfo), '("cellBackgroundSet", CellRendererCellBackgroundSetPropertyInfo), '("editing", CellRendererEditingPropertyInfo), '("height", CellRendererHeightPropertyInfo), '("isExpanded", CellRendererIsExpandedPropertyInfo), '("isExpander", CellRendererIsExpanderPropertyInfo), '("mode", CellRendererModePropertyInfo), '("sensitive", CellRendererSensitivePropertyInfo), '("visible", CellRendererVisiblePropertyInfo), '("width", CellRendererWidthPropertyInfo), '("xalign", CellRendererXalignPropertyInfo), '("xpad", CellRendererXpadPropertyInfo), '("yalign", CellRendererYalignPropertyInfo), '("ypad", CellRendererYpadPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
cellRendererCellBackground :: AttrLabelProxy "cellBackground"
cellRendererCellBackground = AttrLabelProxy

cellRendererCellBackgroundGdk :: AttrLabelProxy "cellBackgroundGdk"
cellRendererCellBackgroundGdk = AttrLabelProxy

cellRendererCellBackgroundRgba :: AttrLabelProxy "cellBackgroundRgba"
cellRendererCellBackgroundRgba = AttrLabelProxy

cellRendererCellBackgroundSet :: AttrLabelProxy "cellBackgroundSet"
cellRendererCellBackgroundSet = AttrLabelProxy

cellRendererEditing :: AttrLabelProxy "editing"
cellRendererEditing = AttrLabelProxy

cellRendererHeight :: AttrLabelProxy "height"
cellRendererHeight = AttrLabelProxy

cellRendererIsExpanded :: AttrLabelProxy "isExpanded"
cellRendererIsExpanded = AttrLabelProxy

cellRendererIsExpander :: AttrLabelProxy "isExpander"
cellRendererIsExpander = AttrLabelProxy

cellRendererMode :: AttrLabelProxy "mode"
cellRendererMode = AttrLabelProxy

cellRendererSensitive :: AttrLabelProxy "sensitive"
cellRendererSensitive = AttrLabelProxy

cellRendererVisible :: AttrLabelProxy "visible"
cellRendererVisible = AttrLabelProxy

cellRendererWidth :: AttrLabelProxy "width"
cellRendererWidth = AttrLabelProxy

cellRendererXalign :: AttrLabelProxy "xalign"
cellRendererXalign = AttrLabelProxy

cellRendererXpad :: AttrLabelProxy "xpad"
cellRendererXpad = AttrLabelProxy

cellRendererYalign :: AttrLabelProxy "yalign"
cellRendererYalign = AttrLabelProxy

cellRendererYpad :: AttrLabelProxy "ypad"
cellRendererYpad = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList CellRenderer = CellRendererSignalList
type CellRendererSignalList = ('[ '("editingCanceled", CellRendererEditingCanceledSignalInfo), '("editingStarted", CellRendererEditingStartedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method CellRenderer::activate
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "event"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Event" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkEvent" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "widget that received the event"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "widget-dependent string representation of the event location;\n   e.g. for #GtkTreeView, a string representation of #GtkTreePath"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "background_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "background area as passed to gtk_cell_renderer_render()"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "cell area as passed to gtk_cell_renderer_render()"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRendererState" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "render flags" , sinceVersion = Nothing }
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

foreign import ccall "gtk_cell_renderer_activate" gtk_cell_renderer_activate :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Gdk.Event.Event ->                  -- event : TInterface (Name {namespace = "Gdk", name = "Event"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- path : TBasicType TUTF8
    Ptr Gdk.Rectangle.Rectangle ->          -- background_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    Ptr Gdk.Rectangle.Rectangle ->          -- cell_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "CellRendererState"})
    IO CInt

-- | Passes an activate event to the cell renderer for possible processing.
-- Some cell renderers may use events; for example, t'GI.Gtk.Objects.CellRendererToggle.CellRendererToggle'
-- toggles when it gets a mouse click.
cellRendererActivate ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> Gdk.Event.Event
    -- ^ /@event@/: a t'GI.Gdk.Unions.Event.Event'
    -> b
    -- ^ /@widget@/: widget that received the event
    -> T.Text
    -- ^ /@path@/: widget-dependent string representation of the event location;
    --    e.g. for t'GI.Gtk.Objects.TreeView.TreeView', a string representation of t'GI.Gtk.Structs.TreePath.TreePath'
    -> Gdk.Rectangle.Rectangle
    -- ^ /@backgroundArea@/: background area as passed to 'GI.Gtk.Objects.CellRenderer.cellRendererRender'
    -> Gdk.Rectangle.Rectangle
    -- ^ /@cellArea@/: cell area as passed to 'GI.Gtk.Objects.CellRenderer.cellRendererRender'
    -> [Gtk.Flags.CellRendererState]
    -- ^ /@flags@/: render flags
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the event was consumed\/handled
cellRendererActivate cell event widget path backgroundArea cellArea flags = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    event' <- unsafeManagedPtrGetPtr event
    widget' <- unsafeManagedPtrCastPtr widget
    path' <- textToCString path
    backgroundArea' <- unsafeManagedPtrGetPtr backgroundArea
    cellArea' <- unsafeManagedPtrGetPtr cellArea
    let flags' = gflagsToWord flags
    result <- gtk_cell_renderer_activate cell' event' widget' path' backgroundArea' cellArea' flags'
    let result' = (/= 0) result
    touchManagedPtr cell
    touchManagedPtr event
    touchManagedPtr widget
    touchManagedPtr backgroundArea
    touchManagedPtr cellArea
    freeMem path'
    return result'

#if defined(ENABLE_OVERLOADING)
data CellRendererActivateMethodInfo
instance (signature ~ (Gdk.Event.Event -> b -> T.Text -> Gdk.Rectangle.Rectangle -> Gdk.Rectangle.Rectangle -> [Gtk.Flags.CellRendererState] -> m Bool), MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod CellRendererActivateMethodInfo a signature where
    overloadedMethod = cellRendererActivate

instance O.OverloadedMethodInfo CellRendererActivateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererActivate",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererActivate"
        })


#endif

-- method CellRenderer::get_aligned_area
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer instance"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget this cell will be rendering to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRendererState" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "render flags" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "cell area which would be passed to gtk_cell_renderer_render()"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "aligned_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the return location for the space inside @cell_area\n               that would acually be used to render."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_renderer_get_aligned_area" gtk_cell_renderer_get_aligned_area :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "CellRendererState"})
    Ptr Gdk.Rectangle.Rectangle ->          -- cell_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    Ptr Gdk.Rectangle.Rectangle ->          -- aligned_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO ()

-- | Gets the aligned area used by /@cell@/ inside /@cellArea@/. Used for finding
-- the appropriate edit and focus rectangle.
-- 
-- /Since: 3.0/
cellRendererGetAlignedArea ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer' instance
    -> b
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' this cell will be rendering to
    -> [Gtk.Flags.CellRendererState]
    -- ^ /@flags@/: render flags
    -> Gdk.Rectangle.Rectangle
    -- ^ /@cellArea@/: cell area which would be passed to 'GI.Gtk.Objects.CellRenderer.cellRendererRender'
    -> m (Gdk.Rectangle.Rectangle)
cellRendererGetAlignedArea cell widget flags cellArea = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    widget' <- unsafeManagedPtrCastPtr widget
    let flags' = gflagsToWord flags
    cellArea' <- unsafeManagedPtrGetPtr cellArea
    alignedArea <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    gtk_cell_renderer_get_aligned_area cell' widget' flags' cellArea' alignedArea
    alignedArea' <- (wrapBoxed Gdk.Rectangle.Rectangle) alignedArea
    touchManagedPtr cell
    touchManagedPtr widget
    touchManagedPtr cellArea
    return alignedArea'

#if defined(ENABLE_OVERLOADING)
data CellRendererGetAlignedAreaMethodInfo
instance (signature ~ (b -> [Gtk.Flags.CellRendererState] -> Gdk.Rectangle.Rectangle -> m (Gdk.Rectangle.Rectangle)), MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod CellRendererGetAlignedAreaMethodInfo a signature where
    overloadedMethod = cellRendererGetAlignedArea

instance O.OverloadedMethodInfo CellRendererGetAlignedAreaMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererGetAlignedArea",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererGetAlignedArea"
        })


#endif

-- method CellRenderer::get_alignment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xalign"
--           , argType = TBasicType TFloat
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to fill in with the x alignment of the cell, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "yalign"
--           , argType = TBasicType TFloat
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to fill in with the y alignment of the cell, or %NULL"
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

foreign import ccall "gtk_cell_renderer_get_alignment" gtk_cell_renderer_get_alignment :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr CFloat ->                           -- xalign : TBasicType TFloat
    Ptr CFloat ->                           -- yalign : TBasicType TFloat
    IO ()

-- | Fills in /@xalign@/ and /@yalign@/ with the appropriate values of /@cell@/.
-- 
-- /Since: 2.18/
cellRendererGetAlignment ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a) =>
    a
    -- ^ /@cell@/: A t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> m ((Float, Float))
cellRendererGetAlignment cell = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    xalign <- allocMem :: IO (Ptr CFloat)
    yalign <- allocMem :: IO (Ptr CFloat)
    gtk_cell_renderer_get_alignment cell' xalign yalign
    xalign' <- peek xalign
    let xalign'' = realToFrac xalign'
    yalign' <- peek yalign
    let yalign'' = realToFrac yalign'
    touchManagedPtr cell
    freeMem xalign
    freeMem yalign
    return (xalign'', yalign'')

#if defined(ENABLE_OVERLOADING)
data CellRendererGetAlignmentMethodInfo
instance (signature ~ (m ((Float, Float))), MonadIO m, IsCellRenderer a) => O.OverloadedMethod CellRendererGetAlignmentMethodInfo a signature where
    overloadedMethod = cellRendererGetAlignment

instance O.OverloadedMethodInfo CellRendererGetAlignmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererGetAlignment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererGetAlignment"
        })


#endif

-- method CellRenderer::get_fixed_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to fill in with the fixed width of the cell, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to fill in with the fixed height of the cell, or %NULL"
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

foreign import ccall "gtk_cell_renderer_get_fixed_size" gtk_cell_renderer_get_fixed_size :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Int32 ->                            -- width : TBasicType TInt
    Ptr Int32 ->                            -- height : TBasicType TInt
    IO ()

-- | Fills in /@width@/ and /@height@/ with the appropriate size of /@cell@/.
cellRendererGetFixedSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a) =>
    a
    -- ^ /@cell@/: A t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> m ((Int32, Int32))
cellRendererGetFixedSize cell = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    width <- allocMem :: IO (Ptr Int32)
    height <- allocMem :: IO (Ptr Int32)
    gtk_cell_renderer_get_fixed_size cell' width height
    width' <- peek width
    height' <- peek height
    touchManagedPtr cell
    freeMem width
    freeMem height
    return (width', height')

#if defined(ENABLE_OVERLOADING)
data CellRendererGetFixedSizeMethodInfo
instance (signature ~ (m ((Int32, Int32))), MonadIO m, IsCellRenderer a) => O.OverloadedMethod CellRendererGetFixedSizeMethodInfo a signature where
    overloadedMethod = cellRendererGetFixedSize

instance O.OverloadedMethodInfo CellRendererGetFixedSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererGetFixedSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererGetFixedSize"
        })


#endif

-- method CellRenderer::get_padding
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xpad"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to fill in with the x padding of the cell, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "ypad"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to fill in with the y padding of the cell, or %NULL"
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

foreign import ccall "gtk_cell_renderer_get_padding" gtk_cell_renderer_get_padding :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Int32 ->                            -- xpad : TBasicType TInt
    Ptr Int32 ->                            -- ypad : TBasicType TInt
    IO ()

-- | Fills in /@xpad@/ and /@ypad@/ with the appropriate values of /@cell@/.
-- 
-- /Since: 2.18/
cellRendererGetPadding ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a) =>
    a
    -- ^ /@cell@/: A t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> m ((Int32, Int32))
cellRendererGetPadding cell = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    xpad <- allocMem :: IO (Ptr Int32)
    ypad <- allocMem :: IO (Ptr Int32)
    gtk_cell_renderer_get_padding cell' xpad ypad
    xpad' <- peek xpad
    ypad' <- peek ypad
    touchManagedPtr cell
    freeMem xpad
    freeMem ypad
    return (xpad', ypad')

#if defined(ENABLE_OVERLOADING)
data CellRendererGetPaddingMethodInfo
instance (signature ~ (m ((Int32, Int32))), MonadIO m, IsCellRenderer a) => O.OverloadedMethod CellRendererGetPaddingMethodInfo a signature where
    overloadedMethod = cellRendererGetPadding

instance O.OverloadedMethodInfo CellRendererGetPaddingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererGetPadding",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererGetPadding"
        })


#endif

-- method CellRenderer::get_preferred_height
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer instance"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget this cell will be rendering to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minimum_size"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to store the minimum size, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "natural_size"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to store the natural size, or %NULL"
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

foreign import ccall "gtk_cell_renderer_get_preferred_height" gtk_cell_renderer_get_preferred_height :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Int32 ->                            -- minimum_size : TBasicType TInt
    Ptr Int32 ->                            -- natural_size : TBasicType TInt
    IO ()

-- | Retreives a renderer’s natural size when rendered to /@widget@/.
-- 
-- /Since: 3.0/
cellRendererGetPreferredHeight ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer' instance
    -> b
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' this cell will be rendering to
    -> m ((Int32, Int32))
cellRendererGetPreferredHeight cell widget = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    widget' <- unsafeManagedPtrCastPtr widget
    minimumSize <- allocMem :: IO (Ptr Int32)
    naturalSize <- allocMem :: IO (Ptr Int32)
    gtk_cell_renderer_get_preferred_height cell' widget' minimumSize naturalSize
    minimumSize' <- peek minimumSize
    naturalSize' <- peek naturalSize
    touchManagedPtr cell
    touchManagedPtr widget
    freeMem minimumSize
    freeMem naturalSize
    return (minimumSize', naturalSize')

#if defined(ENABLE_OVERLOADING)
data CellRendererGetPreferredHeightMethodInfo
instance (signature ~ (b -> m ((Int32, Int32))), MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod CellRendererGetPreferredHeightMethodInfo a signature where
    overloadedMethod = cellRendererGetPreferredHeight

instance O.OverloadedMethodInfo CellRendererGetPreferredHeightMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererGetPreferredHeight",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererGetPreferredHeight"
        })


#endif

-- method CellRenderer::get_preferred_height_for_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer instance"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget this cell will be rendering to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the size which is available for allocation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minimum_height"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location for storing the minimum size, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "natural_height"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location for storing the preferred size, or %NULL"
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

foreign import ccall "gtk_cell_renderer_get_preferred_height_for_width" gtk_cell_renderer_get_preferred_height_for_width :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- width : TBasicType TInt
    Ptr Int32 ->                            -- minimum_height : TBasicType TInt
    Ptr Int32 ->                            -- natural_height : TBasicType TInt
    IO ()

-- | Retreives a cell renderers’s minimum and natural height if it were rendered to
-- /@widget@/ with the specified /@width@/.
-- 
-- /Since: 3.0/
cellRendererGetPreferredHeightForWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer' instance
    -> b
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' this cell will be rendering to
    -> Int32
    -- ^ /@width@/: the size which is available for allocation
    -> m ((Int32, Int32))
cellRendererGetPreferredHeightForWidth cell widget width = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    widget' <- unsafeManagedPtrCastPtr widget
    minimumHeight <- allocMem :: IO (Ptr Int32)
    naturalHeight <- allocMem :: IO (Ptr Int32)
    gtk_cell_renderer_get_preferred_height_for_width cell' widget' width minimumHeight naturalHeight
    minimumHeight' <- peek minimumHeight
    naturalHeight' <- peek naturalHeight
    touchManagedPtr cell
    touchManagedPtr widget
    freeMem minimumHeight
    freeMem naturalHeight
    return (minimumHeight', naturalHeight')

#if defined(ENABLE_OVERLOADING)
data CellRendererGetPreferredHeightForWidthMethodInfo
instance (signature ~ (b -> Int32 -> m ((Int32, Int32))), MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod CellRendererGetPreferredHeightForWidthMethodInfo a signature where
    overloadedMethod = cellRendererGetPreferredHeightForWidth

instance O.OverloadedMethodInfo CellRendererGetPreferredHeightForWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererGetPreferredHeightForWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererGetPreferredHeightForWidth"
        })


#endif

-- method CellRenderer::get_preferred_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer instance"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget this cell will be rendering to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minimum_size"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Requisition" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location for storing the minimum size, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "natural_size"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Requisition" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location for storing the natural size, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_renderer_get_preferred_size" gtk_cell_renderer_get_preferred_size :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gtk.Requisition.Requisition ->      -- minimum_size : TInterface (Name {namespace = "Gtk", name = "Requisition"})
    Ptr Gtk.Requisition.Requisition ->      -- natural_size : TInterface (Name {namespace = "Gtk", name = "Requisition"})
    IO ()

-- | Retrieves the minimum and natural size of a cell taking
-- into account the widget’s preference for height-for-width management.
-- 
-- /Since: 3.0/
cellRendererGetPreferredSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer' instance
    -> b
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' this cell will be rendering to
    -> m ((Gtk.Requisition.Requisition, Gtk.Requisition.Requisition))
cellRendererGetPreferredSize cell widget = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    widget' <- unsafeManagedPtrCastPtr widget
    minimumSize <- SP.callocBoxedBytes 8 :: IO (Ptr Gtk.Requisition.Requisition)
    naturalSize <- SP.callocBoxedBytes 8 :: IO (Ptr Gtk.Requisition.Requisition)
    gtk_cell_renderer_get_preferred_size cell' widget' minimumSize naturalSize
    minimumSize' <- (wrapBoxed Gtk.Requisition.Requisition) minimumSize
    naturalSize' <- (wrapBoxed Gtk.Requisition.Requisition) naturalSize
    touchManagedPtr cell
    touchManagedPtr widget
    return (minimumSize', naturalSize')

#if defined(ENABLE_OVERLOADING)
data CellRendererGetPreferredSizeMethodInfo
instance (signature ~ (b -> m ((Gtk.Requisition.Requisition, Gtk.Requisition.Requisition))), MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod CellRendererGetPreferredSizeMethodInfo a signature where
    overloadedMethod = cellRendererGetPreferredSize

instance O.OverloadedMethodInfo CellRendererGetPreferredSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererGetPreferredSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererGetPreferredSize"
        })


#endif

-- method CellRenderer::get_preferred_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer instance"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget this cell will be rendering to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minimum_size"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to store the minimum size, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "natural_size"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to store the natural size, or %NULL"
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

foreign import ccall "gtk_cell_renderer_get_preferred_width" gtk_cell_renderer_get_preferred_width :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Int32 ->                            -- minimum_size : TBasicType TInt
    Ptr Int32 ->                            -- natural_size : TBasicType TInt
    IO ()

-- | Retreives a renderer’s natural size when rendered to /@widget@/.
-- 
-- /Since: 3.0/
cellRendererGetPreferredWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer' instance
    -> b
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' this cell will be rendering to
    -> m ((Int32, Int32))
cellRendererGetPreferredWidth cell widget = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    widget' <- unsafeManagedPtrCastPtr widget
    minimumSize <- allocMem :: IO (Ptr Int32)
    naturalSize <- allocMem :: IO (Ptr Int32)
    gtk_cell_renderer_get_preferred_width cell' widget' minimumSize naturalSize
    minimumSize' <- peek minimumSize
    naturalSize' <- peek naturalSize
    touchManagedPtr cell
    touchManagedPtr widget
    freeMem minimumSize
    freeMem naturalSize
    return (minimumSize', naturalSize')

#if defined(ENABLE_OVERLOADING)
data CellRendererGetPreferredWidthMethodInfo
instance (signature ~ (b -> m ((Int32, Int32))), MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod CellRendererGetPreferredWidthMethodInfo a signature where
    overloadedMethod = cellRendererGetPreferredWidth

instance O.OverloadedMethodInfo CellRendererGetPreferredWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererGetPreferredWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererGetPreferredWidth"
        })


#endif

-- method CellRenderer::get_preferred_width_for_height
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer instance"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the #GtkWidget this cell will be rendering to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the size which is available for allocation"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "minimum_width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location for storing the minimum size, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "natural_width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location for storing the preferred size, or %NULL"
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

foreign import ccall "gtk_cell_renderer_get_preferred_width_for_height" gtk_cell_renderer_get_preferred_width_for_height :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Int32 ->                                -- height : TBasicType TInt
    Ptr Int32 ->                            -- minimum_width : TBasicType TInt
    Ptr Int32 ->                            -- natural_width : TBasicType TInt
    IO ()

-- | Retreives a cell renderers’s minimum and natural width if it were rendered to
-- /@widget@/ with the specified /@height@/.
-- 
-- /Since: 3.0/
cellRendererGetPreferredWidthForHeight ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer' instance
    -> b
    -- ^ /@widget@/: the t'GI.Gtk.Objects.Widget.Widget' this cell will be rendering to
    -> Int32
    -- ^ /@height@/: the size which is available for allocation
    -> m ((Int32, Int32))
cellRendererGetPreferredWidthForHeight cell widget height = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    widget' <- unsafeManagedPtrCastPtr widget
    minimumWidth <- allocMem :: IO (Ptr Int32)
    naturalWidth <- allocMem :: IO (Ptr Int32)
    gtk_cell_renderer_get_preferred_width_for_height cell' widget' height minimumWidth naturalWidth
    minimumWidth' <- peek minimumWidth
    naturalWidth' <- peek naturalWidth
    touchManagedPtr cell
    touchManagedPtr widget
    freeMem minimumWidth
    freeMem naturalWidth
    return (minimumWidth', naturalWidth')

#if defined(ENABLE_OVERLOADING)
data CellRendererGetPreferredWidthForHeightMethodInfo
instance (signature ~ (b -> Int32 -> m ((Int32, Int32))), MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod CellRendererGetPreferredWidthForHeightMethodInfo a signature where
    overloadedMethod = cellRendererGetPreferredWidthForHeight

instance O.OverloadedMethodInfo CellRendererGetPreferredWidthForHeightMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererGetPreferredWidthForHeight",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererGetPreferredWidthForHeight"
        })


#endif

-- method CellRenderer::get_request_mode
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer    instance"
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
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "SizeRequestMode" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_renderer_get_request_mode" gtk_cell_renderer_get_request_mode :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO CUInt

-- | Gets whether the cell renderer prefers a height-for-width layout
-- or a width-for-height layout.
-- 
-- /Since: 3.0/
cellRendererGetRequestMode ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a) =>
    a
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer'    instance
    -> m Gtk.Enums.SizeRequestMode
    -- ^ __Returns:__ The t'GI.Gtk.Enums.SizeRequestMode' preferred by this renderer.
cellRendererGetRequestMode cell = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    result <- gtk_cell_renderer_get_request_mode cell'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr cell
    return result'

#if defined(ENABLE_OVERLOADING)
data CellRendererGetRequestModeMethodInfo
instance (signature ~ (m Gtk.Enums.SizeRequestMode), MonadIO m, IsCellRenderer a) => O.OverloadedMethod CellRendererGetRequestModeMethodInfo a signature where
    overloadedMethod = cellRendererGetRequestMode

instance O.OverloadedMethodInfo CellRendererGetRequestModeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererGetRequestMode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererGetRequestMode"
        })


#endif

-- method CellRenderer::get_sensitive
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkCellRenderer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_cell_renderer_get_sensitive" gtk_cell_renderer_get_sensitive :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO CInt

-- | Returns the cell renderer’s sensitivity.
-- 
-- /Since: 2.18/
cellRendererGetSensitive ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a) =>
    a
    -- ^ /@cell@/: A t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the cell renderer is sensitive
cellRendererGetSensitive cell = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    result <- gtk_cell_renderer_get_sensitive cell'
    let result' = (/= 0) result
    touchManagedPtr cell
    return result'

#if defined(ENABLE_OVERLOADING)
data CellRendererGetSensitiveMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsCellRenderer a) => O.OverloadedMethod CellRendererGetSensitiveMethodInfo a signature where
    overloadedMethod = cellRendererGetSensitive

instance O.OverloadedMethodInfo CellRendererGetSensitiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererGetSensitive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererGetSensitive"
        })


#endif

-- method CellRenderer::get_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the widget the renderer is rendering to"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The area a cell will be allocated, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x_offset"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to return x offset of cell relative to @cell_area, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "y_offset"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to return y offset of cell relative to @cell_area, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to return width needed to render a cell, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to return height needed to render a cell, or %NULL"
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

foreign import ccall "gtk_cell_renderer_get_size" gtk_cell_renderer_get_size :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Rectangle.Rectangle ->          -- cell_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    Ptr Int32 ->                            -- x_offset : TBasicType TInt
    Ptr Int32 ->                            -- y_offset : TBasicType TInt
    Ptr Int32 ->                            -- width : TBasicType TInt
    Ptr Int32 ->                            -- height : TBasicType TInt
    IO ()

{-# DEPRECATED cellRendererGetSize ["(Since version 3.0)","Use 'GI.Gtk.Objects.CellRenderer.cellRendererGetPreferredSize' instead."] #-}
-- | Obtains the width and height needed to render the cell. Used by view
-- widgets to determine the appropriate size for the cell_area passed to
-- 'GI.Gtk.Objects.CellRenderer.cellRendererRender'.  If /@cellArea@/ is not 'P.Nothing', fills in the
-- x and y offsets (if set) of the cell relative to this location.
-- 
-- Please note that the values set in /@width@/ and /@height@/, as well as those
-- in /@xOffset@/ and /@yOffset@/ are inclusive of the xpad and ypad properties.
cellRendererGetSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> b
    -- ^ /@widget@/: the widget the renderer is rendering to
    -> Maybe (Gdk.Rectangle.Rectangle)
    -- ^ /@cellArea@/: The area a cell will be allocated, or 'P.Nothing'
    -> m ((Int32, Int32, Int32, Int32))
cellRendererGetSize cell widget cellArea = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    widget' <- unsafeManagedPtrCastPtr widget
    maybeCellArea <- case cellArea of
        Nothing -> return nullPtr
        Just jCellArea -> do
            jCellArea' <- unsafeManagedPtrGetPtr jCellArea
            return jCellArea'
    xOffset <- allocMem :: IO (Ptr Int32)
    yOffset <- allocMem :: IO (Ptr Int32)
    width <- allocMem :: IO (Ptr Int32)
    height <- allocMem :: IO (Ptr Int32)
    gtk_cell_renderer_get_size cell' widget' maybeCellArea xOffset yOffset width height
    xOffset' <- peek xOffset
    yOffset' <- peek yOffset
    width' <- peek width
    height' <- peek height
    touchManagedPtr cell
    touchManagedPtr widget
    whenJust cellArea touchManagedPtr
    freeMem xOffset
    freeMem yOffset
    freeMem width
    freeMem height
    return (xOffset', yOffset', width', height')

#if defined(ENABLE_OVERLOADING)
data CellRendererGetSizeMethodInfo
instance (signature ~ (b -> Maybe (Gdk.Rectangle.Rectangle) -> m ((Int32, Int32, Int32, Int32))), MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod CellRendererGetSizeMethodInfo a signature where
    overloadedMethod = cellRendererGetSize

instance O.OverloadedMethodInfo CellRendererGetSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererGetSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererGetSize"
        })


#endif

-- method CellRenderer::get_state
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer, or %NULL"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "a #GtkWidget, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_state"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRendererState" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "cell renderer state"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "StateFlags" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_renderer_get_state" gtk_cell_renderer_get_state :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CUInt ->                                -- cell_state : TInterface (Name {namespace = "Gtk", name = "CellRendererState"})
    IO CUInt

-- | Translates the cell renderer state to t'GI.Gtk.Flags.StateFlags',
-- based on the cell renderer and widget sensitivity, and
-- the given t'GI.Gtk.Flags.CellRendererState'.
-- 
-- /Since: 3.0/
cellRendererGetState ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer', or 'P.Nothing'
    -> Maybe (b)
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget', or 'P.Nothing'
    -> [Gtk.Flags.CellRendererState]
    -- ^ /@cellState@/: cell renderer state
    -> m [Gtk.Flags.StateFlags]
    -- ^ __Returns:__ the widget state flags applying to /@cell@/
cellRendererGetState cell widget cellState = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    let cellState' = gflagsToWord cellState
    result <- gtk_cell_renderer_get_state cell' maybeWidget cellState'
    let result' = wordToGFlags result
    touchManagedPtr cell
    whenJust widget touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
data CellRendererGetStateMethodInfo
instance (signature ~ (Maybe (b) -> [Gtk.Flags.CellRendererState] -> m [Gtk.Flags.StateFlags]), MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod CellRendererGetStateMethodInfo a signature where
    overloadedMethod = cellRendererGetState

instance O.OverloadedMethodInfo CellRendererGetStateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererGetState",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererGetState"
        })


#endif

-- method CellRenderer::get_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkCellRenderer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_cell_renderer_get_visible" gtk_cell_renderer_get_visible :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO CInt

-- | Returns the cell renderer’s visibility.
-- 
-- /Since: 2.18/
cellRendererGetVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a) =>
    a
    -- ^ /@cell@/: A t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the cell renderer is visible
cellRendererGetVisible cell = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    result <- gtk_cell_renderer_get_visible cell'
    let result' = (/= 0) result
    touchManagedPtr cell
    return result'

#if defined(ENABLE_OVERLOADING)
data CellRendererGetVisibleMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsCellRenderer a) => O.OverloadedMethod CellRendererGetVisibleMethodInfo a signature where
    overloadedMethod = cellRendererGetVisible

instance O.OverloadedMethodInfo CellRendererGetVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererGetVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererGetVisible"
        })


#endif

-- method CellRenderer::is_activatable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkCellRenderer" , sinceVersion = Nothing }
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

foreign import ccall "gtk_cell_renderer_is_activatable" gtk_cell_renderer_is_activatable :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    IO CInt

-- | Checks whether the cell renderer can do something when activated.
-- 
-- /Since: 3.0/
cellRendererIsActivatable ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a) =>
    a
    -- ^ /@cell@/: A t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the cell renderer can do anything when activated
cellRendererIsActivatable cell = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    result <- gtk_cell_renderer_is_activatable cell'
    let result' = (/= 0) result
    touchManagedPtr cell
    return result'

#if defined(ENABLE_OVERLOADING)
data CellRendererIsActivatableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsCellRenderer a) => O.OverloadedMethod CellRendererIsActivatableMethodInfo a signature where
    overloadedMethod = cellRendererIsActivatable

instance O.OverloadedMethodInfo CellRendererIsActivatableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererIsActivatable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererIsActivatable"
        })


#endif

-- method CellRenderer::render
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a cairo context to draw to"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the widget owning @window"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "background_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "entire cell area (including tree expanders and maybe\n   padding on the sides)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "area normally rendered by a cell renderer"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRendererState" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "flags that affect rendering"
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

foreign import ccall "gtk_cell_renderer_render" gtk_cell_renderer_render :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    Ptr Gdk.Rectangle.Rectangle ->          -- background_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    Ptr Gdk.Rectangle.Rectangle ->          -- cell_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "CellRendererState"})
    IO ()

-- | Invokes the virtual render function of the t'GI.Gtk.Objects.CellRenderer.CellRenderer'. The three
-- passed-in rectangles are areas in /@cr@/. Most renderers will draw within
-- /@cellArea@/; the xalign, yalign, xpad, and ypad fields of the t'GI.Gtk.Objects.CellRenderer.CellRenderer'
-- should be honored with respect to /@cellArea@/. /@backgroundArea@/ includes the
-- blank space around the cell, and also the area containing the tree expander;
-- so the /@backgroundArea@/ rectangles for all cells tile to cover the entire
-- /@window@/.
cellRendererRender ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> Cairo.Context.Context
    -- ^ /@cr@/: a cairo context to draw to
    -> b
    -- ^ /@widget@/: the widget owning /@window@/
    -> Gdk.Rectangle.Rectangle
    -- ^ /@backgroundArea@/: entire cell area (including tree expanders and maybe
    --    padding on the sides)
    -> Gdk.Rectangle.Rectangle
    -- ^ /@cellArea@/: area normally rendered by a cell renderer
    -> [Gtk.Flags.CellRendererState]
    -- ^ /@flags@/: flags that affect rendering
    -> m ()
cellRendererRender cell cr widget backgroundArea cellArea flags = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    cr' <- unsafeManagedPtrGetPtr cr
    widget' <- unsafeManagedPtrCastPtr widget
    backgroundArea' <- unsafeManagedPtrGetPtr backgroundArea
    cellArea' <- unsafeManagedPtrGetPtr cellArea
    let flags' = gflagsToWord flags
    gtk_cell_renderer_render cell' cr' widget' backgroundArea' cellArea' flags'
    touchManagedPtr cell
    touchManagedPtr cr
    touchManagedPtr widget
    touchManagedPtr backgroundArea
    touchManagedPtr cellArea
    return ()

#if defined(ENABLE_OVERLOADING)
data CellRendererRenderMethodInfo
instance (signature ~ (Cairo.Context.Context -> b -> Gdk.Rectangle.Rectangle -> Gdk.Rectangle.Rectangle -> [Gtk.Flags.CellRendererState] -> m ()), MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod CellRendererRenderMethodInfo a signature where
    overloadedMethod = cellRendererRender

instance O.OverloadedMethodInfo CellRendererRenderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererRender",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererRender"
        })


#endif

-- method CellRenderer::set_alignment
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkCellRenderer" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the x alignment of the cell renderer"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "the y alignment of the cell renderer"
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

foreign import ccall "gtk_cell_renderer_set_alignment" gtk_cell_renderer_set_alignment :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CFloat ->                               -- xalign : TBasicType TFloat
    CFloat ->                               -- yalign : TBasicType TFloat
    IO ()

-- | Sets the renderer’s alignment within its available space.
-- 
-- /Since: 2.18/
cellRendererSetAlignment ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a) =>
    a
    -- ^ /@cell@/: A t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> Float
    -- ^ /@xalign@/: the x alignment of the cell renderer
    -> Float
    -- ^ /@yalign@/: the y alignment of the cell renderer
    -> m ()
cellRendererSetAlignment cell xalign yalign = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    let xalign' = realToFrac xalign
    let yalign' = realToFrac yalign
    gtk_cell_renderer_set_alignment cell' xalign' yalign'
    touchManagedPtr cell
    return ()

#if defined(ENABLE_OVERLOADING)
data CellRendererSetAlignmentMethodInfo
instance (signature ~ (Float -> Float -> m ()), MonadIO m, IsCellRenderer a) => O.OverloadedMethod CellRendererSetAlignmentMethodInfo a signature where
    overloadedMethod = cellRendererSetAlignment

instance O.OverloadedMethodInfo CellRendererSetAlignmentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererSetAlignment",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererSetAlignment"
        })


#endif

-- method CellRenderer::set_fixed_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the width of the cell renderer, or -1"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the height of the cell renderer, or -1"
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

foreign import ccall "gtk_cell_renderer_set_fixed_size" gtk_cell_renderer_set_fixed_size :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

-- | Sets the renderer size to be explicit, independent of the properties set.
cellRendererSetFixedSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a) =>
    a
    -- ^ /@cell@/: A t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> Int32
    -- ^ /@width@/: the width of the cell renderer, or -1
    -> Int32
    -- ^ /@height@/: the height of the cell renderer, or -1
    -> m ()
cellRendererSetFixedSize cell width height = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    gtk_cell_renderer_set_fixed_size cell' width height
    touchManagedPtr cell
    return ()

#if defined(ENABLE_OVERLOADING)
data CellRendererSetFixedSizeMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsCellRenderer a) => O.OverloadedMethod CellRendererSetFixedSizeMethodInfo a signature where
    overloadedMethod = cellRendererSetFixedSize

instance O.OverloadedMethodInfo CellRendererSetFixedSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererSetFixedSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererSetFixedSize"
        })


#endif

-- method CellRenderer::set_padding
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "xpad"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the x padding of the cell renderer"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "ypad"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the y padding of the cell renderer"
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

foreign import ccall "gtk_cell_renderer_set_padding" gtk_cell_renderer_set_padding :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Int32 ->                                -- xpad : TBasicType TInt
    Int32 ->                                -- ypad : TBasicType TInt
    IO ()

-- | Sets the renderer’s padding.
-- 
-- /Since: 2.18/
cellRendererSetPadding ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a) =>
    a
    -- ^ /@cell@/: A t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> Int32
    -- ^ /@xpad@/: the x padding of the cell renderer
    -> Int32
    -- ^ /@ypad@/: the y padding of the cell renderer
    -> m ()
cellRendererSetPadding cell xpad ypad = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    gtk_cell_renderer_set_padding cell' xpad ypad
    touchManagedPtr cell
    return ()

#if defined(ENABLE_OVERLOADING)
data CellRendererSetPaddingMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsCellRenderer a) => O.OverloadedMethod CellRendererSetPaddingMethodInfo a signature where
    overloadedMethod = cellRendererSetPadding

instance O.OverloadedMethodInfo CellRendererSetPaddingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererSetPadding",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererSetPadding"
        })


#endif

-- method CellRenderer::set_sensitive
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sensitive"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the sensitivity of the cell"
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

foreign import ccall "gtk_cell_renderer_set_sensitive" gtk_cell_renderer_set_sensitive :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CInt ->                                 -- sensitive : TBasicType TBoolean
    IO ()

-- | Sets the cell renderer’s sensitivity.
-- 
-- /Since: 2.18/
cellRendererSetSensitive ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a) =>
    a
    -- ^ /@cell@/: A t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> Bool
    -- ^ /@sensitive@/: the sensitivity of the cell
    -> m ()
cellRendererSetSensitive cell sensitive = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    let sensitive' = (fromIntegral . fromEnum) sensitive
    gtk_cell_renderer_set_sensitive cell' sensitive'
    touchManagedPtr cell
    return ()

#if defined(ENABLE_OVERLOADING)
data CellRendererSetSensitiveMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsCellRenderer a) => O.OverloadedMethod CellRendererSetSensitiveMethodInfo a signature where
    overloadedMethod = cellRendererSetSensitive

instance O.OverloadedMethodInfo CellRendererSetSensitiveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererSetSensitive",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererSetSensitive"
        })


#endif

-- method CellRenderer::set_visible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "visible"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the visibility of the cell"
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

foreign import ccall "gtk_cell_renderer_set_visible" gtk_cell_renderer_set_visible :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CInt ->                                 -- visible : TBasicType TBoolean
    IO ()

-- | Sets the cell renderer’s visibility.
-- 
-- /Since: 2.18/
cellRendererSetVisible ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a) =>
    a
    -- ^ /@cell@/: A t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> Bool
    -- ^ /@visible@/: the visibility of the cell
    -> m ()
cellRendererSetVisible cell visible = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    let visible' = (fromIntegral . fromEnum) visible
    gtk_cell_renderer_set_visible cell' visible'
    touchManagedPtr cell
    return ()

#if defined(ENABLE_OVERLOADING)
data CellRendererSetVisibleMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsCellRenderer a) => O.OverloadedMethod CellRendererSetVisibleMethodInfo a signature where
    overloadedMethod = cellRendererSetVisible

instance O.OverloadedMethodInfo CellRendererSetVisibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererSetVisible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererSetVisible"
        })


#endif

-- method CellRenderer::start_editing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "event"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Event" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkEvent" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "widget that received the event"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "widget-dependent string representation of the event location;\n   e.g. for #GtkTreeView, a string representation of #GtkTreePath"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "background_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "background area as passed to gtk_cell_renderer_render()"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cell_area"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "cell area as passed to gtk_cell_renderer_render()"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRendererState" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "render flags" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "CellEditable" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_cell_renderer_start_editing" gtk_cell_renderer_start_editing :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    Ptr Gdk.Event.Event ->                  -- event : TInterface (Name {namespace = "Gdk", name = "Event"})
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- path : TBasicType TUTF8
    Ptr Gdk.Rectangle.Rectangle ->          -- background_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    Ptr Gdk.Rectangle.Rectangle ->          -- cell_area : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "CellRendererState"})
    IO (Ptr Gtk.CellEditable.CellEditable)

-- | Starts editing the contents of this /@cell@/, through a new t'GI.Gtk.Interfaces.CellEditable.CellEditable'
-- widget created by the t'GI.Gtk.Structs.CellRendererClass.CellRendererClass'.@/start_editing/@ virtual function.
cellRendererStartEditing ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@cell@/: a t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> Maybe (Gdk.Event.Event)
    -- ^ /@event@/: a t'GI.Gdk.Unions.Event.Event'
    -> b
    -- ^ /@widget@/: widget that received the event
    -> T.Text
    -- ^ /@path@/: widget-dependent string representation of the event location;
    --    e.g. for t'GI.Gtk.Objects.TreeView.TreeView', a string representation of t'GI.Gtk.Structs.TreePath.TreePath'
    -> Gdk.Rectangle.Rectangle
    -- ^ /@backgroundArea@/: background area as passed to 'GI.Gtk.Objects.CellRenderer.cellRendererRender'
    -> Gdk.Rectangle.Rectangle
    -- ^ /@cellArea@/: cell area as passed to 'GI.Gtk.Objects.CellRenderer.cellRendererRender'
    -> [Gtk.Flags.CellRendererState]
    -- ^ /@flags@/: render flags
    -> m (Maybe Gtk.CellEditable.CellEditable)
    -- ^ __Returns:__ A new t'GI.Gtk.Interfaces.CellEditable.CellEditable' for editing this
    --   /@cell@/, or 'P.Nothing' if editing is not possible
cellRendererStartEditing cell event widget path backgroundArea cellArea flags = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    maybeEvent <- case event of
        Nothing -> return nullPtr
        Just jEvent -> do
            jEvent' <- unsafeManagedPtrGetPtr jEvent
            return jEvent'
    widget' <- unsafeManagedPtrCastPtr widget
    path' <- textToCString path
    backgroundArea' <- unsafeManagedPtrGetPtr backgroundArea
    cellArea' <- unsafeManagedPtrGetPtr cellArea
    let flags' = gflagsToWord flags
    result <- gtk_cell_renderer_start_editing cell' maybeEvent widget' path' backgroundArea' cellArea' flags'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.CellEditable.CellEditable) result'
        return result''
    touchManagedPtr cell
    whenJust event touchManagedPtr
    touchManagedPtr widget
    touchManagedPtr backgroundArea
    touchManagedPtr cellArea
    freeMem path'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data CellRendererStartEditingMethodInfo
instance (signature ~ (Maybe (Gdk.Event.Event) -> b -> T.Text -> Gdk.Rectangle.Rectangle -> Gdk.Rectangle.Rectangle -> [Gtk.Flags.CellRendererState] -> m (Maybe Gtk.CellEditable.CellEditable)), MonadIO m, IsCellRenderer a, Gtk.Widget.IsWidget b) => O.OverloadedMethod CellRendererStartEditingMethodInfo a signature where
    overloadedMethod = cellRendererStartEditing

instance O.OverloadedMethodInfo CellRendererStartEditingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererStartEditing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererStartEditing"
        })


#endif

-- method CellRenderer::stop_editing
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "cell"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellRenderer" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkCellRenderer" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "canceled"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE if the editing has been canceled"
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

foreign import ccall "gtk_cell_renderer_stop_editing" gtk_cell_renderer_stop_editing :: 
    Ptr CellRenderer ->                     -- cell : TInterface (Name {namespace = "Gtk", name = "CellRenderer"})
    CInt ->                                 -- canceled : TBasicType TBoolean
    IO ()

-- | Informs the cell renderer that the editing is stopped.
-- If /@canceled@/ is 'P.True', the cell renderer will emit the
-- [CellRenderer::editingCanceled]("GI.Gtk.Objects.CellRenderer#g:signal:editingCanceled") signal.
-- 
-- This function should be called by cell renderer implementations
-- in response to the [CellEditable::editingDone]("GI.Gtk.Interfaces.CellEditable#g:signal:editingDone") signal of
-- t'GI.Gtk.Interfaces.CellEditable.CellEditable'.
-- 
-- /Since: 2.6/
cellRendererStopEditing ::
    (B.CallStack.HasCallStack, MonadIO m, IsCellRenderer a) =>
    a
    -- ^ /@cell@/: A t'GI.Gtk.Objects.CellRenderer.CellRenderer'
    -> Bool
    -- ^ /@canceled@/: 'P.True' if the editing has been canceled
    -> m ()
cellRendererStopEditing cell canceled = liftIO $ do
    cell' <- unsafeManagedPtrCastPtr cell
    let canceled' = (fromIntegral . fromEnum) canceled
    gtk_cell_renderer_stop_editing cell' canceled'
    touchManagedPtr cell
    return ()

#if defined(ENABLE_OVERLOADING)
data CellRendererStopEditingMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsCellRenderer a) => O.OverloadedMethod CellRendererStopEditingMethodInfo a signature where
    overloadedMethod = cellRendererStopEditing

instance O.OverloadedMethodInfo CellRendererStopEditingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CellRenderer.cellRendererStopEditing",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CellRenderer.html#v:cellRendererStopEditing"
        })


#endif


