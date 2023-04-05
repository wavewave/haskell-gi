{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.EntryCompletion.EntryCompletion' is an auxiliary object to be used in conjunction with
-- t'GI.Gtk.Objects.Entry.Entry' to provide the completion functionality. It implements the
-- t'GI.Gtk.Interfaces.CellLayout.CellLayout' interface, to allow the user to add extra cells to the
-- t'GI.Gtk.Objects.TreeView.TreeView' with completion matches.
-- 
-- “Completion functionality” means that when the user modifies the text
-- in the entry, t'GI.Gtk.Objects.EntryCompletion.EntryCompletion' checks which rows in the model match
-- the current content of the entry, and displays a list of matches.
-- By default, the matching is done by comparing the entry text
-- case-insensitively against the text column of the model (see
-- 'GI.Gtk.Objects.EntryCompletion.entryCompletionSetTextColumn'), but this can be overridden
-- with a custom match function (see 'GI.Gtk.Objects.EntryCompletion.entryCompletionSetMatchFunc').
-- 
-- When the user selects a completion, the content of the entry is
-- updated. By default, the content of the entry is replaced by the
-- text column of the model, but this can be overridden by connecting
-- to the [EntryCompletion::matchSelected]("GI.Gtk.Objects.EntryCompletion#g:signal:matchSelected") signal and updating the
-- entry in the signal handler. Note that you should return 'P.True' from
-- the signal handler to suppress the default behaviour.
-- 
-- To add completion functionality to an entry, use 'GI.Gtk.Objects.Entry.entrySetCompletion'.
-- 
-- In addition to regular completion matches, which will be inserted into the
-- entry when they are selected, t'GI.Gtk.Objects.EntryCompletion.EntryCompletion' also allows to display
-- “actions” in the popup window. Their appearance is similar to menuitems,
-- to differentiate them clearly from completion strings. When an action is
-- selected, the [EntryCompletion::actionActivated]("GI.Gtk.Objects.EntryCompletion#g:signal:actionActivated") signal is emitted.
-- 
-- GtkEntryCompletion uses a t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter' model to represent the
-- subset of the entire model that is currently matching. While the
-- GtkEntryCompletion signals [EntryCompletion::matchSelected]("GI.Gtk.Objects.EntryCompletion#g:signal:matchSelected") and
-- [EntryCompletion::cursorOnMatch]("GI.Gtk.Objects.EntryCompletion#g:signal:cursorOnMatch") take the original model and an
-- iter pointing to that model as arguments, other callbacks and signals
-- (such as @/GtkCellLayoutDataFuncs/@ or [CellArea::applyAttributes]("GI.Gtk.Objects.CellArea#g:signal:applyAttributes"))
-- will generally take the filter model as argument. As long as you are
-- only calling @/gtk_tree_model_get()/@, this will make no difference to
-- you. If for some reason, you need the original model, use
-- 'GI.Gtk.Objects.TreeModelFilter.treeModelFilterGetModel'. Don’t forget to use
-- 'GI.Gtk.Objects.TreeModelFilter.treeModelFilterConvertIterToChildIter' to obtain a
-- matching iter.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.EntryCompletion
    ( 

-- * Exported types
    EntryCompletion(..)                     ,
    IsEntryCompletion                       ,
    toEntryCompletion                       ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addAttribute]("GI.Gtk.Interfaces.CellLayout#g:method:addAttribute"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [clear]("GI.Gtk.Interfaces.CellLayout#g:method:clear"), [clearAttributes]("GI.Gtk.Interfaces.CellLayout#g:method:clearAttributes"), [complete]("GI.Gtk.Objects.EntryCompletion#g:method:complete"), [computePrefix]("GI.Gtk.Objects.EntryCompletion#g:method:computePrefix"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [deleteAction]("GI.Gtk.Objects.EntryCompletion#g:method:deleteAction"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [insertActionMarkup]("GI.Gtk.Objects.EntryCompletion#g:method:insertActionMarkup"), [insertActionText]("GI.Gtk.Objects.EntryCompletion#g:method:insertActionText"), [insertPrefix]("GI.Gtk.Objects.EntryCompletion#g:method:insertPrefix"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [packEnd]("GI.Gtk.Interfaces.CellLayout#g:method:packEnd"), [packStart]("GI.Gtk.Interfaces.CellLayout#g:method:packStart"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reorder]("GI.Gtk.Interfaces.CellLayout#g:method:reorder"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getArea]("GI.Gtk.Interfaces.CellLayout#g:method:getArea"), [getCells]("GI.Gtk.Interfaces.CellLayout#g:method:getCells"), [getCompletionPrefix]("GI.Gtk.Objects.EntryCompletion#g:method:getCompletionPrefix"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getEntry]("GI.Gtk.Objects.EntryCompletion#g:method:getEntry"), [getInlineCompletion]("GI.Gtk.Objects.EntryCompletion#g:method:getInlineCompletion"), [getInlineSelection]("GI.Gtk.Objects.EntryCompletion#g:method:getInlineSelection"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getMinimumKeyLength]("GI.Gtk.Objects.EntryCompletion#g:method:getMinimumKeyLength"), [getModel]("GI.Gtk.Objects.EntryCompletion#g:method:getModel"), [getName]("GI.Gtk.Interfaces.Buildable#g:method:getName"), [getPopupCompletion]("GI.Gtk.Objects.EntryCompletion#g:method:getPopupCompletion"), [getPopupSetWidth]("GI.Gtk.Objects.EntryCompletion#g:method:getPopupSetWidth"), [getPopupSingleMatch]("GI.Gtk.Objects.EntryCompletion#g:method:getPopupSingleMatch"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getTextColumn]("GI.Gtk.Objects.EntryCompletion#g:method:getTextColumn").
-- 
-- ==== Setters
-- [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setCellDataFunc]("GI.Gtk.Interfaces.CellLayout#g:method:setCellDataFunc"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setInlineCompletion]("GI.Gtk.Objects.EntryCompletion#g:method:setInlineCompletion"), [setInlineSelection]("GI.Gtk.Objects.EntryCompletion#g:method:setInlineSelection"), [setMatchFunc]("GI.Gtk.Objects.EntryCompletion#g:method:setMatchFunc"), [setMinimumKeyLength]("GI.Gtk.Objects.EntryCompletion#g:method:setMinimumKeyLength"), [setModel]("GI.Gtk.Objects.EntryCompletion#g:method:setModel"), [setName]("GI.Gtk.Interfaces.Buildable#g:method:setName"), [setPopupCompletion]("GI.Gtk.Objects.EntryCompletion#g:method:setPopupCompletion"), [setPopupSetWidth]("GI.Gtk.Objects.EntryCompletion#g:method:setPopupSetWidth"), [setPopupSingleMatch]("GI.Gtk.Objects.EntryCompletion#g:method:setPopupSingleMatch"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setTextColumn]("GI.Gtk.Objects.EntryCompletion#g:method:setTextColumn").

#if defined(ENABLE_OVERLOADING)
    ResolveEntryCompletionMethod            ,
#endif

-- ** complete #method:complete#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionCompleteMethodInfo       ,
#endif
    entryCompletionComplete                 ,


-- ** computePrefix #method:computePrefix#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionComputePrefixMethodInfo  ,
#endif
    entryCompletionComputePrefix            ,


-- ** deleteAction #method:deleteAction#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionDeleteActionMethodInfo   ,
#endif
    entryCompletionDeleteAction             ,


-- ** getCompletionPrefix #method:getCompletionPrefix#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionGetCompletionPrefixMethodInfo,
#endif
    entryCompletionGetCompletionPrefix      ,


-- ** getEntry #method:getEntry#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionGetEntryMethodInfo       ,
#endif
    entryCompletionGetEntry                 ,


-- ** getInlineCompletion #method:getInlineCompletion#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionGetInlineCompletionMethodInfo,
#endif
    entryCompletionGetInlineCompletion      ,


-- ** getInlineSelection #method:getInlineSelection#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionGetInlineSelectionMethodInfo,
#endif
    entryCompletionGetInlineSelection       ,


-- ** getMinimumKeyLength #method:getMinimumKeyLength#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionGetMinimumKeyLengthMethodInfo,
#endif
    entryCompletionGetMinimumKeyLength      ,


-- ** getModel #method:getModel#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionGetModelMethodInfo       ,
#endif
    entryCompletionGetModel                 ,


-- ** getPopupCompletion #method:getPopupCompletion#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionGetPopupCompletionMethodInfo,
#endif
    entryCompletionGetPopupCompletion       ,


-- ** getPopupSetWidth #method:getPopupSetWidth#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionGetPopupSetWidthMethodInfo,
#endif
    entryCompletionGetPopupSetWidth         ,


-- ** getPopupSingleMatch #method:getPopupSingleMatch#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionGetPopupSingleMatchMethodInfo,
#endif
    entryCompletionGetPopupSingleMatch      ,


-- ** getTextColumn #method:getTextColumn#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionGetTextColumnMethodInfo  ,
#endif
    entryCompletionGetTextColumn            ,


-- ** insertActionMarkup #method:insertActionMarkup#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionInsertActionMarkupMethodInfo,
#endif
    entryCompletionInsertActionMarkup       ,


-- ** insertActionText #method:insertActionText#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionInsertActionTextMethodInfo,
#endif
    entryCompletionInsertActionText         ,


-- ** insertPrefix #method:insertPrefix#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionInsertPrefixMethodInfo   ,
#endif
    entryCompletionInsertPrefix             ,


-- ** new #method:new#

    entryCompletionNew                      ,


-- ** newWithArea #method:newWithArea#

    entryCompletionNewWithArea              ,


-- ** setInlineCompletion #method:setInlineCompletion#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionSetInlineCompletionMethodInfo,
#endif
    entryCompletionSetInlineCompletion      ,


-- ** setInlineSelection #method:setInlineSelection#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionSetInlineSelectionMethodInfo,
#endif
    entryCompletionSetInlineSelection       ,


-- ** setMatchFunc #method:setMatchFunc#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionSetMatchFuncMethodInfo   ,
#endif
    entryCompletionSetMatchFunc             ,


-- ** setMinimumKeyLength #method:setMinimumKeyLength#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionSetMinimumKeyLengthMethodInfo,
#endif
    entryCompletionSetMinimumKeyLength      ,


-- ** setModel #method:setModel#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionSetModelMethodInfo       ,
#endif
    entryCompletionSetModel                 ,


-- ** setPopupCompletion #method:setPopupCompletion#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionSetPopupCompletionMethodInfo,
#endif
    entryCompletionSetPopupCompletion       ,


-- ** setPopupSetWidth #method:setPopupSetWidth#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionSetPopupSetWidthMethodInfo,
#endif
    entryCompletionSetPopupSetWidth         ,


-- ** setPopupSingleMatch #method:setPopupSingleMatch#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionSetPopupSingleMatchMethodInfo,
#endif
    entryCompletionSetPopupSingleMatch      ,


-- ** setTextColumn #method:setTextColumn#

#if defined(ENABLE_OVERLOADING)
    EntryCompletionSetTextColumnMethodInfo  ,
#endif
    entryCompletionSetTextColumn            ,




 -- * Properties


-- ** cellArea #attr:cellArea#
-- | The t'GI.Gtk.Objects.CellArea.CellArea' used to layout cell renderers in the treeview column.
-- 
-- If no area is specified when creating the entry completion with
-- 'GI.Gtk.Objects.EntryCompletion.entryCompletionNewWithArea' a horizontally oriented
-- t'GI.Gtk.Objects.CellAreaBox.CellAreaBox' will be used.
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    EntryCompletionCellAreaPropertyInfo     ,
#endif
    constructEntryCompletionCellArea        ,
#if defined(ENABLE_OVERLOADING)
    entryCompletionCellArea                 ,
#endif
    getEntryCompletionCellArea              ,


-- ** inlineCompletion #attr:inlineCompletion#
-- | Determines whether the common prefix of the possible completions
-- should be inserted automatically in the entry. Note that this
-- requires text-column to be set, even if you are using a custom
-- match function.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    EntryCompletionInlineCompletionPropertyInfo,
#endif
    constructEntryCompletionInlineCompletion,
#if defined(ENABLE_OVERLOADING)
    entryCompletionInlineCompletion         ,
#endif
    getEntryCompletionInlineCompletion      ,
    setEntryCompletionInlineCompletion      ,


-- ** inlineSelection #attr:inlineSelection#
-- | Determines whether the possible completions on the popup
-- will appear in the entry as you navigate through them.
-- 
-- /Since: 2.12/

#if defined(ENABLE_OVERLOADING)
    EntryCompletionInlineSelectionPropertyInfo,
#endif
    constructEntryCompletionInlineSelection ,
#if defined(ENABLE_OVERLOADING)
    entryCompletionInlineSelection          ,
#endif
    getEntryCompletionInlineSelection       ,
    setEntryCompletionInlineSelection       ,


-- ** minimumKeyLength #attr:minimumKeyLength#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    EntryCompletionMinimumKeyLengthPropertyInfo,
#endif
    constructEntryCompletionMinimumKeyLength,
#if defined(ENABLE_OVERLOADING)
    entryCompletionMinimumKeyLength         ,
#endif
    getEntryCompletionMinimumKeyLength      ,
    setEntryCompletionMinimumKeyLength      ,


-- ** model #attr:model#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    EntryCompletionModelPropertyInfo        ,
#endif
    clearEntryCompletionModel               ,
    constructEntryCompletionModel           ,
#if defined(ENABLE_OVERLOADING)
    entryCompletionModel                    ,
#endif
    getEntryCompletionModel                 ,
    setEntryCompletionModel                 ,


-- ** popupCompletion #attr:popupCompletion#
-- | Determines whether the possible completions should be
-- shown in a popup window.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    EntryCompletionPopupCompletionPropertyInfo,
#endif
    constructEntryCompletionPopupCompletion ,
#if defined(ENABLE_OVERLOADING)
    entryCompletionPopupCompletion          ,
#endif
    getEntryCompletionPopupCompletion       ,
    setEntryCompletionPopupCompletion       ,


-- ** popupSetWidth #attr:popupSetWidth#
-- | Determines whether the completions popup window will be
-- resized to the width of the entry.
-- 
-- /Since: 2.8/

#if defined(ENABLE_OVERLOADING)
    EntryCompletionPopupSetWidthPropertyInfo,
#endif
    constructEntryCompletionPopupSetWidth   ,
#if defined(ENABLE_OVERLOADING)
    entryCompletionPopupSetWidth            ,
#endif
    getEntryCompletionPopupSetWidth         ,
    setEntryCompletionPopupSetWidth         ,


-- ** popupSingleMatch #attr:popupSingleMatch#
-- | Determines whether the completions popup window will shown
-- for a single possible completion. You probably want to set
-- this to 'P.False' if you are using
-- [inline completion][GtkEntryCompletion--inline-completion].
-- 
-- /Since: 2.8/

#if defined(ENABLE_OVERLOADING)
    EntryCompletionPopupSingleMatchPropertyInfo,
#endif
    constructEntryCompletionPopupSingleMatch,
#if defined(ENABLE_OVERLOADING)
    entryCompletionPopupSingleMatch         ,
#endif
    getEntryCompletionPopupSingleMatch      ,
    setEntryCompletionPopupSingleMatch      ,


-- ** textColumn #attr:textColumn#
-- | The column of the model containing the strings.
-- Note that the strings must be UTF-8.
-- 
-- /Since: 2.6/

#if defined(ENABLE_OVERLOADING)
    EntryCompletionTextColumnPropertyInfo   ,
#endif
    constructEntryCompletionTextColumn      ,
#if defined(ENABLE_OVERLOADING)
    entryCompletionTextColumn               ,
#endif
    getEntryCompletionTextColumn            ,
    setEntryCompletionTextColumn            ,




 -- * Signals


-- ** actionActivated #signal:actionActivated#

    EntryCompletionActionActivatedCallback  ,
#if defined(ENABLE_OVERLOADING)
    EntryCompletionActionActivatedSignalInfo,
#endif
    afterEntryCompletionActionActivated     ,
    onEntryCompletionActionActivated        ,


-- ** cursorOnMatch #signal:cursorOnMatch#

    EntryCompletionCursorOnMatchCallback    ,
#if defined(ENABLE_OVERLOADING)
    EntryCompletionCursorOnMatchSignalInfo  ,
#endif
    afterEntryCompletionCursorOnMatch       ,
    onEntryCompletionCursorOnMatch          ,


-- ** insertPrefix #signal:insertPrefix#

    EntryCompletionInsertPrefixCallback     ,
#if defined(ENABLE_OVERLOADING)
    EntryCompletionInsertPrefixSignalInfo   ,
#endif
    afterEntryCompletionInsertPrefix        ,
    onEntryCompletionInsertPrefix           ,


-- ** matchSelected #signal:matchSelected#

    EntryCompletionMatchSelectedCallback    ,
#if defined(ENABLE_OVERLOADING)
    EntryCompletionMatchSelectedSignalInfo  ,
#endif
    afterEntryCompletionMatchSelected       ,
    onEntryCompletionMatchSelected          ,


-- ** noMatches #signal:noMatches#

    EntryCompletionNoMatchesCallback        ,
#if defined(ENABLE_OVERLOADING)
    EntryCompletionNoMatchesSignalInfo      ,
#endif
    afterEntryCompletionNoMatches           ,
    onEntryCompletionNoMatches              ,




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

import qualified GI.GLib.Callbacks as GLib.Callbacks
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.CellLayout as Gtk.CellLayout
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeModel as Gtk.TreeModel
import {-# SOURCE #-} qualified GI.Gtk.Objects.CellArea as Gtk.CellArea
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreeIter as Gtk.TreeIter

-- | Memory-managed wrapper type.
newtype EntryCompletion = EntryCompletion (SP.ManagedPtr EntryCompletion)
    deriving (Eq)

instance SP.ManagedPtrNewtype EntryCompletion where
    toManagedPtr (EntryCompletion p) = p

foreign import ccall "gtk_entry_completion_get_type"
    c_gtk_entry_completion_get_type :: IO B.Types.GType

instance B.Types.TypedObject EntryCompletion where
    glibType = c_gtk_entry_completion_get_type

instance B.Types.GObject EntryCompletion

-- | Type class for types which can be safely cast to `EntryCompletion`, for instance with `toEntryCompletion`.
class (SP.GObject o, O.IsDescendantOf EntryCompletion o) => IsEntryCompletion o
instance (SP.GObject o, O.IsDescendantOf EntryCompletion o) => IsEntryCompletion o

instance O.HasParentTypes EntryCompletion
type instance O.ParentTypes EntryCompletion = '[GObject.Object.Object, Gtk.Buildable.Buildable, Gtk.CellLayout.CellLayout]

-- | Cast to `EntryCompletion`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toEntryCompletion :: (MIO.MonadIO m, IsEntryCompletion o) => o -> m EntryCompletion
toEntryCompletion = MIO.liftIO . B.ManagedPtr.unsafeCastTo EntryCompletion

-- | Convert 'EntryCompletion' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe EntryCompletion) where
    gvalueGType_ = c_gtk_entry_completion_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr EntryCompletion)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr EntryCompletion)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject EntryCompletion ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveEntryCompletionMethod (t :: Symbol) (o :: *) :: * where
    ResolveEntryCompletionMethod "addAttribute" o = Gtk.CellLayout.CellLayoutAddAttributeMethodInfo
    ResolveEntryCompletionMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveEntryCompletionMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveEntryCompletionMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveEntryCompletionMethod "clear" o = Gtk.CellLayout.CellLayoutClearMethodInfo
    ResolveEntryCompletionMethod "clearAttributes" o = Gtk.CellLayout.CellLayoutClearAttributesMethodInfo
    ResolveEntryCompletionMethod "complete" o = EntryCompletionCompleteMethodInfo
    ResolveEntryCompletionMethod "computePrefix" o = EntryCompletionComputePrefixMethodInfo
    ResolveEntryCompletionMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveEntryCompletionMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveEntryCompletionMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveEntryCompletionMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveEntryCompletionMethod "deleteAction" o = EntryCompletionDeleteActionMethodInfo
    ResolveEntryCompletionMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveEntryCompletionMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveEntryCompletionMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveEntryCompletionMethod "insertActionMarkup" o = EntryCompletionInsertActionMarkupMethodInfo
    ResolveEntryCompletionMethod "insertActionText" o = EntryCompletionInsertActionTextMethodInfo
    ResolveEntryCompletionMethod "insertPrefix" o = EntryCompletionInsertPrefixMethodInfo
    ResolveEntryCompletionMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveEntryCompletionMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveEntryCompletionMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveEntryCompletionMethod "packEnd" o = Gtk.CellLayout.CellLayoutPackEndMethodInfo
    ResolveEntryCompletionMethod "packStart" o = Gtk.CellLayout.CellLayoutPackStartMethodInfo
    ResolveEntryCompletionMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveEntryCompletionMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveEntryCompletionMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveEntryCompletionMethod "reorder" o = Gtk.CellLayout.CellLayoutReorderMethodInfo
    ResolveEntryCompletionMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveEntryCompletionMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveEntryCompletionMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveEntryCompletionMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveEntryCompletionMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveEntryCompletionMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveEntryCompletionMethod "getArea" o = Gtk.CellLayout.CellLayoutGetAreaMethodInfo
    ResolveEntryCompletionMethod "getCells" o = Gtk.CellLayout.CellLayoutGetCellsMethodInfo
    ResolveEntryCompletionMethod "getCompletionPrefix" o = EntryCompletionGetCompletionPrefixMethodInfo
    ResolveEntryCompletionMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveEntryCompletionMethod "getEntry" o = EntryCompletionGetEntryMethodInfo
    ResolveEntryCompletionMethod "getInlineCompletion" o = EntryCompletionGetInlineCompletionMethodInfo
    ResolveEntryCompletionMethod "getInlineSelection" o = EntryCompletionGetInlineSelectionMethodInfo
    ResolveEntryCompletionMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveEntryCompletionMethod "getMinimumKeyLength" o = EntryCompletionGetMinimumKeyLengthMethodInfo
    ResolveEntryCompletionMethod "getModel" o = EntryCompletionGetModelMethodInfo
    ResolveEntryCompletionMethod "getName" o = Gtk.Buildable.BuildableGetNameMethodInfo
    ResolveEntryCompletionMethod "getPopupCompletion" o = EntryCompletionGetPopupCompletionMethodInfo
    ResolveEntryCompletionMethod "getPopupSetWidth" o = EntryCompletionGetPopupSetWidthMethodInfo
    ResolveEntryCompletionMethod "getPopupSingleMatch" o = EntryCompletionGetPopupSingleMatchMethodInfo
    ResolveEntryCompletionMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveEntryCompletionMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveEntryCompletionMethod "getTextColumn" o = EntryCompletionGetTextColumnMethodInfo
    ResolveEntryCompletionMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveEntryCompletionMethod "setCellDataFunc" o = Gtk.CellLayout.CellLayoutSetCellDataFuncMethodInfo
    ResolveEntryCompletionMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveEntryCompletionMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveEntryCompletionMethod "setInlineCompletion" o = EntryCompletionSetInlineCompletionMethodInfo
    ResolveEntryCompletionMethod "setInlineSelection" o = EntryCompletionSetInlineSelectionMethodInfo
    ResolveEntryCompletionMethod "setMatchFunc" o = EntryCompletionSetMatchFuncMethodInfo
    ResolveEntryCompletionMethod "setMinimumKeyLength" o = EntryCompletionSetMinimumKeyLengthMethodInfo
    ResolveEntryCompletionMethod "setModel" o = EntryCompletionSetModelMethodInfo
    ResolveEntryCompletionMethod "setName" o = Gtk.Buildable.BuildableSetNameMethodInfo
    ResolveEntryCompletionMethod "setPopupCompletion" o = EntryCompletionSetPopupCompletionMethodInfo
    ResolveEntryCompletionMethod "setPopupSetWidth" o = EntryCompletionSetPopupSetWidthMethodInfo
    ResolveEntryCompletionMethod "setPopupSingleMatch" o = EntryCompletionSetPopupSingleMatchMethodInfo
    ResolveEntryCompletionMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveEntryCompletionMethod "setTextColumn" o = EntryCompletionSetTextColumnMethodInfo
    ResolveEntryCompletionMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveEntryCompletionMethod t EntryCompletion, O.OverloadedMethod info EntryCompletion p) => OL.IsLabel t (EntryCompletion -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveEntryCompletionMethod t EntryCompletion, O.OverloadedMethod info EntryCompletion p, R.HasField t EntryCompletion p) => R.HasField t EntryCompletion p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveEntryCompletionMethod t EntryCompletion, O.OverloadedMethodInfo info EntryCompletion) => OL.IsLabel t (O.MethodProxy info EntryCompletion) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal EntryCompletion::action-activated
-- | Gets emitted when an action is activated.
-- 
-- /Since: 2.4/
type EntryCompletionActionActivatedCallback =
    Int32
    -- ^ /@index@/: the index of the activated action
    -> IO ()

type C_EntryCompletionActionActivatedCallback =
    Ptr EntryCompletion ->                  -- object
    Int32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryCompletionActionActivatedCallback`.
foreign import ccall "wrapper"
    mk_EntryCompletionActionActivatedCallback :: C_EntryCompletionActionActivatedCallback -> IO (FunPtr C_EntryCompletionActionActivatedCallback)

wrap_EntryCompletionActionActivatedCallback :: 
    GObject a => (a -> EntryCompletionActionActivatedCallback) ->
    C_EntryCompletionActionActivatedCallback
wrap_EntryCompletionActionActivatedCallback gi'cb gi'selfPtr index _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  index


-- | Connect a signal handler for the [actionActivated](#signal:actionActivated) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entryCompletion #actionActivated callback
-- @
-- 
-- 
onEntryCompletionActionActivated :: (IsEntryCompletion a, MonadIO m) => a -> ((?self :: a) => EntryCompletionActionActivatedCallback) -> m SignalHandlerId
onEntryCompletionActionActivated obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryCompletionActionActivatedCallback wrapped
    wrapped'' <- mk_EntryCompletionActionActivatedCallback wrapped'
    connectSignalFunPtr obj "action-activated" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [actionActivated](#signal:actionActivated) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entryCompletion #actionActivated callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryCompletionActionActivated :: (IsEntryCompletion a, MonadIO m) => a -> ((?self :: a) => EntryCompletionActionActivatedCallback) -> m SignalHandlerId
afterEntryCompletionActionActivated obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryCompletionActionActivatedCallback wrapped
    wrapped'' <- mk_EntryCompletionActionActivatedCallback wrapped'
    connectSignalFunPtr obj "action-activated" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryCompletionActionActivatedSignalInfo
instance SignalInfo EntryCompletionActionActivatedSignalInfo where
    type HaskellCallbackType EntryCompletionActionActivatedSignalInfo = EntryCompletionActionActivatedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryCompletionActionActivatedCallback cb
        cb'' <- mk_EntryCompletionActionActivatedCallback cb'
        connectSignalFunPtr obj "action-activated" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion::action-activated"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#g:signal:actionActivated"})

#endif

-- signal EntryCompletion::cursor-on-match
-- | Gets emitted when a match from the cursor is on a match
-- of the list. The default behaviour is to replace the contents
-- of the entry with the contents of the text column in the row
-- pointed to by /@iter@/.
-- 
-- Note that /@model@/ is the model that was passed to
-- 'GI.Gtk.Objects.EntryCompletion.entryCompletionSetModel'.
-- 
-- /Since: 2.12/
type EntryCompletionCursorOnMatchCallback =
    Gtk.TreeModel.TreeModel
    -- ^ /@model@/: the t'GI.Gtk.Interfaces.TreeModel.TreeModel' containing the matches
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TreeIter.TreeIter' positioned at the selected match
    -> IO Bool
    -- ^ __Returns:__ 'P.True' if the signal has been handled

type C_EntryCompletionCursorOnMatchCallback =
    Ptr EntryCompletion ->                  -- object
    Ptr Gtk.TreeModel.TreeModel ->
    Ptr Gtk.TreeIter.TreeIter ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_EntryCompletionCursorOnMatchCallback`.
foreign import ccall "wrapper"
    mk_EntryCompletionCursorOnMatchCallback :: C_EntryCompletionCursorOnMatchCallback -> IO (FunPtr C_EntryCompletionCursorOnMatchCallback)

wrap_EntryCompletionCursorOnMatchCallback :: 
    GObject a => (a -> EntryCompletionCursorOnMatchCallback) ->
    C_EntryCompletionCursorOnMatchCallback
wrap_EntryCompletionCursorOnMatchCallback gi'cb gi'selfPtr model iter _ = do
    model' <- (newObject Gtk.TreeModel.TreeModel) model
    B.ManagedPtr.withTransient  iter $ \iter' -> do
        result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  model' iter'
        let result' = (fromIntegral . fromEnum) result
        return result'


-- | Connect a signal handler for the [cursorOnMatch](#signal:cursorOnMatch) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entryCompletion #cursorOnMatch callback
-- @
-- 
-- 
onEntryCompletionCursorOnMatch :: (IsEntryCompletion a, MonadIO m) => a -> ((?self :: a) => EntryCompletionCursorOnMatchCallback) -> m SignalHandlerId
onEntryCompletionCursorOnMatch obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryCompletionCursorOnMatchCallback wrapped
    wrapped'' <- mk_EntryCompletionCursorOnMatchCallback wrapped'
    connectSignalFunPtr obj "cursor-on-match" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [cursorOnMatch](#signal:cursorOnMatch) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entryCompletion #cursorOnMatch callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryCompletionCursorOnMatch :: (IsEntryCompletion a, MonadIO m) => a -> ((?self :: a) => EntryCompletionCursorOnMatchCallback) -> m SignalHandlerId
afterEntryCompletionCursorOnMatch obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryCompletionCursorOnMatchCallback wrapped
    wrapped'' <- mk_EntryCompletionCursorOnMatchCallback wrapped'
    connectSignalFunPtr obj "cursor-on-match" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryCompletionCursorOnMatchSignalInfo
instance SignalInfo EntryCompletionCursorOnMatchSignalInfo where
    type HaskellCallbackType EntryCompletionCursorOnMatchSignalInfo = EntryCompletionCursorOnMatchCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryCompletionCursorOnMatchCallback cb
        cb'' <- mk_EntryCompletionCursorOnMatchCallback cb'
        connectSignalFunPtr obj "cursor-on-match" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion::cursor-on-match"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#g:signal:cursorOnMatch"})

#endif

-- signal EntryCompletion::insert-prefix
-- | Gets emitted when the inline autocompletion is triggered.
-- The default behaviour is to make the entry display the
-- whole prefix and select the newly inserted part.
-- 
-- Applications may connect to this signal in order to insert only a
-- smaller part of the /@prefix@/ into the entry - e.g. the entry used in
-- the t'GI.Gtk.Interfaces.FileChooser.FileChooser' inserts only the part of the prefix up to the
-- next \'\/\'.
-- 
-- /Since: 2.6/
type EntryCompletionInsertPrefixCallback =
    T.Text
    -- ^ /@prefix@/: the common prefix of all possible completions
    -> IO Bool
    -- ^ __Returns:__ 'P.True' if the signal has been handled

type C_EntryCompletionInsertPrefixCallback =
    Ptr EntryCompletion ->                  -- object
    CString ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_EntryCompletionInsertPrefixCallback`.
foreign import ccall "wrapper"
    mk_EntryCompletionInsertPrefixCallback :: C_EntryCompletionInsertPrefixCallback -> IO (FunPtr C_EntryCompletionInsertPrefixCallback)

wrap_EntryCompletionInsertPrefixCallback :: 
    GObject a => (a -> EntryCompletionInsertPrefixCallback) ->
    C_EntryCompletionInsertPrefixCallback
wrap_EntryCompletionInsertPrefixCallback gi'cb gi'selfPtr prefix _ = do
    prefix' <- cstringToText prefix
    result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  prefix'
    let result' = (fromIntegral . fromEnum) result
    return result'


-- | Connect a signal handler for the [insertPrefix](#signal:insertPrefix) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entryCompletion #insertPrefix callback
-- @
-- 
-- 
onEntryCompletionInsertPrefix :: (IsEntryCompletion a, MonadIO m) => a -> ((?self :: a) => EntryCompletionInsertPrefixCallback) -> m SignalHandlerId
onEntryCompletionInsertPrefix obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryCompletionInsertPrefixCallback wrapped
    wrapped'' <- mk_EntryCompletionInsertPrefixCallback wrapped'
    connectSignalFunPtr obj "insert-prefix" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [insertPrefix](#signal:insertPrefix) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entryCompletion #insertPrefix callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryCompletionInsertPrefix :: (IsEntryCompletion a, MonadIO m) => a -> ((?self :: a) => EntryCompletionInsertPrefixCallback) -> m SignalHandlerId
afterEntryCompletionInsertPrefix obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryCompletionInsertPrefixCallback wrapped
    wrapped'' <- mk_EntryCompletionInsertPrefixCallback wrapped'
    connectSignalFunPtr obj "insert-prefix" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryCompletionInsertPrefixSignalInfo
instance SignalInfo EntryCompletionInsertPrefixSignalInfo where
    type HaskellCallbackType EntryCompletionInsertPrefixSignalInfo = EntryCompletionInsertPrefixCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryCompletionInsertPrefixCallback cb
        cb'' <- mk_EntryCompletionInsertPrefixCallback cb'
        connectSignalFunPtr obj "insert-prefix" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion::insert-prefix"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#g:signal:insertPrefix"})

#endif

-- signal EntryCompletion::match-selected
-- | Gets emitted when a match from the list is selected.
-- The default behaviour is to replace the contents of the
-- entry with the contents of the text column in the row
-- pointed to by /@iter@/.
-- 
-- Note that /@model@/ is the model that was passed to
-- 'GI.Gtk.Objects.EntryCompletion.entryCompletionSetModel'.
-- 
-- /Since: 2.4/
type EntryCompletionMatchSelectedCallback =
    Gtk.TreeModel.TreeModel
    -- ^ /@model@/: the t'GI.Gtk.Interfaces.TreeModel.TreeModel' containing the matches
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TreeIter.TreeIter' positioned at the selected match
    -> IO Bool
    -- ^ __Returns:__ 'P.True' if the signal has been handled

type C_EntryCompletionMatchSelectedCallback =
    Ptr EntryCompletion ->                  -- object
    Ptr Gtk.TreeModel.TreeModel ->
    Ptr Gtk.TreeIter.TreeIter ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_EntryCompletionMatchSelectedCallback`.
foreign import ccall "wrapper"
    mk_EntryCompletionMatchSelectedCallback :: C_EntryCompletionMatchSelectedCallback -> IO (FunPtr C_EntryCompletionMatchSelectedCallback)

wrap_EntryCompletionMatchSelectedCallback :: 
    GObject a => (a -> EntryCompletionMatchSelectedCallback) ->
    C_EntryCompletionMatchSelectedCallback
wrap_EntryCompletionMatchSelectedCallback gi'cb gi'selfPtr model iter _ = do
    model' <- (newObject Gtk.TreeModel.TreeModel) model
    B.ManagedPtr.withTransient  iter $ \iter' -> do
        result <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  model' iter'
        let result' = (fromIntegral . fromEnum) result
        return result'


-- | Connect a signal handler for the [matchSelected](#signal:matchSelected) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entryCompletion #matchSelected callback
-- @
-- 
-- 
onEntryCompletionMatchSelected :: (IsEntryCompletion a, MonadIO m) => a -> ((?self :: a) => EntryCompletionMatchSelectedCallback) -> m SignalHandlerId
onEntryCompletionMatchSelected obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryCompletionMatchSelectedCallback wrapped
    wrapped'' <- mk_EntryCompletionMatchSelectedCallback wrapped'
    connectSignalFunPtr obj "match-selected" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [matchSelected](#signal:matchSelected) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entryCompletion #matchSelected callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryCompletionMatchSelected :: (IsEntryCompletion a, MonadIO m) => a -> ((?self :: a) => EntryCompletionMatchSelectedCallback) -> m SignalHandlerId
afterEntryCompletionMatchSelected obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryCompletionMatchSelectedCallback wrapped
    wrapped'' <- mk_EntryCompletionMatchSelectedCallback wrapped'
    connectSignalFunPtr obj "match-selected" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryCompletionMatchSelectedSignalInfo
instance SignalInfo EntryCompletionMatchSelectedSignalInfo where
    type HaskellCallbackType EntryCompletionMatchSelectedSignalInfo = EntryCompletionMatchSelectedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryCompletionMatchSelectedCallback cb
        cb'' <- mk_EntryCompletionMatchSelectedCallback cb'
        connectSignalFunPtr obj "match-selected" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion::match-selected"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#g:signal:matchSelected"})

#endif

-- signal EntryCompletion::no-matches
-- | Gets emitted when the filter model has zero
-- number of rows in completion_complete method.
-- (In other words when GtkEntryCompletion is out of
--  suggestions)
-- 
-- /Since: 3.14/
type EntryCompletionNoMatchesCallback =
    IO ()

type C_EntryCompletionNoMatchesCallback =
    Ptr EntryCompletion ->                  -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EntryCompletionNoMatchesCallback`.
foreign import ccall "wrapper"
    mk_EntryCompletionNoMatchesCallback :: C_EntryCompletionNoMatchesCallback -> IO (FunPtr C_EntryCompletionNoMatchesCallback)

wrap_EntryCompletionNoMatchesCallback :: 
    GObject a => (a -> EntryCompletionNoMatchesCallback) ->
    C_EntryCompletionNoMatchesCallback
wrap_EntryCompletionNoMatchesCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [noMatches](#signal:noMatches) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' entryCompletion #noMatches callback
-- @
-- 
-- 
onEntryCompletionNoMatches :: (IsEntryCompletion a, MonadIO m) => a -> ((?self :: a) => EntryCompletionNoMatchesCallback) -> m SignalHandlerId
onEntryCompletionNoMatches obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryCompletionNoMatchesCallback wrapped
    wrapped'' <- mk_EntryCompletionNoMatchesCallback wrapped'
    connectSignalFunPtr obj "no-matches" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [noMatches](#signal:noMatches) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' entryCompletion #noMatches callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEntryCompletionNoMatches :: (IsEntryCompletion a, MonadIO m) => a -> ((?self :: a) => EntryCompletionNoMatchesCallback) -> m SignalHandlerId
afterEntryCompletionNoMatches obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EntryCompletionNoMatchesCallback wrapped
    wrapped'' <- mk_EntryCompletionNoMatchesCallback wrapped'
    connectSignalFunPtr obj "no-matches" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EntryCompletionNoMatchesSignalInfo
instance SignalInfo EntryCompletionNoMatchesSignalInfo where
    type HaskellCallbackType EntryCompletionNoMatchesSignalInfo = EntryCompletionNoMatchesCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EntryCompletionNoMatchesCallback cb
        cb'' <- mk_EntryCompletionNoMatchesCallback cb'
        connectSignalFunPtr obj "no-matches" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion::no-matches"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#g:signal:noMatches"})

#endif

-- VVV Prop "cell-area"
   -- Type: TInterface (Name {namespace = "Gtk", name = "CellArea"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@cell-area@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entryCompletion #cellArea
-- @
getEntryCompletionCellArea :: (MonadIO m, IsEntryCompletion o) => o -> m (Maybe Gtk.CellArea.CellArea)
getEntryCompletionCellArea obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "cell-area" Gtk.CellArea.CellArea

-- | Construct a `GValueConstruct` with valid value for the “@cell-area@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryCompletionCellArea :: (IsEntryCompletion o, MIO.MonadIO m, Gtk.CellArea.IsCellArea a) => a -> m (GValueConstruct o)
constructEntryCompletionCellArea val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "cell-area" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data EntryCompletionCellAreaPropertyInfo
instance AttrInfo EntryCompletionCellAreaPropertyInfo where
    type AttrAllowedOps EntryCompletionCellAreaPropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntryCompletionCellAreaPropertyInfo = IsEntryCompletion
    type AttrSetTypeConstraint EntryCompletionCellAreaPropertyInfo = Gtk.CellArea.IsCellArea
    type AttrTransferTypeConstraint EntryCompletionCellAreaPropertyInfo = Gtk.CellArea.IsCellArea
    type AttrTransferType EntryCompletionCellAreaPropertyInfo = Gtk.CellArea.CellArea
    type AttrGetType EntryCompletionCellAreaPropertyInfo = (Maybe Gtk.CellArea.CellArea)
    type AttrLabel EntryCompletionCellAreaPropertyInfo = "cell-area"
    type AttrOrigin EntryCompletionCellAreaPropertyInfo = EntryCompletion
    attrGet = getEntryCompletionCellArea
    attrSet = undefined
    attrTransfer _ v = do
        unsafeCastTo Gtk.CellArea.CellArea v
    attrConstruct = constructEntryCompletionCellArea
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.cellArea"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#g:attr:cellArea"
        })
#endif

-- VVV Prop "inline-completion"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@inline-completion@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entryCompletion #inlineCompletion
-- @
getEntryCompletionInlineCompletion :: (MonadIO m, IsEntryCompletion o) => o -> m Bool
getEntryCompletionInlineCompletion obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "inline-completion"

-- | Set the value of the “@inline-completion@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entryCompletion [ #inlineCompletion 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryCompletionInlineCompletion :: (MonadIO m, IsEntryCompletion o) => o -> Bool -> m ()
setEntryCompletionInlineCompletion obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "inline-completion" val

-- | Construct a `GValueConstruct` with valid value for the “@inline-completion@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryCompletionInlineCompletion :: (IsEntryCompletion o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryCompletionInlineCompletion val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "inline-completion" val

#if defined(ENABLE_OVERLOADING)
data EntryCompletionInlineCompletionPropertyInfo
instance AttrInfo EntryCompletionInlineCompletionPropertyInfo where
    type AttrAllowedOps EntryCompletionInlineCompletionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryCompletionInlineCompletionPropertyInfo = IsEntryCompletion
    type AttrSetTypeConstraint EntryCompletionInlineCompletionPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryCompletionInlineCompletionPropertyInfo = (~) Bool
    type AttrTransferType EntryCompletionInlineCompletionPropertyInfo = Bool
    type AttrGetType EntryCompletionInlineCompletionPropertyInfo = Bool
    type AttrLabel EntryCompletionInlineCompletionPropertyInfo = "inline-completion"
    type AttrOrigin EntryCompletionInlineCompletionPropertyInfo = EntryCompletion
    attrGet = getEntryCompletionInlineCompletion
    attrSet = setEntryCompletionInlineCompletion
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryCompletionInlineCompletion
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.inlineCompletion"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#g:attr:inlineCompletion"
        })
#endif

-- VVV Prop "inline-selection"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@inline-selection@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entryCompletion #inlineSelection
-- @
getEntryCompletionInlineSelection :: (MonadIO m, IsEntryCompletion o) => o -> m Bool
getEntryCompletionInlineSelection obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "inline-selection"

-- | Set the value of the “@inline-selection@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entryCompletion [ #inlineSelection 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryCompletionInlineSelection :: (MonadIO m, IsEntryCompletion o) => o -> Bool -> m ()
setEntryCompletionInlineSelection obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "inline-selection" val

-- | Construct a `GValueConstruct` with valid value for the “@inline-selection@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryCompletionInlineSelection :: (IsEntryCompletion o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryCompletionInlineSelection val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "inline-selection" val

#if defined(ENABLE_OVERLOADING)
data EntryCompletionInlineSelectionPropertyInfo
instance AttrInfo EntryCompletionInlineSelectionPropertyInfo where
    type AttrAllowedOps EntryCompletionInlineSelectionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryCompletionInlineSelectionPropertyInfo = IsEntryCompletion
    type AttrSetTypeConstraint EntryCompletionInlineSelectionPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryCompletionInlineSelectionPropertyInfo = (~) Bool
    type AttrTransferType EntryCompletionInlineSelectionPropertyInfo = Bool
    type AttrGetType EntryCompletionInlineSelectionPropertyInfo = Bool
    type AttrLabel EntryCompletionInlineSelectionPropertyInfo = "inline-selection"
    type AttrOrigin EntryCompletionInlineSelectionPropertyInfo = EntryCompletion
    attrGet = getEntryCompletionInlineSelection
    attrSet = setEntryCompletionInlineSelection
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryCompletionInlineSelection
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.inlineSelection"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#g:attr:inlineSelection"
        })
#endif

-- VVV Prop "minimum-key-length"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@minimum-key-length@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entryCompletion #minimumKeyLength
-- @
getEntryCompletionMinimumKeyLength :: (MonadIO m, IsEntryCompletion o) => o -> m Int32
getEntryCompletionMinimumKeyLength obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "minimum-key-length"

-- | Set the value of the “@minimum-key-length@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entryCompletion [ #minimumKeyLength 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryCompletionMinimumKeyLength :: (MonadIO m, IsEntryCompletion o) => o -> Int32 -> m ()
setEntryCompletionMinimumKeyLength obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "minimum-key-length" val

-- | Construct a `GValueConstruct` with valid value for the “@minimum-key-length@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryCompletionMinimumKeyLength :: (IsEntryCompletion o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructEntryCompletionMinimumKeyLength val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "minimum-key-length" val

#if defined(ENABLE_OVERLOADING)
data EntryCompletionMinimumKeyLengthPropertyInfo
instance AttrInfo EntryCompletionMinimumKeyLengthPropertyInfo where
    type AttrAllowedOps EntryCompletionMinimumKeyLengthPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryCompletionMinimumKeyLengthPropertyInfo = IsEntryCompletion
    type AttrSetTypeConstraint EntryCompletionMinimumKeyLengthPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint EntryCompletionMinimumKeyLengthPropertyInfo = (~) Int32
    type AttrTransferType EntryCompletionMinimumKeyLengthPropertyInfo = Int32
    type AttrGetType EntryCompletionMinimumKeyLengthPropertyInfo = Int32
    type AttrLabel EntryCompletionMinimumKeyLengthPropertyInfo = "minimum-key-length"
    type AttrOrigin EntryCompletionMinimumKeyLengthPropertyInfo = EntryCompletion
    attrGet = getEntryCompletionMinimumKeyLength
    attrSet = setEntryCompletionMinimumKeyLength
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryCompletionMinimumKeyLength
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.minimumKeyLength"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#g:attr:minimumKeyLength"
        })
#endif

-- VVV Prop "model"
   -- Type: TInterface (Name {namespace = "Gtk", name = "TreeModel"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just True,Just True)

-- | Get the value of the “@model@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entryCompletion #model
-- @
getEntryCompletionModel :: (MonadIO m, IsEntryCompletion o) => o -> m (Maybe Gtk.TreeModel.TreeModel)
getEntryCompletionModel obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "model" Gtk.TreeModel.TreeModel

-- | Set the value of the “@model@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entryCompletion [ #model 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryCompletionModel :: (MonadIO m, IsEntryCompletion o, Gtk.TreeModel.IsTreeModel a) => o -> a -> m ()
setEntryCompletionModel obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyObject obj "model" (Just val)

-- | Construct a `GValueConstruct` with valid value for the “@model@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryCompletionModel :: (IsEntryCompletion o, MIO.MonadIO m, Gtk.TreeModel.IsTreeModel a) => a -> m (GValueConstruct o)
constructEntryCompletionModel val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "model" (P.Just val)

-- | Set the value of the “@model@” property to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #model
-- @
clearEntryCompletionModel :: (MonadIO m, IsEntryCompletion o) => o -> m ()
clearEntryCompletionModel obj = liftIO $ B.Properties.setObjectPropertyObject obj "model" (Nothing :: Maybe Gtk.TreeModel.TreeModel)

#if defined(ENABLE_OVERLOADING)
data EntryCompletionModelPropertyInfo
instance AttrInfo EntryCompletionModelPropertyInfo where
    type AttrAllowedOps EntryCompletionModelPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint EntryCompletionModelPropertyInfo = IsEntryCompletion
    type AttrSetTypeConstraint EntryCompletionModelPropertyInfo = Gtk.TreeModel.IsTreeModel
    type AttrTransferTypeConstraint EntryCompletionModelPropertyInfo = Gtk.TreeModel.IsTreeModel
    type AttrTransferType EntryCompletionModelPropertyInfo = Gtk.TreeModel.TreeModel
    type AttrGetType EntryCompletionModelPropertyInfo = (Maybe Gtk.TreeModel.TreeModel)
    type AttrLabel EntryCompletionModelPropertyInfo = "model"
    type AttrOrigin EntryCompletionModelPropertyInfo = EntryCompletion
    attrGet = getEntryCompletionModel
    attrSet = setEntryCompletionModel
    attrTransfer _ v = do
        unsafeCastTo Gtk.TreeModel.TreeModel v
    attrConstruct = constructEntryCompletionModel
    attrClear = clearEntryCompletionModel
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.model"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#g:attr:model"
        })
#endif

-- VVV Prop "popup-completion"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@popup-completion@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entryCompletion #popupCompletion
-- @
getEntryCompletionPopupCompletion :: (MonadIO m, IsEntryCompletion o) => o -> m Bool
getEntryCompletionPopupCompletion obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "popup-completion"

-- | Set the value of the “@popup-completion@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entryCompletion [ #popupCompletion 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryCompletionPopupCompletion :: (MonadIO m, IsEntryCompletion o) => o -> Bool -> m ()
setEntryCompletionPopupCompletion obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "popup-completion" val

-- | Construct a `GValueConstruct` with valid value for the “@popup-completion@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryCompletionPopupCompletion :: (IsEntryCompletion o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryCompletionPopupCompletion val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "popup-completion" val

#if defined(ENABLE_OVERLOADING)
data EntryCompletionPopupCompletionPropertyInfo
instance AttrInfo EntryCompletionPopupCompletionPropertyInfo where
    type AttrAllowedOps EntryCompletionPopupCompletionPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryCompletionPopupCompletionPropertyInfo = IsEntryCompletion
    type AttrSetTypeConstraint EntryCompletionPopupCompletionPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryCompletionPopupCompletionPropertyInfo = (~) Bool
    type AttrTransferType EntryCompletionPopupCompletionPropertyInfo = Bool
    type AttrGetType EntryCompletionPopupCompletionPropertyInfo = Bool
    type AttrLabel EntryCompletionPopupCompletionPropertyInfo = "popup-completion"
    type AttrOrigin EntryCompletionPopupCompletionPropertyInfo = EntryCompletion
    attrGet = getEntryCompletionPopupCompletion
    attrSet = setEntryCompletionPopupCompletion
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryCompletionPopupCompletion
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.popupCompletion"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#g:attr:popupCompletion"
        })
#endif

-- VVV Prop "popup-set-width"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@popup-set-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entryCompletion #popupSetWidth
-- @
getEntryCompletionPopupSetWidth :: (MonadIO m, IsEntryCompletion o) => o -> m Bool
getEntryCompletionPopupSetWidth obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "popup-set-width"

-- | Set the value of the “@popup-set-width@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entryCompletion [ #popupSetWidth 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryCompletionPopupSetWidth :: (MonadIO m, IsEntryCompletion o) => o -> Bool -> m ()
setEntryCompletionPopupSetWidth obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "popup-set-width" val

-- | Construct a `GValueConstruct` with valid value for the “@popup-set-width@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryCompletionPopupSetWidth :: (IsEntryCompletion o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryCompletionPopupSetWidth val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "popup-set-width" val

#if defined(ENABLE_OVERLOADING)
data EntryCompletionPopupSetWidthPropertyInfo
instance AttrInfo EntryCompletionPopupSetWidthPropertyInfo where
    type AttrAllowedOps EntryCompletionPopupSetWidthPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryCompletionPopupSetWidthPropertyInfo = IsEntryCompletion
    type AttrSetTypeConstraint EntryCompletionPopupSetWidthPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryCompletionPopupSetWidthPropertyInfo = (~) Bool
    type AttrTransferType EntryCompletionPopupSetWidthPropertyInfo = Bool
    type AttrGetType EntryCompletionPopupSetWidthPropertyInfo = Bool
    type AttrLabel EntryCompletionPopupSetWidthPropertyInfo = "popup-set-width"
    type AttrOrigin EntryCompletionPopupSetWidthPropertyInfo = EntryCompletion
    attrGet = getEntryCompletionPopupSetWidth
    attrSet = setEntryCompletionPopupSetWidth
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryCompletionPopupSetWidth
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.popupSetWidth"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#g:attr:popupSetWidth"
        })
#endif

-- VVV Prop "popup-single-match"
   -- Type: TBasicType TBoolean
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@popup-single-match@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entryCompletion #popupSingleMatch
-- @
getEntryCompletionPopupSingleMatch :: (MonadIO m, IsEntryCompletion o) => o -> m Bool
getEntryCompletionPopupSingleMatch obj = MIO.liftIO $ B.Properties.getObjectPropertyBool obj "popup-single-match"

-- | Set the value of the “@popup-single-match@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entryCompletion [ #popupSingleMatch 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryCompletionPopupSingleMatch :: (MonadIO m, IsEntryCompletion o) => o -> Bool -> m ()
setEntryCompletionPopupSingleMatch obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyBool obj "popup-single-match" val

-- | Construct a `GValueConstruct` with valid value for the “@popup-single-match@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryCompletionPopupSingleMatch :: (IsEntryCompletion o, MIO.MonadIO m) => Bool -> m (GValueConstruct o)
constructEntryCompletionPopupSingleMatch val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBool "popup-single-match" val

#if defined(ENABLE_OVERLOADING)
data EntryCompletionPopupSingleMatchPropertyInfo
instance AttrInfo EntryCompletionPopupSingleMatchPropertyInfo where
    type AttrAllowedOps EntryCompletionPopupSingleMatchPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryCompletionPopupSingleMatchPropertyInfo = IsEntryCompletion
    type AttrSetTypeConstraint EntryCompletionPopupSingleMatchPropertyInfo = (~) Bool
    type AttrTransferTypeConstraint EntryCompletionPopupSingleMatchPropertyInfo = (~) Bool
    type AttrTransferType EntryCompletionPopupSingleMatchPropertyInfo = Bool
    type AttrGetType EntryCompletionPopupSingleMatchPropertyInfo = Bool
    type AttrLabel EntryCompletionPopupSingleMatchPropertyInfo = "popup-single-match"
    type AttrOrigin EntryCompletionPopupSingleMatchPropertyInfo = EntryCompletion
    attrGet = getEntryCompletionPopupSingleMatch
    attrSet = setEntryCompletionPopupSingleMatch
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryCompletionPopupSingleMatch
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.popupSingleMatch"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#g:attr:popupSingleMatch"
        })
#endif

-- VVV Prop "text-column"
   -- Type: TBasicType TInt
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@text-column@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' entryCompletion #textColumn
-- @
getEntryCompletionTextColumn :: (MonadIO m, IsEntryCompletion o) => o -> m Int32
getEntryCompletionTextColumn obj = MIO.liftIO $ B.Properties.getObjectPropertyInt32 obj "text-column"

-- | Set the value of the “@text-column@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' entryCompletion [ #textColumn 'Data.GI.Base.Attributes.:=' value ]
-- @
setEntryCompletionTextColumn :: (MonadIO m, IsEntryCompletion o) => o -> Int32 -> m ()
setEntryCompletionTextColumn obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyInt32 obj "text-column" val

-- | Construct a `GValueConstruct` with valid value for the “@text-column@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEntryCompletionTextColumn :: (IsEntryCompletion o, MIO.MonadIO m) => Int32 -> m (GValueConstruct o)
constructEntryCompletionTextColumn val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyInt32 "text-column" val

#if defined(ENABLE_OVERLOADING)
data EntryCompletionTextColumnPropertyInfo
instance AttrInfo EntryCompletionTextColumnPropertyInfo where
    type AttrAllowedOps EntryCompletionTextColumnPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EntryCompletionTextColumnPropertyInfo = IsEntryCompletion
    type AttrSetTypeConstraint EntryCompletionTextColumnPropertyInfo = (~) Int32
    type AttrTransferTypeConstraint EntryCompletionTextColumnPropertyInfo = (~) Int32
    type AttrTransferType EntryCompletionTextColumnPropertyInfo = Int32
    type AttrGetType EntryCompletionTextColumnPropertyInfo = Int32
    type AttrLabel EntryCompletionTextColumnPropertyInfo = "text-column"
    type AttrOrigin EntryCompletionTextColumnPropertyInfo = EntryCompletion
    attrGet = getEntryCompletionTextColumn
    attrSet = setEntryCompletionTextColumn
    attrTransfer _ v = do
        return v
    attrConstruct = constructEntryCompletionTextColumn
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.textColumn"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#g:attr:textColumn"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList EntryCompletion
type instance O.AttributeList EntryCompletion = EntryCompletionAttributeList
type EntryCompletionAttributeList = ('[ '("cellArea", EntryCompletionCellAreaPropertyInfo), '("inlineCompletion", EntryCompletionInlineCompletionPropertyInfo), '("inlineSelection", EntryCompletionInlineSelectionPropertyInfo), '("minimumKeyLength", EntryCompletionMinimumKeyLengthPropertyInfo), '("model", EntryCompletionModelPropertyInfo), '("popupCompletion", EntryCompletionPopupCompletionPropertyInfo), '("popupSetWidth", EntryCompletionPopupSetWidthPropertyInfo), '("popupSingleMatch", EntryCompletionPopupSingleMatchPropertyInfo), '("textColumn", EntryCompletionTextColumnPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
entryCompletionCellArea :: AttrLabelProxy "cellArea"
entryCompletionCellArea = AttrLabelProxy

entryCompletionInlineCompletion :: AttrLabelProxy "inlineCompletion"
entryCompletionInlineCompletion = AttrLabelProxy

entryCompletionInlineSelection :: AttrLabelProxy "inlineSelection"
entryCompletionInlineSelection = AttrLabelProxy

entryCompletionMinimumKeyLength :: AttrLabelProxy "minimumKeyLength"
entryCompletionMinimumKeyLength = AttrLabelProxy

entryCompletionModel :: AttrLabelProxy "model"
entryCompletionModel = AttrLabelProxy

entryCompletionPopupCompletion :: AttrLabelProxy "popupCompletion"
entryCompletionPopupCompletion = AttrLabelProxy

entryCompletionPopupSetWidth :: AttrLabelProxy "popupSetWidth"
entryCompletionPopupSetWidth = AttrLabelProxy

entryCompletionPopupSingleMatch :: AttrLabelProxy "popupSingleMatch"
entryCompletionPopupSingleMatch = AttrLabelProxy

entryCompletionTextColumn :: AttrLabelProxy "textColumn"
entryCompletionTextColumn = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList EntryCompletion = EntryCompletionSignalList
type EntryCompletionSignalList = ('[ '("actionActivated", EntryCompletionActionActivatedSignalInfo), '("cursorOnMatch", EntryCompletionCursorOnMatchSignalInfo), '("insertPrefix", EntryCompletionInsertPrefixSignalInfo), '("matchSelected", EntryCompletionMatchSelectedSignalInfo), '("noMatches", EntryCompletionNoMatchesSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method EntryCompletion::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "EntryCompletion" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_completion_new" gtk_entry_completion_new :: 
    IO (Ptr EntryCompletion)

-- | Creates a new t'GI.Gtk.Objects.EntryCompletion.EntryCompletion' object.
-- 
-- /Since: 2.4/
entryCompletionNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m EntryCompletion
    -- ^ __Returns:__ A newly created t'GI.Gtk.Objects.EntryCompletion.EntryCompletion' object
entryCompletionNew  = liftIO $ do
    result <- gtk_entry_completion_new
    checkUnexpectedReturnNULL "entryCompletionNew" result
    result' <- (wrapObject EntryCompletion) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method EntryCompletion::new_with_area
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "area"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CellArea" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkCellArea used to layout cells"
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
--               (TInterface Name { namespace = "Gtk" , name = "EntryCompletion" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_completion_new_with_area" gtk_entry_completion_new_with_area :: 
    Ptr Gtk.CellArea.CellArea ->            -- area : TInterface (Name {namespace = "Gtk", name = "CellArea"})
    IO (Ptr EntryCompletion)

-- | Creates a new t'GI.Gtk.Objects.EntryCompletion.EntryCompletion' object using the
-- specified /@area@/ to layout cells in the underlying
-- t'GI.Gtk.Objects.TreeViewColumn.TreeViewColumn' for the drop-down menu.
-- 
-- /Since: 3.0/
entryCompletionNewWithArea ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.CellArea.IsCellArea a) =>
    a
    -- ^ /@area@/: the t'GI.Gtk.Objects.CellArea.CellArea' used to layout cells
    -> m EntryCompletion
    -- ^ __Returns:__ A newly created t'GI.Gtk.Objects.EntryCompletion.EntryCompletion' object
entryCompletionNewWithArea area = liftIO $ do
    area' <- unsafeManagedPtrCastPtr area
    result <- gtk_entry_completion_new_with_area area'
    checkUnexpectedReturnNULL "entryCompletionNewWithArea" result
    result' <- (wrapObject EntryCompletion) result
    touchManagedPtr area
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method EntryCompletion::complete
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
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

foreign import ccall "gtk_entry_completion_complete" gtk_entry_completion_complete :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    IO ()

-- | Requests a completion operation, or in other words a refiltering of the
-- current list with completions, using the current key. The completion list
-- view will be updated accordingly.
-- 
-- /Since: 2.4/
entryCompletionComplete ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> m ()
entryCompletionComplete completion = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    gtk_entry_completion_complete completion'
    touchManagedPtr completion
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryCompletionCompleteMethodInfo
instance (signature ~ (m ()), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionCompleteMethodInfo a signature where
    overloadedMethod = entryCompletionComplete

instance O.OverloadedMethodInfo EntryCompletionCompleteMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionComplete",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionComplete"
        })


#endif

-- method EntryCompletion::compute_prefix
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the entry completion"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "key"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The text to complete for"
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

foreign import ccall "gtk_entry_completion_compute_prefix" gtk_entry_completion_compute_prefix :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    CString ->                              -- key : TBasicType TUTF8
    IO CString

-- | Computes the common prefix that is shared by all rows in /@completion@/
-- that start with /@key@/. If no row matches /@key@/, 'P.Nothing' will be returned.
-- Note that a text column must have been set for this function to work,
-- see 'GI.Gtk.Objects.EntryCompletion.entryCompletionSetTextColumn' for details.
-- 
-- /Since: 3.4/
entryCompletionComputePrefix ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: the entry completion
    -> T.Text
    -- ^ /@key@/: The text to complete for
    -> m (Maybe T.Text)
    -- ^ __Returns:__ The common prefix all rows starting with
    --   /@key@/ or 'P.Nothing' if no row matches /@key@/.
entryCompletionComputePrefix completion key = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    key' <- textToCString key
    result <- gtk_entry_completion_compute_prefix completion' key'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        freeMem result'
        return result''
    touchManagedPtr completion
    freeMem key'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data EntryCompletionComputePrefixMethodInfo
instance (signature ~ (T.Text -> m (Maybe T.Text)), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionComputePrefixMethodInfo a signature where
    overloadedMethod = entryCompletionComputePrefix

instance O.OverloadedMethodInfo EntryCompletionComputePrefixMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionComputePrefix",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionComputePrefix"
        })


#endif

-- method EntryCompletion::delete_action
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "index_"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the index of the item to delete"
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

foreign import ccall "gtk_entry_completion_delete_action" gtk_entry_completion_delete_action :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    Int32 ->                                -- index_ : TBasicType TInt
    IO ()

-- | Deletes the action at /@index_@/ from /@completion@/’s action list.
-- 
-- Note that /@index_@/ is a relative position and the position of an
-- action may have changed since it was inserted.
-- 
-- /Since: 2.4/
entryCompletionDeleteAction ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> Int32
    -- ^ /@index_@/: the index of the item to delete
    -> m ()
entryCompletionDeleteAction completion index_ = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    gtk_entry_completion_delete_action completion' index_
    touchManagedPtr completion
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryCompletionDeleteActionMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionDeleteActionMethodInfo a signature where
    overloadedMethod = entryCompletionDeleteAction

instance O.OverloadedMethodInfo EntryCompletionDeleteActionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionDeleteAction",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionDeleteAction"
        })


#endif

-- method EntryCompletion::get_completion_prefix
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
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

foreign import ccall "gtk_entry_completion_get_completion_prefix" gtk_entry_completion_get_completion_prefix :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    IO CString

-- | Get the original text entered by the user that triggered
-- the completion or 'P.Nothing' if there’s no completion ongoing.
-- 
-- /Since: 2.12/
entryCompletionGetCompletionPrefix ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> m T.Text
    -- ^ __Returns:__ the prefix for the current completion
entryCompletionGetCompletionPrefix completion = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    result <- gtk_entry_completion_get_completion_prefix completion'
    checkUnexpectedReturnNULL "entryCompletionGetCompletionPrefix" result
    result' <- cstringToText result
    touchManagedPtr completion
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetCompletionPrefixMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionGetCompletionPrefixMethodInfo a signature where
    overloadedMethod = entryCompletionGetCompletionPrefix

instance O.OverloadedMethodInfo EntryCompletionGetCompletionPrefixMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionGetCompletionPrefix",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionGetCompletionPrefix"
        })


#endif

-- method EntryCompletion::get_entry
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
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

foreign import ccall "gtk_entry_completion_get_entry" gtk_entry_completion_get_entry :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    IO (Ptr Gtk.Widget.Widget)

-- | Gets the entry /@completion@/ has been attached to.
-- 
-- /Since: 2.4/
entryCompletionGetEntry ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> m Gtk.Widget.Widget
    -- ^ __Returns:__ The entry /@completion@/ has been attached to
entryCompletionGetEntry completion = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    result <- gtk_entry_completion_get_entry completion'
    checkUnexpectedReturnNULL "entryCompletionGetEntry" result
    result' <- (newObject Gtk.Widget.Widget) result
    touchManagedPtr completion
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetEntryMethodInfo
instance (signature ~ (m Gtk.Widget.Widget), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionGetEntryMethodInfo a signature where
    overloadedMethod = entryCompletionGetEntry

instance O.OverloadedMethodInfo EntryCompletionGetEntryMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionGetEntry",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionGetEntry"
        })


#endif

-- method EntryCompletion::get_inline_completion
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
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

foreign import ccall "gtk_entry_completion_get_inline_completion" gtk_entry_completion_get_inline_completion :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    IO CInt

-- | Returns whether the common prefix of the possible completions should
-- be automatically inserted in the entry.
-- 
-- /Since: 2.6/
entryCompletionGetInlineCompletion ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if inline completion is turned on
entryCompletionGetInlineCompletion completion = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    result <- gtk_entry_completion_get_inline_completion completion'
    let result' = (/= 0) result
    touchManagedPtr completion
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetInlineCompletionMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionGetInlineCompletionMethodInfo a signature where
    overloadedMethod = entryCompletionGetInlineCompletion

instance O.OverloadedMethodInfo EntryCompletionGetInlineCompletionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionGetInlineCompletion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionGetInlineCompletion"
        })


#endif

-- method EntryCompletion::get_inline_selection
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
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

foreign import ccall "gtk_entry_completion_get_inline_selection" gtk_entry_completion_get_inline_selection :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    IO CInt

-- | Returns 'P.True' if inline-selection mode is turned on.
-- 
-- /Since: 2.12/
entryCompletionGetInlineSelection ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if inline-selection mode is on
entryCompletionGetInlineSelection completion = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    result <- gtk_entry_completion_get_inline_selection completion'
    let result' = (/= 0) result
    touchManagedPtr completion
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetInlineSelectionMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionGetInlineSelectionMethodInfo a signature where
    overloadedMethod = entryCompletionGetInlineSelection

instance O.OverloadedMethodInfo EntryCompletionGetInlineSelectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionGetInlineSelection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionGetInlineSelection"
        })


#endif

-- method EntryCompletion::get_minimum_key_length
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
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

foreign import ccall "gtk_entry_completion_get_minimum_key_length" gtk_entry_completion_get_minimum_key_length :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    IO Int32

-- | Returns the minimum key length as set for /@completion@/.
-- 
-- /Since: 2.4/
entryCompletionGetMinimumKeyLength ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> m Int32
    -- ^ __Returns:__ The currently used minimum key length
entryCompletionGetMinimumKeyLength completion = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    result <- gtk_entry_completion_get_minimum_key_length completion'
    touchManagedPtr completion
    return result

#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetMinimumKeyLengthMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionGetMinimumKeyLengthMethodInfo a signature where
    overloadedMethod = entryCompletionGetMinimumKeyLength

instance O.OverloadedMethodInfo EntryCompletionGetMinimumKeyLengthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionGetMinimumKeyLength",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionGetMinimumKeyLength"
        })


#endif

-- method EntryCompletion::get_model
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TreeModel" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_entry_completion_get_model" gtk_entry_completion_get_model :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    IO (Ptr Gtk.TreeModel.TreeModel)

-- | Returns the model the t'GI.Gtk.Objects.EntryCompletion.EntryCompletion' is using as data source.
-- Returns 'P.Nothing' if the model is unset.
-- 
-- /Since: 2.4/
entryCompletionGetModel ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> m (Maybe Gtk.TreeModel.TreeModel)
    -- ^ __Returns:__ A t'GI.Gtk.Interfaces.TreeModel.TreeModel', or 'P.Nothing' if none
    --     is currently being used
entryCompletionGetModel completion = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    result <- gtk_entry_completion_get_model completion'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.TreeModel.TreeModel) result'
        return result''
    touchManagedPtr completion
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetModelMethodInfo
instance (signature ~ (m (Maybe Gtk.TreeModel.TreeModel)), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionGetModelMethodInfo a signature where
    overloadedMethod = entryCompletionGetModel

instance O.OverloadedMethodInfo EntryCompletionGetModelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionGetModel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionGetModel"
        })


#endif

-- method EntryCompletion::get_popup_completion
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
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

foreign import ccall "gtk_entry_completion_get_popup_completion" gtk_entry_completion_get_popup_completion :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    IO CInt

-- | Returns whether the completions should be presented in a popup window.
-- 
-- /Since: 2.6/
entryCompletionGetPopupCompletion ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if popup completion is turned on
entryCompletionGetPopupCompletion completion = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    result <- gtk_entry_completion_get_popup_completion completion'
    let result' = (/= 0) result
    touchManagedPtr completion
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetPopupCompletionMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionGetPopupCompletionMethodInfo a signature where
    overloadedMethod = entryCompletionGetPopupCompletion

instance O.OverloadedMethodInfo EntryCompletionGetPopupCompletionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionGetPopupCompletion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionGetPopupCompletion"
        })


#endif

-- method EntryCompletion::get_popup_set_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
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

foreign import ccall "gtk_entry_completion_get_popup_set_width" gtk_entry_completion_get_popup_set_width :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    IO CInt

-- | Returns whether the  completion popup window will be resized to the
-- width of the entry.
-- 
-- /Since: 2.8/
entryCompletionGetPopupSetWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the popup window will be resized to the width of
    --   the entry
entryCompletionGetPopupSetWidth completion = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    result <- gtk_entry_completion_get_popup_set_width completion'
    let result' = (/= 0) result
    touchManagedPtr completion
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetPopupSetWidthMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionGetPopupSetWidthMethodInfo a signature where
    overloadedMethod = entryCompletionGetPopupSetWidth

instance O.OverloadedMethodInfo EntryCompletionGetPopupSetWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionGetPopupSetWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionGetPopupSetWidth"
        })


#endif

-- method EntryCompletion::get_popup_single_match
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
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

foreign import ccall "gtk_entry_completion_get_popup_single_match" gtk_entry_completion_get_popup_single_match :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    IO CInt

-- | Returns whether the completion popup window will appear even if there is
-- only a single match.
-- 
-- /Since: 2.8/
entryCompletionGetPopupSingleMatch ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the popup window will appear regardless of the
    --    number of matches
entryCompletionGetPopupSingleMatch completion = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    result <- gtk_entry_completion_get_popup_single_match completion'
    let result' = (/= 0) result
    touchManagedPtr completion
    return result'

#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetPopupSingleMatchMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionGetPopupSingleMatchMethodInfo a signature where
    overloadedMethod = entryCompletionGetPopupSingleMatch

instance O.OverloadedMethodInfo EntryCompletionGetPopupSingleMatchMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionGetPopupSingleMatch",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionGetPopupSingleMatch"
        })


#endif

-- method EntryCompletion::get_text_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
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

foreign import ccall "gtk_entry_completion_get_text_column" gtk_entry_completion_get_text_column :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    IO Int32

-- | Returns the column in the model of /@completion@/ to get strings from.
-- 
-- /Since: 2.6/
entryCompletionGetTextColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> m Int32
    -- ^ __Returns:__ the column containing the strings
entryCompletionGetTextColumn completion = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    result <- gtk_entry_completion_get_text_column completion'
    touchManagedPtr completion
    return result

#if defined(ENABLE_OVERLOADING)
data EntryCompletionGetTextColumnMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionGetTextColumnMethodInfo a signature where
    overloadedMethod = entryCompletionGetTextColumn

instance O.OverloadedMethodInfo EntryCompletionGetTextColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionGetTextColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionGetTextColumn"
        })


#endif

-- method EntryCompletion::insert_action_markup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "index_"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the index of the item to insert"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "markup"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "markup of the item to insert"
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

foreign import ccall "gtk_entry_completion_insert_action_markup" gtk_entry_completion_insert_action_markup :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    Int32 ->                                -- index_ : TBasicType TInt
    CString ->                              -- markup : TBasicType TUTF8
    IO ()

-- | Inserts an action in /@completion@/’s action item list at position /@index_@/
-- with markup /@markup@/.
-- 
-- /Since: 2.4/
entryCompletionInsertActionMarkup ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> Int32
    -- ^ /@index_@/: the index of the item to insert
    -> T.Text
    -- ^ /@markup@/: markup of the item to insert
    -> m ()
entryCompletionInsertActionMarkup completion index_ markup = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    markup' <- textToCString markup
    gtk_entry_completion_insert_action_markup completion' index_ markup'
    touchManagedPtr completion
    freeMem markup'
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryCompletionInsertActionMarkupMethodInfo
instance (signature ~ (Int32 -> T.Text -> m ()), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionInsertActionMarkupMethodInfo a signature where
    overloadedMethod = entryCompletionInsertActionMarkup

instance O.OverloadedMethodInfo EntryCompletionInsertActionMarkupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionInsertActionMarkup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionInsertActionMarkup"
        })


#endif

-- method EntryCompletion::insert_action_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "index_"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the index of the item to insert"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "text of the item to insert"
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

foreign import ccall "gtk_entry_completion_insert_action_text" gtk_entry_completion_insert_action_text :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    Int32 ->                                -- index_ : TBasicType TInt
    CString ->                              -- text : TBasicType TUTF8
    IO ()

-- | Inserts an action in /@completion@/’s action item list at position /@index_@/
-- with text /@text@/. If you want the action item to have markup, use
-- 'GI.Gtk.Objects.EntryCompletion.entryCompletionInsertActionMarkup'.
-- 
-- Note that /@index_@/ is a relative position in the list of actions and
-- the position of an action can change when deleting a different action.
-- 
-- /Since: 2.4/
entryCompletionInsertActionText ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> Int32
    -- ^ /@index_@/: the index of the item to insert
    -> T.Text
    -- ^ /@text@/: text of the item to insert
    -> m ()
entryCompletionInsertActionText completion index_ text = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    text' <- textToCString text
    gtk_entry_completion_insert_action_text completion' index_ text'
    touchManagedPtr completion
    freeMem text'
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryCompletionInsertActionTextMethodInfo
instance (signature ~ (Int32 -> T.Text -> m ()), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionInsertActionTextMethodInfo a signature where
    overloadedMethod = entryCompletionInsertActionText

instance O.OverloadedMethodInfo EntryCompletionInsertActionTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionInsertActionText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionInsertActionText"
        })


#endif

-- method EntryCompletion::insert_prefix
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
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

foreign import ccall "gtk_entry_completion_insert_prefix" gtk_entry_completion_insert_prefix :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    IO ()

-- | Requests a prefix insertion.
-- 
-- /Since: 2.6/
entryCompletionInsertPrefix ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> m ()
entryCompletionInsertPrefix completion = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    gtk_entry_completion_insert_prefix completion'
    touchManagedPtr completion
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryCompletionInsertPrefixMethodInfo
instance (signature ~ (m ()), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionInsertPrefixMethodInfo a signature where
    overloadedMethod = entryCompletionInsertPrefix

instance O.OverloadedMethodInfo EntryCompletionInsertPrefixMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionInsertPrefix",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionInsertPrefix"
        })


#endif

-- method EntryCompletion::set_inline_completion
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "inline_completion"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to do inline completion"
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

foreign import ccall "gtk_entry_completion_set_inline_completion" gtk_entry_completion_set_inline_completion :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    CInt ->                                 -- inline_completion : TBasicType TBoolean
    IO ()

-- | Sets whether the common prefix of the possible completions should
-- be automatically inserted in the entry.
-- 
-- /Since: 2.6/
entryCompletionSetInlineCompletion ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> Bool
    -- ^ /@inlineCompletion@/: 'P.True' to do inline completion
    -> m ()
entryCompletionSetInlineCompletion completion inlineCompletion = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    let inlineCompletion' = (fromIntegral . fromEnum) inlineCompletion
    gtk_entry_completion_set_inline_completion completion' inlineCompletion'
    touchManagedPtr completion
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetInlineCompletionMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionSetInlineCompletionMethodInfo a signature where
    overloadedMethod = entryCompletionSetInlineCompletion

instance O.OverloadedMethodInfo EntryCompletionSetInlineCompletionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionSetInlineCompletion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionSetInlineCompletion"
        })


#endif

-- method EntryCompletion::set_inline_selection
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "inline_selection"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to do inline selection"
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

foreign import ccall "gtk_entry_completion_set_inline_selection" gtk_entry_completion_set_inline_selection :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    CInt ->                                 -- inline_selection : TBasicType TBoolean
    IO ()

-- | Sets whether it is possible to cycle through the possible completions
-- inside the entry.
-- 
-- /Since: 2.12/
entryCompletionSetInlineSelection ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> Bool
    -- ^ /@inlineSelection@/: 'P.True' to do inline selection
    -> m ()
entryCompletionSetInlineSelection completion inlineSelection = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    let inlineSelection' = (fromIntegral . fromEnum) inlineSelection
    gtk_entry_completion_set_inline_selection completion' inlineSelection'
    touchManagedPtr completion
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetInlineSelectionMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionSetInlineSelectionMethodInfo a signature where
    overloadedMethod = entryCompletionSetInlineSelection

instance O.OverloadedMethodInfo EntryCompletionSetInlineSelectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionSetInlineSelection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionSetInlineSelection"
        })


#endif

-- method EntryCompletion::set_match_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "EntryCompletionMatchFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkEntryCompletionMatchFunc to use"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "user data for @func"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "func_notify"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "destroy notify for @func_data."
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

foreign import ccall "gtk_entry_completion_set_match_func" gtk_entry_completion_set_match_func :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    FunPtr Gtk.Callbacks.C_EntryCompletionMatchFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "EntryCompletionMatchFunc"})
    Ptr () ->                               -- func_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- func_notify : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Sets the match function for /@completion@/ to be /@func@/. The match function
-- is used to determine if a row should or should not be in the completion
-- list.
-- 
-- /Since: 2.4/
entryCompletionSetMatchFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> Gtk.Callbacks.EntryCompletionMatchFunc
    -- ^ /@func@/: the t'GI.Gtk.Callbacks.EntryCompletionMatchFunc' to use
    -> m ()
entryCompletionSetMatchFunc completion func = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    func' <- Gtk.Callbacks.mk_EntryCompletionMatchFunc (Gtk.Callbacks.wrap_EntryCompletionMatchFunc Nothing (Gtk.Callbacks.drop_closures_EntryCompletionMatchFunc func))
    let funcData = castFunPtrToPtr func'
    let funcNotify = SP.safeFreeFunPtrPtr
    gtk_entry_completion_set_match_func completion' func' funcData funcNotify
    touchManagedPtr completion
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetMatchFuncMethodInfo
instance (signature ~ (Gtk.Callbacks.EntryCompletionMatchFunc -> m ()), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionSetMatchFuncMethodInfo a signature where
    overloadedMethod = entryCompletionSetMatchFunc

instance O.OverloadedMethodInfo EntryCompletionSetMatchFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionSetMatchFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionSetMatchFunc"
        })


#endif

-- method EntryCompletion::set_minimum_key_length
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "length"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the minimum length of the key in order to start completing"
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

foreign import ccall "gtk_entry_completion_set_minimum_key_length" gtk_entry_completion_set_minimum_key_length :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    Int32 ->                                -- length : TBasicType TInt
    IO ()

-- | Requires the length of the search key for /@completion@/ to be at least
-- /@length@/. This is useful for long lists, where completing using a small
-- key takes a lot of time and will come up with meaningless results anyway
-- (ie, a too large dataset).
-- 
-- /Since: 2.4/
entryCompletionSetMinimumKeyLength ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> Int32
    -- ^ /@length@/: the minimum length of the key in order to start completing
    -> m ()
entryCompletionSetMinimumKeyLength completion length_ = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    gtk_entry_completion_set_minimum_key_length completion' length_
    touchManagedPtr completion
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetMinimumKeyLengthMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionSetMinimumKeyLengthMethodInfo a signature where
    overloadedMethod = entryCompletionSetMinimumKeyLength

instance O.OverloadedMethodInfo EntryCompletionSetMinimumKeyLengthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionSetMinimumKeyLength",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionSetMinimumKeyLength"
        })


#endif

-- method EntryCompletion::set_model
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkTreeModel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_entry_completion_set_model" gtk_entry_completion_set_model :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    Ptr Gtk.TreeModel.TreeModel ->          -- model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    IO ()

-- | Sets the model for a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'. If /@completion@/ already has
-- a model set, it will remove it before setting the new model.
-- If model is 'P.Nothing', then it will unset the model.
-- 
-- /Since: 2.4/
entryCompletionSetModel ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a, Gtk.TreeModel.IsTreeModel b) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> Maybe (b)
    -- ^ /@model@/: the t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> m ()
entryCompletionSetModel completion model = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    maybeModel <- case model of
        Nothing -> return nullPtr
        Just jModel -> do
            jModel' <- unsafeManagedPtrCastPtr jModel
            return jModel'
    gtk_entry_completion_set_model completion' maybeModel
    touchManagedPtr completion
    whenJust model touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetModelMethodInfo
instance (signature ~ (Maybe (b) -> m ()), MonadIO m, IsEntryCompletion a, Gtk.TreeModel.IsTreeModel b) => O.OverloadedMethod EntryCompletionSetModelMethodInfo a signature where
    overloadedMethod = entryCompletionSetModel

instance O.OverloadedMethodInfo EntryCompletionSetModelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionSetModel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionSetModel"
        })


#endif

-- method EntryCompletion::set_popup_completion
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "popup_completion"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "%TRUE to do popup completion"
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

foreign import ccall "gtk_entry_completion_set_popup_completion" gtk_entry_completion_set_popup_completion :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    CInt ->                                 -- popup_completion : TBasicType TBoolean
    IO ()

-- | Sets whether the completions should be presented in a popup window.
-- 
-- /Since: 2.6/
entryCompletionSetPopupCompletion ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> Bool
    -- ^ /@popupCompletion@/: 'P.True' to do popup completion
    -> m ()
entryCompletionSetPopupCompletion completion popupCompletion = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    let popupCompletion' = (fromIntegral . fromEnum) popupCompletion
    gtk_entry_completion_set_popup_completion completion' popupCompletion'
    touchManagedPtr completion
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetPopupCompletionMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionSetPopupCompletionMethodInfo a signature where
    overloadedMethod = entryCompletionSetPopupCompletion

instance O.OverloadedMethodInfo EntryCompletionSetPopupCompletionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionSetPopupCompletion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionSetPopupCompletion"
        })


#endif

-- method EntryCompletion::set_popup_set_width
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "popup_set_width"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%TRUE to make the width of the popup the same as the entry"
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

foreign import ccall "gtk_entry_completion_set_popup_set_width" gtk_entry_completion_set_popup_set_width :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    CInt ->                                 -- popup_set_width : TBasicType TBoolean
    IO ()

-- | Sets whether the completion popup window will be resized to be the same
-- width as the entry.
-- 
-- /Since: 2.8/
entryCompletionSetPopupSetWidth ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> Bool
    -- ^ /@popupSetWidth@/: 'P.True' to make the width of the popup the same as the entry
    -> m ()
entryCompletionSetPopupSetWidth completion popupSetWidth = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    let popupSetWidth' = (fromIntegral . fromEnum) popupSetWidth
    gtk_entry_completion_set_popup_set_width completion' popupSetWidth'
    touchManagedPtr completion
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetPopupSetWidthMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionSetPopupSetWidthMethodInfo a signature where
    overloadedMethod = entryCompletionSetPopupSetWidth

instance O.OverloadedMethodInfo EntryCompletionSetPopupSetWidthMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionSetPopupSetWidth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionSetPopupSetWidth"
        })


#endif

-- method EntryCompletion::set_popup_single_match
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "popup_single_match"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "%TRUE if the popup should appear even for a single\n    match"
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

foreign import ccall "gtk_entry_completion_set_popup_single_match" gtk_entry_completion_set_popup_single_match :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    CInt ->                                 -- popup_single_match : TBasicType TBoolean
    IO ()

-- | Sets whether the completion popup window will appear even if there is
-- only a single match. You may want to set this to 'P.False' if you
-- are using [inline completion][GtkEntryCompletion--inline-completion].
-- 
-- /Since: 2.8/
entryCompletionSetPopupSingleMatch ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> Bool
    -- ^ /@popupSingleMatch@/: 'P.True' if the popup should appear even for a single
    --     match
    -> m ()
entryCompletionSetPopupSingleMatch completion popupSingleMatch = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    let popupSingleMatch' = (fromIntegral . fromEnum) popupSingleMatch
    gtk_entry_completion_set_popup_single_match completion' popupSingleMatch'
    touchManagedPtr completion
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetPopupSingleMatchMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionSetPopupSingleMatchMethodInfo a signature where
    overloadedMethod = entryCompletionSetPopupSingleMatch

instance O.OverloadedMethodInfo EntryCompletionSetPopupSingleMatchMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionSetPopupSingleMatch",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionSetPopupSingleMatch"
        })


#endif

-- method EntryCompletion::set_text_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "completion"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "EntryCompletion" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEntryCompletion"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "column"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the column in the model of @completion to get strings from"
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

foreign import ccall "gtk_entry_completion_set_text_column" gtk_entry_completion_set_text_column :: 
    Ptr EntryCompletion ->                  -- completion : TInterface (Name {namespace = "Gtk", name = "EntryCompletion"})
    Int32 ->                                -- column : TBasicType TInt
    IO ()

-- | Convenience function for setting up the most used case of this code: a
-- completion list with just strings. This function will set up /@completion@/
-- to have a list displaying all (and just) strings in the completion list,
-- and to get those strings from /@column@/ in the model of /@completion@/.
-- 
-- This functions creates and adds a t'GI.Gtk.Objects.CellRendererText.CellRendererText' for the selected
-- column. If you need to set the text column, but don\'t want the cell
-- renderer, use @/g_object_set()/@ to set the [EntryCompletion:textColumn]("GI.Gtk.Objects.EntryCompletion#g:attr:textColumn")
-- property directly.
-- 
-- /Since: 2.4/
entryCompletionSetTextColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsEntryCompletion a) =>
    a
    -- ^ /@completion@/: a t'GI.Gtk.Objects.EntryCompletion.EntryCompletion'
    -> Int32
    -- ^ /@column@/: the column in the model of /@completion@/ to get strings from
    -> m ()
entryCompletionSetTextColumn completion column = liftIO $ do
    completion' <- unsafeManagedPtrCastPtr completion
    gtk_entry_completion_set_text_column completion' column
    touchManagedPtr completion
    return ()

#if defined(ENABLE_OVERLOADING)
data EntryCompletionSetTextColumnMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsEntryCompletion a) => O.OverloadedMethod EntryCompletionSetTextColumnMethodInfo a signature where
    overloadedMethod = entryCompletionSetTextColumn

instance O.OverloadedMethodInfo EntryCompletionSetTextColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EntryCompletion.entryCompletionSetTextColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EntryCompletion.html#v:entryCompletionSetTextColumn"
        })


#endif


