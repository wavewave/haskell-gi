{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Interfaces.Editable.Editable' interface is an interface which should be implemented by
-- text editing widgets, such as t'GI.Gtk.Objects.Entry.Entry' and t'GI.Gtk.Objects.SpinButton.SpinButton'. It contains functions
-- for generically manipulating an editable widget, a large number of action
-- signals used for key bindings, and several signals that an application can
-- connect to to modify the behavior of a widget.
-- 
-- As an example of the latter usage, by connecting
-- the following handler to [Editable::insertText]("GI.Gtk.Interfaces.Editable#g:signal:insertText"), an application
-- can convert all entry into a widget into uppercase.
-- 
-- == Forcing entry to uppercase.
-- 
-- 
-- === /C code/
-- >
-- >#include <ctype.h>;
-- >
-- >void
-- >insert_text_handler (GtkEditable *editable,
-- >                     const gchar *text,
-- >                     gint         length,
-- >                     gint        *position,
-- >                     gpointer     data)
-- >{
-- >  gchar *result = g_utf8_strup (text, length);
-- >
-- >  g_signal_handlers_block_by_func (editable,
-- >                               (gpointer) insert_text_handler, data);
-- >  gtk_editable_insert_text (editable, result, length, position);
-- >  g_signal_handlers_unblock_by_func (editable,
-- >                                     (gpointer) insert_text_handler, data);
-- >
-- >  g_signal_stop_emission_by_name (editable, "insert_text");
-- >
-- >  g_free (result);
-- >}
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Interfaces.Editable
    ( 

-- * Exported types
    Editable(..)                            ,
    IsEditable                              ,
    toEditable                              ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [copyClipboard]("GI.Gtk.Interfaces.Editable#g:method:copyClipboard"), [cutClipboard]("GI.Gtk.Interfaces.Editable#g:method:cutClipboard"), [deleteSelection]("GI.Gtk.Interfaces.Editable#g:method:deleteSelection"), [deleteText]("GI.Gtk.Interfaces.Editable#g:method:deleteText"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [insertText]("GI.Gtk.Interfaces.Editable#g:method:insertText"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [pasteClipboard]("GI.Gtk.Interfaces.Editable#g:method:pasteClipboard"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [selectRegion]("GI.Gtk.Interfaces.Editable#g:method:selectRegion"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getChars]("GI.Gtk.Interfaces.Editable#g:method:getChars"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getEditable]("GI.Gtk.Interfaces.Editable#g:method:getEditable"), [getPosition]("GI.Gtk.Interfaces.Editable#g:method:getPosition"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSelectionBounds]("GI.Gtk.Interfaces.Editable#g:method:getSelectionBounds").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setEditable]("GI.Gtk.Interfaces.Editable#g:method:setEditable"), [setPosition]("GI.Gtk.Interfaces.Editable#g:method:setPosition"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveEditableMethod                   ,
#endif

-- ** copyClipboard #method:copyClipboard#

#if defined(ENABLE_OVERLOADING)
    EditableCopyClipboardMethodInfo         ,
#endif
    editableCopyClipboard                   ,


-- ** cutClipboard #method:cutClipboard#

#if defined(ENABLE_OVERLOADING)
    EditableCutClipboardMethodInfo          ,
#endif
    editableCutClipboard                    ,


-- ** deleteSelection #method:deleteSelection#

#if defined(ENABLE_OVERLOADING)
    EditableDeleteSelectionMethodInfo       ,
#endif
    editableDeleteSelection                 ,


-- ** deleteText #method:deleteText#

#if defined(ENABLE_OVERLOADING)
    EditableDeleteTextMethodInfo            ,
#endif
    editableDeleteText                      ,


-- ** getChars #method:getChars#

#if defined(ENABLE_OVERLOADING)
    EditableGetCharsMethodInfo              ,
#endif
    editableGetChars                        ,


-- ** getEditable #method:getEditable#

#if defined(ENABLE_OVERLOADING)
    EditableGetEditableMethodInfo           ,
#endif
    editableGetEditable                     ,


-- ** getPosition #method:getPosition#

#if defined(ENABLE_OVERLOADING)
    EditableGetPositionMethodInfo           ,
#endif
    editableGetPosition                     ,


-- ** getSelectionBounds #method:getSelectionBounds#

#if defined(ENABLE_OVERLOADING)
    EditableGetSelectionBoundsMethodInfo    ,
#endif
    editableGetSelectionBounds              ,


-- ** insertText #method:insertText#

#if defined(ENABLE_OVERLOADING)
    EditableInsertTextMethodInfo            ,
#endif
    editableInsertText                      ,


-- ** pasteClipboard #method:pasteClipboard#

#if defined(ENABLE_OVERLOADING)
    EditablePasteClipboardMethodInfo        ,
#endif
    editablePasteClipboard                  ,


-- ** selectRegion #method:selectRegion#

#if defined(ENABLE_OVERLOADING)
    EditableSelectRegionMethodInfo          ,
#endif
    editableSelectRegion                    ,


-- ** setEditable #method:setEditable#

#if defined(ENABLE_OVERLOADING)
    EditableSetEditableMethodInfo           ,
#endif
    editableSetEditable                     ,


-- ** setPosition #method:setPosition#

#if defined(ENABLE_OVERLOADING)
    EditableSetPositionMethodInfo           ,
#endif
    editableSetPosition                     ,




 -- * Signals


-- ** changed #signal:changed#

    EditableChangedCallback                 ,
#if defined(ENABLE_OVERLOADING)
    EditableChangedSignalInfo               ,
#endif
    afterEditableChanged                    ,
    onEditableChanged                       ,


-- ** deleteText #signal:deleteText#

    EditableDeleteTextCallback              ,
#if defined(ENABLE_OVERLOADING)
    EditableDeleteTextSignalInfo            ,
#endif
    afterEditableDeleteText                 ,
    onEditableDeleteText                    ,


-- ** insertText #signal:insertText#

    EditableInsertTextCallback              ,
#if defined(ENABLE_OVERLOADING)
    EditableInsertTextSignalInfo            ,
#endif
    afterEditableInsertText                 ,
    onEditableInsertText                    ,




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

-- interface Editable 
-- | Memory-managed wrapper type.
newtype Editable = Editable (SP.ManagedPtr Editable)
    deriving (Eq)

instance SP.ManagedPtrNewtype Editable where
    toManagedPtr (Editable p) = p

foreign import ccall "gtk_editable_get_type"
    c_gtk_editable_get_type :: IO B.Types.GType

instance B.Types.TypedObject Editable where
    glibType = c_gtk_editable_get_type

instance B.Types.GObject Editable

-- | Type class for types which can be safely cast to `Editable`, for instance with `toEditable`.
class (SP.GObject o, O.IsDescendantOf Editable o) => IsEditable o
instance (SP.GObject o, O.IsDescendantOf Editable o) => IsEditable o

instance O.HasParentTypes Editable
type instance O.ParentTypes Editable = '[GObject.Object.Object]

-- | Cast to `Editable`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toEditable :: (MIO.MonadIO m, IsEditable o) => o -> m Editable
toEditable = MIO.liftIO . B.ManagedPtr.unsafeCastTo Editable

-- | Convert 'Editable' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Editable) where
    gvalueGType_ = c_gtk_editable_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Editable)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Editable)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Editable ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Editable
type instance O.AttributeList Editable = EditableAttributeList
type EditableAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveEditableMethod (t :: Symbol) (o :: *) :: * where
    ResolveEditableMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveEditableMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveEditableMethod "copyClipboard" o = EditableCopyClipboardMethodInfo
    ResolveEditableMethod "cutClipboard" o = EditableCutClipboardMethodInfo
    ResolveEditableMethod "deleteSelection" o = EditableDeleteSelectionMethodInfo
    ResolveEditableMethod "deleteText" o = EditableDeleteTextMethodInfo
    ResolveEditableMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveEditableMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveEditableMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveEditableMethod "insertText" o = EditableInsertTextMethodInfo
    ResolveEditableMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveEditableMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveEditableMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveEditableMethod "pasteClipboard" o = EditablePasteClipboardMethodInfo
    ResolveEditableMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveEditableMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveEditableMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveEditableMethod "selectRegion" o = EditableSelectRegionMethodInfo
    ResolveEditableMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveEditableMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveEditableMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveEditableMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveEditableMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveEditableMethod "getChars" o = EditableGetCharsMethodInfo
    ResolveEditableMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveEditableMethod "getEditable" o = EditableGetEditableMethodInfo
    ResolveEditableMethod "getPosition" o = EditableGetPositionMethodInfo
    ResolveEditableMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveEditableMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveEditableMethod "getSelectionBounds" o = EditableGetSelectionBoundsMethodInfo
    ResolveEditableMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveEditableMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveEditableMethod "setEditable" o = EditableSetEditableMethodInfo
    ResolveEditableMethod "setPosition" o = EditableSetPositionMethodInfo
    ResolveEditableMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveEditableMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveEditableMethod t Editable, O.OverloadedMethod info Editable p) => OL.IsLabel t (Editable -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveEditableMethod t Editable, O.OverloadedMethod info Editable p, R.HasField t Editable p) => R.HasField t Editable p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveEditableMethod t Editable, O.OverloadedMethodInfo info Editable) => OL.IsLabel t (O.MethodProxy info Editable) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- method Editable::copy_clipboard
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "editable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Editable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEditable" , sinceVersion = Nothing }
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

foreign import ccall "gtk_editable_copy_clipboard" gtk_editable_copy_clipboard :: 
    Ptr Editable ->                         -- editable : TInterface (Name {namespace = "Gtk", name = "Editable"})
    IO ()

-- | Copies the contents of the currently selected content in the editable and
-- puts it on the clipboard.
editableCopyClipboard ::
    (B.CallStack.HasCallStack, MonadIO m, IsEditable a) =>
    a
    -- ^ /@editable@/: a t'GI.Gtk.Interfaces.Editable.Editable'
    -> m ()
editableCopyClipboard editable = liftIO $ do
    editable' <- unsafeManagedPtrCastPtr editable
    gtk_editable_copy_clipboard editable'
    touchManagedPtr editable
    return ()

#if defined(ENABLE_OVERLOADING)
data EditableCopyClipboardMethodInfo
instance (signature ~ (m ()), MonadIO m, IsEditable a) => O.OverloadedMethod EditableCopyClipboardMethodInfo a signature where
    overloadedMethod = editableCopyClipboard

instance O.OverloadedMethodInfo EditableCopyClipboardMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Editable.editableCopyClipboard",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Editable.html#v:editableCopyClipboard"
        })


#endif

-- method Editable::cut_clipboard
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "editable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Editable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEditable" , sinceVersion = Nothing }
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

foreign import ccall "gtk_editable_cut_clipboard" gtk_editable_cut_clipboard :: 
    Ptr Editable ->                         -- editable : TInterface (Name {namespace = "Gtk", name = "Editable"})
    IO ()

-- | Removes the contents of the currently selected content in the editable and
-- puts it on the clipboard.
editableCutClipboard ::
    (B.CallStack.HasCallStack, MonadIO m, IsEditable a) =>
    a
    -- ^ /@editable@/: a t'GI.Gtk.Interfaces.Editable.Editable'
    -> m ()
editableCutClipboard editable = liftIO $ do
    editable' <- unsafeManagedPtrCastPtr editable
    gtk_editable_cut_clipboard editable'
    touchManagedPtr editable
    return ()

#if defined(ENABLE_OVERLOADING)
data EditableCutClipboardMethodInfo
instance (signature ~ (m ()), MonadIO m, IsEditable a) => O.OverloadedMethod EditableCutClipboardMethodInfo a signature where
    overloadedMethod = editableCutClipboard

instance O.OverloadedMethodInfo EditableCutClipboardMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Editable.editableCutClipboard",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Editable.html#v:editableCutClipboard"
        })


#endif

-- method Editable::delete_selection
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "editable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Editable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEditable" , sinceVersion = Nothing }
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

foreign import ccall "gtk_editable_delete_selection" gtk_editable_delete_selection :: 
    Ptr Editable ->                         -- editable : TInterface (Name {namespace = "Gtk", name = "Editable"})
    IO ()

-- | Deletes the currently selected text of the editable.
-- This call doesn’t do anything if there is no selected text.
editableDeleteSelection ::
    (B.CallStack.HasCallStack, MonadIO m, IsEditable a) =>
    a
    -- ^ /@editable@/: a t'GI.Gtk.Interfaces.Editable.Editable'
    -> m ()
editableDeleteSelection editable = liftIO $ do
    editable' <- unsafeManagedPtrCastPtr editable
    gtk_editable_delete_selection editable'
    touchManagedPtr editable
    return ()

#if defined(ENABLE_OVERLOADING)
data EditableDeleteSelectionMethodInfo
instance (signature ~ (m ()), MonadIO m, IsEditable a) => O.OverloadedMethod EditableDeleteSelectionMethodInfo a signature where
    overloadedMethod = editableDeleteSelection

instance O.OverloadedMethodInfo EditableDeleteSelectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Editable.editableDeleteSelection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Editable.html#v:editableDeleteSelection"
        })


#endif

-- method Editable::delete_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "editable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Editable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEditable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start_pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "start position" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end_pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "end position" , sinceVersion = Nothing }
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

foreign import ccall "gtk_editable_delete_text" gtk_editable_delete_text :: 
    Ptr Editable ->                         -- editable : TInterface (Name {namespace = "Gtk", name = "Editable"})
    Int32 ->                                -- start_pos : TBasicType TInt
    Int32 ->                                -- end_pos : TBasicType TInt
    IO ()

-- | Deletes a sequence of characters. The characters that are deleted are
-- those characters at positions from /@startPos@/ up to, but not including
-- /@endPos@/. If /@endPos@/ is negative, then the characters deleted
-- are those from /@startPos@/ to the end of the text.
-- 
-- Note that the positions are specified in characters, not bytes.
editableDeleteText ::
    (B.CallStack.HasCallStack, MonadIO m, IsEditable a) =>
    a
    -- ^ /@editable@/: a t'GI.Gtk.Interfaces.Editable.Editable'
    -> Int32
    -- ^ /@startPos@/: start position
    -> Int32
    -- ^ /@endPos@/: end position
    -> m ()
editableDeleteText editable startPos endPos = liftIO $ do
    editable' <- unsafeManagedPtrCastPtr editable
    gtk_editable_delete_text editable' startPos endPos
    touchManagedPtr editable
    return ()

#if defined(ENABLE_OVERLOADING)
data EditableDeleteTextMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsEditable a) => O.OverloadedMethod EditableDeleteTextMethodInfo a signature where
    overloadedMethod = editableDeleteText

instance O.OverloadedMethodInfo EditableDeleteTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Editable.editableDeleteText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Editable.html#v:editableDeleteText"
        })


#endif

-- method Editable::get_chars
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "editable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Editable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEditable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start_pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "start of text" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end_pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "end of text" , sinceVersion = Nothing }
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

foreign import ccall "gtk_editable_get_chars" gtk_editable_get_chars :: 
    Ptr Editable ->                         -- editable : TInterface (Name {namespace = "Gtk", name = "Editable"})
    Int32 ->                                -- start_pos : TBasicType TInt
    Int32 ->                                -- end_pos : TBasicType TInt
    IO CString

-- | Retrieves a sequence of characters. The characters that are retrieved
-- are those characters at positions from /@startPos@/ up to, but not
-- including /@endPos@/. If /@endPos@/ is negative, then the characters
-- retrieved are those characters from /@startPos@/ to the end of the text.
-- 
-- Note that positions are specified in characters, not bytes.
editableGetChars ::
    (B.CallStack.HasCallStack, MonadIO m, IsEditable a) =>
    a
    -- ^ /@editable@/: a t'GI.Gtk.Interfaces.Editable.Editable'
    -> Int32
    -- ^ /@startPos@/: start of text
    -> Int32
    -- ^ /@endPos@/: end of text
    -> m T.Text
    -- ^ __Returns:__ a pointer to the contents of the widget as a
    --      string. This string is allocated by the t'GI.Gtk.Interfaces.Editable.Editable'
    --      implementation and should be freed by the caller.
editableGetChars editable startPos endPos = liftIO $ do
    editable' <- unsafeManagedPtrCastPtr editable
    result <- gtk_editable_get_chars editable' startPos endPos
    checkUnexpectedReturnNULL "editableGetChars" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr editable
    return result'

#if defined(ENABLE_OVERLOADING)
data EditableGetCharsMethodInfo
instance (signature ~ (Int32 -> Int32 -> m T.Text), MonadIO m, IsEditable a) => O.OverloadedMethod EditableGetCharsMethodInfo a signature where
    overloadedMethod = editableGetChars

instance O.OverloadedMethodInfo EditableGetCharsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Editable.editableGetChars",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Editable.html#v:editableGetChars"
        })


#endif

-- method Editable::get_editable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "editable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Editable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEditable" , sinceVersion = Nothing }
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

foreign import ccall "gtk_editable_get_editable" gtk_editable_get_editable :: 
    Ptr Editable ->                         -- editable : TInterface (Name {namespace = "Gtk", name = "Editable"})
    IO CInt

-- | Retrieves whether /@editable@/ is editable. See
-- 'GI.Gtk.Interfaces.Editable.editableSetEditable'.
editableGetEditable ::
    (B.CallStack.HasCallStack, MonadIO m, IsEditable a) =>
    a
    -- ^ /@editable@/: a t'GI.Gtk.Interfaces.Editable.Editable'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@editable@/ is editable.
editableGetEditable editable = liftIO $ do
    editable' <- unsafeManagedPtrCastPtr editable
    result <- gtk_editable_get_editable editable'
    let result' = (/= 0) result
    touchManagedPtr editable
    return result'

#if defined(ENABLE_OVERLOADING)
data EditableGetEditableMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsEditable a) => O.OverloadedMethod EditableGetEditableMethodInfo a signature where
    overloadedMethod = editableGetEditable

instance O.OverloadedMethodInfo EditableGetEditableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Editable.editableGetEditable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Editable.html#v:editableGetEditable"
        })


#endif

-- method Editable::get_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "editable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Editable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEditable" , sinceVersion = Nothing }
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

foreign import ccall "gtk_editable_get_position" gtk_editable_get_position :: 
    Ptr Editable ->                         -- editable : TInterface (Name {namespace = "Gtk", name = "Editable"})
    IO Int32

-- | Retrieves the current position of the cursor relative to the start
-- of the content of the editable.
-- 
-- Note that this position is in characters, not in bytes.
editableGetPosition ::
    (B.CallStack.HasCallStack, MonadIO m, IsEditable a) =>
    a
    -- ^ /@editable@/: a t'GI.Gtk.Interfaces.Editable.Editable'
    -> m Int32
    -- ^ __Returns:__ the cursor position
editableGetPosition editable = liftIO $ do
    editable' <- unsafeManagedPtrCastPtr editable
    result <- gtk_editable_get_position editable'
    touchManagedPtr editable
    return result

#if defined(ENABLE_OVERLOADING)
data EditableGetPositionMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsEditable a) => O.OverloadedMethod EditableGetPositionMethodInfo a signature where
    overloadedMethod = editableGetPosition

instance O.OverloadedMethodInfo EditableGetPositionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Editable.editableGetPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Editable.html#v:editableGetPosition"
        })


#endif

-- method Editable::get_selection_bounds
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "editable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Editable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEditable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start_pos"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store the starting position, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "end_pos"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "location to store the end position, or %NULL"
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

foreign import ccall "gtk_editable_get_selection_bounds" gtk_editable_get_selection_bounds :: 
    Ptr Editable ->                         -- editable : TInterface (Name {namespace = "Gtk", name = "Editable"})
    Ptr Int32 ->                            -- start_pos : TBasicType TInt
    Ptr Int32 ->                            -- end_pos : TBasicType TInt
    IO CInt

-- | Retrieves the selection bound of the editable. start_pos will be filled
-- with the start of the selection and /@endPos@/ with end. If no text was
-- selected both will be identical and 'P.False' will be returned.
-- 
-- Note that positions are specified in characters, not bytes.
editableGetSelectionBounds ::
    (B.CallStack.HasCallStack, MonadIO m, IsEditable a) =>
    a
    -- ^ /@editable@/: a t'GI.Gtk.Interfaces.Editable.Editable'
    -> m ((Bool, Int32, Int32))
    -- ^ __Returns:__ 'P.True' if an area is selected, 'P.False' otherwise
editableGetSelectionBounds editable = liftIO $ do
    editable' <- unsafeManagedPtrCastPtr editable
    startPos <- allocMem :: IO (Ptr Int32)
    endPos <- allocMem :: IO (Ptr Int32)
    result <- gtk_editable_get_selection_bounds editable' startPos endPos
    let result' = (/= 0) result
    startPos' <- peek startPos
    endPos' <- peek endPos
    touchManagedPtr editable
    freeMem startPos
    freeMem endPos
    return (result', startPos', endPos')

#if defined(ENABLE_OVERLOADING)
data EditableGetSelectionBoundsMethodInfo
instance (signature ~ (m ((Bool, Int32, Int32))), MonadIO m, IsEditable a) => O.OverloadedMethod EditableGetSelectionBoundsMethodInfo a signature where
    overloadedMethod = editableGetSelectionBounds

instance O.OverloadedMethodInfo EditableGetSelectionBoundsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Editable.editableGetSelectionBounds",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Editable.html#v:editableGetSelectionBounds"
        })


#endif

-- method Editable::insert_text
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "editable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Editable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEditable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "new_text"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the text to append" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "new_text_length"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the length of the text in bytes, or -1"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "position"
--           , argType = TBasicType TInt
--           , direction = DirectionInout
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location of the position text will be inserted at"
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

foreign import ccall "gtk_editable_insert_text" gtk_editable_insert_text :: 
    Ptr Editable ->                         -- editable : TInterface (Name {namespace = "Gtk", name = "Editable"})
    CString ->                              -- new_text : TBasicType TUTF8
    Int32 ->                                -- new_text_length : TBasicType TInt
    Ptr Int32 ->                            -- position : TBasicType TInt
    IO ()

-- | Inserts /@newTextLength@/ bytes of /@newText@/ into the contents of the
-- widget, at position /@position@/.
-- 
-- Note that the position is in characters, not in bytes.
-- The function updates /@position@/ to point after the newly inserted text.
editableInsertText ::
    (B.CallStack.HasCallStack, MonadIO m, IsEditable a) =>
    a
    -- ^ /@editable@/: a t'GI.Gtk.Interfaces.Editable.Editable'
    -> T.Text
    -- ^ /@newText@/: the text to append
    -> Int32
    -- ^ /@newTextLength@/: the length of the text in bytes, or -1
    -> Int32
    -- ^ /@position@/: location of the position text will be inserted at
    -> m (Int32)
editableInsertText editable newText newTextLength position = liftIO $ do
    editable' <- unsafeManagedPtrCastPtr editable
    newText' <- textToCString newText
    position' <- allocMem :: IO (Ptr Int32)
    poke position' position
    gtk_editable_insert_text editable' newText' newTextLength position'
    position'' <- peek position'
    touchManagedPtr editable
    freeMem newText'
    freeMem position'
    return position''

#if defined(ENABLE_OVERLOADING)
data EditableInsertTextMethodInfo
instance (signature ~ (T.Text -> Int32 -> Int32 -> m (Int32)), MonadIO m, IsEditable a) => O.OverloadedMethod EditableInsertTextMethodInfo a signature where
    overloadedMethod = editableInsertText

instance O.OverloadedMethodInfo EditableInsertTextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Editable.editableInsertText",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Editable.html#v:editableInsertText"
        })


#endif

-- method Editable::paste_clipboard
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "editable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Editable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEditable" , sinceVersion = Nothing }
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

foreign import ccall "gtk_editable_paste_clipboard" gtk_editable_paste_clipboard :: 
    Ptr Editable ->                         -- editable : TInterface (Name {namespace = "Gtk", name = "Editable"})
    IO ()

-- | Pastes the content of the clipboard to the current position of the
-- cursor in the editable.
editablePasteClipboard ::
    (B.CallStack.HasCallStack, MonadIO m, IsEditable a) =>
    a
    -- ^ /@editable@/: a t'GI.Gtk.Interfaces.Editable.Editable'
    -> m ()
editablePasteClipboard editable = liftIO $ do
    editable' <- unsafeManagedPtrCastPtr editable
    gtk_editable_paste_clipboard editable'
    touchManagedPtr editable
    return ()

#if defined(ENABLE_OVERLOADING)
data EditablePasteClipboardMethodInfo
instance (signature ~ (m ()), MonadIO m, IsEditable a) => O.OverloadedMethod EditablePasteClipboardMethodInfo a signature where
    overloadedMethod = editablePasteClipboard

instance O.OverloadedMethodInfo EditablePasteClipboardMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Editable.editablePasteClipboard",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Editable.html#v:editablePasteClipboard"
        })


#endif

-- method Editable::select_region
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "editable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Editable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEditable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "start_pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "start of region" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "end_pos"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "end of region" , sinceVersion = Nothing }
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

foreign import ccall "gtk_editable_select_region" gtk_editable_select_region :: 
    Ptr Editable ->                         -- editable : TInterface (Name {namespace = "Gtk", name = "Editable"})
    Int32 ->                                -- start_pos : TBasicType TInt
    Int32 ->                                -- end_pos : TBasicType TInt
    IO ()

-- | Selects a region of text. The characters that are selected are
-- those characters at positions from /@startPos@/ up to, but not
-- including /@endPos@/. If /@endPos@/ is negative, then the
-- characters selected are those characters from /@startPos@/ to
-- the end of the text.
-- 
-- Note that positions are specified in characters, not bytes.
editableSelectRegion ::
    (B.CallStack.HasCallStack, MonadIO m, IsEditable a) =>
    a
    -- ^ /@editable@/: a t'GI.Gtk.Interfaces.Editable.Editable'
    -> Int32
    -- ^ /@startPos@/: start of region
    -> Int32
    -- ^ /@endPos@/: end of region
    -> m ()
editableSelectRegion editable startPos endPos = liftIO $ do
    editable' <- unsafeManagedPtrCastPtr editable
    gtk_editable_select_region editable' startPos endPos
    touchManagedPtr editable
    return ()

#if defined(ENABLE_OVERLOADING)
data EditableSelectRegionMethodInfo
instance (signature ~ (Int32 -> Int32 -> m ()), MonadIO m, IsEditable a) => O.OverloadedMethod EditableSelectRegionMethodInfo a signature where
    overloadedMethod = editableSelectRegion

instance O.OverloadedMethodInfo EditableSelectRegionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Editable.editableSelectRegion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Editable.html#v:editableSelectRegion"
        })


#endif

-- method Editable::set_editable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "editable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Editable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEditable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "is_editable"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "%TRUE if the user is allowed to edit the text\n  in the widget"
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

foreign import ccall "gtk_editable_set_editable" gtk_editable_set_editable :: 
    Ptr Editable ->                         -- editable : TInterface (Name {namespace = "Gtk", name = "Editable"})
    CInt ->                                 -- is_editable : TBasicType TBoolean
    IO ()

-- | Determines if the user can edit the text in the editable
-- widget or not.
editableSetEditable ::
    (B.CallStack.HasCallStack, MonadIO m, IsEditable a) =>
    a
    -- ^ /@editable@/: a t'GI.Gtk.Interfaces.Editable.Editable'
    -> Bool
    -- ^ /@isEditable@/: 'P.True' if the user is allowed to edit the text
    --   in the widget
    -> m ()
editableSetEditable editable isEditable = liftIO $ do
    editable' <- unsafeManagedPtrCastPtr editable
    let isEditable' = (fromIntegral . fromEnum) isEditable
    gtk_editable_set_editable editable' isEditable'
    touchManagedPtr editable
    return ()

#if defined(ENABLE_OVERLOADING)
data EditableSetEditableMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsEditable a) => O.OverloadedMethod EditableSetEditableMethodInfo a signature where
    overloadedMethod = editableSetEditable

instance O.OverloadedMethodInfo EditableSetEditableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Editable.editableSetEditable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Editable.html#v:editableSetEditable"
        })


#endif

-- method Editable::set_position
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "editable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "Editable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkEditable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "position"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the position of the cursor"
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

foreign import ccall "gtk_editable_set_position" gtk_editable_set_position :: 
    Ptr Editable ->                         -- editable : TInterface (Name {namespace = "Gtk", name = "Editable"})
    Int32 ->                                -- position : TBasicType TInt
    IO ()

-- | Sets the cursor position in the editable to the given value.
-- 
-- The cursor is displayed before the character with the given (base 0)
-- index in the contents of the editable. The value must be less than or
-- equal to the number of characters in the editable. A value of -1
-- indicates that the position should be set after the last character
-- of the editable. Note that /@position@/ is in characters, not in bytes.
editableSetPosition ::
    (B.CallStack.HasCallStack, MonadIO m, IsEditable a) =>
    a
    -- ^ /@editable@/: a t'GI.Gtk.Interfaces.Editable.Editable'
    -> Int32
    -- ^ /@position@/: the position of the cursor
    -> m ()
editableSetPosition editable position = liftIO $ do
    editable' <- unsafeManagedPtrCastPtr editable
    gtk_editable_set_position editable' position
    touchManagedPtr editable
    return ()

#if defined(ENABLE_OVERLOADING)
data EditableSetPositionMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsEditable a) => O.OverloadedMethod EditableSetPositionMethodInfo a signature where
    overloadedMethod = editableSetPosition

instance O.OverloadedMethodInfo EditableSetPositionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Editable.editableSetPosition",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Editable.html#v:editableSetPosition"
        })


#endif

-- signal Editable::changed
-- | The [changed](#g:signal:changed) signal is emitted at the end of a single
-- user-visible operation on the contents of the t'GI.Gtk.Interfaces.Editable.Editable'.
-- 
-- E.g., a paste operation that replaces the contents of the
-- selection will cause only one signal emission (even though it
-- is implemented by first deleting the selection, then inserting
-- the new content, and may cause multiple [notify](#g:signal:notify)[text](#g:signal:text) signals
-- to be emitted).
type EditableChangedCallback =
    IO ()

type C_EditableChangedCallback =
    Ptr Editable ->                         -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EditableChangedCallback`.
foreign import ccall "wrapper"
    mk_EditableChangedCallback :: C_EditableChangedCallback -> IO (FunPtr C_EditableChangedCallback)

wrap_EditableChangedCallback :: 
    GObject a => (a -> EditableChangedCallback) ->
    C_EditableChangedCallback
wrap_EditableChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' editable #changed callback
-- @
-- 
-- 
onEditableChanged :: (IsEditable a, MonadIO m) => a -> ((?self :: a) => EditableChangedCallback) -> m SignalHandlerId
onEditableChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EditableChangedCallback wrapped
    wrapped'' <- mk_EditableChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' editable #changed callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEditableChanged :: (IsEditable a, MonadIO m) => a -> ((?self :: a) => EditableChangedCallback) -> m SignalHandlerId
afterEditableChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EditableChangedCallback wrapped
    wrapped'' <- mk_EditableChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EditableChangedSignalInfo
instance SignalInfo EditableChangedSignalInfo where
    type HaskellCallbackType EditableChangedSignalInfo = EditableChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EditableChangedCallback cb
        cb'' <- mk_EditableChangedCallback cb'
        connectSignalFunPtr obj "changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Editable::changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Editable.html#g:signal:changed"})

#endif

-- signal Editable::delete-text
-- | This signal is emitted when text is deleted from
-- the widget by the user. The default handler for
-- this signal will normally be responsible for deleting
-- the text, so by connecting to this signal and then
-- stopping the signal with 'GI.GObject.Functions.signalStopEmission', it
-- is possible to modify the range of deleted text, or
-- prevent it from being deleted entirely. The /@startPos@/
-- and /@endPos@/ parameters are interpreted as for
-- 'GI.Gtk.Interfaces.Editable.editableDeleteText'.
type EditableDeleteTextCallback =
    Int32
    -- ^ /@startPos@/: the starting position
    -> Int32
    -- ^ /@endPos@/: the end position
    -> IO ()

type C_EditableDeleteTextCallback =
    Ptr Editable ->                         -- object
    Int32 ->
    Int32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EditableDeleteTextCallback`.
foreign import ccall "wrapper"
    mk_EditableDeleteTextCallback :: C_EditableDeleteTextCallback -> IO (FunPtr C_EditableDeleteTextCallback)

wrap_EditableDeleteTextCallback :: 
    GObject a => (a -> EditableDeleteTextCallback) ->
    C_EditableDeleteTextCallback
wrap_EditableDeleteTextCallback gi'cb gi'selfPtr startPos endPos _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  startPos endPos


-- | Connect a signal handler for the [deleteText](#signal:deleteText) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' editable #deleteText callback
-- @
-- 
-- 
onEditableDeleteText :: (IsEditable a, MonadIO m) => a -> ((?self :: a) => EditableDeleteTextCallback) -> m SignalHandlerId
onEditableDeleteText obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EditableDeleteTextCallback wrapped
    wrapped'' <- mk_EditableDeleteTextCallback wrapped'
    connectSignalFunPtr obj "delete-text" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [deleteText](#signal:deleteText) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' editable #deleteText callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEditableDeleteText :: (IsEditable a, MonadIO m) => a -> ((?self :: a) => EditableDeleteTextCallback) -> m SignalHandlerId
afterEditableDeleteText obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EditableDeleteTextCallback wrapped
    wrapped'' <- mk_EditableDeleteTextCallback wrapped'
    connectSignalFunPtr obj "delete-text" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EditableDeleteTextSignalInfo
instance SignalInfo EditableDeleteTextSignalInfo where
    type HaskellCallbackType EditableDeleteTextSignalInfo = EditableDeleteTextCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EditableDeleteTextCallback cb
        cb'' <- mk_EditableDeleteTextCallback cb'
        connectSignalFunPtr obj "delete-text" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Editable::delete-text"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Editable.html#g:signal:deleteText"})

#endif

-- signal Editable::insert-text
-- | This signal is emitted when text is inserted into
-- the widget by the user. The default handler for
-- this signal will normally be responsible for inserting
-- the text, so by connecting to this signal and then
-- stopping the signal with 'GI.GObject.Functions.signalStopEmission', it
-- is possible to modify the inserted text, or prevent
-- it from being inserted entirely.
type EditableInsertTextCallback =
    T.Text
    -- ^ /@newText@/: the new text to insert
    -> Int32
    -- ^ /@newTextLength@/: the length of the new text, in bytes,
    --     or -1 if new_text is nul-terminated
    -> Int32
    -- ^ /@position@/: the position, in characters,
    --     at which to insert the new text. this is an in-out
    --     parameter.  After the signal emission is finished, it
    --     should point after the newly inserted text.
    -> IO (Int32)

type C_EditableInsertTextCallback =
    Ptr Editable ->                         -- object
    CString ->
    Int32 ->
    Ptr Int32 ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EditableInsertTextCallback`.
foreign import ccall "wrapper"
    mk_EditableInsertTextCallback :: C_EditableInsertTextCallback -> IO (FunPtr C_EditableInsertTextCallback)

wrap_EditableInsertTextCallback :: 
    GObject a => (a -> EditableInsertTextCallback) ->
    C_EditableInsertTextCallback
wrap_EditableInsertTextCallback gi'cb gi'selfPtr newText newTextLength position _ = do
    newText' <- cstringToText newText
    position' <- peek position
    outposition <- B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  newText' newTextLength position'
    poke position outposition


-- | Connect a signal handler for the [insertText](#signal:insertText) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' editable #insertText callback
-- @
-- 
-- 
onEditableInsertText :: (IsEditable a, MonadIO m) => a -> ((?self :: a) => EditableInsertTextCallback) -> m SignalHandlerId
onEditableInsertText obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EditableInsertTextCallback wrapped
    wrapped'' <- mk_EditableInsertTextCallback wrapped'
    connectSignalFunPtr obj "insert-text" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [insertText](#signal:insertText) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' editable #insertText callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEditableInsertText :: (IsEditable a, MonadIO m) => a -> ((?self :: a) => EditableInsertTextCallback) -> m SignalHandlerId
afterEditableInsertText obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EditableInsertTextCallback wrapped
    wrapped'' <- mk_EditableInsertTextCallback wrapped'
    connectSignalFunPtr obj "insert-text" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EditableInsertTextSignalInfo
instance SignalInfo EditableInsertTextSignalInfo where
    type HaskellCallbackType EditableInsertTextSignalInfo = EditableInsertTextCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EditableInsertTextCallback cb
        cb'' <- mk_EditableInsertTextCallback cb'
        connectSignalFunPtr obj "insert-text" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.Editable::insert-text"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-Editable.html#g:signal:insertText"})

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Editable = EditableSignalList
type EditableSignalList = ('[ '("changed", EditableChangedSignalInfo), '("deleteText", EditableDeleteTextSignalInfo), '("insertText", EditableInsertTextSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif


