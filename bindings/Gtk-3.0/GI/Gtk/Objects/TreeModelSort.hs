{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.TreeModelSort.TreeModelSort' is a model which implements the t'GI.Gtk.Interfaces.TreeSortable.TreeSortable'
-- interface.  It does not hold any data itself, but rather is created with
-- a child model and proxies its data.  It has identical column types to
-- this child model, and the changes in the child are propagated.  The
-- primary purpose of this model is to provide a way to sort a different
-- model without modifying it. Note that the sort function used by
-- t'GI.Gtk.Objects.TreeModelSort.TreeModelSort' is not guaranteed to be stable.
-- 
-- The use of this is best demonstrated through an example.  In the
-- following sample code we create two t'GI.Gtk.Objects.TreeView.TreeView' widgets each with a
-- view of the same data.  As the model is wrapped here by a
-- t'GI.Gtk.Objects.TreeModelSort.TreeModelSort', the two @/GtkTreeViews/@ can each sort their
-- view of the data without affecting the other.  By contrast, if we
-- simply put the same model in each widget, then sorting the first would
-- sort the second.
-- 
-- ## Using a t'GI.Gtk.Objects.TreeModelSort.TreeModelSort'
-- 
-- 
-- === /C code/
-- >
-- >{
-- >  GtkTreeView *tree_view1;
-- >  GtkTreeView *tree_view2;
-- >  GtkTreeModel *sort_model1;
-- >  GtkTreeModel *sort_model2;
-- >  GtkTreeModel *child_model;
-- >
-- >  // get the child model
-- >  child_model = get_my_model ();
-- >
-- >  // Create the first tree
-- >  sort_model1 = gtk_tree_model_sort_new_with_model (child_model);
-- >  tree_view1 = gtk_tree_view_new_with_model (sort_model1);
-- >
-- >  // Create the second tree
-- >  sort_model2 = gtk_tree_model_sort_new_with_model (child_model);
-- >  tree_view2 = gtk_tree_view_new_with_model (sort_model2);
-- >
-- >  // Now we can sort the two models independently
-- >  gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (sort_model1),
-- >                                        COLUMN_1, GTK_SORT_ASCENDING);
-- >  gtk_tree_sortable_set_sort_column_id (GTK_TREE_SORTABLE (sort_model2),
-- >                                        COLUMN_1, GTK_SORT_DESCENDING);
-- >}
-- 
-- 
-- To demonstrate how to access the underlying child model from the sort
-- model, the next example will be a callback for the t'GI.Gtk.Objects.TreeSelection.TreeSelection'
-- [TreeSelection::changed]("GI.Gtk.Objects.TreeSelection#g:signal:changed") signal.  In this callback, we get a string
-- from COLUMN_1 of the model.  We then modify the string, find the same
-- selected row on the child model, and change the row there.
-- 
-- == Accessing the child model of in a selection changed callback
-- 
-- 
-- === /C code/
-- >
-- >void
-- >selection_changed (GtkTreeSelection *selection, gpointer data)
-- >{
-- >  GtkTreeModel *sort_model = NULL;
-- >  GtkTreeModel *child_model;
-- >  GtkTreeIter sort_iter;
-- >  GtkTreeIter child_iter;
-- >  char *some_data = NULL;
-- >  char *modified_data;
-- >
-- >  // Get the current selected row and the model.
-- >  if (! gtk_tree_selection_get_selected (selection,
-- >                                         &sort_model,
-- >                                         &sort_iter))
-- >    return;
-- >
-- >  // Look up the current value on the selected row and get
-- >  // a new value to change it to.
-- >  gtk_tree_model_get (GTK_TREE_MODEL (sort_model), &sort_iter,
-- >                      COLUMN_1, &some_data,
-- >                      -1);
-- >
-- >  modified_data = change_the_data (some_data);
-- >  g_free (some_data);
-- >
-- >  // Get an iterator on the child model, instead of the sort model.
-- >  gtk_tree_model_sort_convert_iter_to_child_iter (GTK_TREE_MODEL_SORT (sort_model),
-- >                                                  &child_iter,
-- >                                                  &sort_iter);
-- >
-- >  // Get the child model and change the value of the row. In this
-- >  // example, the child model is a GtkListStore. It could be any other
-- >  // type of model, though.
-- >  child_model = gtk_tree_model_sort_get_model (GTK_TREE_MODEL_SORT (sort_model));
-- >  gtk_list_store_set (GTK_LIST_STORE (child_model), &child_iter,
-- >                      COLUMN_1, &modified_data,
-- >                      -1);
-- >  g_free (modified_data);
-- >}
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.TreeModelSort
    ( 

-- * Exported types
    TreeModelSort(..)                       ,
    IsTreeModelSort                         ,
    toTreeModelSort                         ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [clearCache]("GI.Gtk.Objects.TreeModelSort#g:method:clearCache"), [convertChildIterToIter]("GI.Gtk.Objects.TreeModelSort#g:method:convertChildIterToIter"), [convertChildPathToPath]("GI.Gtk.Objects.TreeModelSort#g:method:convertChildPathToPath"), [convertIterToChildIter]("GI.Gtk.Objects.TreeModelSort#g:method:convertIterToChildIter"), [convertPathToChildPath]("GI.Gtk.Objects.TreeModelSort#g:method:convertPathToChildPath"), [dragDataDelete]("GI.Gtk.Interfaces.TreeDragSource#g:method:dragDataDelete"), [dragDataGet]("GI.Gtk.Interfaces.TreeDragSource#g:method:dragDataGet"), [filterNew]("GI.Gtk.Interfaces.TreeModel#g:method:filterNew"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Interfaces.TreeModel#g:method:foreach"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [hasDefaultSortFunc]("GI.Gtk.Interfaces.TreeSortable#g:method:hasDefaultSortFunc"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [iterChildren]("GI.Gtk.Interfaces.TreeModel#g:method:iterChildren"), [iterHasChild]("GI.Gtk.Interfaces.TreeModel#g:method:iterHasChild"), [iterIsValid]("GI.Gtk.Objects.TreeModelSort#g:method:iterIsValid"), [iterNChildren]("GI.Gtk.Interfaces.TreeModel#g:method:iterNChildren"), [iterNext]("GI.Gtk.Interfaces.TreeModel#g:method:iterNext"), [iterNthChild]("GI.Gtk.Interfaces.TreeModel#g:method:iterNthChild"), [iterParent]("GI.Gtk.Interfaces.TreeModel#g:method:iterParent"), [iterPrevious]("GI.Gtk.Interfaces.TreeModel#g:method:iterPrevious"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refNode]("GI.Gtk.Interfaces.TreeModel#g:method:refNode"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [resetDefaultSortFunc]("GI.Gtk.Objects.TreeModelSort#g:method:resetDefaultSortFunc"), [rowChanged]("GI.Gtk.Interfaces.TreeModel#g:method:rowChanged"), [rowDeleted]("GI.Gtk.Interfaces.TreeModel#g:method:rowDeleted"), [rowDraggable]("GI.Gtk.Interfaces.TreeDragSource#g:method:rowDraggable"), [rowHasChildToggled]("GI.Gtk.Interfaces.TreeModel#g:method:rowHasChildToggled"), [rowInserted]("GI.Gtk.Interfaces.TreeModel#g:method:rowInserted"), [rowsReordered]("GI.Gtk.Interfaces.TreeModel#g:method:rowsReordered"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sortColumnChanged]("GI.Gtk.Interfaces.TreeSortable#g:method:sortColumnChanged"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unrefNode]("GI.Gtk.Interfaces.TreeModel#g:method:unrefNode"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getColumnType]("GI.Gtk.Interfaces.TreeModel#g:method:getColumnType"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getFlags]("GI.Gtk.Interfaces.TreeModel#g:method:getFlags"), [getIter]("GI.Gtk.Interfaces.TreeModel#g:method:getIter"), [getIterFirst]("GI.Gtk.Interfaces.TreeModel#g:method:getIterFirst"), [getIterFromString]("GI.Gtk.Interfaces.TreeModel#g:method:getIterFromString"), [getModel]("GI.Gtk.Objects.TreeModelSort#g:method:getModel"), [getNColumns]("GI.Gtk.Interfaces.TreeModel#g:method:getNColumns"), [getPath]("GI.Gtk.Interfaces.TreeModel#g:method:getPath"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSortColumnId]("GI.Gtk.Interfaces.TreeSortable#g:method:getSortColumnId"), [getStringFromIter]("GI.Gtk.Interfaces.TreeModel#g:method:getStringFromIter"), [getValue]("GI.Gtk.Interfaces.TreeModel#g:method:getValue").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDefaultSortFunc]("GI.Gtk.Interfaces.TreeSortable#g:method:setDefaultSortFunc"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSortColumnId]("GI.Gtk.Interfaces.TreeSortable#g:method:setSortColumnId"), [setSortFunc]("GI.Gtk.Interfaces.TreeSortable#g:method:setSortFunc").

#if defined(ENABLE_OVERLOADING)
    ResolveTreeModelSortMethod              ,
#endif

-- ** clearCache #method:clearCache#

#if defined(ENABLE_OVERLOADING)
    TreeModelSortClearCacheMethodInfo       ,
#endif
    treeModelSortClearCache                 ,


-- ** convertChildIterToIter #method:convertChildIterToIter#

#if defined(ENABLE_OVERLOADING)
    TreeModelSortConvertChildIterToIterMethodInfo,
#endif
    treeModelSortConvertChildIterToIter     ,


-- ** convertChildPathToPath #method:convertChildPathToPath#

#if defined(ENABLE_OVERLOADING)
    TreeModelSortConvertChildPathToPathMethodInfo,
#endif
    treeModelSortConvertChildPathToPath     ,


-- ** convertIterToChildIter #method:convertIterToChildIter#

#if defined(ENABLE_OVERLOADING)
    TreeModelSortConvertIterToChildIterMethodInfo,
#endif
    treeModelSortConvertIterToChildIter     ,


-- ** convertPathToChildPath #method:convertPathToChildPath#

#if defined(ENABLE_OVERLOADING)
    TreeModelSortConvertPathToChildPathMethodInfo,
#endif
    treeModelSortConvertPathToChildPath     ,


-- ** getModel #method:getModel#

#if defined(ENABLE_OVERLOADING)
    TreeModelSortGetModelMethodInfo         ,
#endif
    treeModelSortGetModel                   ,


-- ** iterIsValid #method:iterIsValid#

#if defined(ENABLE_OVERLOADING)
    TreeModelSortIterIsValidMethodInfo      ,
#endif
    treeModelSortIterIsValid                ,


-- ** newWithModel #method:newWithModel#

    treeModelSortNewWithModel               ,


-- ** resetDefaultSortFunc #method:resetDefaultSortFunc#

#if defined(ENABLE_OVERLOADING)
    TreeModelSortResetDefaultSortFuncMethodInfo,
#endif
    treeModelSortResetDefaultSortFunc       ,




 -- * Properties


-- ** model #attr:model#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeModelSortModelPropertyInfo          ,
#endif
    constructTreeModelSortModel             ,
    getTreeModelSortModel                   ,
#if defined(ENABLE_OVERLOADING)
    treeModelSortModel                      ,
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
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeDragSource as Gtk.TreeDragSource
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeModel as Gtk.TreeModel
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeSortable as Gtk.TreeSortable
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreeIter as Gtk.TreeIter
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreePath as Gtk.TreePath

-- | Memory-managed wrapper type.
newtype TreeModelSort = TreeModelSort (SP.ManagedPtr TreeModelSort)
    deriving (Eq)

instance SP.ManagedPtrNewtype TreeModelSort where
    toManagedPtr (TreeModelSort p) = p

foreign import ccall "gtk_tree_model_sort_get_type"
    c_gtk_tree_model_sort_get_type :: IO B.Types.GType

instance B.Types.TypedObject TreeModelSort where
    glibType = c_gtk_tree_model_sort_get_type

instance B.Types.GObject TreeModelSort

-- | Type class for types which can be safely cast to `TreeModelSort`, for instance with `toTreeModelSort`.
class (SP.GObject o, O.IsDescendantOf TreeModelSort o) => IsTreeModelSort o
instance (SP.GObject o, O.IsDescendantOf TreeModelSort o) => IsTreeModelSort o

instance O.HasParentTypes TreeModelSort
type instance O.ParentTypes TreeModelSort = '[GObject.Object.Object, Gtk.TreeDragSource.TreeDragSource, Gtk.TreeModel.TreeModel, Gtk.TreeSortable.TreeSortable]

-- | Cast to `TreeModelSort`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTreeModelSort :: (MIO.MonadIO m, IsTreeModelSort o) => o -> m TreeModelSort
toTreeModelSort = MIO.liftIO . B.ManagedPtr.unsafeCastTo TreeModelSort

-- | Convert 'TreeModelSort' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TreeModelSort) where
    gvalueGType_ = c_gtk_tree_model_sort_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr TreeModelSort)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr TreeModelSort)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject TreeModelSort ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveTreeModelSortMethod (t :: Symbol) (o :: *) :: * where
    ResolveTreeModelSortMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTreeModelSortMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTreeModelSortMethod "clearCache" o = TreeModelSortClearCacheMethodInfo
    ResolveTreeModelSortMethod "convertChildIterToIter" o = TreeModelSortConvertChildIterToIterMethodInfo
    ResolveTreeModelSortMethod "convertChildPathToPath" o = TreeModelSortConvertChildPathToPathMethodInfo
    ResolveTreeModelSortMethod "convertIterToChildIter" o = TreeModelSortConvertIterToChildIterMethodInfo
    ResolveTreeModelSortMethod "convertPathToChildPath" o = TreeModelSortConvertPathToChildPathMethodInfo
    ResolveTreeModelSortMethod "dragDataDelete" o = Gtk.TreeDragSource.TreeDragSourceDragDataDeleteMethodInfo
    ResolveTreeModelSortMethod "dragDataGet" o = Gtk.TreeDragSource.TreeDragSourceDragDataGetMethodInfo
    ResolveTreeModelSortMethod "filterNew" o = Gtk.TreeModel.TreeModelFilterNewMethodInfo
    ResolveTreeModelSortMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTreeModelSortMethod "foreach" o = Gtk.TreeModel.TreeModelForeachMethodInfo
    ResolveTreeModelSortMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTreeModelSortMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTreeModelSortMethod "hasDefaultSortFunc" o = Gtk.TreeSortable.TreeSortableHasDefaultSortFuncMethodInfo
    ResolveTreeModelSortMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTreeModelSortMethod "iterChildren" o = Gtk.TreeModel.TreeModelIterChildrenMethodInfo
    ResolveTreeModelSortMethod "iterHasChild" o = Gtk.TreeModel.TreeModelIterHasChildMethodInfo
    ResolveTreeModelSortMethod "iterIsValid" o = TreeModelSortIterIsValidMethodInfo
    ResolveTreeModelSortMethod "iterNChildren" o = Gtk.TreeModel.TreeModelIterNChildrenMethodInfo
    ResolveTreeModelSortMethod "iterNext" o = Gtk.TreeModel.TreeModelIterNextMethodInfo
    ResolveTreeModelSortMethod "iterNthChild" o = Gtk.TreeModel.TreeModelIterNthChildMethodInfo
    ResolveTreeModelSortMethod "iterParent" o = Gtk.TreeModel.TreeModelIterParentMethodInfo
    ResolveTreeModelSortMethod "iterPrevious" o = Gtk.TreeModel.TreeModelIterPreviousMethodInfo
    ResolveTreeModelSortMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTreeModelSortMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTreeModelSortMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTreeModelSortMethod "refNode" o = Gtk.TreeModel.TreeModelRefNodeMethodInfo
    ResolveTreeModelSortMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTreeModelSortMethod "resetDefaultSortFunc" o = TreeModelSortResetDefaultSortFuncMethodInfo
    ResolveTreeModelSortMethod "rowChanged" o = Gtk.TreeModel.TreeModelRowChangedMethodInfo
    ResolveTreeModelSortMethod "rowDeleted" o = Gtk.TreeModel.TreeModelRowDeletedMethodInfo
    ResolveTreeModelSortMethod "rowDraggable" o = Gtk.TreeDragSource.TreeDragSourceRowDraggableMethodInfo
    ResolveTreeModelSortMethod "rowHasChildToggled" o = Gtk.TreeModel.TreeModelRowHasChildToggledMethodInfo
    ResolveTreeModelSortMethod "rowInserted" o = Gtk.TreeModel.TreeModelRowInsertedMethodInfo
    ResolveTreeModelSortMethod "rowsReordered" o = Gtk.TreeModel.TreeModelRowsReorderedMethodInfo
    ResolveTreeModelSortMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTreeModelSortMethod "sortColumnChanged" o = Gtk.TreeSortable.TreeSortableSortColumnChangedMethodInfo
    ResolveTreeModelSortMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTreeModelSortMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTreeModelSortMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTreeModelSortMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTreeModelSortMethod "unrefNode" o = Gtk.TreeModel.TreeModelUnrefNodeMethodInfo
    ResolveTreeModelSortMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTreeModelSortMethod "getColumnType" o = Gtk.TreeModel.TreeModelGetColumnTypeMethodInfo
    ResolveTreeModelSortMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTreeModelSortMethod "getFlags" o = Gtk.TreeModel.TreeModelGetFlagsMethodInfo
    ResolveTreeModelSortMethod "getIter" o = Gtk.TreeModel.TreeModelGetIterMethodInfo
    ResolveTreeModelSortMethod "getIterFirst" o = Gtk.TreeModel.TreeModelGetIterFirstMethodInfo
    ResolveTreeModelSortMethod "getIterFromString" o = Gtk.TreeModel.TreeModelGetIterFromStringMethodInfo
    ResolveTreeModelSortMethod "getModel" o = TreeModelSortGetModelMethodInfo
    ResolveTreeModelSortMethod "getNColumns" o = Gtk.TreeModel.TreeModelGetNColumnsMethodInfo
    ResolveTreeModelSortMethod "getPath" o = Gtk.TreeModel.TreeModelGetPathMethodInfo
    ResolveTreeModelSortMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTreeModelSortMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTreeModelSortMethod "getSortColumnId" o = Gtk.TreeSortable.TreeSortableGetSortColumnIdMethodInfo
    ResolveTreeModelSortMethod "getStringFromIter" o = Gtk.TreeModel.TreeModelGetStringFromIterMethodInfo
    ResolveTreeModelSortMethod "getValue" o = Gtk.TreeModel.TreeModelGetValueMethodInfo
    ResolveTreeModelSortMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTreeModelSortMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTreeModelSortMethod "setDefaultSortFunc" o = Gtk.TreeSortable.TreeSortableSetDefaultSortFuncMethodInfo
    ResolveTreeModelSortMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTreeModelSortMethod "setSortColumnId" o = Gtk.TreeSortable.TreeSortableSetSortColumnIdMethodInfo
    ResolveTreeModelSortMethod "setSortFunc" o = Gtk.TreeSortable.TreeSortableSetSortFuncMethodInfo
    ResolveTreeModelSortMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTreeModelSortMethod t TreeModelSort, O.OverloadedMethod info TreeModelSort p) => OL.IsLabel t (TreeModelSort -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTreeModelSortMethod t TreeModelSort, O.OverloadedMethod info TreeModelSort p, R.HasField t TreeModelSort p) => R.HasField t TreeModelSort p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTreeModelSortMethod t TreeModelSort, O.OverloadedMethodInfo info TreeModelSort) => OL.IsLabel t (O.MethodProxy info TreeModelSort) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "model"
   -- Type: TInterface (Name {namespace = "Gtk", name = "TreeModel"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Just False,Nothing)

-- | Get the value of the “@model@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeModelSort #model
-- @
getTreeModelSortModel :: (MonadIO m, IsTreeModelSort o) => o -> m Gtk.TreeModel.TreeModel
getTreeModelSortModel obj = MIO.liftIO $ checkUnexpectedNothing "getTreeModelSortModel" $ B.Properties.getObjectPropertyObject obj "model" Gtk.TreeModel.TreeModel

-- | Construct a `GValueConstruct` with valid value for the “@model@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeModelSortModel :: (IsTreeModelSort o, MIO.MonadIO m, Gtk.TreeModel.IsTreeModel a) => a -> m (GValueConstruct o)
constructTreeModelSortModel val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "model" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data TreeModelSortModelPropertyInfo
instance AttrInfo TreeModelSortModelPropertyInfo where
    type AttrAllowedOps TreeModelSortModelPropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TreeModelSortModelPropertyInfo = IsTreeModelSort
    type AttrSetTypeConstraint TreeModelSortModelPropertyInfo = Gtk.TreeModel.IsTreeModel
    type AttrTransferTypeConstraint TreeModelSortModelPropertyInfo = Gtk.TreeModel.IsTreeModel
    type AttrTransferType TreeModelSortModelPropertyInfo = Gtk.TreeModel.TreeModel
    type AttrGetType TreeModelSortModelPropertyInfo = Gtk.TreeModel.TreeModel
    type AttrLabel TreeModelSortModelPropertyInfo = "model"
    type AttrOrigin TreeModelSortModelPropertyInfo = TreeModelSort
    attrGet = getTreeModelSortModel
    attrSet = undefined
    attrTransfer _ v = do
        unsafeCastTo Gtk.TreeModel.TreeModel v
    attrConstruct = constructTreeModelSortModel
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelSort.model"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelSort.html#g:attr:model"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TreeModelSort
type instance O.AttributeList TreeModelSort = TreeModelSortAttributeList
type TreeModelSortAttributeList = ('[ '("model", TreeModelSortModelPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
treeModelSortModel :: AttrLabelProxy "model"
treeModelSortModel = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TreeModelSort = TreeModelSortSignalList
type TreeModelSortSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo), '("rowChanged", Gtk.TreeModel.TreeModelRowChangedSignalInfo), '("rowDeleted", Gtk.TreeModel.TreeModelRowDeletedSignalInfo), '("rowHasChildToggled", Gtk.TreeModel.TreeModelRowHasChildToggledSignalInfo), '("rowInserted", Gtk.TreeModel.TreeModelRowInsertedSignalInfo), '("sortColumnChanged", Gtk.TreeSortable.TreeSortableSortColumnChangedSignalInfo)] :: [(Symbol, *)])

#endif

-- method TreeModelSort::new_with_model
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "child_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "TreeModelSort" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_model_sort_new_with_model" gtk_tree_model_sort_new_with_model :: 
    Ptr Gtk.TreeModel.TreeModel ->          -- child_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    IO (Ptr TreeModelSort)

-- | Creates a new t'GI.Gtk.Objects.TreeModelSort.TreeModelSort', with /@childModel@/ as the child model.
treeModelSortNewWithModel ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TreeModel.IsTreeModel a) =>
    a
    -- ^ /@childModel@/: A t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> m TreeModelSort
    -- ^ __Returns:__ A new t'GI.Gtk.Objects.TreeModelSort.TreeModelSort'.
treeModelSortNewWithModel childModel = liftIO $ do
    childModel' <- unsafeManagedPtrCastPtr childModel
    result <- gtk_tree_model_sort_new_with_model childModel'
    checkUnexpectedReturnNULL "treeModelSortNewWithModel" result
    result' <- (wrapObject TreeModelSort) result
    touchManagedPtr childModel
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TreeModelSort::clear_cache
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model_sort"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelSort" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModelSort"
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

foreign import ccall "gtk_tree_model_sort_clear_cache" gtk_tree_model_sort_clear_cache :: 
    Ptr TreeModelSort ->                    -- tree_model_sort : TInterface (Name {namespace = "Gtk", name = "TreeModelSort"})
    IO ()

-- | This function should almost never be called.  It clears the /@treeModelSort@/
-- of any cached iterators that haven’t been reffed with
-- 'GI.Gtk.Interfaces.TreeModel.treeModelRefNode'.  This might be useful if the child model being
-- sorted is static (and doesn’t change often) and there has been a lot of
-- unreffed access to nodes.  As a side effect of this function, all unreffed
-- iters will be invalid.
treeModelSortClearCache ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelSort a) =>
    a
    -- ^ /@treeModelSort@/: A t'GI.Gtk.Objects.TreeModelSort.TreeModelSort'
    -> m ()
treeModelSortClearCache treeModelSort = liftIO $ do
    treeModelSort' <- unsafeManagedPtrCastPtr treeModelSort
    gtk_tree_model_sort_clear_cache treeModelSort'
    touchManagedPtr treeModelSort
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeModelSortClearCacheMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTreeModelSort a) => O.OverloadedMethod TreeModelSortClearCacheMethodInfo a signature where
    overloadedMethod = treeModelSortClearCache

instance O.OverloadedMethodInfo TreeModelSortClearCacheMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelSort.treeModelSortClearCache",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelSort.html#v:treeModelSortClearCache"
        })


#endif

-- method TreeModelSort::convert_child_iter_to_iter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model_sort"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelSort" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModelSort"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sort_iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "An uninitialized #GtkTreeIter."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "child_iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "A valid #GtkTreeIter pointing to a row on the child model"
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

foreign import ccall "gtk_tree_model_sort_convert_child_iter_to_iter" gtk_tree_model_sort_convert_child_iter_to_iter :: 
    Ptr TreeModelSort ->                    -- tree_model_sort : TInterface (Name {namespace = "Gtk", name = "TreeModelSort"})
    Ptr Gtk.TreeIter.TreeIter ->            -- sort_iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Ptr Gtk.TreeIter.TreeIter ->            -- child_iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO CInt

-- | Sets /@sortIter@/ to point to the row in /@treeModelSort@/ that corresponds to
-- the row pointed at by /@childIter@/.  If /@sortIter@/ was not set, 'P.False'
-- is returned.  Note: a boolean is only returned since 2.14.
treeModelSortConvertChildIterToIter ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelSort a) =>
    a
    -- ^ /@treeModelSort@/: A t'GI.Gtk.Objects.TreeModelSort.TreeModelSort'
    -> Gtk.TreeIter.TreeIter
    -- ^ /@childIter@/: A valid t'GI.Gtk.Structs.TreeIter.TreeIter' pointing to a row on the child model
    -> m ((Bool, Gtk.TreeIter.TreeIter))
    -- ^ __Returns:__ 'P.True', if /@sortIter@/ was set, i.e. if /@sortIter@/ is a
    -- valid iterator pointer to a visible row in the child model.
treeModelSortConvertChildIterToIter treeModelSort childIter = liftIO $ do
    treeModelSort' <- unsafeManagedPtrCastPtr treeModelSort
    sortIter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    childIter' <- unsafeManagedPtrGetPtr childIter
    result <- gtk_tree_model_sort_convert_child_iter_to_iter treeModelSort' sortIter childIter'
    let result' = (/= 0) result
    sortIter' <- (wrapBoxed Gtk.TreeIter.TreeIter) sortIter
    touchManagedPtr treeModelSort
    touchManagedPtr childIter
    return (result', sortIter')

#if defined(ENABLE_OVERLOADING)
data TreeModelSortConvertChildIterToIterMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m ((Bool, Gtk.TreeIter.TreeIter))), MonadIO m, IsTreeModelSort a) => O.OverloadedMethod TreeModelSortConvertChildIterToIterMethodInfo a signature where
    overloadedMethod = treeModelSortConvertChildIterToIter

instance O.OverloadedMethodInfo TreeModelSortConvertChildIterToIterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelSort.treeModelSortConvertChildIterToIter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelSort.html#v:treeModelSortConvertChildIterToIter"
        })


#endif

-- method TreeModelSort::convert_child_path_to_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model_sort"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelSort" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModelSort"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "child_path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreePath to convert"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TreePath" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_model_sort_convert_child_path_to_path" gtk_tree_model_sort_convert_child_path_to_path :: 
    Ptr TreeModelSort ->                    -- tree_model_sort : TInterface (Name {namespace = "Gtk", name = "TreeModelSort"})
    Ptr Gtk.TreePath.TreePath ->            -- child_path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO (Ptr Gtk.TreePath.TreePath)

-- | Converts /@childPath@/ to a path relative to /@treeModelSort@/.  That is,
-- /@childPath@/ points to a path in the child model.  The returned path will
-- point to the same row in the sorted model.  If /@childPath@/ isn’t a valid
-- path on the child model, then 'P.Nothing' is returned.
treeModelSortConvertChildPathToPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelSort a) =>
    a
    -- ^ /@treeModelSort@/: A t'GI.Gtk.Objects.TreeModelSort.TreeModelSort'
    -> Gtk.TreePath.TreePath
    -- ^ /@childPath@/: A t'GI.Gtk.Structs.TreePath.TreePath' to convert
    -> m (Maybe Gtk.TreePath.TreePath)
    -- ^ __Returns:__ A newly allocated t'GI.Gtk.Structs.TreePath.TreePath', or 'P.Nothing'
treeModelSortConvertChildPathToPath treeModelSort childPath = liftIO $ do
    treeModelSort' <- unsafeManagedPtrCastPtr treeModelSort
    childPath' <- unsafeManagedPtrGetPtr childPath
    result <- gtk_tree_model_sort_convert_child_path_to_path treeModelSort' childPath'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapBoxed Gtk.TreePath.TreePath) result'
        return result''
    touchManagedPtr treeModelSort
    touchManagedPtr childPath
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data TreeModelSortConvertChildPathToPathMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> m (Maybe Gtk.TreePath.TreePath)), MonadIO m, IsTreeModelSort a) => O.OverloadedMethod TreeModelSortConvertChildPathToPathMethodInfo a signature where
    overloadedMethod = treeModelSortConvertChildPathToPath

instance O.OverloadedMethodInfo TreeModelSortConvertChildPathToPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelSort.treeModelSortConvertChildPathToPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelSort.html#v:treeModelSortConvertChildPathToPath"
        })


#endif

-- method TreeModelSort::convert_iter_to_child_iter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model_sort"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelSort" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModelSort"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "child_iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "An uninitialized #GtkTreeIter"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sorted_iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "A valid #GtkTreeIter pointing to a row on @tree_model_sort."
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

foreign import ccall "gtk_tree_model_sort_convert_iter_to_child_iter" gtk_tree_model_sort_convert_iter_to_child_iter :: 
    Ptr TreeModelSort ->                    -- tree_model_sort : TInterface (Name {namespace = "Gtk", name = "TreeModelSort"})
    Ptr Gtk.TreeIter.TreeIter ->            -- child_iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Ptr Gtk.TreeIter.TreeIter ->            -- sorted_iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Sets /@childIter@/ to point to the row pointed to by /@sortedIter@/.
treeModelSortConvertIterToChildIter ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelSort a) =>
    a
    -- ^ /@treeModelSort@/: A t'GI.Gtk.Objects.TreeModelSort.TreeModelSort'
    -> Gtk.TreeIter.TreeIter
    -- ^ /@sortedIter@/: A valid t'GI.Gtk.Structs.TreeIter.TreeIter' pointing to a row on /@treeModelSort@/.
    -> m (Gtk.TreeIter.TreeIter)
treeModelSortConvertIterToChildIter treeModelSort sortedIter = liftIO $ do
    treeModelSort' <- unsafeManagedPtrCastPtr treeModelSort
    childIter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    sortedIter' <- unsafeManagedPtrGetPtr sortedIter
    gtk_tree_model_sort_convert_iter_to_child_iter treeModelSort' childIter sortedIter'
    childIter' <- (wrapBoxed Gtk.TreeIter.TreeIter) childIter
    touchManagedPtr treeModelSort
    touchManagedPtr sortedIter
    return childIter'

#if defined(ENABLE_OVERLOADING)
data TreeModelSortConvertIterToChildIterMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m (Gtk.TreeIter.TreeIter)), MonadIO m, IsTreeModelSort a) => O.OverloadedMethod TreeModelSortConvertIterToChildIterMethodInfo a signature where
    overloadedMethod = treeModelSortConvertIterToChildIter

instance O.OverloadedMethodInfo TreeModelSortConvertIterToChildIterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelSort.treeModelSortConvertIterToChildIter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelSort.html#v:treeModelSortConvertIterToChildIter"
        })


#endif

-- method TreeModelSort::convert_path_to_child_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model_sort"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelSort" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModelSort"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sorted_path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreePath to convert"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TreePath" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_model_sort_convert_path_to_child_path" gtk_tree_model_sort_convert_path_to_child_path :: 
    Ptr TreeModelSort ->                    -- tree_model_sort : TInterface (Name {namespace = "Gtk", name = "TreeModelSort"})
    Ptr Gtk.TreePath.TreePath ->            -- sorted_path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO (Ptr Gtk.TreePath.TreePath)

-- | Converts /@sortedPath@/ to a path on the child model of /@treeModelSort@/.
-- That is, /@sortedPath@/ points to a location in /@treeModelSort@/.  The
-- returned path will point to the same location in the model not being
-- sorted.  If /@sortedPath@/ does not point to a location in the child model,
-- 'P.Nothing' is returned.
treeModelSortConvertPathToChildPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelSort a) =>
    a
    -- ^ /@treeModelSort@/: A t'GI.Gtk.Objects.TreeModelSort.TreeModelSort'
    -> Gtk.TreePath.TreePath
    -- ^ /@sortedPath@/: A t'GI.Gtk.Structs.TreePath.TreePath' to convert
    -> m (Maybe Gtk.TreePath.TreePath)
    -- ^ __Returns:__ A newly allocated t'GI.Gtk.Structs.TreePath.TreePath', or 'P.Nothing'
treeModelSortConvertPathToChildPath treeModelSort sortedPath = liftIO $ do
    treeModelSort' <- unsafeManagedPtrCastPtr treeModelSort
    sortedPath' <- unsafeManagedPtrGetPtr sortedPath
    result <- gtk_tree_model_sort_convert_path_to_child_path treeModelSort' sortedPath'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapBoxed Gtk.TreePath.TreePath) result'
        return result''
    touchManagedPtr treeModelSort
    touchManagedPtr sortedPath
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data TreeModelSortConvertPathToChildPathMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> m (Maybe Gtk.TreePath.TreePath)), MonadIO m, IsTreeModelSort a) => O.OverloadedMethod TreeModelSortConvertPathToChildPathMethodInfo a signature where
    overloadedMethod = treeModelSortConvertPathToChildPath

instance O.OverloadedMethodInfo TreeModelSortConvertPathToChildPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelSort.treeModelSortConvertPathToChildPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelSort.html#v:treeModelSortConvertPathToChildPath"
        })


#endif

-- method TreeModelSort::get_model
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelSort" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModelSort"
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

foreign import ccall "gtk_tree_model_sort_get_model" gtk_tree_model_sort_get_model :: 
    Ptr TreeModelSort ->                    -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModelSort"})
    IO (Ptr Gtk.TreeModel.TreeModel)

-- | Returns the model the t'GI.Gtk.Objects.TreeModelSort.TreeModelSort' is sorting.
treeModelSortGetModel ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelSort a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Objects.TreeModelSort.TreeModelSort'
    -> m Gtk.TreeModel.TreeModel
    -- ^ __Returns:__ the \"child model\" being sorted
treeModelSortGetModel treeModel = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    result <- gtk_tree_model_sort_get_model treeModel'
    checkUnexpectedReturnNULL "treeModelSortGetModel" result
    result' <- (newObject Gtk.TreeModel.TreeModel) result
    touchManagedPtr treeModel
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeModelSortGetModelMethodInfo
instance (signature ~ (m Gtk.TreeModel.TreeModel), MonadIO m, IsTreeModelSort a) => O.OverloadedMethod TreeModelSortGetModelMethodInfo a signature where
    overloadedMethod = treeModelSortGetModel

instance O.OverloadedMethodInfo TreeModelSortGetModelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelSort.treeModelSortGetModel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelSort.html#v:treeModelSortGetModel"
        })


#endif

-- method TreeModelSort::iter_is_valid
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model_sort"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelSort" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModelSort."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeIter." , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_model_sort_iter_is_valid" gtk_tree_model_sort_iter_is_valid :: 
    Ptr TreeModelSort ->                    -- tree_model_sort : TInterface (Name {namespace = "Gtk", name = "TreeModelSort"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO CInt

-- | > This function is slow. Only use it for debugging and\/or testing
-- > purposes.
-- 
-- Checks if the given iter is a valid iter for this t'GI.Gtk.Objects.TreeModelSort.TreeModelSort'.
-- 
-- /Since: 2.2/
treeModelSortIterIsValid ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelSort a) =>
    a
    -- ^ /@treeModelSort@/: A t'GI.Gtk.Objects.TreeModelSort.TreeModelSort'.
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: A t'GI.Gtk.Structs.TreeIter.TreeIter'.
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the iter is valid, 'P.False' if the iter is invalid.
treeModelSortIterIsValid treeModelSort iter = liftIO $ do
    treeModelSort' <- unsafeManagedPtrCastPtr treeModelSort
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_tree_model_sort_iter_is_valid treeModelSort' iter'
    let result' = (/= 0) result
    touchManagedPtr treeModelSort
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeModelSortIterIsValidMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m Bool), MonadIO m, IsTreeModelSort a) => O.OverloadedMethod TreeModelSortIterIsValidMethodInfo a signature where
    overloadedMethod = treeModelSortIterIsValid

instance O.OverloadedMethodInfo TreeModelSortIterIsValidMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelSort.treeModelSortIterIsValid",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelSort.html#v:treeModelSortIterIsValid"
        })


#endif

-- method TreeModelSort::reset_default_sort_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model_sort"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelSort" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModelSort"
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

foreign import ccall "gtk_tree_model_sort_reset_default_sort_func" gtk_tree_model_sort_reset_default_sort_func :: 
    Ptr TreeModelSort ->                    -- tree_model_sort : TInterface (Name {namespace = "Gtk", name = "TreeModelSort"})
    IO ()

-- | This resets the default sort function to be in the “unsorted” state.  That
-- is, it is in the same order as the child model. It will re-sort the model
-- to be in the same order as the child model only if the t'GI.Gtk.Objects.TreeModelSort.TreeModelSort'
-- is in “unsorted” state.
treeModelSortResetDefaultSortFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelSort a) =>
    a
    -- ^ /@treeModelSort@/: A t'GI.Gtk.Objects.TreeModelSort.TreeModelSort'
    -> m ()
treeModelSortResetDefaultSortFunc treeModelSort = liftIO $ do
    treeModelSort' <- unsafeManagedPtrCastPtr treeModelSort
    gtk_tree_model_sort_reset_default_sort_func treeModelSort'
    touchManagedPtr treeModelSort
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeModelSortResetDefaultSortFuncMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTreeModelSort a) => O.OverloadedMethod TreeModelSortResetDefaultSortFuncMethodInfo a signature where
    overloadedMethod = treeModelSortResetDefaultSortFunc

instance O.OverloadedMethodInfo TreeModelSortResetDefaultSortFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelSort.treeModelSortResetDefaultSortFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelSort.html#v:treeModelSortResetDefaultSortFunc"
        })


#endif


