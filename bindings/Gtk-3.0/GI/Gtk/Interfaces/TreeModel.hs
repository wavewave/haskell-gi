{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Interfaces.TreeModel.TreeModel' interface defines a generic tree interface for
-- use by the t'GI.Gtk.Objects.TreeView.TreeView' widget. It is an abstract interface, and
-- is designed to be usable with any appropriate data structure. The
-- programmer just has to implement this interface on their own data
-- type for it to be viewable by a t'GI.Gtk.Objects.TreeView.TreeView' widget.
-- 
-- The model is represented as a hierarchical tree of strongly-typed,
-- columned data. In other words, the model can be seen as a tree where
-- every node has different values depending on which column is being
-- queried. The type of data found in a column is determined by using
-- the GType system (ie. @/G_TYPE_INT/@, @/GTK_TYPE_BUTTON/@, @/G_TYPE_POINTER/@,
-- etc). The types are homogeneous per column across all nodes. It is
-- important to note that this interface only provides a way of examining
-- a model and observing changes. The implementation of each individual
-- model decides how and if changes are made.
-- 
-- In order to make life simpler for programmers who do not need to
-- write their own specialized model, two generic models are provided
-- — the t'GI.Gtk.Objects.TreeStore.TreeStore' and the t'GI.Gtk.Objects.ListStore.ListStore'. To use these, the
-- developer simply pushes data into these models as necessary. These
-- models provide the data structure as well as all appropriate tree
-- interfaces. As a result, implementing drag and drop, sorting, and
-- storing data is trivial. For the vast majority of trees and lists,
-- these two models are sufficient.
-- 
-- Models are accessed on a node\/column level of granularity. One can
-- query for the value of a model at a certain node and a certain
-- column on that node. There are two structures used to reference a
-- particular node in a model. They are the t'GI.Gtk.Structs.TreePath.TreePath'-struct and
-- the t'GI.Gtk.Structs.TreeIter.TreeIter'-struct (“iter” is short for iterator). Most of the
-- interface consists of operations on a t'GI.Gtk.Structs.TreeIter.TreeIter'-struct.
-- 
-- A path is essentially a potential node. It is a location on a model
-- that may or may not actually correspond to a node on a specific
-- model. The t'GI.Gtk.Structs.TreePath.TreePath'-struct can be converted into either an
-- array of unsigned integers or a string. The string form is a list
-- of numbers separated by a colon. Each number refers to the offset
-- at that level. Thus, the path @0@ refers to the root
-- node and the path @2:4@ refers to the fifth child of
-- the third node.
-- 
-- By contrast, a t'GI.Gtk.Structs.TreeIter.TreeIter'-struct is a reference to a specific node on
-- a specific model. It is a generic struct with an integer and three
-- generic pointers. These are filled in by the model in a model-specific
-- way. One can convert a path to an iterator by calling
-- 'GI.Gtk.Interfaces.TreeModel.treeModelGetIter'. These iterators are the primary way
-- of accessing a model and are similar to the iterators used by
-- t'GI.Gtk.Objects.TextBuffer.TextBuffer'. They are generally statically allocated on the
-- stack and only used for a short time. The model interface defines
-- a set of operations using them for navigating the model.
-- 
-- It is expected that models fill in the iterator with private data.
-- For example, the t'GI.Gtk.Objects.ListStore.ListStore' model, which is internally a simple
-- linked list, stores a list node in one of the pointers. The
-- t'GI.Gtk.Objects.TreeModelSort.TreeModelSort' stores an array and an offset in two of the
-- pointers. Additionally, there is an integer field. This field is
-- generally filled with a unique stamp per model. This stamp is for
-- catching errors resulting from using invalid iterators with a model.
-- 
-- The lifecycle of an iterator can be a little confusing at first.
-- Iterators are expected to always be valid for as long as the model
-- is unchanged (and doesn’t emit a signal). The model is considered
-- to own all outstanding iterators and nothing needs to be done to
-- free them from the user’s point of view. Additionally, some models
-- guarantee that an iterator is valid for as long as the node it refers
-- to is valid (most notably the t'GI.Gtk.Objects.TreeStore.TreeStore' and t'GI.Gtk.Objects.ListStore.ListStore').
-- Although generally uninteresting, as one always has to allow for
-- the case where iterators do not persist beyond a signal, some very
-- important performance enhancements were made in the sort model.
-- As a result, the @/GTK_TREE_MODEL_ITERS_PERSIST/@ flag was added to
-- indicate this behavior.
-- 
-- To help show some common operation of a model, some examples are
-- provided. The first example shows three ways of getting the iter at
-- the location @3:2:5@. While the first method shown is
-- easier, the second is much more common, as you often get paths from
-- callbacks.
-- 
-- ## Acquiring a t'GI.Gtk.Structs.TreeIter.TreeIter'-struct
-- 
-- 
-- === /C code/
-- >
-- >// Three ways of getting the iter pointing to the location
-- >GtkTreePath *path;
-- >GtkTreeIter iter;
-- >GtkTreeIter parent_iter;
-- >
-- >// get the iterator from a string
-- >gtk_tree_model_get_iter_from_string (model,
-- >                                     &iter,
-- >                                     "3:2:5");
-- >
-- >// get the iterator from a path
-- >path = gtk_tree_path_new_from_string ("3:2:5");
-- >gtk_tree_model_get_iter (model, &iter, path);
-- >gtk_tree_path_free (path);
-- >
-- >// walk the tree to find the iterator
-- >gtk_tree_model_iter_nth_child (model, &iter,
-- >                               NULL, 3);
-- >parent_iter = iter;
-- >gtk_tree_model_iter_nth_child (model, &iter,
-- >                               &parent_iter, 2);
-- >parent_iter = iter;
-- >gtk_tree_model_iter_nth_child (model, &iter,
-- >                               &parent_iter, 5);
-- 
-- 
-- This second example shows a quick way of iterating through a list
-- and getting a string and an integer from each row. The
-- @/populate_model()/@ function used below is not
-- shown, as it is specific to the t'GI.Gtk.Objects.ListStore.ListStore'. For information on
-- how to write such a function, see the t'GI.Gtk.Objects.ListStore.ListStore' documentation.
-- 
-- ## Reading data from a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
-- 
-- 
-- === /C code/
-- >
-- >enum
-- >{
-- >  STRING_COLUMN,
-- >  INT_COLUMN,
-- >  N_COLUMNS
-- >};
-- >
-- >...
-- >
-- >GtkTreeModel *list_store;
-- >GtkTreeIter iter;
-- >gboolean valid;
-- >gint row_count = 0;
-- >
-- >// make a new list_store
-- >list_store = gtk_list_store_new (N_COLUMNS,
-- >                                 G_TYPE_STRING,
-- >                                 G_TYPE_INT);
-- >
-- >// Fill the list store with data
-- >populate_model (list_store);
-- >
-- >// Get the first iter in the list, check it is valid and walk
-- >// through the list, reading each row.
-- >
-- >valid = gtk_tree_model_get_iter_first (list_store,
-- >                                       &iter);
-- >while (valid)
-- > {
-- >   gchar *str_data;
-- >   gint   int_data;
-- >
-- >   // Make sure you terminate calls to gtk_tree_model_get() with a “-1” value
-- >   gtk_tree_model_get (list_store, &iter,
-- >                       STRING_COLUMN, &str_data,
-- >                       INT_COLUMN, &int_data,
-- >                       -1);
-- >
-- >   // Do something with the data
-- >   g_print ("Row %d: (%s,%d)\n",
-- >            row_count, str_data, int_data);
-- >   g_free (str_data);
-- >
-- >   valid = gtk_tree_model_iter_next (list_store,
-- >                                     &iter);
-- >   row_count++;
-- > }
-- 
-- 
-- The t'GI.Gtk.Interfaces.TreeModel.TreeModel' interface contains two methods for reference
-- counting: 'GI.Gtk.Interfaces.TreeModel.treeModelRefNode' and 'GI.Gtk.Interfaces.TreeModel.treeModelUnrefNode'.
-- These two methods are optional to implement. The reference counting
-- is meant as a way for views to let models know when nodes are being
-- displayed. t'GI.Gtk.Objects.TreeView.TreeView' will take a reference on a node when it is
-- visible, which means the node is either in the toplevel or expanded.
-- Being displayed does not mean that the node is currently directly
-- visible to the user in the viewport. Based on this reference counting
-- scheme a caching model, for example, can decide whether or not to cache
-- a node based on the reference count. A file-system based model would
-- not want to keep the entire file hierarchy in memory, but just the
-- folders that are currently expanded in every current view.
-- 
-- When working with reference counting, the following rules must be taken
-- into account:
-- 
-- * Never take a reference on a node without owning a reference on its parent.
-- This means that all parent nodes of a referenced node must be referenced
-- as well.
-- * Outstanding references on a deleted node are not released. This is not
-- possible because the node has already been deleted by the time the
-- row-deleted signal is received.
-- * Models are not obligated to emit a signal on rows of which none of its
-- siblings are referenced. To phrase this differently, signals are only
-- required for levels in which nodes are referenced. For the root level
-- however, signals must be emitted at all times (however the root level
-- is always referenced when any view is attached).
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Interfaces.TreeModel
    ( 

-- * Exported types
    TreeModel(..)                           ,
    IsTreeModel                             ,
    toTreeModel                             ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [filterNew]("GI.Gtk.Interfaces.TreeModel#g:method:filterNew"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Interfaces.TreeModel#g:method:foreach"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [iterChildren]("GI.Gtk.Interfaces.TreeModel#g:method:iterChildren"), [iterHasChild]("GI.Gtk.Interfaces.TreeModel#g:method:iterHasChild"), [iterNChildren]("GI.Gtk.Interfaces.TreeModel#g:method:iterNChildren"), [iterNext]("GI.Gtk.Interfaces.TreeModel#g:method:iterNext"), [iterNthChild]("GI.Gtk.Interfaces.TreeModel#g:method:iterNthChild"), [iterParent]("GI.Gtk.Interfaces.TreeModel#g:method:iterParent"), [iterPrevious]("GI.Gtk.Interfaces.TreeModel#g:method:iterPrevious"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refNode]("GI.Gtk.Interfaces.TreeModel#g:method:refNode"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [rowChanged]("GI.Gtk.Interfaces.TreeModel#g:method:rowChanged"), [rowDeleted]("GI.Gtk.Interfaces.TreeModel#g:method:rowDeleted"), [rowHasChildToggled]("GI.Gtk.Interfaces.TreeModel#g:method:rowHasChildToggled"), [rowInserted]("GI.Gtk.Interfaces.TreeModel#g:method:rowInserted"), [rowsReordered]("GI.Gtk.Interfaces.TreeModel#g:method:rowsReordered"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unrefNode]("GI.Gtk.Interfaces.TreeModel#g:method:unrefNode"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getColumnType]("GI.Gtk.Interfaces.TreeModel#g:method:getColumnType"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getFlags]("GI.Gtk.Interfaces.TreeModel#g:method:getFlags"), [getIter]("GI.Gtk.Interfaces.TreeModel#g:method:getIter"), [getIterFirst]("GI.Gtk.Interfaces.TreeModel#g:method:getIterFirst"), [getIterFromString]("GI.Gtk.Interfaces.TreeModel#g:method:getIterFromString"), [getNColumns]("GI.Gtk.Interfaces.TreeModel#g:method:getNColumns"), [getPath]("GI.Gtk.Interfaces.TreeModel#g:method:getPath"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getStringFromIter]("GI.Gtk.Interfaces.TreeModel#g:method:getStringFromIter"), [getValue]("GI.Gtk.Interfaces.TreeModel#g:method:getValue").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveTreeModelMethod                  ,
#endif

-- ** filterNew #method:filterNew#

#if defined(ENABLE_OVERLOADING)
    TreeModelFilterNewMethodInfo            ,
#endif
    treeModelFilterNew                      ,


-- ** foreach #method:foreach#

#if defined(ENABLE_OVERLOADING)
    TreeModelForeachMethodInfo              ,
#endif
    treeModelForeach                        ,


-- ** getColumnType #method:getColumnType#

#if defined(ENABLE_OVERLOADING)
    TreeModelGetColumnTypeMethodInfo        ,
#endif
    treeModelGetColumnType                  ,


-- ** getFlags #method:getFlags#

#if defined(ENABLE_OVERLOADING)
    TreeModelGetFlagsMethodInfo             ,
#endif
    treeModelGetFlags                       ,


-- ** getIter #method:getIter#

#if defined(ENABLE_OVERLOADING)
    TreeModelGetIterMethodInfo              ,
#endif
    treeModelGetIter                        ,


-- ** getIterFirst #method:getIterFirst#

#if defined(ENABLE_OVERLOADING)
    TreeModelGetIterFirstMethodInfo         ,
#endif
    treeModelGetIterFirst                   ,


-- ** getIterFromString #method:getIterFromString#

#if defined(ENABLE_OVERLOADING)
    TreeModelGetIterFromStringMethodInfo    ,
#endif
    treeModelGetIterFromString              ,


-- ** getNColumns #method:getNColumns#

#if defined(ENABLE_OVERLOADING)
    TreeModelGetNColumnsMethodInfo          ,
#endif
    treeModelGetNColumns                    ,


-- ** getPath #method:getPath#

#if defined(ENABLE_OVERLOADING)
    TreeModelGetPathMethodInfo              ,
#endif
    treeModelGetPath                        ,


-- ** getStringFromIter #method:getStringFromIter#

#if defined(ENABLE_OVERLOADING)
    TreeModelGetStringFromIterMethodInfo    ,
#endif
    treeModelGetStringFromIter              ,


-- ** getValue #method:getValue#

#if defined(ENABLE_OVERLOADING)
    TreeModelGetValueMethodInfo             ,
#endif
    treeModelGetValue                       ,


-- ** iterChildren #method:iterChildren#

#if defined(ENABLE_OVERLOADING)
    TreeModelIterChildrenMethodInfo         ,
#endif
    treeModelIterChildren                   ,


-- ** iterHasChild #method:iterHasChild#

#if defined(ENABLE_OVERLOADING)
    TreeModelIterHasChildMethodInfo         ,
#endif
    treeModelIterHasChild                   ,


-- ** iterNChildren #method:iterNChildren#

#if defined(ENABLE_OVERLOADING)
    TreeModelIterNChildrenMethodInfo        ,
#endif
    treeModelIterNChildren                  ,


-- ** iterNext #method:iterNext#

#if defined(ENABLE_OVERLOADING)
    TreeModelIterNextMethodInfo             ,
#endif
    treeModelIterNext                       ,


-- ** iterNthChild #method:iterNthChild#

#if defined(ENABLE_OVERLOADING)
    TreeModelIterNthChildMethodInfo         ,
#endif
    treeModelIterNthChild                   ,


-- ** iterParent #method:iterParent#

#if defined(ENABLE_OVERLOADING)
    TreeModelIterParentMethodInfo           ,
#endif
    treeModelIterParent                     ,


-- ** iterPrevious #method:iterPrevious#

#if defined(ENABLE_OVERLOADING)
    TreeModelIterPreviousMethodInfo         ,
#endif
    treeModelIterPrevious                   ,


-- ** refNode #method:refNode#

#if defined(ENABLE_OVERLOADING)
    TreeModelRefNodeMethodInfo              ,
#endif
    treeModelRefNode                        ,


-- ** rowChanged #method:rowChanged#

#if defined(ENABLE_OVERLOADING)
    TreeModelRowChangedMethodInfo           ,
#endif
    treeModelRowChanged                     ,


-- ** rowDeleted #method:rowDeleted#

#if defined(ENABLE_OVERLOADING)
    TreeModelRowDeletedMethodInfo           ,
#endif
    treeModelRowDeleted                     ,


-- ** rowHasChildToggled #method:rowHasChildToggled#

#if defined(ENABLE_OVERLOADING)
    TreeModelRowHasChildToggledMethodInfo   ,
#endif
    treeModelRowHasChildToggled             ,


-- ** rowInserted #method:rowInserted#

#if defined(ENABLE_OVERLOADING)
    TreeModelRowInsertedMethodInfo          ,
#endif
    treeModelRowInserted                    ,


-- ** rowsReordered #method:rowsReordered#

#if defined(ENABLE_OVERLOADING)
    TreeModelRowsReorderedMethodInfo        ,
#endif
    treeModelRowsReordered                  ,


-- ** unrefNode #method:unrefNode#

#if defined(ENABLE_OVERLOADING)
    TreeModelUnrefNodeMethodInfo            ,
#endif
    treeModelUnrefNode                      ,




 -- * Signals


-- ** rowChanged #signal:rowChanged#

    TreeModelRowChangedCallback             ,
#if defined(ENABLE_OVERLOADING)
    TreeModelRowChangedSignalInfo           ,
#endif
    afterTreeModelRowChanged                ,
    onTreeModelRowChanged                   ,


-- ** rowDeleted #signal:rowDeleted#

    TreeModelRowDeletedCallback             ,
#if defined(ENABLE_OVERLOADING)
    TreeModelRowDeletedSignalInfo           ,
#endif
    afterTreeModelRowDeleted                ,
    onTreeModelRowDeleted                   ,


-- ** rowHasChildToggled #signal:rowHasChildToggled#

    TreeModelRowHasChildToggledCallback     ,
#if defined(ENABLE_OVERLOADING)
    TreeModelRowHasChildToggledSignalInfo   ,
#endif
    afterTreeModelRowHasChildToggled        ,
    onTreeModelRowHasChildToggled           ,


-- ** rowInserted #signal:rowInserted#

    TreeModelRowInsertedCallback            ,
#if defined(ENABLE_OVERLOADING)
    TreeModelRowInsertedSignalInfo          ,
#endif
    afterTreeModelRowInserted               ,
    onTreeModelRowInserted                  ,




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
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreeIter as Gtk.TreeIter
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreePath as Gtk.TreePath

-- interface TreeModel 
-- | Memory-managed wrapper type.
newtype TreeModel = TreeModel (SP.ManagedPtr TreeModel)
    deriving (Eq)

instance SP.ManagedPtrNewtype TreeModel where
    toManagedPtr (TreeModel p) = p

foreign import ccall "gtk_tree_model_get_type"
    c_gtk_tree_model_get_type :: IO B.Types.GType

instance B.Types.TypedObject TreeModel where
    glibType = c_gtk_tree_model_get_type

instance B.Types.GObject TreeModel

-- | Type class for types which can be safely cast to `TreeModel`, for instance with `toTreeModel`.
class (SP.GObject o, O.IsDescendantOf TreeModel o) => IsTreeModel o
instance (SP.GObject o, O.IsDescendantOf TreeModel o) => IsTreeModel o

instance O.HasParentTypes TreeModel
type instance O.ParentTypes TreeModel = '[GObject.Object.Object]

-- | Cast to `TreeModel`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTreeModel :: (MIO.MonadIO m, IsTreeModel o) => o -> m TreeModel
toTreeModel = MIO.liftIO . B.ManagedPtr.unsafeCastTo TreeModel

-- | Convert 'TreeModel' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TreeModel) where
    gvalueGType_ = c_gtk_tree_model_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr TreeModel)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr TreeModel)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject TreeModel ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TreeModel
type instance O.AttributeList TreeModel = TreeModelAttributeList
type TreeModelAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveTreeModelMethod (t :: Symbol) (o :: *) :: * where
    ResolveTreeModelMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTreeModelMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTreeModelMethod "filterNew" o = TreeModelFilterNewMethodInfo
    ResolveTreeModelMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTreeModelMethod "foreach" o = TreeModelForeachMethodInfo
    ResolveTreeModelMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTreeModelMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTreeModelMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTreeModelMethod "iterChildren" o = TreeModelIterChildrenMethodInfo
    ResolveTreeModelMethod "iterHasChild" o = TreeModelIterHasChildMethodInfo
    ResolveTreeModelMethod "iterNChildren" o = TreeModelIterNChildrenMethodInfo
    ResolveTreeModelMethod "iterNext" o = TreeModelIterNextMethodInfo
    ResolveTreeModelMethod "iterNthChild" o = TreeModelIterNthChildMethodInfo
    ResolveTreeModelMethod "iterParent" o = TreeModelIterParentMethodInfo
    ResolveTreeModelMethod "iterPrevious" o = TreeModelIterPreviousMethodInfo
    ResolveTreeModelMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTreeModelMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTreeModelMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTreeModelMethod "refNode" o = TreeModelRefNodeMethodInfo
    ResolveTreeModelMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTreeModelMethod "rowChanged" o = TreeModelRowChangedMethodInfo
    ResolveTreeModelMethod "rowDeleted" o = TreeModelRowDeletedMethodInfo
    ResolveTreeModelMethod "rowHasChildToggled" o = TreeModelRowHasChildToggledMethodInfo
    ResolveTreeModelMethod "rowInserted" o = TreeModelRowInsertedMethodInfo
    ResolveTreeModelMethod "rowsReordered" o = TreeModelRowsReorderedMethodInfo
    ResolveTreeModelMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTreeModelMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTreeModelMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTreeModelMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTreeModelMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTreeModelMethod "unrefNode" o = TreeModelUnrefNodeMethodInfo
    ResolveTreeModelMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTreeModelMethod "getColumnType" o = TreeModelGetColumnTypeMethodInfo
    ResolveTreeModelMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTreeModelMethod "getFlags" o = TreeModelGetFlagsMethodInfo
    ResolveTreeModelMethod "getIter" o = TreeModelGetIterMethodInfo
    ResolveTreeModelMethod "getIterFirst" o = TreeModelGetIterFirstMethodInfo
    ResolveTreeModelMethod "getIterFromString" o = TreeModelGetIterFromStringMethodInfo
    ResolveTreeModelMethod "getNColumns" o = TreeModelGetNColumnsMethodInfo
    ResolveTreeModelMethod "getPath" o = TreeModelGetPathMethodInfo
    ResolveTreeModelMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTreeModelMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTreeModelMethod "getStringFromIter" o = TreeModelGetStringFromIterMethodInfo
    ResolveTreeModelMethod "getValue" o = TreeModelGetValueMethodInfo
    ResolveTreeModelMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTreeModelMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTreeModelMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTreeModelMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTreeModelMethod t TreeModel, O.OverloadedMethod info TreeModel p) => OL.IsLabel t (TreeModel -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTreeModelMethod t TreeModel, O.OverloadedMethod info TreeModel p, R.HasField t TreeModel p) => R.HasField t TreeModel p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTreeModelMethod t TreeModel, O.OverloadedMethodInfo info TreeModel) => OL.IsLabel t (O.MethodProxy info TreeModel) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- method TreeModel::filter_new
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "child_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModel." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "root"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreePath or %NULL."
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

foreign import ccall "gtk_tree_model_filter_new" gtk_tree_model_filter_new :: 
    Ptr TreeModel ->                        -- child_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreePath.TreePath ->            -- root : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO (Ptr TreeModel)

-- | Creates a new t'GI.Gtk.Interfaces.TreeModel.TreeModel', with /@childModel@/ as the child_model
-- and /@root@/ as the virtual root.
-- 
-- /Since: 2.4/
treeModelFilterNew ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@childModel@/: A t'GI.Gtk.Interfaces.TreeModel.TreeModel'.
    -> Maybe (Gtk.TreePath.TreePath)
    -- ^ /@root@/: A t'GI.Gtk.Structs.TreePath.TreePath' or 'P.Nothing'.
    -> m TreeModel
    -- ^ __Returns:__ A new t'GI.Gtk.Interfaces.TreeModel.TreeModel'.
treeModelFilterNew childModel root = liftIO $ do
    childModel' <- unsafeManagedPtrCastPtr childModel
    maybeRoot <- case root of
        Nothing -> return nullPtr
        Just jRoot -> do
            jRoot' <- unsafeManagedPtrGetPtr jRoot
            return jRoot'
    result <- gtk_tree_model_filter_new childModel' maybeRoot
    checkUnexpectedReturnNULL "treeModelFilterNew" result
    result' <- (wrapObject TreeModel) result
    touchManagedPtr childModel
    whenJust root touchManagedPtr
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeModelFilterNewMethodInfo
instance (signature ~ (Maybe (Gtk.TreePath.TreePath) -> m TreeModel), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelFilterNewMethodInfo a signature where
    overloadedMethod = treeModelFilterNew

instance O.OverloadedMethodInfo TreeModelFilterNewMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelFilterNew",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelFilterNew"
        })


#endif

-- method TreeModel::foreach
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--                 Name { namespace = "Gtk" , name = "TreeModelForeachFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a function to be called on each row"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeCall
--           , argClosure = 2
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "user data to passed to @func"
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

foreign import ccall "gtk_tree_model_foreach" gtk_tree_model_foreach :: 
    Ptr TreeModel ->                        -- model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    FunPtr Gtk.Callbacks.C_TreeModelForeachFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "TreeModelForeachFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    IO ()

-- | Calls func on each node in model in a depth-first fashion.
-- 
-- If /@func@/ returns 'P.True', then the tree ceases to be walked,
-- and 'GI.Gtk.Interfaces.TreeModel.treeModelForeach' returns.
treeModelForeach ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@model@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.Callbacks.TreeModelForeachFunc
    -- ^ /@func@/: a function to be called on each row
    -> m ()
treeModelForeach model func = liftIO $ do
    model' <- unsafeManagedPtrCastPtr model
    func' <- Gtk.Callbacks.mk_TreeModelForeachFunc (Gtk.Callbacks.wrap_TreeModelForeachFunc Nothing (Gtk.Callbacks.drop_closures_TreeModelForeachFunc func))
    let userData = nullPtr
    gtk_tree_model_foreach model' func' userData
    safeFreeFunPtr $ castFunPtrToPtr func'
    touchManagedPtr model
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeModelForeachMethodInfo
instance (signature ~ (Gtk.Callbacks.TreeModelForeachFunc -> m ()), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelForeachMethodInfo a signature where
    overloadedMethod = treeModelForeach

instance O.OverloadedMethodInfo TreeModelForeachMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelForeach",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelForeach"
        })


#endif

-- method TreeModel::get_column_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the column index" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TGType)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_model_get_column_type" gtk_tree_model_get_column_type :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Int32 ->                                -- index_ : TBasicType TInt
    IO CGType

-- | Returns the type of the column.
treeModelGetColumnType ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Int32
    -- ^ /@index_@/: the column index
    -> m GType
    -- ^ __Returns:__ the type of the column
treeModelGetColumnType treeModel index_ = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    result <- gtk_tree_model_get_column_type treeModel' index_
    let result' = GType result
    touchManagedPtr treeModel
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeModelGetColumnTypeMethodInfo
instance (signature ~ (Int32 -> m GType), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelGetColumnTypeMethodInfo a signature where
    overloadedMethod = treeModelGetColumnType

instance O.OverloadedMethodInfo TreeModelGetColumnTypeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelGetColumnType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelGetColumnType"
        })


#endif

-- method TreeModel::get_flags
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TInterface Name { namespace = "Gtk" , name = "TreeModelFlags" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_model_get_flags" gtk_tree_model_get_flags :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    IO CUInt

-- | Returns a set of flags supported by this interface.
-- 
-- The flags are a bitwise combination of t'GI.Gtk.Flags.TreeModelFlags'.
-- The flags supported should not change during the lifetime
-- of the /@treeModel@/.
treeModelGetFlags ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> m [Gtk.Flags.TreeModelFlags]
    -- ^ __Returns:__ the flags supported by this interface
treeModelGetFlags treeModel = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    result <- gtk_tree_model_get_flags treeModel'
    let result' = wordToGFlags result
    touchManagedPtr treeModel
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeModelGetFlagsMethodInfo
instance (signature ~ (m [Gtk.Flags.TreeModelFlags]), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelGetFlagsMethodInfo a signature where
    overloadedMethod = treeModelGetFlags

instance O.OverloadedMethodInfo TreeModelGetFlagsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelGetFlags",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelGetFlags"
        })


#endif

-- method TreeModel::get_iter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the uninitialized #GtkTreeIter-struct"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkTreePath-struct"
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

foreign import ccall "gtk_tree_model_get_iter" gtk_tree_model_get_iter :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO CInt

-- | Sets /@iter@/ to a valid iterator pointing to /@path@/.  If /@path@/ does
-- not exist, /@iter@/ is set to an invalid iterator and 'P.False' is returned.
treeModelGetIter ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: the t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> m ((Bool, Gtk.TreeIter.TreeIter))
    -- ^ __Returns:__ 'P.True', if /@iter@/ was set
treeModelGetIter treeModel path = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    iter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_tree_model_get_iter treeModel' iter path'
    let result' = (/= 0) result
    iter' <- (wrapBoxed Gtk.TreeIter.TreeIter) iter
    touchManagedPtr treeModel
    touchManagedPtr path
    return (result', iter')

#if defined(ENABLE_OVERLOADING)
data TreeModelGetIterMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> m ((Bool, Gtk.TreeIter.TreeIter))), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelGetIterMethodInfo a signature where
    overloadedMethod = treeModelGetIter

instance O.OverloadedMethodInfo TreeModelGetIterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelGetIter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelGetIter"
        })


#endif

-- method TreeModel::get_iter_first
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the uninitialized #GtkTreeIter-struct"
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
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_model_get_iter_first" gtk_tree_model_get_iter_first :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO CInt

-- | Initializes /@iter@/ with the first iterator in the tree
-- (the one at the path \"0\") and returns 'P.True'. Returns
-- 'P.False' if the tree is empty.
treeModelGetIterFirst ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> m ((Bool, Gtk.TreeIter.TreeIter))
    -- ^ __Returns:__ 'P.True', if /@iter@/ was set
treeModelGetIterFirst treeModel = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    iter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    result <- gtk_tree_model_get_iter_first treeModel' iter
    let result' = (/= 0) result
    iter' <- (wrapBoxed Gtk.TreeIter.TreeIter) iter
    touchManagedPtr treeModel
    return (result', iter')

#if defined(ENABLE_OVERLOADING)
data TreeModelGetIterFirstMethodInfo
instance (signature ~ (m ((Bool, Gtk.TreeIter.TreeIter))), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelGetIterFirstMethodInfo a signature where
    overloadedMethod = treeModelGetIterFirst

instance O.OverloadedMethodInfo TreeModelGetIterFirstMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelGetIterFirst",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelGetIterFirst"
        })


#endif

-- method TreeModel::get_iter_from_string
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an uninitialized #GtkTreeIter-struct"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path_string"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a string representation of a #GtkTreePath-struct"
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

foreign import ccall "gtk_tree_model_get_iter_from_string" gtk_tree_model_get_iter_from_string :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    CString ->                              -- path_string : TBasicType TUTF8
    IO CInt

-- | Sets /@iter@/ to a valid iterator pointing to /@pathString@/, if it
-- exists. Otherwise, /@iter@/ is left invalid and 'P.False' is returned.
treeModelGetIterFromString ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> T.Text
    -- ^ /@pathString@/: a string representation of a t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> m ((Bool, Gtk.TreeIter.TreeIter))
    -- ^ __Returns:__ 'P.True', if /@iter@/ was set
treeModelGetIterFromString treeModel pathString = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    iter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    pathString' <- textToCString pathString
    result <- gtk_tree_model_get_iter_from_string treeModel' iter pathString'
    let result' = (/= 0) result
    iter' <- (wrapBoxed Gtk.TreeIter.TreeIter) iter
    touchManagedPtr treeModel
    freeMem pathString'
    return (result', iter')

#if defined(ENABLE_OVERLOADING)
data TreeModelGetIterFromStringMethodInfo
instance (signature ~ (T.Text -> m ((Bool, Gtk.TreeIter.TreeIter))), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelGetIterFromStringMethodInfo a signature where
    overloadedMethod = treeModelGetIterFromString

instance O.OverloadedMethodInfo TreeModelGetIterFromStringMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelGetIterFromString",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelGetIterFromString"
        })


#endif

-- method TreeModel::get_n_columns
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_model_get_n_columns" gtk_tree_model_get_n_columns :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    IO Int32

-- | Returns the number of columns supported by /@treeModel@/.
treeModelGetNColumns ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> m Int32
    -- ^ __Returns:__ the number of columns
treeModelGetNColumns treeModel = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    result <- gtk_tree_model_get_n_columns treeModel'
    touchManagedPtr treeModel
    return result

#if defined(ENABLE_OVERLOADING)
data TreeModelGetNColumnsMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelGetNColumnsMethodInfo a signature where
    overloadedMethod = treeModelGetNColumns

instance O.OverloadedMethodInfo TreeModelGetNColumnsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelGetNColumns",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelGetNColumns"
        })


#endif

-- method TreeModel::get_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the #GtkTreeIter-struct"
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

foreign import ccall "gtk_tree_model_get_path" gtk_tree_model_get_path :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO (Ptr Gtk.TreePath.TreePath)

-- | Returns a newly-created t'GI.Gtk.Structs.TreePath.TreePath'-struct referenced by /@iter@/.
-- 
-- This path should be freed with 'GI.Gtk.Structs.TreePath.treePathFree'.
treeModelGetPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: the t'GI.Gtk.Structs.TreeIter.TreeIter'-struct
    -> m Gtk.TreePath.TreePath
    -- ^ __Returns:__ a newly-created t'GI.Gtk.Structs.TreePath.TreePath'-struct
treeModelGetPath treeModel iter = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_tree_model_get_path treeModel' iter'
    checkUnexpectedReturnNULL "treeModelGetPath" result
    result' <- (wrapBoxed Gtk.TreePath.TreePath) result
    touchManagedPtr treeModel
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeModelGetPathMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m Gtk.TreePath.TreePath), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelGetPathMethodInfo a signature where
    overloadedMethod = treeModelGetPath

instance O.OverloadedMethodInfo TreeModelGetPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelGetPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelGetPath"
        })


#endif

-- method TreeModel::get_string_from_iter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a #GtkTreeIter-struct"
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

foreign import ccall "gtk_tree_model_get_string_from_iter" gtk_tree_model_get_string_from_iter :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO CString

-- | Generates a string representation of the iter.
-- 
-- This string is a “:” separated list of numbers.
-- For example, “4:10:0:3” would be an acceptable
-- return value for this string.
-- 
-- /Since: 2.2/
treeModelGetStringFromIter ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TreeIter.TreeIter'-struct
    -> m T.Text
    -- ^ __Returns:__ a newly-allocated string.
    --     Must be freed with 'GI.GLib.Functions.free'.
treeModelGetStringFromIter treeModel iter = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_tree_model_get_string_from_iter treeModel' iter'
    checkUnexpectedReturnNULL "treeModelGetStringFromIter" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr treeModel
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeModelGetStringFromIterMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m T.Text), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelGetStringFromIterMethodInfo a signature where
    overloadedMethod = treeModelGetStringFromIter

instance O.OverloadedMethodInfo TreeModelGetStringFromIterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelGetStringFromIter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelGetStringFromIter"
        })


#endif

-- method TreeModel::get_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the #GtkTreeIter-struct"
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
--                 { rawDocText = Just "the column to lookup the value at"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TGValue
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an empty #GValue to set"
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

foreign import ccall "gtk_tree_model_get_value" gtk_tree_model_get_value :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Int32 ->                                -- column : TBasicType TInt
    Ptr GValue ->                           -- value : TGValue
    IO ()

-- | Initializes and sets /@value@/ to that at /@column@/.
-- 
-- When done with /@value@/, 'GI.GObject.Structs.Value.valueUnset' needs to be called
-- to free any allocated memory.
treeModelGetValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: the t'GI.Gtk.Structs.TreeIter.TreeIter'-struct
    -> Int32
    -- ^ /@column@/: the column to lookup the value at
    -> m (GValue)
treeModelGetValue treeModel iter column = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    iter' <- unsafeManagedPtrGetPtr iter
    value <- SP.callocBytes 24 :: IO (Ptr GValue)
    gtk_tree_model_get_value treeModel' iter' column value
    value' <- B.GValue.wrapGValuePtr value
    touchManagedPtr treeModel
    touchManagedPtr iter
    return value'

#if defined(ENABLE_OVERLOADING)
data TreeModelGetValueMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> Int32 -> m (GValue)), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelGetValueMethodInfo a signature where
    overloadedMethod = treeModelGetValue

instance O.OverloadedMethodInfo TreeModelGetValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelGetValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelGetValue"
        })


#endif

-- method TreeModel::iter_children
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the new #GtkTreeIter-struct to be set to the child"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "parent"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkTreeIter-struct, or %NULL"
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

foreign import ccall "gtk_tree_model_iter_children" gtk_tree_model_iter_children :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Ptr Gtk.TreeIter.TreeIter ->            -- parent : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO CInt

-- | Sets /@iter@/ to point to the first child of /@parent@/.
-- 
-- If /@parent@/ has no children, 'P.False' is returned and /@iter@/ is
-- set to be invalid. /@parent@/ will remain a valid node after this
-- function has been called.
-- 
-- If /@parent@/ is 'P.Nothing' returns the first node, equivalent to
-- @gtk_tree_model_get_iter_first (tree_model, iter);@
treeModelIterChildren ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Maybe (Gtk.TreeIter.TreeIter)
    -- ^ /@parent@/: the t'GI.Gtk.Structs.TreeIter.TreeIter'-struct, or 'P.Nothing'
    -> m ((Bool, Gtk.TreeIter.TreeIter))
    -- ^ __Returns:__ 'P.True', if /@iter@/ has been set to the first child
treeModelIterChildren treeModel parent = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    iter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    maybeParent <- case parent of
        Nothing -> return nullPtr
        Just jParent -> do
            jParent' <- unsafeManagedPtrGetPtr jParent
            return jParent'
    result <- gtk_tree_model_iter_children treeModel' iter maybeParent
    let result' = (/= 0) result
    iter' <- (wrapBoxed Gtk.TreeIter.TreeIter) iter
    touchManagedPtr treeModel
    whenJust parent touchManagedPtr
    return (result', iter')

#if defined(ENABLE_OVERLOADING)
data TreeModelIterChildrenMethodInfo
instance (signature ~ (Maybe (Gtk.TreeIter.TreeIter) -> m ((Bool, Gtk.TreeIter.TreeIter))), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelIterChildrenMethodInfo a signature where
    overloadedMethod = treeModelIterChildren

instance O.OverloadedMethodInfo TreeModelIterChildrenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelIterChildren",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelIterChildren"
        })


#endif

-- method TreeModel::iter_has_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the #GtkTreeIter-struct to test for children"
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

foreign import ccall "gtk_tree_model_iter_has_child" gtk_tree_model_iter_has_child :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO CInt

-- | Returns 'P.True' if /@iter@/ has children, 'P.False' otherwise.
treeModelIterHasChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: the t'GI.Gtk.Structs.TreeIter.TreeIter'-struct to test for children
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ has children
treeModelIterHasChild treeModel iter = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_tree_model_iter_has_child treeModel' iter'
    let result' = (/= 0) result
    touchManagedPtr treeModel
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeModelIterHasChildMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m Bool), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelIterHasChildMethodInfo a signature where
    overloadedMethod = treeModelIterHasChild

instance O.OverloadedMethodInfo TreeModelIterHasChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelIterHasChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelIterHasChild"
        })


#endif

-- method TreeModel::iter_n_children
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkTreeIter-struct, or %NULL"
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

foreign import ccall "gtk_tree_model_iter_n_children" gtk_tree_model_iter_n_children :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO Int32

-- | Returns the number of children that /@iter@/ has.
-- 
-- As a special case, if /@iter@/ is 'P.Nothing', then the number
-- of toplevel nodes is returned.
treeModelIterNChildren ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Maybe (Gtk.TreeIter.TreeIter)
    -- ^ /@iter@/: the t'GI.Gtk.Structs.TreeIter.TreeIter'-struct, or 'P.Nothing'
    -> m Int32
    -- ^ __Returns:__ the number of children of /@iter@/
treeModelIterNChildren treeModel iter = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    maybeIter <- case iter of
        Nothing -> return nullPtr
        Just jIter -> do
            jIter' <- unsafeManagedPtrGetPtr jIter
            return jIter'
    result <- gtk_tree_model_iter_n_children treeModel' maybeIter
    touchManagedPtr treeModel
    whenJust iter touchManagedPtr
    return result

#if defined(ENABLE_OVERLOADING)
data TreeModelIterNChildrenMethodInfo
instance (signature ~ (Maybe (Gtk.TreeIter.TreeIter) -> m Int32), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelIterNChildrenMethodInfo a signature where
    overloadedMethod = treeModelIterNChildren

instance O.OverloadedMethodInfo TreeModelIterNChildrenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelIterNChildren",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelIterNChildren"
        })


#endif

-- method TreeModel::iter_next
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the #GtkTreeIter-struct"
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

foreign import ccall "gtk_tree_model_iter_next" gtk_tree_model_iter_next :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO CInt

-- | Sets /@iter@/ to point to the node following it at the current level.
-- 
-- If there is no next /@iter@/, 'P.False' is returned and /@iter@/ is set
-- to be invalid.
treeModelIterNext ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: the t'GI.Gtk.Structs.TreeIter.TreeIter'-struct
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ has been changed to the next node
treeModelIterNext treeModel iter = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_tree_model_iter_next treeModel' iter'
    let result' = (/= 0) result
    touchManagedPtr treeModel
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeModelIterNextMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m Bool), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelIterNextMethodInfo a signature where
    overloadedMethod = treeModelIterNext

instance O.OverloadedMethodInfo TreeModelIterNextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelIterNext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelIterNext"
        })


#endif

-- method TreeModel::iter_nth_child
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkTreeIter-struct to set to the nth child"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "parent"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkTreeIter-struct to get the child from, or %NULL."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the index of the desired child"
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

foreign import ccall "gtk_tree_model_iter_nth_child" gtk_tree_model_iter_nth_child :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Ptr Gtk.TreeIter.TreeIter ->            -- parent : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Int32 ->                                -- n : TBasicType TInt
    IO CInt

-- | Sets /@iter@/ to be the child of /@parent@/, using the given index.
-- 
-- The first index is 0. If /@n@/ is too big, or /@parent@/ has no children,
-- /@iter@/ is set to an invalid iterator and 'P.False' is returned. /@parent@/
-- will remain a valid node after this function has been called. As a
-- special case, if /@parent@/ is 'P.Nothing', then the /@n@/-th root node
-- is set.
treeModelIterNthChild ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Maybe (Gtk.TreeIter.TreeIter)
    -- ^ /@parent@/: the t'GI.Gtk.Structs.TreeIter.TreeIter'-struct to get the child from, or 'P.Nothing'.
    -> Int32
    -- ^ /@n@/: the index of the desired child
    -> m ((Bool, Gtk.TreeIter.TreeIter))
    -- ^ __Returns:__ 'P.True', if /@parent@/ has an /@n@/-th child
treeModelIterNthChild treeModel parent n = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    iter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    maybeParent <- case parent of
        Nothing -> return nullPtr
        Just jParent -> do
            jParent' <- unsafeManagedPtrGetPtr jParent
            return jParent'
    result <- gtk_tree_model_iter_nth_child treeModel' iter maybeParent n
    let result' = (/= 0) result
    iter' <- (wrapBoxed Gtk.TreeIter.TreeIter) iter
    touchManagedPtr treeModel
    whenJust parent touchManagedPtr
    return (result', iter')

#if defined(ENABLE_OVERLOADING)
data TreeModelIterNthChildMethodInfo
instance (signature ~ (Maybe (Gtk.TreeIter.TreeIter) -> Int32 -> m ((Bool, Gtk.TreeIter.TreeIter))), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelIterNthChildMethodInfo a signature where
    overloadedMethod = treeModelIterNthChild

instance O.OverloadedMethodInfo TreeModelIterNthChildMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelIterNthChild",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelIterNthChild"
        })


#endif

-- method TreeModel::iter_parent
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the new #GtkTreeIter-struct to set to the parent"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "child"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkTreeIter-struct"
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

foreign import ccall "gtk_tree_model_iter_parent" gtk_tree_model_iter_parent :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Ptr Gtk.TreeIter.TreeIter ->            -- child : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO CInt

-- | Sets /@iter@/ to be the parent of /@child@/.
-- 
-- If /@child@/ is at the toplevel, and doesn’t have a parent, then
-- /@iter@/ is set to an invalid iterator and 'P.False' is returned.
-- /@child@/ will remain a valid node after this function has been
-- called.
-- 
-- /@iter@/ will be initialized before the lookup is performed, so /@child@/
-- and /@iter@/ cannot point to the same memory location.
treeModelIterParent ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreeIter.TreeIter
    -- ^ /@child@/: the t'GI.Gtk.Structs.TreeIter.TreeIter'-struct
    -> m ((Bool, Gtk.TreeIter.TreeIter))
    -- ^ __Returns:__ 'P.True', if /@iter@/ is set to the parent of /@child@/
treeModelIterParent treeModel child = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    iter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    child' <- unsafeManagedPtrGetPtr child
    result <- gtk_tree_model_iter_parent treeModel' iter child'
    let result' = (/= 0) result
    iter' <- (wrapBoxed Gtk.TreeIter.TreeIter) iter
    touchManagedPtr treeModel
    touchManagedPtr child
    return (result', iter')

#if defined(ENABLE_OVERLOADING)
data TreeModelIterParentMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m ((Bool, Gtk.TreeIter.TreeIter))), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelIterParentMethodInfo a signature where
    overloadedMethod = treeModelIterParent

instance O.OverloadedMethodInfo TreeModelIterParentMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelIterParent",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelIterParent"
        })


#endif

-- method TreeModel::iter_previous
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the #GtkTreeIter-struct"
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

foreign import ccall "gtk_tree_model_iter_previous" gtk_tree_model_iter_previous :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO CInt

-- | Sets /@iter@/ to point to the previous node at the current level.
-- 
-- If there is no previous /@iter@/, 'P.False' is returned and /@iter@/ is
-- set to be invalid.
-- 
-- /Since: 3.0/
treeModelIterPrevious ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: the t'GI.Gtk.Structs.TreeIter.TreeIter'-struct
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ has been changed to the previous node
treeModelIterPrevious treeModel iter = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_tree_model_iter_previous treeModel' iter'
    let result' = (/= 0) result
    touchManagedPtr treeModel
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeModelIterPreviousMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m Bool), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelIterPreviousMethodInfo a signature where
    overloadedMethod = treeModelIterPrevious

instance O.OverloadedMethodInfo TreeModelIterPreviousMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelIterPrevious",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelIterPrevious"
        })


#endif

-- method TreeModel::ref_node
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the #GtkTreeIter-struct"
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

foreign import ccall "gtk_tree_model_ref_node" gtk_tree_model_ref_node :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Lets the tree ref the node.
-- 
-- This is an optional method for models to implement.
-- To be more specific, models may ignore this call as it exists
-- primarily for performance reasons.
-- 
-- This function is primarily meant as a way for views to let
-- caching models know when nodes are being displayed (and hence,
-- whether or not to cache that node). Being displayed means a node
-- is in an expanded branch, regardless of whether the node is currently
-- visible in the viewport. For example, a file-system based model
-- would not want to keep the entire file-hierarchy in memory,
-- just the sections that are currently being displayed by
-- every current view.
-- 
-- A model should be expected to be able to get an iter independent
-- of its reffed state.
treeModelRefNode ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: the t'GI.Gtk.Structs.TreeIter.TreeIter'-struct
    -> m ()
treeModelRefNode treeModel iter = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    iter' <- unsafeManagedPtrGetPtr iter
    gtk_tree_model_ref_node treeModel' iter'
    touchManagedPtr treeModel
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeModelRefNodeMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m ()), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelRefNodeMethodInfo a signature where
    overloadedMethod = treeModelRefNode

instance O.OverloadedMethodInfo TreeModelRefNodeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelRefNode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelRefNode"
        })


#endif

-- method TreeModel::row_changed
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkTreePath-struct pointing to the changed row"
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
--                 { rawDocText =
--                     Just "a valid #GtkTreeIter-struct pointing to the changed row"
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

foreign import ccall "gtk_tree_model_row_changed" gtk_tree_model_row_changed :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Emits the [TreeModel::rowChanged]("GI.Gtk.Interfaces.TreeModel#g:signal:rowChanged") signal on /@treeModel@/.
treeModelRowChanged ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct pointing to the changed row
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: a valid t'GI.Gtk.Structs.TreeIter.TreeIter'-struct pointing to the changed row
    -> m ()
treeModelRowChanged treeModel path iter = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    path' <- unsafeManagedPtrGetPtr path
    iter' <- unsafeManagedPtrGetPtr iter
    gtk_tree_model_row_changed treeModel' path' iter'
    touchManagedPtr treeModel
    touchManagedPtr path
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeModelRowChangedMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> Gtk.TreeIter.TreeIter -> m ()), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelRowChangedMethodInfo a signature where
    overloadedMethod = treeModelRowChanged

instance O.OverloadedMethodInfo TreeModelRowChangedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelRowChanged",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelRowChanged"
        })


#endif

-- method TreeModel::row_deleted
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GtkTreePath-struct pointing to the previous location of\n    the deleted row"
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

foreign import ccall "gtk_tree_model_row_deleted" gtk_tree_model_row_deleted :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO ()

-- | Emits the [TreeModel::rowDeleted]("GI.Gtk.Interfaces.TreeModel#g:signal:rowDeleted") signal on /@treeModel@/.
-- 
-- This should be called by models after a row has been removed.
-- The location pointed to by /@path@/ should be the location that
-- the row previously was at. It may not be a valid location anymore.
-- 
-- Nodes that are deleted are not unreffed, this means that any
-- outstanding references on the deleted node should not be released.
treeModelRowDeleted ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct pointing to the previous location of
    --     the deleted row
    -> m ()
treeModelRowDeleted treeModel path = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    path' <- unsafeManagedPtrGetPtr path
    gtk_tree_model_row_deleted treeModel' path'
    touchManagedPtr treeModel
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeModelRowDeletedMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> m ()), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelRowDeletedMethodInfo a signature where
    overloadedMethod = treeModelRowDeleted

instance O.OverloadedMethodInfo TreeModelRowDeletedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelRowDeleted",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelRowDeleted"
        })


#endif

-- method TreeModel::row_has_child_toggled
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkTreePath-struct pointing to the changed row"
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
--                 { rawDocText =
--                     Just "a valid #GtkTreeIter-struct pointing to the changed row"
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

foreign import ccall "gtk_tree_model_row_has_child_toggled" gtk_tree_model_row_has_child_toggled :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Emits the [TreeModel::rowHasChildToggled]("GI.Gtk.Interfaces.TreeModel#g:signal:rowHasChildToggled") signal on
-- /@treeModel@/. This should be called by models after the child
-- state of a node changes.
treeModelRowHasChildToggled ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct pointing to the changed row
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: a valid t'GI.Gtk.Structs.TreeIter.TreeIter'-struct pointing to the changed row
    -> m ()
treeModelRowHasChildToggled treeModel path iter = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    path' <- unsafeManagedPtrGetPtr path
    iter' <- unsafeManagedPtrGetPtr iter
    gtk_tree_model_row_has_child_toggled treeModel' path' iter'
    touchManagedPtr treeModel
    touchManagedPtr path
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeModelRowHasChildToggledMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> Gtk.TreeIter.TreeIter -> m ()), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelRowHasChildToggledMethodInfo a signature where
    overloadedMethod = treeModelRowHasChildToggled

instance O.OverloadedMethodInfo TreeModelRowHasChildToggledMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelRowHasChildToggled",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelRowHasChildToggled"
        })


#endif

-- method TreeModel::row_inserted
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkTreePath-struct pointing to the inserted row"
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
--                 { rawDocText =
--                     Just "a valid #GtkTreeIter-struct pointing to the inserted row"
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

foreign import ccall "gtk_tree_model_row_inserted" gtk_tree_model_row_inserted :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Emits the [TreeModel::rowInserted]("GI.Gtk.Interfaces.TreeModel#g:signal:rowInserted") signal on /@treeModel@/.
treeModelRowInserted ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct pointing to the inserted row
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: a valid t'GI.Gtk.Structs.TreeIter.TreeIter'-struct pointing to the inserted row
    -> m ()
treeModelRowInserted treeModel path iter = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    path' <- unsafeManagedPtrGetPtr path
    iter' <- unsafeManagedPtrGetPtr iter
    gtk_tree_model_row_inserted treeModel' path' iter'
    touchManagedPtr treeModel
    touchManagedPtr path
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeModelRowInsertedMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> Gtk.TreeIter.TreeIter -> m ()), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelRowInsertedMethodInfo a signature where
    overloadedMethod = treeModelRowInserted

instance O.OverloadedMethodInfo TreeModelRowInsertedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelRowInserted",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelRowInserted"
        })


#endif

-- method TreeModel::rows_reordered
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GtkTreePath-struct pointing to the tree node whose children\n    have been reordered"
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
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a valid #GtkTreeIter-struct pointing to the node\n    whose children have been reordered, or %NULL if the depth\n    of @path is 0"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "new_order"
--           , argType = TCArray False (-1) 4 (TBasicType TInt)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "an array of integers\n    mapping the current position of each child to its old\n    position before the re-ordering,\n    i.e. @new_order`[newpos] = oldpos`"
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
--                 { rawDocText = Just "length of @new_order array"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "length"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "length of @new_order array"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_model_rows_reordered_with_length" gtk_tree_model_rows_reordered_with_length :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Ptr Int32 ->                            -- new_order : TCArray False (-1) 4 (TBasicType TInt)
    Int32 ->                                -- length : TBasicType TInt
    IO ()

-- | Emits the t'GI.Gtk.Interfaces.TreeModel.TreeModel'::@/rows-reordered/@ signal on /@treeModel@/.
-- 
-- This should be called by models when their rows have been
-- reordered.
-- 
-- /Since: 3.10/
treeModelRowsReordered ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct pointing to the tree node whose children
    --     have been reordered
    -> Maybe (Gtk.TreeIter.TreeIter)
    -- ^ /@iter@/: a valid t'GI.Gtk.Structs.TreeIter.TreeIter'-struct pointing to the node
    --     whose children have been reordered, or 'P.Nothing' if the depth
    --     of /@path@/ is 0
    -> [Int32]
    -- ^ /@newOrder@/: an array of integers
    --     mapping the current position of each child to its old
    --     position before the re-ordering,
    --     i.e. /@newOrder@/@[newpos] = oldpos@
    -> m ()
treeModelRowsReordered treeModel path iter newOrder = liftIO $ do
    let length_ = fromIntegral $ P.length newOrder
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    path' <- unsafeManagedPtrGetPtr path
    maybeIter <- case iter of
        Nothing -> return nullPtr
        Just jIter -> do
            jIter' <- unsafeManagedPtrGetPtr jIter
            return jIter'
    newOrder' <- packStorableArray newOrder
    gtk_tree_model_rows_reordered_with_length treeModel' path' maybeIter newOrder' length_
    touchManagedPtr treeModel
    touchManagedPtr path
    whenJust iter touchManagedPtr
    freeMem newOrder'
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeModelRowsReorderedMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> Maybe (Gtk.TreeIter.TreeIter) -> [Int32] -> m ()), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelRowsReorderedMethodInfo a signature where
    overloadedMethod = treeModelRowsReordered

instance O.OverloadedMethodInfo TreeModelRowsReorderedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelRowsReordered",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelRowsReordered"
        })


#endif

-- method TreeModel::unref_node
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "tree_model"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModel" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeModel" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the #GtkTreeIter-struct"
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

foreign import ccall "gtk_tree_model_unref_node" gtk_tree_model_unref_node :: 
    Ptr TreeModel ->                        -- tree_model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Lets the tree unref the node.
-- 
-- This is an optional method for models to implement.
-- To be more specific, models may ignore this call as it exists
-- primarily for performance reasons. For more information on what
-- this means, see 'GI.Gtk.Interfaces.TreeModel.treeModelRefNode'.
-- 
-- Please note that nodes that are deleted are not unreffed.
treeModelUnrefNode ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModel a) =>
    a
    -- ^ /@treeModel@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: the t'GI.Gtk.Structs.TreeIter.TreeIter'-struct
    -> m ()
treeModelUnrefNode treeModel iter = liftIO $ do
    treeModel' <- unsafeManagedPtrCastPtr treeModel
    iter' <- unsafeManagedPtrGetPtr iter
    gtk_tree_model_unref_node treeModel' iter'
    touchManagedPtr treeModel
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeModelUnrefNodeMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m ()), MonadIO m, IsTreeModel a) => O.OverloadedMethod TreeModelUnrefNodeMethodInfo a signature where
    overloadedMethod = treeModelUnrefNode

instance O.OverloadedMethodInfo TreeModelUnrefNodeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel.treeModelUnrefNode",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#v:treeModelUnrefNode"
        })


#endif

-- signal TreeModel::row-changed
-- | This signal is emitted when a row in the model has changed.
type TreeModelRowChangedCallback =
    Gtk.TreePath.TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct identifying the changed row
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: a valid t'GI.Gtk.Structs.TreeIter.TreeIter'-struct pointing to the changed row
    -> IO ()

type C_TreeModelRowChangedCallback =
    Ptr TreeModel ->                        -- object
    Ptr Gtk.TreePath.TreePath ->
    Ptr Gtk.TreeIter.TreeIter ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TreeModelRowChangedCallback`.
foreign import ccall "wrapper"
    mk_TreeModelRowChangedCallback :: C_TreeModelRowChangedCallback -> IO (FunPtr C_TreeModelRowChangedCallback)

wrap_TreeModelRowChangedCallback :: 
    GObject a => (a -> TreeModelRowChangedCallback) ->
    C_TreeModelRowChangedCallback
wrap_TreeModelRowChangedCallback gi'cb gi'selfPtr path iter _ = do
    B.ManagedPtr.withTransient  path $ \path' -> do
        B.ManagedPtr.withTransient  iter $ \iter' -> do
            B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  path' iter'


-- | Connect a signal handler for the [rowChanged](#signal:rowChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeModel #rowChanged callback
-- @
-- 
-- 
onTreeModelRowChanged :: (IsTreeModel a, MonadIO m) => a -> ((?self :: a) => TreeModelRowChangedCallback) -> m SignalHandlerId
onTreeModelRowChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeModelRowChangedCallback wrapped
    wrapped'' <- mk_TreeModelRowChangedCallback wrapped'
    connectSignalFunPtr obj "row-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [rowChanged](#signal:rowChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeModel #rowChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeModelRowChanged :: (IsTreeModel a, MonadIO m) => a -> ((?self :: a) => TreeModelRowChangedCallback) -> m SignalHandlerId
afterTreeModelRowChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeModelRowChangedCallback wrapped
    wrapped'' <- mk_TreeModelRowChangedCallback wrapped'
    connectSignalFunPtr obj "row-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeModelRowChangedSignalInfo
instance SignalInfo TreeModelRowChangedSignalInfo where
    type HaskellCallbackType TreeModelRowChangedSignalInfo = TreeModelRowChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeModelRowChangedCallback cb
        cb'' <- mk_TreeModelRowChangedCallback cb'
        connectSignalFunPtr obj "row-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel::row-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#g:signal:rowChanged"})

#endif

-- signal TreeModel::row-deleted
-- | This signal is emitted when a row has been deleted.
-- 
-- Note that no iterator is passed to the signal handler,
-- since the row is already deleted.
-- 
-- This should be called by models after a row has been removed.
-- The location pointed to by /@path@/ should be the location that
-- the row previously was at. It may not be a valid location anymore.
type TreeModelRowDeletedCallback =
    Gtk.TreePath.TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct identifying the row
    -> IO ()

type C_TreeModelRowDeletedCallback =
    Ptr TreeModel ->                        -- object
    Ptr Gtk.TreePath.TreePath ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TreeModelRowDeletedCallback`.
foreign import ccall "wrapper"
    mk_TreeModelRowDeletedCallback :: C_TreeModelRowDeletedCallback -> IO (FunPtr C_TreeModelRowDeletedCallback)

wrap_TreeModelRowDeletedCallback :: 
    GObject a => (a -> TreeModelRowDeletedCallback) ->
    C_TreeModelRowDeletedCallback
wrap_TreeModelRowDeletedCallback gi'cb gi'selfPtr path _ = do
    B.ManagedPtr.withTransient  path $ \path' -> do
        B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  path'


-- | Connect a signal handler for the [rowDeleted](#signal:rowDeleted) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeModel #rowDeleted callback
-- @
-- 
-- 
onTreeModelRowDeleted :: (IsTreeModel a, MonadIO m) => a -> ((?self :: a) => TreeModelRowDeletedCallback) -> m SignalHandlerId
onTreeModelRowDeleted obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeModelRowDeletedCallback wrapped
    wrapped'' <- mk_TreeModelRowDeletedCallback wrapped'
    connectSignalFunPtr obj "row-deleted" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [rowDeleted](#signal:rowDeleted) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeModel #rowDeleted callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeModelRowDeleted :: (IsTreeModel a, MonadIO m) => a -> ((?self :: a) => TreeModelRowDeletedCallback) -> m SignalHandlerId
afterTreeModelRowDeleted obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeModelRowDeletedCallback wrapped
    wrapped'' <- mk_TreeModelRowDeletedCallback wrapped'
    connectSignalFunPtr obj "row-deleted" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeModelRowDeletedSignalInfo
instance SignalInfo TreeModelRowDeletedSignalInfo where
    type HaskellCallbackType TreeModelRowDeletedSignalInfo = TreeModelRowDeletedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeModelRowDeletedCallback cb
        cb'' <- mk_TreeModelRowDeletedCallback cb'
        connectSignalFunPtr obj "row-deleted" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel::row-deleted"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#g:signal:rowDeleted"})

#endif

-- signal TreeModel::row-has-child-toggled
-- | This signal is emitted when a row has gotten the first child
-- row or lost its last child row.
type TreeModelRowHasChildToggledCallback =
    Gtk.TreePath.TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct identifying the row
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: a valid t'GI.Gtk.Structs.TreeIter.TreeIter'-struct pointing to the row
    -> IO ()

type C_TreeModelRowHasChildToggledCallback =
    Ptr TreeModel ->                        -- object
    Ptr Gtk.TreePath.TreePath ->
    Ptr Gtk.TreeIter.TreeIter ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TreeModelRowHasChildToggledCallback`.
foreign import ccall "wrapper"
    mk_TreeModelRowHasChildToggledCallback :: C_TreeModelRowHasChildToggledCallback -> IO (FunPtr C_TreeModelRowHasChildToggledCallback)

wrap_TreeModelRowHasChildToggledCallback :: 
    GObject a => (a -> TreeModelRowHasChildToggledCallback) ->
    C_TreeModelRowHasChildToggledCallback
wrap_TreeModelRowHasChildToggledCallback gi'cb gi'selfPtr path iter _ = do
    B.ManagedPtr.withTransient  path $ \path' -> do
        B.ManagedPtr.withTransient  iter $ \iter' -> do
            B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  path' iter'


-- | Connect a signal handler for the [rowHasChildToggled](#signal:rowHasChildToggled) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeModel #rowHasChildToggled callback
-- @
-- 
-- 
onTreeModelRowHasChildToggled :: (IsTreeModel a, MonadIO m) => a -> ((?self :: a) => TreeModelRowHasChildToggledCallback) -> m SignalHandlerId
onTreeModelRowHasChildToggled obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeModelRowHasChildToggledCallback wrapped
    wrapped'' <- mk_TreeModelRowHasChildToggledCallback wrapped'
    connectSignalFunPtr obj "row-has-child-toggled" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [rowHasChildToggled](#signal:rowHasChildToggled) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeModel #rowHasChildToggled callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeModelRowHasChildToggled :: (IsTreeModel a, MonadIO m) => a -> ((?self :: a) => TreeModelRowHasChildToggledCallback) -> m SignalHandlerId
afterTreeModelRowHasChildToggled obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeModelRowHasChildToggledCallback wrapped
    wrapped'' <- mk_TreeModelRowHasChildToggledCallback wrapped'
    connectSignalFunPtr obj "row-has-child-toggled" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeModelRowHasChildToggledSignalInfo
instance SignalInfo TreeModelRowHasChildToggledSignalInfo where
    type HaskellCallbackType TreeModelRowHasChildToggledSignalInfo = TreeModelRowHasChildToggledCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeModelRowHasChildToggledCallback cb
        cb'' <- mk_TreeModelRowHasChildToggledCallback cb'
        connectSignalFunPtr obj "row-has-child-toggled" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel::row-has-child-toggled"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#g:signal:rowHasChildToggled"})

#endif

-- signal TreeModel::row-inserted
-- | This signal is emitted when a new row has been inserted in
-- the model.
-- 
-- Note that the row may still be empty at this point, since
-- it is a common pattern to first insert an empty row, and
-- then fill it with the desired values.
type TreeModelRowInsertedCallback =
    Gtk.TreePath.TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct identifying the new row
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: a valid t'GI.Gtk.Structs.TreeIter.TreeIter'-struct pointing to the new row
    -> IO ()

type C_TreeModelRowInsertedCallback =
    Ptr TreeModel ->                        -- object
    Ptr Gtk.TreePath.TreePath ->
    Ptr Gtk.TreeIter.TreeIter ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TreeModelRowInsertedCallback`.
foreign import ccall "wrapper"
    mk_TreeModelRowInsertedCallback :: C_TreeModelRowInsertedCallback -> IO (FunPtr C_TreeModelRowInsertedCallback)

wrap_TreeModelRowInsertedCallback :: 
    GObject a => (a -> TreeModelRowInsertedCallback) ->
    C_TreeModelRowInsertedCallback
wrap_TreeModelRowInsertedCallback gi'cb gi'selfPtr path iter _ = do
    B.ManagedPtr.withTransient  path $ \path' -> do
        B.ManagedPtr.withTransient  iter $ \iter' -> do
            B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  path' iter'


-- | Connect a signal handler for the [rowInserted](#signal:rowInserted) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeModel #rowInserted callback
-- @
-- 
-- 
onTreeModelRowInserted :: (IsTreeModel a, MonadIO m) => a -> ((?self :: a) => TreeModelRowInsertedCallback) -> m SignalHandlerId
onTreeModelRowInserted obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeModelRowInsertedCallback wrapped
    wrapped'' <- mk_TreeModelRowInsertedCallback wrapped'
    connectSignalFunPtr obj "row-inserted" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [rowInserted](#signal:rowInserted) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeModel #rowInserted callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeModelRowInserted :: (IsTreeModel a, MonadIO m) => a -> ((?self :: a) => TreeModelRowInsertedCallback) -> m SignalHandlerId
afterTreeModelRowInserted obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeModelRowInsertedCallback wrapped
    wrapped'' <- mk_TreeModelRowInsertedCallback wrapped'
    connectSignalFunPtr obj "row-inserted" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeModelRowInsertedSignalInfo
instance SignalInfo TreeModelRowInsertedSignalInfo where
    type HaskellCallbackType TreeModelRowInsertedSignalInfo = TreeModelRowInsertedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeModelRowInsertedCallback cb
        cb'' <- mk_TreeModelRowInsertedCallback cb'
        connectSignalFunPtr obj "row-inserted" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeModel::row-inserted"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeModel.html#g:signal:rowInserted"})

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TreeModel = TreeModelSignalList
type TreeModelSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo), '("rowChanged", TreeModelRowChangedSignalInfo), '("rowDeleted", TreeModelRowDeletedSignalInfo), '("rowHasChildToggled", TreeModelRowHasChildToggledSignalInfo), '("rowInserted", TreeModelRowInsertedSignalInfo)] :: [(Symbol, *)])

#endif


