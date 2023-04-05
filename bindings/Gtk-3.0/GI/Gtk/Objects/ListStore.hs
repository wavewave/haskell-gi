{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Objects.ListStore.ListStore' object is a list model for use with a t'GI.Gtk.Objects.TreeView.TreeView'
-- widget.  It implements the t'GI.Gtk.Interfaces.TreeModel.TreeModel' interface, and consequentialy,
-- can use all of the methods available there.  It also implements the
-- t'GI.Gtk.Interfaces.TreeSortable.TreeSortable' interface so it can be sorted by the view.
-- Finally, it also implements the tree
-- [drag and drop][gtk3-GtkTreeView-drag-and-drop]
-- interfaces.
-- 
-- The t'GI.Gtk.Objects.ListStore.ListStore' can accept most GObject types as a column type, though
-- it can’t accept all custom types.  Internally, it will keep a copy of
-- data passed in (such as a string or a boxed pointer).  Columns that
-- accept @/GObjects/@ are handled a little differently.  The
-- t'GI.Gtk.Objects.ListStore.ListStore' will keep a reference to the object instead of copying the
-- value.  As a result, if the object is modified, it is up to the
-- application writer to call 'GI.Gtk.Interfaces.TreeModel.treeModelRowChanged' to emit the
-- t'GI.Gtk.Interfaces.TreeModel.TreeModel'::@/row_changed/@ signal.  This most commonly affects lists with
-- @/GdkPixbufs/@ stored.
-- 
-- An example for creating a simple list store:
-- 
-- 
-- === /C code/
-- >
-- >enum {
-- >  COLUMN_STRING,
-- >  COLUMN_INT,
-- >  COLUMN_BOOLEAN,
-- >  N_COLUMNS
-- >};
-- >
-- >{
-- >  GtkListStore *list_store;
-- >  GtkTreePath *path;
-- >  GtkTreeIter iter;
-- >  gint i;
-- >
-- >  list_store = gtk_list_store_new (N_COLUMNS,
-- >                                   G_TYPE_STRING,
-- >                                   G_TYPE_INT,
-- >                                   G_TYPE_BOOLEAN);
-- >
-- >  for (i = 0; i < 10; i++)
-- >    {
-- >      gchar *some_data;
-- >
-- >      some_data = get_some_data (i);
-- >
-- >      // Add a new row to the model
-- >      gtk_list_store_append (list_store, &iter);
-- >      gtk_list_store_set (list_store, &iter,
-- >                          COLUMN_STRING, some_data,
-- >                          COLUMN_INT, i,
-- >                          COLUMN_BOOLEAN,  FALSE,
-- >                          -1);
-- >
-- >      // As the store will keep a copy of the string internally,
-- >      // we free some_data.
-- >      g_free (some_data);
-- >    }
-- >
-- >  // Modify a particular row
-- >  path = gtk_tree_path_new_from_string ("4");
-- >  gtk_tree_model_get_iter (GTK_TREE_MODEL (list_store),
-- >                           &iter,
-- >                           path);
-- >  gtk_tree_path_free (path);
-- >  gtk_list_store_set (list_store, &iter,
-- >                      COLUMN_BOOLEAN, TRUE,
-- >                      -1);
-- >}
-- 
-- 
-- = Performance Considerations
-- 
-- Internally, the t'GI.Gtk.Objects.ListStore.ListStore' was implemented with a linked list with
-- a tail pointer prior to GTK+ 2.6.  As a result, it was fast at data
-- insertion and deletion, and not fast at random data access.  The
-- t'GI.Gtk.Objects.ListStore.ListStore' sets the @/GTK_TREE_MODEL_ITERS_PERSIST/@ flag, which means
-- that @/GtkTreeIters/@ can be cached while the row exists.  Thus, if
-- access to a particular row is needed often and your code is expected to
-- run on older versions of GTK+, it is worth keeping the iter around.
-- 
-- = Atomic Operations
-- 
-- It is important to note that only the methods
-- @/gtk_list_store_insert_with_values()/@ and 'GI.Gtk.Objects.ListStore.listStoreInsertWithValuesv'
-- are atomic, in the sense that the row is being appended to the store and the
-- values filled in in a single operation with regard to t'GI.Gtk.Interfaces.TreeModel.TreeModel' signaling.
-- In contrast, using e.g. 'GI.Gtk.Objects.ListStore.listStoreAppend' and then @/gtk_list_store_set()/@
-- will first create a row, which triggers the [TreeModel::rowInserted]("GI.Gtk.Interfaces.TreeModel#g:signal:rowInserted") signal
-- on t'GI.Gtk.Objects.ListStore.ListStore'. The row, however, is still empty, and any signal handler
-- connecting to [TreeModel::rowInserted]("GI.Gtk.Interfaces.TreeModel#g:signal:rowInserted") on this particular store should be prepared
-- for the situation that the row might be empty. This is especially important
-- if you are wrapping the t'GI.Gtk.Objects.ListStore.ListStore' inside a t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter' and are
-- using a t'GI.Gtk.Callbacks.TreeModelFilterVisibleFunc'. Using any of the non-atomic operations
-- to append rows to the t'GI.Gtk.Objects.ListStore.ListStore' will cause the
-- t'GI.Gtk.Callbacks.TreeModelFilterVisibleFunc' to be visited with an empty row first; the
-- function must be prepared for that.
-- 
-- = GtkListStore as GtkBuildable
-- 
-- The GtkListStore implementation of the GtkBuildable interface allows
-- to specify the model columns with a @\<columns>@ element that may contain
-- multiple @\<column>@ elements, each specifying one model column. The “type”
-- attribute specifies the data type for the column.
-- 
-- Additionally, it is possible to specify content for the list store
-- in the UI definition, with the @\<data>@ element. It can contain multiple
-- @\<row>@ elements, each specifying to content for one row of the list model.
-- Inside a @\<row>@, the @\<col>@ elements specify the content for individual cells.
-- 
-- Note that it is probably more common to define your models in the code,
-- and one might consider it a layering violation to specify the content of
-- a list store in a UI definition, data, not presentation, and common wisdom
-- is to separate the two, as far as possible.
-- 
-- An example of a UI Definition fragment for a list store:
-- 
-- 
-- === /xml code/
-- >
-- ><object class="GtkListStore">
-- >  <columns>
-- >    <column type="gchararray"/>
-- >    <column type="gchararray"/>
-- >    <column type="gint"/>
-- >  </columns>
-- >  <data>
-- >    <row>
-- >      <col id="0">John</col>
-- >      <col id="1">Doe</col>
-- >      <col id="2">25</col>
-- >    </row>
-- >    <row>
-- >      <col id="0">Johan</col>
-- >      <col id="1">Dahlin</col>
-- >      <col id="2">50</col>
-- >    </row>
-- >  </data>
-- ></object>
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.ListStore
    ( 

-- * Exported types
    ListStore(..)                           ,
    IsListStore                             ,
    toListStore                             ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [append]("GI.Gtk.Objects.ListStore#g:method:append"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [clear]("GI.Gtk.Objects.ListStore#g:method:clear"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [dragDataDelete]("GI.Gtk.Interfaces.TreeDragSource#g:method:dragDataDelete"), [dragDataGet]("GI.Gtk.Interfaces.TreeDragSource#g:method:dragDataGet"), [dragDataReceived]("GI.Gtk.Interfaces.TreeDragDest#g:method:dragDataReceived"), [filterNew]("GI.Gtk.Interfaces.TreeModel#g:method:filterNew"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Interfaces.TreeModel#g:method:foreach"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [hasDefaultSortFunc]("GI.Gtk.Interfaces.TreeSortable#g:method:hasDefaultSortFunc"), [insert]("GI.Gtk.Objects.ListStore#g:method:insert"), [insertAfter]("GI.Gtk.Objects.ListStore#g:method:insertAfter"), [insertBefore]("GI.Gtk.Objects.ListStore#g:method:insertBefore"), [insertWithValuesv]("GI.Gtk.Objects.ListStore#g:method:insertWithValuesv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [iterChildren]("GI.Gtk.Interfaces.TreeModel#g:method:iterChildren"), [iterHasChild]("GI.Gtk.Interfaces.TreeModel#g:method:iterHasChild"), [iterIsValid]("GI.Gtk.Objects.ListStore#g:method:iterIsValid"), [iterNChildren]("GI.Gtk.Interfaces.TreeModel#g:method:iterNChildren"), [iterNext]("GI.Gtk.Interfaces.TreeModel#g:method:iterNext"), [iterNthChild]("GI.Gtk.Interfaces.TreeModel#g:method:iterNthChild"), [iterParent]("GI.Gtk.Interfaces.TreeModel#g:method:iterParent"), [iterPrevious]("GI.Gtk.Interfaces.TreeModel#g:method:iterPrevious"), [moveAfter]("GI.Gtk.Objects.ListStore#g:method:moveAfter"), [moveBefore]("GI.Gtk.Objects.ListStore#g:method:moveBefore"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [prepend]("GI.Gtk.Objects.ListStore#g:method:prepend"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refNode]("GI.Gtk.Interfaces.TreeModel#g:method:refNode"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [remove]("GI.Gtk.Objects.ListStore#g:method:remove"), [reorder]("GI.Gtk.Objects.ListStore#g:method:reorder"), [rowChanged]("GI.Gtk.Interfaces.TreeModel#g:method:rowChanged"), [rowDeleted]("GI.Gtk.Interfaces.TreeModel#g:method:rowDeleted"), [rowDraggable]("GI.Gtk.Interfaces.TreeDragSource#g:method:rowDraggable"), [rowDropPossible]("GI.Gtk.Interfaces.TreeDragDest#g:method:rowDropPossible"), [rowHasChildToggled]("GI.Gtk.Interfaces.TreeModel#g:method:rowHasChildToggled"), [rowInserted]("GI.Gtk.Interfaces.TreeModel#g:method:rowInserted"), [rowsReordered]("GI.Gtk.Interfaces.TreeModel#g:method:rowsReordered"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [set]("GI.Gtk.Objects.ListStore#g:method:set"), [sortColumnChanged]("GI.Gtk.Interfaces.TreeSortable#g:method:sortColumnChanged"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [swap]("GI.Gtk.Objects.ListStore#g:method:swap"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unrefNode]("GI.Gtk.Interfaces.TreeModel#g:method:unrefNode"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getColumnType]("GI.Gtk.Interfaces.TreeModel#g:method:getColumnType"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getFlags]("GI.Gtk.Interfaces.TreeModel#g:method:getFlags"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getIter]("GI.Gtk.Interfaces.TreeModel#g:method:getIter"), [getIterFirst]("GI.Gtk.Interfaces.TreeModel#g:method:getIterFirst"), [getIterFromString]("GI.Gtk.Interfaces.TreeModel#g:method:getIterFromString"), [getNColumns]("GI.Gtk.Interfaces.TreeModel#g:method:getNColumns"), [getName]("GI.Gtk.Interfaces.Buildable#g:method:getName"), [getPath]("GI.Gtk.Interfaces.TreeModel#g:method:getPath"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSortColumnId]("GI.Gtk.Interfaces.TreeSortable#g:method:getSortColumnId"), [getStringFromIter]("GI.Gtk.Interfaces.TreeModel#g:method:getStringFromIter"), [getValue]("GI.Gtk.Interfaces.TreeModel#g:method:getValue").
-- 
-- ==== Setters
-- [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setColumnTypes]("GI.Gtk.Objects.ListStore#g:method:setColumnTypes"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDefaultSortFunc]("GI.Gtk.Interfaces.TreeSortable#g:method:setDefaultSortFunc"), [setName]("GI.Gtk.Interfaces.Buildable#g:method:setName"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSortColumnId]("GI.Gtk.Interfaces.TreeSortable#g:method:setSortColumnId"), [setSortFunc]("GI.Gtk.Interfaces.TreeSortable#g:method:setSortFunc"), [setValue]("GI.Gtk.Objects.ListStore#g:method:setValue").

#if defined(ENABLE_OVERLOADING)
    ResolveListStoreMethod                  ,
#endif

-- ** append #method:append#

#if defined(ENABLE_OVERLOADING)
    ListStoreAppendMethodInfo               ,
#endif
    listStoreAppend                         ,


-- ** clear #method:clear#

#if defined(ENABLE_OVERLOADING)
    ListStoreClearMethodInfo                ,
#endif
    listStoreClear                          ,


-- ** insert #method:insert#

#if defined(ENABLE_OVERLOADING)
    ListStoreInsertMethodInfo               ,
#endif
    listStoreInsert                         ,


-- ** insertAfter #method:insertAfter#

#if defined(ENABLE_OVERLOADING)
    ListStoreInsertAfterMethodInfo          ,
#endif
    listStoreInsertAfter                    ,


-- ** insertBefore #method:insertBefore#

#if defined(ENABLE_OVERLOADING)
    ListStoreInsertBeforeMethodInfo         ,
#endif
    listStoreInsertBefore                   ,


-- ** insertWithValuesv #method:insertWithValuesv#

#if defined(ENABLE_OVERLOADING)
    ListStoreInsertWithValuesvMethodInfo    ,
#endif
    listStoreInsertWithValuesv              ,


-- ** iterIsValid #method:iterIsValid#

#if defined(ENABLE_OVERLOADING)
    ListStoreIterIsValidMethodInfo          ,
#endif
    listStoreIterIsValid                    ,


-- ** moveAfter #method:moveAfter#

#if defined(ENABLE_OVERLOADING)
    ListStoreMoveAfterMethodInfo            ,
#endif
    listStoreMoveAfter                      ,


-- ** moveBefore #method:moveBefore#

#if defined(ENABLE_OVERLOADING)
    ListStoreMoveBeforeMethodInfo           ,
#endif
    listStoreMoveBefore                     ,


-- ** new #method:new#

    listStoreNew                            ,


-- ** prepend #method:prepend#

#if defined(ENABLE_OVERLOADING)
    ListStorePrependMethodInfo              ,
#endif
    listStorePrepend                        ,


-- ** remove #method:remove#

#if defined(ENABLE_OVERLOADING)
    ListStoreRemoveMethodInfo               ,
#endif
    listStoreRemove                         ,


-- ** reorder #method:reorder#

#if defined(ENABLE_OVERLOADING)
    ListStoreReorderMethodInfo              ,
#endif
    listStoreReorder                        ,


-- ** set #method:set#

#if defined(ENABLE_OVERLOADING)
    ListStoreSetMethodInfo                  ,
#endif
    listStoreSet                            ,


-- ** setColumnTypes #method:setColumnTypes#

#if defined(ENABLE_OVERLOADING)
    ListStoreSetColumnTypesMethodInfo       ,
#endif
    listStoreSetColumnTypes                 ,


-- ** setValue #method:setValue#

#if defined(ENABLE_OVERLOADING)
    ListStoreSetValueMethodInfo             ,
#endif
    listStoreSetValue                       ,


-- ** swap #method:swap#

#if defined(ENABLE_OVERLOADING)
    ListStoreSwapMethodInfo                 ,
#endif
    listStoreSwap                           ,




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
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeDragDest as Gtk.TreeDragDest
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeDragSource as Gtk.TreeDragSource
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeModel as Gtk.TreeModel
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeSortable as Gtk.TreeSortable
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreeIter as Gtk.TreeIter

-- | Memory-managed wrapper type.
newtype ListStore = ListStore (SP.ManagedPtr ListStore)
    deriving (Eq)

instance SP.ManagedPtrNewtype ListStore where
    toManagedPtr (ListStore p) = p

foreign import ccall "gtk_list_store_get_type"
    c_gtk_list_store_get_type :: IO B.Types.GType

instance B.Types.TypedObject ListStore where
    glibType = c_gtk_list_store_get_type

instance B.Types.GObject ListStore

-- | Type class for types which can be safely cast to `ListStore`, for instance with `toListStore`.
class (SP.GObject o, O.IsDescendantOf ListStore o) => IsListStore o
instance (SP.GObject o, O.IsDescendantOf ListStore o) => IsListStore o

instance O.HasParentTypes ListStore
type instance O.ParentTypes ListStore = '[GObject.Object.Object, Gtk.Buildable.Buildable, Gtk.TreeDragDest.TreeDragDest, Gtk.TreeDragSource.TreeDragSource, Gtk.TreeModel.TreeModel, Gtk.TreeSortable.TreeSortable]

-- | Cast to `ListStore`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toListStore :: (MIO.MonadIO m, IsListStore o) => o -> m ListStore
toListStore = MIO.liftIO . B.ManagedPtr.unsafeCastTo ListStore

-- | Convert 'ListStore' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe ListStore) where
    gvalueGType_ = c_gtk_list_store_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr ListStore)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr ListStore)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject ListStore ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveListStoreMethod (t :: Symbol) (o :: *) :: * where
    ResolveListStoreMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveListStoreMethod "append" o = ListStoreAppendMethodInfo
    ResolveListStoreMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveListStoreMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveListStoreMethod "clear" o = ListStoreClearMethodInfo
    ResolveListStoreMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveListStoreMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveListStoreMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveListStoreMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveListStoreMethod "dragDataDelete" o = Gtk.TreeDragSource.TreeDragSourceDragDataDeleteMethodInfo
    ResolveListStoreMethod "dragDataGet" o = Gtk.TreeDragSource.TreeDragSourceDragDataGetMethodInfo
    ResolveListStoreMethod "dragDataReceived" o = Gtk.TreeDragDest.TreeDragDestDragDataReceivedMethodInfo
    ResolveListStoreMethod "filterNew" o = Gtk.TreeModel.TreeModelFilterNewMethodInfo
    ResolveListStoreMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveListStoreMethod "foreach" o = Gtk.TreeModel.TreeModelForeachMethodInfo
    ResolveListStoreMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveListStoreMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveListStoreMethod "hasDefaultSortFunc" o = Gtk.TreeSortable.TreeSortableHasDefaultSortFuncMethodInfo
    ResolveListStoreMethod "insert" o = ListStoreInsertMethodInfo
    ResolveListStoreMethod "insertAfter" o = ListStoreInsertAfterMethodInfo
    ResolveListStoreMethod "insertBefore" o = ListStoreInsertBeforeMethodInfo
    ResolveListStoreMethod "insertWithValuesv" o = ListStoreInsertWithValuesvMethodInfo
    ResolveListStoreMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveListStoreMethod "iterChildren" o = Gtk.TreeModel.TreeModelIterChildrenMethodInfo
    ResolveListStoreMethod "iterHasChild" o = Gtk.TreeModel.TreeModelIterHasChildMethodInfo
    ResolveListStoreMethod "iterIsValid" o = ListStoreIterIsValidMethodInfo
    ResolveListStoreMethod "iterNChildren" o = Gtk.TreeModel.TreeModelIterNChildrenMethodInfo
    ResolveListStoreMethod "iterNext" o = Gtk.TreeModel.TreeModelIterNextMethodInfo
    ResolveListStoreMethod "iterNthChild" o = Gtk.TreeModel.TreeModelIterNthChildMethodInfo
    ResolveListStoreMethod "iterParent" o = Gtk.TreeModel.TreeModelIterParentMethodInfo
    ResolveListStoreMethod "iterPrevious" o = Gtk.TreeModel.TreeModelIterPreviousMethodInfo
    ResolveListStoreMethod "moveAfter" o = ListStoreMoveAfterMethodInfo
    ResolveListStoreMethod "moveBefore" o = ListStoreMoveBeforeMethodInfo
    ResolveListStoreMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveListStoreMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveListStoreMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveListStoreMethod "prepend" o = ListStorePrependMethodInfo
    ResolveListStoreMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveListStoreMethod "refNode" o = Gtk.TreeModel.TreeModelRefNodeMethodInfo
    ResolveListStoreMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveListStoreMethod "remove" o = ListStoreRemoveMethodInfo
    ResolveListStoreMethod "reorder" o = ListStoreReorderMethodInfo
    ResolveListStoreMethod "rowChanged" o = Gtk.TreeModel.TreeModelRowChangedMethodInfo
    ResolveListStoreMethod "rowDeleted" o = Gtk.TreeModel.TreeModelRowDeletedMethodInfo
    ResolveListStoreMethod "rowDraggable" o = Gtk.TreeDragSource.TreeDragSourceRowDraggableMethodInfo
    ResolveListStoreMethod "rowDropPossible" o = Gtk.TreeDragDest.TreeDragDestRowDropPossibleMethodInfo
    ResolveListStoreMethod "rowHasChildToggled" o = Gtk.TreeModel.TreeModelRowHasChildToggledMethodInfo
    ResolveListStoreMethod "rowInserted" o = Gtk.TreeModel.TreeModelRowInsertedMethodInfo
    ResolveListStoreMethod "rowsReordered" o = Gtk.TreeModel.TreeModelRowsReorderedMethodInfo
    ResolveListStoreMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveListStoreMethod "set" o = ListStoreSetMethodInfo
    ResolveListStoreMethod "sortColumnChanged" o = Gtk.TreeSortable.TreeSortableSortColumnChangedMethodInfo
    ResolveListStoreMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveListStoreMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveListStoreMethod "swap" o = ListStoreSwapMethodInfo
    ResolveListStoreMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveListStoreMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveListStoreMethod "unrefNode" o = Gtk.TreeModel.TreeModelUnrefNodeMethodInfo
    ResolveListStoreMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveListStoreMethod "getColumnType" o = Gtk.TreeModel.TreeModelGetColumnTypeMethodInfo
    ResolveListStoreMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveListStoreMethod "getFlags" o = Gtk.TreeModel.TreeModelGetFlagsMethodInfo
    ResolveListStoreMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveListStoreMethod "getIter" o = Gtk.TreeModel.TreeModelGetIterMethodInfo
    ResolveListStoreMethod "getIterFirst" o = Gtk.TreeModel.TreeModelGetIterFirstMethodInfo
    ResolveListStoreMethod "getIterFromString" o = Gtk.TreeModel.TreeModelGetIterFromStringMethodInfo
    ResolveListStoreMethod "getNColumns" o = Gtk.TreeModel.TreeModelGetNColumnsMethodInfo
    ResolveListStoreMethod "getName" o = Gtk.Buildable.BuildableGetNameMethodInfo
    ResolveListStoreMethod "getPath" o = Gtk.TreeModel.TreeModelGetPathMethodInfo
    ResolveListStoreMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveListStoreMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveListStoreMethod "getSortColumnId" o = Gtk.TreeSortable.TreeSortableGetSortColumnIdMethodInfo
    ResolveListStoreMethod "getStringFromIter" o = Gtk.TreeModel.TreeModelGetStringFromIterMethodInfo
    ResolveListStoreMethod "getValue" o = Gtk.TreeModel.TreeModelGetValueMethodInfo
    ResolveListStoreMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveListStoreMethod "setColumnTypes" o = ListStoreSetColumnTypesMethodInfo
    ResolveListStoreMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveListStoreMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveListStoreMethod "setDefaultSortFunc" o = Gtk.TreeSortable.TreeSortableSetDefaultSortFuncMethodInfo
    ResolveListStoreMethod "setName" o = Gtk.Buildable.BuildableSetNameMethodInfo
    ResolveListStoreMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveListStoreMethod "setSortColumnId" o = Gtk.TreeSortable.TreeSortableSetSortColumnIdMethodInfo
    ResolveListStoreMethod "setSortFunc" o = Gtk.TreeSortable.TreeSortableSetSortFuncMethodInfo
    ResolveListStoreMethod "setValue" o = ListStoreSetValueMethodInfo
    ResolveListStoreMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveListStoreMethod t ListStore, O.OverloadedMethod info ListStore p) => OL.IsLabel t (ListStore -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveListStoreMethod t ListStore, O.OverloadedMethod info ListStore p, R.HasField t ListStore p) => R.HasField t ListStore p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveListStoreMethod t ListStore, O.OverloadedMethodInfo info ListStore) => OL.IsLabel t (O.MethodProxy info ListStore) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ListStore
type instance O.AttributeList ListStore = ListStoreAttributeList
type ListStoreAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList ListStore = ListStoreSignalList
type ListStoreSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo), '("rowChanged", Gtk.TreeModel.TreeModelRowChangedSignalInfo), '("rowDeleted", Gtk.TreeModel.TreeModelRowDeletedSignalInfo), '("rowHasChildToggled", Gtk.TreeModel.TreeModelRowHasChildToggledSignalInfo), '("rowInserted", Gtk.TreeModel.TreeModelRowInsertedSignalInfo), '("sortColumnChanged", Gtk.TreeSortable.TreeSortableSortColumnChangedSignalInfo)] :: [(Symbol, *)])

#endif

-- method ListStore::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "n_columns"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of columns in the list store"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "types"
--           , argType = TCArray False (-1) 0 (TBasicType TGType)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "an array of #GType types for the columns, from first to last"
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
--              { argCName = "n_columns"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "number of columns in the list store"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "ListStore" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_list_store_newv" gtk_list_store_newv :: 
    Int32 ->                                -- n_columns : TBasicType TInt
    Ptr CGType ->                           -- types : TCArray False (-1) 0 (TBasicType TGType)
    IO (Ptr ListStore)

-- | Non-vararg creation function.  Used primarily by language bindings.
listStoreNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [GType]
    -- ^ /@types@/: an array of t'GType' types for the columns, from first to last
    -> m ListStore
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.ListStore.ListStore'
listStoreNew types = liftIO $ do
    let nColumns = fromIntegral $ P.length types
    types' <- (packMapStorableArray gtypeToCGType) types
    result <- gtk_list_store_newv nColumns types'
    checkUnexpectedReturnNULL "listStoreNew" result
    result' <- (wrapObject ListStore) result
    freeMem types'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method ListStore::append
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "list_store"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListStore" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkListStore" , sinceVersion = Nothing }
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
--                     Just "An unset #GtkTreeIter to set to the appended row"
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

foreign import ccall "gtk_list_store_append" gtk_list_store_append :: 
    Ptr ListStore ->                        -- list_store : TInterface (Name {namespace = "Gtk", name = "ListStore"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Appends a new row to /@listStore@/.  /@iter@/ will be changed to point to this new
-- row.  The row will be empty after this function is called.  To fill in
-- values, you need to call @/gtk_list_store_set()/@ or 'GI.Gtk.Objects.ListStore.listStoreSetValue'.
listStoreAppend ::
    (B.CallStack.HasCallStack, MonadIO m, IsListStore a) =>
    a
    -- ^ /@listStore@/: A t'GI.Gtk.Objects.ListStore.ListStore'
    -> m (Gtk.TreeIter.TreeIter)
listStoreAppend listStore = liftIO $ do
    listStore' <- unsafeManagedPtrCastPtr listStore
    iter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    gtk_list_store_append listStore' iter
    iter' <- (wrapBoxed Gtk.TreeIter.TreeIter) iter
    touchManagedPtr listStore
    return iter'

#if defined(ENABLE_OVERLOADING)
data ListStoreAppendMethodInfo
instance (signature ~ (m (Gtk.TreeIter.TreeIter)), MonadIO m, IsListStore a) => O.OverloadedMethod ListStoreAppendMethodInfo a signature where
    overloadedMethod = listStoreAppend

instance O.OverloadedMethodInfo ListStoreAppendMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListStore.listStoreAppend",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListStore.html#v:listStoreAppend"
        })


#endif

-- method ListStore::clear
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "list_store"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListStore" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkListStore." , sinceVersion = Nothing }
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

foreign import ccall "gtk_list_store_clear" gtk_list_store_clear :: 
    Ptr ListStore ->                        -- list_store : TInterface (Name {namespace = "Gtk", name = "ListStore"})
    IO ()

-- | Removes all rows from the list store.
listStoreClear ::
    (B.CallStack.HasCallStack, MonadIO m, IsListStore a) =>
    a
    -- ^ /@listStore@/: a t'GI.Gtk.Objects.ListStore.ListStore'.
    -> m ()
listStoreClear listStore = liftIO $ do
    listStore' <- unsafeManagedPtrCastPtr listStore
    gtk_list_store_clear listStore'
    touchManagedPtr listStore
    return ()

#if defined(ENABLE_OVERLOADING)
data ListStoreClearMethodInfo
instance (signature ~ (m ()), MonadIO m, IsListStore a) => O.OverloadedMethod ListStoreClearMethodInfo a signature where
    overloadedMethod = listStoreClear

instance O.OverloadedMethodInfo ListStoreClearMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListStore.listStoreClear",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListStore.html#v:listStoreClear"
        })


#endif

-- method ListStore::insert
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "list_store"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListStore" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkListStore" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "An unset #GtkTreeIter to set to the new row"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "position"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "position to insert the new row, or -1 for last"
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

foreign import ccall "gtk_list_store_insert" gtk_list_store_insert :: 
    Ptr ListStore ->                        -- list_store : TInterface (Name {namespace = "Gtk", name = "ListStore"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Int32 ->                                -- position : TBasicType TInt
    IO ()

-- | Creates a new row at /@position@/.  /@iter@/ will be changed to point to this new
-- row.  If /@position@/ is -1 or is larger than the number of rows on the list,
-- then the new row will be appended to the list. The row will be empty after
-- this function is called.  To fill in values, you need to call
-- @/gtk_list_store_set()/@ or 'GI.Gtk.Objects.ListStore.listStoreSetValue'.
listStoreInsert ::
    (B.CallStack.HasCallStack, MonadIO m, IsListStore a) =>
    a
    -- ^ /@listStore@/: A t'GI.Gtk.Objects.ListStore.ListStore'
    -> Int32
    -- ^ /@position@/: position to insert the new row, or -1 for last
    -> m (Gtk.TreeIter.TreeIter)
listStoreInsert listStore position = liftIO $ do
    listStore' <- unsafeManagedPtrCastPtr listStore
    iter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    gtk_list_store_insert listStore' iter position
    iter' <- (wrapBoxed Gtk.TreeIter.TreeIter) iter
    touchManagedPtr listStore
    return iter'

#if defined(ENABLE_OVERLOADING)
data ListStoreInsertMethodInfo
instance (signature ~ (Int32 -> m (Gtk.TreeIter.TreeIter)), MonadIO m, IsListStore a) => O.OverloadedMethod ListStoreInsertMethodInfo a signature where
    overloadedMethod = listStoreInsert

instance O.OverloadedMethodInfo ListStoreInsertMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListStore.listStoreInsert",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListStore.html#v:listStoreInsert"
        })


#endif

-- method ListStore::insert_after
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "list_store"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListStore" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkListStore" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "An unset #GtkTreeIter to set to the new row"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sibling"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A valid #GtkTreeIter, or %NULL"
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

foreign import ccall "gtk_list_store_insert_after" gtk_list_store_insert_after :: 
    Ptr ListStore ->                        -- list_store : TInterface (Name {namespace = "Gtk", name = "ListStore"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Ptr Gtk.TreeIter.TreeIter ->            -- sibling : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Inserts a new row after /@sibling@/. If /@sibling@/ is 'P.Nothing', then the row will be
-- prepended to the beginning of the list. /@iter@/ will be changed to point to
-- this new row. The row will be empty after this function is called. To fill
-- in values, you need to call @/gtk_list_store_set()/@ or 'GI.Gtk.Objects.ListStore.listStoreSetValue'.
listStoreInsertAfter ::
    (B.CallStack.HasCallStack, MonadIO m, IsListStore a) =>
    a
    -- ^ /@listStore@/: A t'GI.Gtk.Objects.ListStore.ListStore'
    -> Maybe (Gtk.TreeIter.TreeIter)
    -- ^ /@sibling@/: A valid t'GI.Gtk.Structs.TreeIter.TreeIter', or 'P.Nothing'
    -> m (Gtk.TreeIter.TreeIter)
listStoreInsertAfter listStore sibling = liftIO $ do
    listStore' <- unsafeManagedPtrCastPtr listStore
    iter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    maybeSibling <- case sibling of
        Nothing -> return nullPtr
        Just jSibling -> do
            jSibling' <- unsafeManagedPtrGetPtr jSibling
            return jSibling'
    gtk_list_store_insert_after listStore' iter maybeSibling
    iter' <- (wrapBoxed Gtk.TreeIter.TreeIter) iter
    touchManagedPtr listStore
    whenJust sibling touchManagedPtr
    return iter'

#if defined(ENABLE_OVERLOADING)
data ListStoreInsertAfterMethodInfo
instance (signature ~ (Maybe (Gtk.TreeIter.TreeIter) -> m (Gtk.TreeIter.TreeIter)), MonadIO m, IsListStore a) => O.OverloadedMethod ListStoreInsertAfterMethodInfo a signature where
    overloadedMethod = listStoreInsertAfter

instance O.OverloadedMethodInfo ListStoreInsertAfterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListStore.listStoreInsertAfter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListStore.html#v:listStoreInsertAfter"
        })


#endif

-- method ListStore::insert_before
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "list_store"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListStore" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkListStore" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "An unset #GtkTreeIter to set to the new row"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sibling"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A valid #GtkTreeIter, or %NULL"
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

foreign import ccall "gtk_list_store_insert_before" gtk_list_store_insert_before :: 
    Ptr ListStore ->                        -- list_store : TInterface (Name {namespace = "Gtk", name = "ListStore"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Ptr Gtk.TreeIter.TreeIter ->            -- sibling : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Inserts a new row before /@sibling@/. If /@sibling@/ is 'P.Nothing', then the row will
-- be appended to the end of the list. /@iter@/ will be changed to point to this
-- new row. The row will be empty after this function is called. To fill in
-- values, you need to call @/gtk_list_store_set()/@ or 'GI.Gtk.Objects.ListStore.listStoreSetValue'.
listStoreInsertBefore ::
    (B.CallStack.HasCallStack, MonadIO m, IsListStore a) =>
    a
    -- ^ /@listStore@/: A t'GI.Gtk.Objects.ListStore.ListStore'
    -> Maybe (Gtk.TreeIter.TreeIter)
    -- ^ /@sibling@/: A valid t'GI.Gtk.Structs.TreeIter.TreeIter', or 'P.Nothing'
    -> m (Gtk.TreeIter.TreeIter)
listStoreInsertBefore listStore sibling = liftIO $ do
    listStore' <- unsafeManagedPtrCastPtr listStore
    iter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    maybeSibling <- case sibling of
        Nothing -> return nullPtr
        Just jSibling -> do
            jSibling' <- unsafeManagedPtrGetPtr jSibling
            return jSibling'
    gtk_list_store_insert_before listStore' iter maybeSibling
    iter' <- (wrapBoxed Gtk.TreeIter.TreeIter) iter
    touchManagedPtr listStore
    whenJust sibling touchManagedPtr
    return iter'

#if defined(ENABLE_OVERLOADING)
data ListStoreInsertBeforeMethodInfo
instance (signature ~ (Maybe (Gtk.TreeIter.TreeIter) -> m (Gtk.TreeIter.TreeIter)), MonadIO m, IsListStore a) => O.OverloadedMethod ListStoreInsertBeforeMethodInfo a signature where
    overloadedMethod = listStoreInsertBefore

instance O.OverloadedMethodInfo ListStoreInsertBeforeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListStore.listStoreInsertBefore",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListStore.html#v:listStoreInsertBefore"
        })


#endif

-- method ListStore::insert_with_valuesv
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "list_store"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListStore" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkListStore" , sinceVersion = Nothing }
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
--                     Just "An unset #GtkTreeIter to set to the new row, or %NULL."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "position"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "position to insert the new row, or -1 for last"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "columns"
--           , argType = TCArray False (-1) 5 (TBasicType TInt)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an array of column numbers"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "values"
--           , argType = TCArray False (-1) 5 TGValue
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an array of GValues"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_values"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the length of the @columns and @values arrays"
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
--              { argCName = "n_values"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "the length of the @columns and @values arrays"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          , Arg
--              { argCName = "n_values"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "the length of the @columns and @values arrays"
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

foreign import ccall "gtk_list_store_insert_with_valuesv" gtk_list_store_insert_with_valuesv :: 
    Ptr ListStore ->                        -- list_store : TInterface (Name {namespace = "Gtk", name = "ListStore"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Int32 ->                                -- position : TBasicType TInt
    Ptr Int32 ->                            -- columns : TCArray False (-1) 5 (TBasicType TInt)
    Ptr B.GValue.GValue ->                  -- values : TCArray False (-1) 5 TGValue
    Int32 ->                                -- n_values : TBasicType TInt
    IO ()

-- | A variant of @/gtk_list_store_insert_with_values()/@ which
-- takes the columns and values as two arrays, instead of
-- varargs. This function is mainly intended for
-- language-bindings.
-- 
-- /Since: 2.6/
listStoreInsertWithValuesv ::
    (B.CallStack.HasCallStack, MonadIO m, IsListStore a) =>
    a
    -- ^ /@listStore@/: A t'GI.Gtk.Objects.ListStore.ListStore'
    -> Int32
    -- ^ /@position@/: position to insert the new row, or -1 for last
    -> [Int32]
    -- ^ /@columns@/: an array of column numbers
    -> [GValue]
    -- ^ /@values@/: an array of GValues
    -> m (Gtk.TreeIter.TreeIter)
listStoreInsertWithValuesv listStore position columns values = liftIO $ do
    let nValues = fromIntegral $ P.length values
    let columns_expected_length_ = fromIntegral $ P.length columns
    when (columns_expected_length_ /= nValues) $
        error "Gtk.listStoreInsertWithValuesv : length of 'columns' does not agree with that of 'values'."
    listStore' <- unsafeManagedPtrCastPtr listStore
    iter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    columns' <- packStorableArray columns
    values' <- B.GValue.packGValueArray values
    gtk_list_store_insert_with_valuesv listStore' iter position columns' values' nValues
    iter' <- (wrapBoxed Gtk.TreeIter.TreeIter) iter
    touchManagedPtr listStore
    mapM_ touchManagedPtr values
    freeMem columns'
    freeMem values'
    return iter'

#if defined(ENABLE_OVERLOADING)
data ListStoreInsertWithValuesvMethodInfo
instance (signature ~ (Int32 -> [Int32] -> [GValue] -> m (Gtk.TreeIter.TreeIter)), MonadIO m, IsListStore a) => O.OverloadedMethod ListStoreInsertWithValuesvMethodInfo a signature where
    overloadedMethod = listStoreInsertWithValuesv

instance O.OverloadedMethodInfo ListStoreInsertWithValuesvMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListStore.listStoreInsertWithValuesv",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListStore.html#v:listStoreInsertWithValuesv"
        })


#endif

-- method ListStore::iter_is_valid
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "list_store"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListStore" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkListStore." , sinceVersion = Nothing }
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

foreign import ccall "gtk_list_store_iter_is_valid" gtk_list_store_iter_is_valid :: 
    Ptr ListStore ->                        -- list_store : TInterface (Name {namespace = "Gtk", name = "ListStore"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO CInt

-- | > This function is slow. Only use it for debugging and\/or testing
-- > purposes.
-- 
-- Checks if the given iter is a valid iter for this t'GI.Gtk.Objects.ListStore.ListStore'.
-- 
-- /Since: 2.2/
listStoreIterIsValid ::
    (B.CallStack.HasCallStack, MonadIO m, IsListStore a) =>
    a
    -- ^ /@listStore@/: A t'GI.Gtk.Objects.ListStore.ListStore'.
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: A t'GI.Gtk.Structs.TreeIter.TreeIter'.
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the iter is valid, 'P.False' if the iter is invalid.
listStoreIterIsValid listStore iter = liftIO $ do
    listStore' <- unsafeManagedPtrCastPtr listStore
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_list_store_iter_is_valid listStore' iter'
    let result' = (/= 0) result
    touchManagedPtr listStore
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data ListStoreIterIsValidMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m Bool), MonadIO m, IsListStore a) => O.OverloadedMethod ListStoreIterIsValidMethodInfo a signature where
    overloadedMethod = listStoreIterIsValid

instance O.OverloadedMethodInfo ListStoreIterIsValidMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListStore.listStoreIterIsValid",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListStore.html#v:listStoreIterIsValid"
        })


#endif

-- method ListStore::move_after
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "store"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListStore" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkListStore." , sinceVersion = Nothing }
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
--       , Arg
--           { argCName = "position"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeIter or %NULL."
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

foreign import ccall "gtk_list_store_move_after" gtk_list_store_move_after :: 
    Ptr ListStore ->                        -- store : TInterface (Name {namespace = "Gtk", name = "ListStore"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Ptr Gtk.TreeIter.TreeIter ->            -- position : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Moves /@iter@/ in /@store@/ to the position after /@position@/. Note that this
-- function only works with unsorted stores. If /@position@/ is 'P.Nothing', /@iter@/
-- will be moved to the start of the list.
-- 
-- /Since: 2.2/
listStoreMoveAfter ::
    (B.CallStack.HasCallStack, MonadIO m, IsListStore a) =>
    a
    -- ^ /@store@/: A t'GI.Gtk.Objects.ListStore.ListStore'.
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: A t'GI.Gtk.Structs.TreeIter.TreeIter'.
    -> Maybe (Gtk.TreeIter.TreeIter)
    -- ^ /@position@/: A t'GI.Gtk.Structs.TreeIter.TreeIter' or 'P.Nothing'.
    -> m ()
listStoreMoveAfter store iter position = liftIO $ do
    store' <- unsafeManagedPtrCastPtr store
    iter' <- unsafeManagedPtrGetPtr iter
    maybePosition <- case position of
        Nothing -> return nullPtr
        Just jPosition -> do
            jPosition' <- unsafeManagedPtrGetPtr jPosition
            return jPosition'
    gtk_list_store_move_after store' iter' maybePosition
    touchManagedPtr store
    touchManagedPtr iter
    whenJust position touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ListStoreMoveAfterMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> Maybe (Gtk.TreeIter.TreeIter) -> m ()), MonadIO m, IsListStore a) => O.OverloadedMethod ListStoreMoveAfterMethodInfo a signature where
    overloadedMethod = listStoreMoveAfter

instance O.OverloadedMethodInfo ListStoreMoveAfterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListStore.listStoreMoveAfter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListStore.html#v:listStoreMoveAfter"
        })


#endif

-- method ListStore::move_before
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "store"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListStore" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkListStore." , sinceVersion = Nothing }
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
--       , Arg
--           { argCName = "position"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeIter, or %NULL."
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

foreign import ccall "gtk_list_store_move_before" gtk_list_store_move_before :: 
    Ptr ListStore ->                        -- store : TInterface (Name {namespace = "Gtk", name = "ListStore"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Ptr Gtk.TreeIter.TreeIter ->            -- position : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Moves /@iter@/ in /@store@/ to the position before /@position@/. Note that this
-- function only works with unsorted stores. If /@position@/ is 'P.Nothing', /@iter@/
-- will be moved to the end of the list.
-- 
-- /Since: 2.2/
listStoreMoveBefore ::
    (B.CallStack.HasCallStack, MonadIO m, IsListStore a) =>
    a
    -- ^ /@store@/: A t'GI.Gtk.Objects.ListStore.ListStore'.
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: A t'GI.Gtk.Structs.TreeIter.TreeIter'.
    -> Maybe (Gtk.TreeIter.TreeIter)
    -- ^ /@position@/: A t'GI.Gtk.Structs.TreeIter.TreeIter', or 'P.Nothing'.
    -> m ()
listStoreMoveBefore store iter position = liftIO $ do
    store' <- unsafeManagedPtrCastPtr store
    iter' <- unsafeManagedPtrGetPtr iter
    maybePosition <- case position of
        Nothing -> return nullPtr
        Just jPosition -> do
            jPosition' <- unsafeManagedPtrGetPtr jPosition
            return jPosition'
    gtk_list_store_move_before store' iter' maybePosition
    touchManagedPtr store
    touchManagedPtr iter
    whenJust position touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data ListStoreMoveBeforeMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> Maybe (Gtk.TreeIter.TreeIter) -> m ()), MonadIO m, IsListStore a) => O.OverloadedMethod ListStoreMoveBeforeMethodInfo a signature where
    overloadedMethod = listStoreMoveBefore

instance O.OverloadedMethodInfo ListStoreMoveBeforeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListStore.listStoreMoveBefore",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListStore.html#v:listStoreMoveBefore"
        })


#endif

-- method ListStore::prepend
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "list_store"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListStore" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkListStore" , sinceVersion = Nothing }
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
--                     Just "An unset #GtkTreeIter to set to the prepend row"
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

foreign import ccall "gtk_list_store_prepend" gtk_list_store_prepend :: 
    Ptr ListStore ->                        -- list_store : TInterface (Name {namespace = "Gtk", name = "ListStore"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Prepends a new row to /@listStore@/. /@iter@/ will be changed to point to this new
-- row. The row will be empty after this function is called. To fill in
-- values, you need to call @/gtk_list_store_set()/@ or 'GI.Gtk.Objects.ListStore.listStoreSetValue'.
listStorePrepend ::
    (B.CallStack.HasCallStack, MonadIO m, IsListStore a) =>
    a
    -- ^ /@listStore@/: A t'GI.Gtk.Objects.ListStore.ListStore'
    -> m (Gtk.TreeIter.TreeIter)
listStorePrepend listStore = liftIO $ do
    listStore' <- unsafeManagedPtrCastPtr listStore
    iter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    gtk_list_store_prepend listStore' iter
    iter' <- (wrapBoxed Gtk.TreeIter.TreeIter) iter
    touchManagedPtr listStore
    return iter'

#if defined(ENABLE_OVERLOADING)
data ListStorePrependMethodInfo
instance (signature ~ (m (Gtk.TreeIter.TreeIter)), MonadIO m, IsListStore a) => O.OverloadedMethod ListStorePrependMethodInfo a signature where
    overloadedMethod = listStorePrepend

instance O.OverloadedMethodInfo ListStorePrependMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListStore.listStorePrepend",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListStore.html#v:listStorePrepend"
        })


#endif

-- method ListStore::remove
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "list_store"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListStore" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkListStore" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "A valid #GtkTreeIter"
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

foreign import ccall "gtk_list_store_remove" gtk_list_store_remove :: 
    Ptr ListStore ->                        -- list_store : TInterface (Name {namespace = "Gtk", name = "ListStore"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO CInt

-- | Removes the given row from the list store.  After being removed,
-- /@iter@/ is set to be the next valid row, or invalidated if it pointed
-- to the last row in /@listStore@/.
listStoreRemove ::
    (B.CallStack.HasCallStack, MonadIO m, IsListStore a) =>
    a
    -- ^ /@listStore@/: A t'GI.Gtk.Objects.ListStore.ListStore'
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: A valid t'GI.Gtk.Structs.TreeIter.TreeIter'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iter@/ is valid, 'P.False' if not.
listStoreRemove listStore iter = liftIO $ do
    listStore' <- unsafeManagedPtrCastPtr listStore
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_list_store_remove listStore' iter'
    let result' = (/= 0) result
    touchManagedPtr listStore
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data ListStoreRemoveMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m Bool), MonadIO m, IsListStore a) => O.OverloadedMethod ListStoreRemoveMethodInfo a signature where
    overloadedMethod = listStoreRemove

instance O.OverloadedMethodInfo ListStoreRemoveMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListStore.listStoreRemove",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListStore.html#v:listStoreRemove"
        })


#endif

-- method ListStore::reorder
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "store"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListStore" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkListStore." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "new_order"
--           , argType = TCArray True (-1) (-1) (TBasicType TInt)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "an array of integers mapping the new\n     position of each child to its old position before the re-ordering,\n     i.e. @new_order`[newpos] = oldpos`. It must have\n     exactly as many items as the list store\8217s length."
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

foreign import ccall "gtk_list_store_reorder" gtk_list_store_reorder :: 
    Ptr ListStore ->                        -- store : TInterface (Name {namespace = "Gtk", name = "ListStore"})
    Ptr Int32 ->                            -- new_order : TCArray True (-1) (-1) (TBasicType TInt)
    IO ()

-- | Reorders /@store@/ to follow the order indicated by /@newOrder@/. Note that
-- this function only works with unsorted stores.
-- 
-- /Since: 2.2/
listStoreReorder ::
    (B.CallStack.HasCallStack, MonadIO m, IsListStore a) =>
    a
    -- ^ /@store@/: A t'GI.Gtk.Objects.ListStore.ListStore'.
    -> [Int32]
    -- ^ /@newOrder@/: an array of integers mapping the new
    --      position of each child to its old position before the re-ordering,
    --      i.e. /@newOrder@/@[newpos] = oldpos@. It must have
    --      exactly as many items as the list store’s length.
    -> m ()
listStoreReorder store newOrder = liftIO $ do
    store' <- unsafeManagedPtrCastPtr store
    newOrder' <- packZeroTerminatedStorableArray newOrder
    gtk_list_store_reorder store' newOrder'
    touchManagedPtr store
    freeMem newOrder'
    return ()

#if defined(ENABLE_OVERLOADING)
data ListStoreReorderMethodInfo
instance (signature ~ ([Int32] -> m ()), MonadIO m, IsListStore a) => O.OverloadedMethod ListStoreReorderMethodInfo a signature where
    overloadedMethod = listStoreReorder

instance O.OverloadedMethodInfo ListStoreReorderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListStore.listStoreReorder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListStore.html#v:listStoreReorder"
        })


#endif

-- method ListStore::set_column_types
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "list_store"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListStore" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkListStore" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_columns"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Number of columns for the list store"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "types"
--           , argType = TCArray False (-1) 1 (TBasicType TGType)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "An array length n of #GTypes"
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
--              { argCName = "n_columns"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "Number of columns for the list store"
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

foreign import ccall "gtk_list_store_set_column_types" gtk_list_store_set_column_types :: 
    Ptr ListStore ->                        -- list_store : TInterface (Name {namespace = "Gtk", name = "ListStore"})
    Int32 ->                                -- n_columns : TBasicType TInt
    Ptr CGType ->                           -- types : TCArray False (-1) 1 (TBasicType TGType)
    IO ()

-- | This function is meant primarily for @/GObjects/@ that inherit from t'GI.Gtk.Objects.ListStore.ListStore',
-- and should only be used when constructing a new t'GI.Gtk.Objects.ListStore.ListStore'.  It will not
-- function after a row has been added, or a method on the t'GI.Gtk.Interfaces.TreeModel.TreeModel'
-- interface is called.
listStoreSetColumnTypes ::
    (B.CallStack.HasCallStack, MonadIO m, IsListStore a) =>
    a
    -- ^ /@listStore@/: A t'GI.Gtk.Objects.ListStore.ListStore'
    -> [GType]
    -- ^ /@types@/: An array length n of @/GTypes/@
    -> m ()
listStoreSetColumnTypes listStore types = liftIO $ do
    let nColumns = fromIntegral $ P.length types
    listStore' <- unsafeManagedPtrCastPtr listStore
    types' <- (packMapStorableArray gtypeToCGType) types
    gtk_list_store_set_column_types listStore' nColumns types'
    touchManagedPtr listStore
    freeMem types'
    return ()

#if defined(ENABLE_OVERLOADING)
data ListStoreSetColumnTypesMethodInfo
instance (signature ~ ([GType] -> m ()), MonadIO m, IsListStore a) => O.OverloadedMethod ListStoreSetColumnTypesMethodInfo a signature where
    overloadedMethod = listStoreSetColumnTypes

instance O.OverloadedMethodInfo ListStoreSetColumnTypesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListStore.listStoreSetColumnTypes",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListStore.html#v:listStoreSetColumnTypes"
        })


#endif

-- method ListStore::set_value
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "list_store"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListStore" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkListStore" , sinceVersion = Nothing }
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
--                     Just "A valid #GtkTreeIter for the row being modified"
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
--                 { rawDocText = Just "column number to modify"
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
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "new value for the cell"
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

foreign import ccall "gtk_list_store_set_value" gtk_list_store_set_value :: 
    Ptr ListStore ->                        -- list_store : TInterface (Name {namespace = "Gtk", name = "ListStore"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Int32 ->                                -- column : TBasicType TInt
    Ptr GValue ->                           -- value : TGValue
    IO ()

-- | Sets the data in the cell specified by /@iter@/ and /@column@/.
-- The type of /@value@/ must be convertible to the type of the
-- column.
listStoreSetValue ::
    (B.CallStack.HasCallStack, MonadIO m, IsListStore a) =>
    a
    -- ^ /@listStore@/: A t'GI.Gtk.Objects.ListStore.ListStore'
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: A valid t'GI.Gtk.Structs.TreeIter.TreeIter' for the row being modified
    -> Int32
    -- ^ /@column@/: column number to modify
    -> GValue
    -- ^ /@value@/: new value for the cell
    -> m ()
listStoreSetValue listStore iter column value = liftIO $ do
    listStore' <- unsafeManagedPtrCastPtr listStore
    iter' <- unsafeManagedPtrGetPtr iter
    value' <- unsafeManagedPtrGetPtr value
    gtk_list_store_set_value listStore' iter' column value'
    touchManagedPtr listStore
    touchManagedPtr iter
    touchManagedPtr value
    return ()

#if defined(ENABLE_OVERLOADING)
data ListStoreSetValueMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> Int32 -> GValue -> m ()), MonadIO m, IsListStore a) => O.OverloadedMethod ListStoreSetValueMethodInfo a signature where
    overloadedMethod = listStoreSetValue

instance O.OverloadedMethodInfo ListStoreSetValueMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListStore.listStoreSetValue",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListStore.html#v:listStoreSetValue"
        })


#endif

-- method ListStore::set
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "list_store"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListStore" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkListStore" , sinceVersion = Nothing }
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
--                     Just "A valid #GtkTreeIter for the row being modified"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "columns"
--           , argType = TCArray False (-1) 4 (TBasicType TInt)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an array of column numbers"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "values"
--           , argType = TCArray False (-1) 4 TGValue
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an array of GValues"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_values"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the length of the @columns and @values arrays"
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
--              { argCName = "n_values"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "the length of the @columns and @values arrays"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          , Arg
--              { argCName = "n_values"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "the length of the @columns and @values arrays"
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

foreign import ccall "gtk_list_store_set_valuesv" gtk_list_store_set_valuesv :: 
    Ptr ListStore ->                        -- list_store : TInterface (Name {namespace = "Gtk", name = "ListStore"})
    Ptr Gtk.TreeIter.TreeIter ->            -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Ptr Int32 ->                            -- columns : TCArray False (-1) 4 (TBasicType TInt)
    Ptr B.GValue.GValue ->                  -- values : TCArray False (-1) 4 TGValue
    Int32 ->                                -- n_values : TBasicType TInt
    IO ()

-- | A variant of @/gtk_list_store_set_valist()/@ which
-- takes the columns and values as two arrays, instead of
-- varargs. This function is mainly intended for
-- language-bindings and in case the number of columns to
-- change is not known until run-time.
-- 
-- /Since: 2.12/
listStoreSet ::
    (B.CallStack.HasCallStack, MonadIO m, IsListStore a) =>
    a
    -- ^ /@listStore@/: A t'GI.Gtk.Objects.ListStore.ListStore'
    -> Gtk.TreeIter.TreeIter
    -- ^ /@iter@/: A valid t'GI.Gtk.Structs.TreeIter.TreeIter' for the row being modified
    -> [Int32]
    -- ^ /@columns@/: an array of column numbers
    -> [GValue]
    -- ^ /@values@/: an array of GValues
    -> m ()
listStoreSet listStore iter columns values = liftIO $ do
    let nValues = fromIntegral $ P.length values
    let columns_expected_length_ = fromIntegral $ P.length columns
    when (columns_expected_length_ /= nValues) $
        error "Gtk.listStoreSet : length of 'columns' does not agree with that of 'values'."
    listStore' <- unsafeManagedPtrCastPtr listStore
    iter' <- unsafeManagedPtrGetPtr iter
    columns' <- packStorableArray columns
    values' <- B.GValue.packGValueArray values
    gtk_list_store_set_valuesv listStore' iter' columns' values' nValues
    touchManagedPtr listStore
    touchManagedPtr iter
    mapM_ touchManagedPtr values
    freeMem columns'
    freeMem values'
    return ()

#if defined(ENABLE_OVERLOADING)
data ListStoreSetMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> [Int32] -> [GValue] -> m ()), MonadIO m, IsListStore a) => O.OverloadedMethod ListStoreSetMethodInfo a signature where
    overloadedMethod = listStoreSet

instance O.OverloadedMethodInfo ListStoreSetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListStore.listStoreSet",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListStore.html#v:listStoreSet"
        })


#endif

-- method ListStore::swap
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "store"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ListStore" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkListStore." , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "a"
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
--       , Arg
--           { argCName = "b"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Another #GtkTreeIter."
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

foreign import ccall "gtk_list_store_swap" gtk_list_store_swap :: 
    Ptr ListStore ->                        -- store : TInterface (Name {namespace = "Gtk", name = "ListStore"})
    Ptr Gtk.TreeIter.TreeIter ->            -- a : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Ptr Gtk.TreeIter.TreeIter ->            -- b : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Swaps /@a@/ and /@b@/ in /@store@/. Note that this function only works with
-- unsorted stores.
-- 
-- /Since: 2.2/
listStoreSwap ::
    (B.CallStack.HasCallStack, MonadIO m, IsListStore a) =>
    a
    -- ^ /@store@/: A t'GI.Gtk.Objects.ListStore.ListStore'.
    -> Gtk.TreeIter.TreeIter
    -- ^ /@a@/: A t'GI.Gtk.Structs.TreeIter.TreeIter'.
    -> Gtk.TreeIter.TreeIter
    -- ^ /@b@/: Another t'GI.Gtk.Structs.TreeIter.TreeIter'.
    -> m ()
listStoreSwap store a b = liftIO $ do
    store' <- unsafeManagedPtrCastPtr store
    a' <- unsafeManagedPtrGetPtr a
    b' <- unsafeManagedPtrGetPtr b
    gtk_list_store_swap store' a' b'
    touchManagedPtr store
    touchManagedPtr a
    touchManagedPtr b
    return ()

#if defined(ENABLE_OVERLOADING)
data ListStoreSwapMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> Gtk.TreeIter.TreeIter -> m ()), MonadIO m, IsListStore a) => O.OverloadedMethod ListStoreSwapMethodInfo a signature where
    overloadedMethod = listStoreSwap

instance O.OverloadedMethodInfo ListStoreSwapMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ListStore.listStoreSwap",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ListStore.html#v:listStoreSwap"
        })


#endif


