{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Interfaces.TreeSortable.TreeSortable' is an interface to be implemented by tree models which
-- support sorting. The t'GI.Gtk.Objects.TreeView.TreeView' uses the methods provided by this interface
-- to sort the model.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Interfaces.TreeSortable
    ( 

-- * Exported types
    TreeSortable(..)                        ,
    IsTreeSortable                          ,
    toTreeSortable                          ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [filterNew]("GI.Gtk.Interfaces.TreeModel#g:method:filterNew"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Interfaces.TreeModel#g:method:foreach"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [hasDefaultSortFunc]("GI.Gtk.Interfaces.TreeSortable#g:method:hasDefaultSortFunc"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [iterChildren]("GI.Gtk.Interfaces.TreeModel#g:method:iterChildren"), [iterHasChild]("GI.Gtk.Interfaces.TreeModel#g:method:iterHasChild"), [iterNChildren]("GI.Gtk.Interfaces.TreeModel#g:method:iterNChildren"), [iterNext]("GI.Gtk.Interfaces.TreeModel#g:method:iterNext"), [iterNthChild]("GI.Gtk.Interfaces.TreeModel#g:method:iterNthChild"), [iterParent]("GI.Gtk.Interfaces.TreeModel#g:method:iterParent"), [iterPrevious]("GI.Gtk.Interfaces.TreeModel#g:method:iterPrevious"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refNode]("GI.Gtk.Interfaces.TreeModel#g:method:refNode"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [rowChanged]("GI.Gtk.Interfaces.TreeModel#g:method:rowChanged"), [rowDeleted]("GI.Gtk.Interfaces.TreeModel#g:method:rowDeleted"), [rowHasChildToggled]("GI.Gtk.Interfaces.TreeModel#g:method:rowHasChildToggled"), [rowInserted]("GI.Gtk.Interfaces.TreeModel#g:method:rowInserted"), [rowsReordered]("GI.Gtk.Interfaces.TreeModel#g:method:rowsReordered"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [sortColumnChanged]("GI.Gtk.Interfaces.TreeSortable#g:method:sortColumnChanged"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unrefNode]("GI.Gtk.Interfaces.TreeModel#g:method:unrefNode"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getColumnType]("GI.Gtk.Interfaces.TreeModel#g:method:getColumnType"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getFlags]("GI.Gtk.Interfaces.TreeModel#g:method:getFlags"), [getIter]("GI.Gtk.Interfaces.TreeModel#g:method:getIter"), [getIterFirst]("GI.Gtk.Interfaces.TreeModel#g:method:getIterFirst"), [getIterFromString]("GI.Gtk.Interfaces.TreeModel#g:method:getIterFromString"), [getNColumns]("GI.Gtk.Interfaces.TreeModel#g:method:getNColumns"), [getPath]("GI.Gtk.Interfaces.TreeModel#g:method:getPath"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSortColumnId]("GI.Gtk.Interfaces.TreeSortable#g:method:getSortColumnId"), [getStringFromIter]("GI.Gtk.Interfaces.TreeModel#g:method:getStringFromIter"), [getValue]("GI.Gtk.Interfaces.TreeModel#g:method:getValue").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setDefaultSortFunc]("GI.Gtk.Interfaces.TreeSortable#g:method:setDefaultSortFunc"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setSortColumnId]("GI.Gtk.Interfaces.TreeSortable#g:method:setSortColumnId"), [setSortFunc]("GI.Gtk.Interfaces.TreeSortable#g:method:setSortFunc").

#if defined(ENABLE_OVERLOADING)
    ResolveTreeSortableMethod               ,
#endif

-- ** getSortColumnId #method:getSortColumnId#

#if defined(ENABLE_OVERLOADING)
    TreeSortableGetSortColumnIdMethodInfo   ,
#endif
    treeSortableGetSortColumnId             ,


-- ** hasDefaultSortFunc #method:hasDefaultSortFunc#

#if defined(ENABLE_OVERLOADING)
    TreeSortableHasDefaultSortFuncMethodInfo,
#endif
    treeSortableHasDefaultSortFunc          ,


-- ** setDefaultSortFunc #method:setDefaultSortFunc#

#if defined(ENABLE_OVERLOADING)
    TreeSortableSetDefaultSortFuncMethodInfo,
#endif
    treeSortableSetDefaultSortFunc          ,


-- ** setSortColumnId #method:setSortColumnId#

#if defined(ENABLE_OVERLOADING)
    TreeSortableSetSortColumnIdMethodInfo   ,
#endif
    treeSortableSetSortColumnId             ,


-- ** setSortFunc #method:setSortFunc#

#if defined(ENABLE_OVERLOADING)
    TreeSortableSetSortFuncMethodInfo       ,
#endif
    treeSortableSetSortFunc                 ,


-- ** sortColumnChanged #method:sortColumnChanged#

#if defined(ENABLE_OVERLOADING)
    TreeSortableSortColumnChangedMethodInfo ,
#endif
    treeSortableSortColumnChanged           ,




 -- * Signals


-- ** sortColumnChanged #signal:sortColumnChanged#

    TreeSortableSortColumnChangedCallback   ,
#if defined(ENABLE_OVERLOADING)
    TreeSortableSortColumnChangedSignalInfo ,
#endif
    afterTreeSortableSortColumnChanged      ,
    onTreeSortableSortColumnChanged         ,




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
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeModel as Gtk.TreeModel

-- interface TreeSortable 
-- | Memory-managed wrapper type.
newtype TreeSortable = TreeSortable (SP.ManagedPtr TreeSortable)
    deriving (Eq)

instance SP.ManagedPtrNewtype TreeSortable where
    toManagedPtr (TreeSortable p) = p

foreign import ccall "gtk_tree_sortable_get_type"
    c_gtk_tree_sortable_get_type :: IO B.Types.GType

instance B.Types.TypedObject TreeSortable where
    glibType = c_gtk_tree_sortable_get_type

instance B.Types.GObject TreeSortable

-- | Type class for types which can be safely cast to `TreeSortable`, for instance with `toTreeSortable`.
class (SP.GObject o, O.IsDescendantOf TreeSortable o) => IsTreeSortable o
instance (SP.GObject o, O.IsDescendantOf TreeSortable o) => IsTreeSortable o

instance O.HasParentTypes TreeSortable
type instance O.ParentTypes TreeSortable = '[Gtk.TreeModel.TreeModel, GObject.Object.Object]

-- | Cast to `TreeSortable`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTreeSortable :: (MIO.MonadIO m, IsTreeSortable o) => o -> m TreeSortable
toTreeSortable = MIO.liftIO . B.ManagedPtr.unsafeCastTo TreeSortable

-- | Convert 'TreeSortable' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TreeSortable) where
    gvalueGType_ = c_gtk_tree_sortable_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr TreeSortable)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr TreeSortable)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject TreeSortable ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TreeSortable
type instance O.AttributeList TreeSortable = TreeSortableAttributeList
type TreeSortableAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveTreeSortableMethod (t :: Symbol) (o :: *) :: * where
    ResolveTreeSortableMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTreeSortableMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTreeSortableMethod "filterNew" o = Gtk.TreeModel.TreeModelFilterNewMethodInfo
    ResolveTreeSortableMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTreeSortableMethod "foreach" o = Gtk.TreeModel.TreeModelForeachMethodInfo
    ResolveTreeSortableMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTreeSortableMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTreeSortableMethod "hasDefaultSortFunc" o = TreeSortableHasDefaultSortFuncMethodInfo
    ResolveTreeSortableMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTreeSortableMethod "iterChildren" o = Gtk.TreeModel.TreeModelIterChildrenMethodInfo
    ResolveTreeSortableMethod "iterHasChild" o = Gtk.TreeModel.TreeModelIterHasChildMethodInfo
    ResolveTreeSortableMethod "iterNChildren" o = Gtk.TreeModel.TreeModelIterNChildrenMethodInfo
    ResolveTreeSortableMethod "iterNext" o = Gtk.TreeModel.TreeModelIterNextMethodInfo
    ResolveTreeSortableMethod "iterNthChild" o = Gtk.TreeModel.TreeModelIterNthChildMethodInfo
    ResolveTreeSortableMethod "iterParent" o = Gtk.TreeModel.TreeModelIterParentMethodInfo
    ResolveTreeSortableMethod "iterPrevious" o = Gtk.TreeModel.TreeModelIterPreviousMethodInfo
    ResolveTreeSortableMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTreeSortableMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTreeSortableMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTreeSortableMethod "refNode" o = Gtk.TreeModel.TreeModelRefNodeMethodInfo
    ResolveTreeSortableMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTreeSortableMethod "rowChanged" o = Gtk.TreeModel.TreeModelRowChangedMethodInfo
    ResolveTreeSortableMethod "rowDeleted" o = Gtk.TreeModel.TreeModelRowDeletedMethodInfo
    ResolveTreeSortableMethod "rowHasChildToggled" o = Gtk.TreeModel.TreeModelRowHasChildToggledMethodInfo
    ResolveTreeSortableMethod "rowInserted" o = Gtk.TreeModel.TreeModelRowInsertedMethodInfo
    ResolveTreeSortableMethod "rowsReordered" o = Gtk.TreeModel.TreeModelRowsReorderedMethodInfo
    ResolveTreeSortableMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTreeSortableMethod "sortColumnChanged" o = TreeSortableSortColumnChangedMethodInfo
    ResolveTreeSortableMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTreeSortableMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTreeSortableMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTreeSortableMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTreeSortableMethod "unrefNode" o = Gtk.TreeModel.TreeModelUnrefNodeMethodInfo
    ResolveTreeSortableMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTreeSortableMethod "getColumnType" o = Gtk.TreeModel.TreeModelGetColumnTypeMethodInfo
    ResolveTreeSortableMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTreeSortableMethod "getFlags" o = Gtk.TreeModel.TreeModelGetFlagsMethodInfo
    ResolveTreeSortableMethod "getIter" o = Gtk.TreeModel.TreeModelGetIterMethodInfo
    ResolveTreeSortableMethod "getIterFirst" o = Gtk.TreeModel.TreeModelGetIterFirstMethodInfo
    ResolveTreeSortableMethod "getIterFromString" o = Gtk.TreeModel.TreeModelGetIterFromStringMethodInfo
    ResolveTreeSortableMethod "getNColumns" o = Gtk.TreeModel.TreeModelGetNColumnsMethodInfo
    ResolveTreeSortableMethod "getPath" o = Gtk.TreeModel.TreeModelGetPathMethodInfo
    ResolveTreeSortableMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTreeSortableMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTreeSortableMethod "getSortColumnId" o = TreeSortableGetSortColumnIdMethodInfo
    ResolveTreeSortableMethod "getStringFromIter" o = Gtk.TreeModel.TreeModelGetStringFromIterMethodInfo
    ResolveTreeSortableMethod "getValue" o = Gtk.TreeModel.TreeModelGetValueMethodInfo
    ResolveTreeSortableMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTreeSortableMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTreeSortableMethod "setDefaultSortFunc" o = TreeSortableSetDefaultSortFuncMethodInfo
    ResolveTreeSortableMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTreeSortableMethod "setSortColumnId" o = TreeSortableSetSortColumnIdMethodInfo
    ResolveTreeSortableMethod "setSortFunc" o = TreeSortableSetSortFuncMethodInfo
    ResolveTreeSortableMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTreeSortableMethod t TreeSortable, O.OverloadedMethod info TreeSortable p) => OL.IsLabel t (TreeSortable -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTreeSortableMethod t TreeSortable, O.OverloadedMethod info TreeSortable p, R.HasField t TreeSortable p) => R.HasField t TreeSortable p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTreeSortableMethod t TreeSortable, O.OverloadedMethodInfo info TreeSortable) => OL.IsLabel t (O.MethodProxy info TreeSortable) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- method TreeSortable::get_sort_column_id
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "sortable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSortable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSortable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sort_column_id"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The sort column id to be filled in"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "order"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SortType" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GtkSortType to be filled in"
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

foreign import ccall "gtk_tree_sortable_get_sort_column_id" gtk_tree_sortable_get_sort_column_id :: 
    Ptr TreeSortable ->                     -- sortable : TInterface (Name {namespace = "Gtk", name = "TreeSortable"})
    Ptr Int32 ->                            -- sort_column_id : TBasicType TInt
    Ptr CUInt ->                            -- order : TInterface (Name {namespace = "Gtk", name = "SortType"})
    IO CInt

-- | Fills in /@sortColumnId@/ and /@order@/ with the current sort column and the
-- order. It returns 'P.True' unless the /@sortColumnId@/ is
-- 'GI.Gtk.Constants.TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID' or
-- 'GI.Gtk.Constants.TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID'.
treeSortableGetSortColumnId ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSortable a) =>
    a
    -- ^ /@sortable@/: A t'GI.Gtk.Interfaces.TreeSortable.TreeSortable'
    -> m ((Bool, Int32, Gtk.Enums.SortType))
    -- ^ __Returns:__ 'P.True' if the sort column is not one of the special sort
    --   column ids.
treeSortableGetSortColumnId sortable = liftIO $ do
    sortable' <- unsafeManagedPtrCastPtr sortable
    sortColumnId <- allocMem :: IO (Ptr Int32)
    order <- allocMem :: IO (Ptr CUInt)
    result <- gtk_tree_sortable_get_sort_column_id sortable' sortColumnId order
    let result' = (/= 0) result
    sortColumnId' <- peek sortColumnId
    order' <- peek order
    let order'' = (toEnum . fromIntegral) order'
    touchManagedPtr sortable
    freeMem sortColumnId
    freeMem order
    return (result', sortColumnId', order'')

#if defined(ENABLE_OVERLOADING)
data TreeSortableGetSortColumnIdMethodInfo
instance (signature ~ (m ((Bool, Int32, Gtk.Enums.SortType))), MonadIO m, IsTreeSortable a) => O.OverloadedMethod TreeSortableGetSortColumnIdMethodInfo a signature where
    overloadedMethod = treeSortableGetSortColumnId

instance O.OverloadedMethodInfo TreeSortableGetSortColumnIdMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeSortable.treeSortableGetSortColumnId",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeSortable.html#v:treeSortableGetSortColumnId"
        })


#endif

-- method TreeSortable::has_default_sort_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "sortable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSortable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSortable" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_sortable_has_default_sort_func" gtk_tree_sortable_has_default_sort_func :: 
    Ptr TreeSortable ->                     -- sortable : TInterface (Name {namespace = "Gtk", name = "TreeSortable"})
    IO CInt

-- | Returns 'P.True' if the model has a default sort function. This is used
-- primarily by GtkTreeViewColumns in order to determine if a model can
-- go back to the default state, or not.
treeSortableHasDefaultSortFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSortable a) =>
    a
    -- ^ /@sortable@/: A t'GI.Gtk.Interfaces.TreeSortable.TreeSortable'
    -> m Bool
    -- ^ __Returns:__ 'P.True', if the model has a default sort function
treeSortableHasDefaultSortFunc sortable = liftIO $ do
    sortable' <- unsafeManagedPtrCastPtr sortable
    result <- gtk_tree_sortable_has_default_sort_func sortable'
    let result' = (/= 0) result
    touchManagedPtr sortable
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeSortableHasDefaultSortFuncMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsTreeSortable a) => O.OverloadedMethod TreeSortableHasDefaultSortFuncMethodInfo a signature where
    overloadedMethod = treeSortableHasDefaultSortFunc

instance O.OverloadedMethodInfo TreeSortableHasDefaultSortFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeSortable.treeSortableHasDefaultSortFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeSortable.html#v:treeSortableHasDefaultSortFunc"
        })


#endif

-- method TreeSortable::set_default_sort_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "sortable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSortable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSortable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sort_func"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "TreeIterCompareFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The comparison function"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
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
--                 { rawDocText = Just "User data to pass to @sort_func, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "destroy"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Destroy notifier of @user_data, or %NULL"
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

foreign import ccall "gtk_tree_sortable_set_default_sort_func" gtk_tree_sortable_set_default_sort_func :: 
    Ptr TreeSortable ->                     -- sortable : TInterface (Name {namespace = "Gtk", name = "TreeSortable"})
    FunPtr Gtk.Callbacks.C_TreeIterCompareFunc -> -- sort_func : TInterface (Name {namespace = "Gtk", name = "TreeIterCompareFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Sets the default comparison function used when sorting to be /@sortFunc@/.
-- If the current sort column id of /@sortable@/ is
-- 'GI.Gtk.Constants.TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID', then the model will sort using
-- this function.
-- 
-- If /@sortFunc@/ is 'P.Nothing', then there will be no default comparison function.
-- This means that once the model  has been sorted, it can’t go back to the
-- default state. In this case, when the current sort column id of /@sortable@/
-- is 'GI.Gtk.Constants.TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID', the model will be unsorted.
treeSortableSetDefaultSortFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSortable a) =>
    a
    -- ^ /@sortable@/: A t'GI.Gtk.Interfaces.TreeSortable.TreeSortable'
    -> Gtk.Callbacks.TreeIterCompareFunc
    -- ^ /@sortFunc@/: The comparison function
    -> m ()
treeSortableSetDefaultSortFunc sortable sortFunc = liftIO $ do
    sortable' <- unsafeManagedPtrCastPtr sortable
    sortFunc' <- Gtk.Callbacks.mk_TreeIterCompareFunc (Gtk.Callbacks.wrap_TreeIterCompareFunc Nothing (Gtk.Callbacks.drop_closures_TreeIterCompareFunc sortFunc))
    let userData = castFunPtrToPtr sortFunc'
    let destroy = SP.safeFreeFunPtrPtr
    gtk_tree_sortable_set_default_sort_func sortable' sortFunc' userData destroy
    touchManagedPtr sortable
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeSortableSetDefaultSortFuncMethodInfo
instance (signature ~ (Gtk.Callbacks.TreeIterCompareFunc -> m ()), MonadIO m, IsTreeSortable a) => O.OverloadedMethod TreeSortableSetDefaultSortFuncMethodInfo a signature where
    overloadedMethod = treeSortableSetDefaultSortFunc

instance O.OverloadedMethodInfo TreeSortableSetDefaultSortFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeSortable.treeSortableSetDefaultSortFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeSortable.html#v:treeSortableSetDefaultSortFunc"
        })


#endif

-- method TreeSortable::set_sort_column_id
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "sortable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSortable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSortable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sort_column_id"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the sort column id to set"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "order"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SortType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The sort order of the column"
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

foreign import ccall "gtk_tree_sortable_set_sort_column_id" gtk_tree_sortable_set_sort_column_id :: 
    Ptr TreeSortable ->                     -- sortable : TInterface (Name {namespace = "Gtk", name = "TreeSortable"})
    Int32 ->                                -- sort_column_id : TBasicType TInt
    CUInt ->                                -- order : TInterface (Name {namespace = "Gtk", name = "SortType"})
    IO ()

-- | Sets the current sort column to be /@sortColumnId@/. The /@sortable@/ will
-- resort itself to reflect this change, after emitting a
-- [TreeSortable::sortColumnChanged]("GI.Gtk.Interfaces.TreeSortable#g:signal:sortColumnChanged") signal. /@sortColumnId@/ may either be
-- a regular column id, or one of the following special values:
-- 
-- * 'GI.Gtk.Constants.TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID': the default sort function
-- will be used, if it is set
-- * 'GI.Gtk.Constants.TREE_SORTABLE_UNSORTED_SORT_COLUMN_ID': no sorting will occur
treeSortableSetSortColumnId ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSortable a) =>
    a
    -- ^ /@sortable@/: A t'GI.Gtk.Interfaces.TreeSortable.TreeSortable'
    -> Int32
    -- ^ /@sortColumnId@/: the sort column id to set
    -> Gtk.Enums.SortType
    -- ^ /@order@/: The sort order of the column
    -> m ()
treeSortableSetSortColumnId sortable sortColumnId order = liftIO $ do
    sortable' <- unsafeManagedPtrCastPtr sortable
    let order' = (fromIntegral . fromEnum) order
    gtk_tree_sortable_set_sort_column_id sortable' sortColumnId order'
    touchManagedPtr sortable
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeSortableSetSortColumnIdMethodInfo
instance (signature ~ (Int32 -> Gtk.Enums.SortType -> m ()), MonadIO m, IsTreeSortable a) => O.OverloadedMethod TreeSortableSetSortColumnIdMethodInfo a signature where
    overloadedMethod = treeSortableSetSortColumnId

instance O.OverloadedMethodInfo TreeSortableSetSortColumnIdMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeSortable.treeSortableSetSortColumnId",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeSortable.html#v:treeSortableSetSortColumnId"
        })


#endif

-- method TreeSortable::set_sort_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "sortable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSortable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSortable" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sort_column_id"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the sort column id to set the function for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "sort_func"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "TreeIterCompareFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The comparison function"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 3
--           , argDestroy = 4
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
--                 { rawDocText = Just "User data to pass to @sort_func, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "destroy"
--           , argType =
--               TInterface Name { namespace = "GLib" , name = "DestroyNotify" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Destroy notifier of @user_data, or %NULL"
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

foreign import ccall "gtk_tree_sortable_set_sort_func" gtk_tree_sortable_set_sort_func :: 
    Ptr TreeSortable ->                     -- sortable : TInterface (Name {namespace = "Gtk", name = "TreeSortable"})
    Int32 ->                                -- sort_column_id : TBasicType TInt
    FunPtr Gtk.Callbacks.C_TreeIterCompareFunc -> -- sort_func : TInterface (Name {namespace = "Gtk", name = "TreeIterCompareFunc"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Sets the comparison function used when sorting to be /@sortFunc@/. If the
-- current sort column id of /@sortable@/ is the same as /@sortColumnId@/, then
-- the model will sort using this function.
treeSortableSetSortFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSortable a) =>
    a
    -- ^ /@sortable@/: A t'GI.Gtk.Interfaces.TreeSortable.TreeSortable'
    -> Int32
    -- ^ /@sortColumnId@/: the sort column id to set the function for
    -> Gtk.Callbacks.TreeIterCompareFunc
    -- ^ /@sortFunc@/: The comparison function
    -> m ()
treeSortableSetSortFunc sortable sortColumnId sortFunc = liftIO $ do
    sortable' <- unsafeManagedPtrCastPtr sortable
    sortFunc' <- Gtk.Callbacks.mk_TreeIterCompareFunc (Gtk.Callbacks.wrap_TreeIterCompareFunc Nothing (Gtk.Callbacks.drop_closures_TreeIterCompareFunc sortFunc))
    let userData = castFunPtrToPtr sortFunc'
    let destroy = SP.safeFreeFunPtrPtr
    gtk_tree_sortable_set_sort_func sortable' sortColumnId sortFunc' userData destroy
    touchManagedPtr sortable
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeSortableSetSortFuncMethodInfo
instance (signature ~ (Int32 -> Gtk.Callbacks.TreeIterCompareFunc -> m ()), MonadIO m, IsTreeSortable a) => O.OverloadedMethod TreeSortableSetSortFuncMethodInfo a signature where
    overloadedMethod = treeSortableSetSortFunc

instance O.OverloadedMethodInfo TreeSortableSetSortFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeSortable.treeSortableSetSortFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeSortable.html#v:treeSortableSetSortFunc"
        })


#endif

-- method TreeSortable::sort_column_changed
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "sortable"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeSortable" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeSortable" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_sortable_sort_column_changed" gtk_tree_sortable_sort_column_changed :: 
    Ptr TreeSortable ->                     -- sortable : TInterface (Name {namespace = "Gtk", name = "TreeSortable"})
    IO ()

-- | Emits a [TreeSortable::sortColumnChanged]("GI.Gtk.Interfaces.TreeSortable#g:signal:sortColumnChanged") signal on /@sortable@/.
treeSortableSortColumnChanged ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeSortable a) =>
    a
    -- ^ /@sortable@/: A t'GI.Gtk.Interfaces.TreeSortable.TreeSortable'
    -> m ()
treeSortableSortColumnChanged sortable = liftIO $ do
    sortable' <- unsafeManagedPtrCastPtr sortable
    gtk_tree_sortable_sort_column_changed sortable'
    touchManagedPtr sortable
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeSortableSortColumnChangedMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTreeSortable a) => O.OverloadedMethod TreeSortableSortColumnChangedMethodInfo a signature where
    overloadedMethod = treeSortableSortColumnChanged

instance O.OverloadedMethodInfo TreeSortableSortColumnChangedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeSortable.treeSortableSortColumnChanged",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeSortable.html#v:treeSortableSortColumnChanged"
        })


#endif

-- signal TreeSortable::sort-column-changed
-- | The [sortColumnChanged](#g:signal:sortColumnChanged) signal is emitted when the sort column
-- or sort order of /@sortable@/ is changed. The signal is emitted before
-- the contents of /@sortable@/ are resorted.
type TreeSortableSortColumnChangedCallback =
    IO ()

type C_TreeSortableSortColumnChangedCallback =
    Ptr TreeSortable ->                     -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_TreeSortableSortColumnChangedCallback`.
foreign import ccall "wrapper"
    mk_TreeSortableSortColumnChangedCallback :: C_TreeSortableSortColumnChangedCallback -> IO (FunPtr C_TreeSortableSortColumnChangedCallback)

wrap_TreeSortableSortColumnChangedCallback :: 
    GObject a => (a -> TreeSortableSortColumnChangedCallback) ->
    C_TreeSortableSortColumnChangedCallback
wrap_TreeSortableSortColumnChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [sortColumnChanged](#signal:sortColumnChanged) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' treeSortable #sortColumnChanged callback
-- @
-- 
-- 
onTreeSortableSortColumnChanged :: (IsTreeSortable a, MonadIO m) => a -> ((?self :: a) => TreeSortableSortColumnChangedCallback) -> m SignalHandlerId
onTreeSortableSortColumnChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeSortableSortColumnChangedCallback wrapped
    wrapped'' <- mk_TreeSortableSortColumnChangedCallback wrapped'
    connectSignalFunPtr obj "sort-column-changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [sortColumnChanged](#signal:sortColumnChanged) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' treeSortable #sortColumnChanged callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterTreeSortableSortColumnChanged :: (IsTreeSortable a, MonadIO m) => a -> ((?self :: a) => TreeSortableSortColumnChangedCallback) -> m SignalHandlerId
afterTreeSortableSortColumnChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_TreeSortableSortColumnChangedCallback wrapped
    wrapped'' <- mk_TreeSortableSortColumnChangedCallback wrapped'
    connectSignalFunPtr obj "sort-column-changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data TreeSortableSortColumnChangedSignalInfo
instance SignalInfo TreeSortableSortColumnChangedSignalInfo where
    type HaskellCallbackType TreeSortableSortColumnChangedSignalInfo = TreeSortableSortColumnChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_TreeSortableSortColumnChangedCallback cb
        cb'' <- mk_TreeSortableSortColumnChangedCallback cb'
        connectSignalFunPtr obj "sort-column-changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeSortable::sort-column-changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeSortable.html#g:signal:sortColumnChanged"})

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TreeSortable = TreeSortableSignalList
type TreeSortableSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo), '("rowChanged", Gtk.TreeModel.TreeModelRowChangedSignalInfo), '("rowDeleted", Gtk.TreeModel.TreeModelRowDeletedSignalInfo), '("rowHasChildToggled", Gtk.TreeModel.TreeModelRowHasChildToggledSignalInfo), '("rowInserted", Gtk.TreeModel.TreeModelRowInsertedSignalInfo), '("sortColumnChanged", TreeSortableSortColumnChangedSignalInfo)] :: [(Symbol, *)])

#endif


