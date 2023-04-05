{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter' is a tree model which wraps another tree model,
-- and can do the following things:
-- 
-- * Filter specific rows, based on data from a “visible column”, a column
-- storing booleans indicating whether the row should be filtered or not,
-- or based on the return value of a “visible function”, which gets a
-- model, iter and user_data and returns a boolean indicating whether the
-- row should be filtered or not.
-- * Modify the “appearance” of the model, using a modify function.
-- This is extremely powerful and allows for just changing some
-- values and also for creating a completely different model based
-- on the given child model.
-- * Set a different root node, also known as a “virtual root”. You can pass
-- in a t'GI.Gtk.Structs.TreePath.TreePath' indicating the root node for the filter at construction
-- time.
-- 
-- 
-- The basic API is similar to t'GI.Gtk.Objects.TreeModelSort.TreeModelSort'. For an example on its usage,
-- see the section on t'GI.Gtk.Objects.TreeModelSort.TreeModelSort'.
-- 
-- When using t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter', it is important to realize that
-- t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter' maintains an internal cache of all nodes which are
-- visible in its clients. The cache is likely to be a subtree of the tree
-- exposed by the child model. t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter' will not cache the entire
-- child model when unnecessary to not compromise the caching mechanism
-- that is exposed by the reference counting scheme. If the child model
-- implements reference counting, unnecessary signals may not be emitted
-- because of reference counting rule 3, see the t'GI.Gtk.Interfaces.TreeModel.TreeModel'
-- documentation. (Note that e.g. t'GI.Gtk.Objects.TreeStore.TreeStore' does not implement
-- reference counting and will always emit all signals, even when
-- the receiving node is not visible).
-- 
-- Because of this, limitations for possible visible functions do apply.
-- In general, visible functions should only use data or properties from
-- the node for which the visibility state must be determined, its siblings
-- or its parents. Usually, having a dependency on the state of any child
-- node is not possible, unless references are taken on these explicitly.
-- When no such reference exists, no signals may be received for these child
-- nodes (see reference couting rule number 3 in the t'GI.Gtk.Interfaces.TreeModel.TreeModel' section).
-- 
-- Determining the visibility state of a given node based on the state
-- of its child nodes is a frequently occurring use case. Therefore,
-- t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter' explicitly supports this. For example, when a node
-- does not have any children, you might not want the node to be visible.
-- As soon as the first row is added to the node’s child level (or the
-- last row removed), the node’s visibility should be updated.
-- 
-- This introduces a dependency from the node on its child nodes. In order
-- to accommodate this, t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter' must make sure the necessary
-- signals are received from the child model. This is achieved by building,
-- for all nodes which are exposed as visible nodes to t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter'\'s
-- clients, the child level (if any) and take a reference on the first node
-- in this level. Furthermore, for every row-inserted, row-changed or
-- row-deleted signal (also these which were not handled because the node
-- was not cached), t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter' will check if the visibility state
-- of any parent node has changed.
-- 
-- Beware, however, that this explicit support is limited to these two
-- cases. For example, if you want a node to be visible only if two nodes
-- in a child’s child level (2 levels deeper) are visible, you are on your
-- own. In this case, either rely on t'GI.Gtk.Objects.TreeStore.TreeStore' to emit all signals
-- because it does not implement reference counting, or for models that
-- do implement reference counting, obtain references on these child levels
-- yourself.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.TreeModelFilter
    ( 

-- * Exported types
    TreeModelFilter(..)                     ,
    IsTreeModelFilter                       ,
    toTreeModelFilter                       ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [clearCache]("GI.Gtk.Objects.TreeModelFilter#g:method:clearCache"), [convertChildIterToIter]("GI.Gtk.Objects.TreeModelFilter#g:method:convertChildIterToIter"), [convertChildPathToPath]("GI.Gtk.Objects.TreeModelFilter#g:method:convertChildPathToPath"), [convertIterToChildIter]("GI.Gtk.Objects.TreeModelFilter#g:method:convertIterToChildIter"), [convertPathToChildPath]("GI.Gtk.Objects.TreeModelFilter#g:method:convertPathToChildPath"), [dragDataDelete]("GI.Gtk.Interfaces.TreeDragSource#g:method:dragDataDelete"), [dragDataGet]("GI.Gtk.Interfaces.TreeDragSource#g:method:dragDataGet"), [filterNew]("GI.Gtk.Interfaces.TreeModel#g:method:filterNew"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [foreach]("GI.Gtk.Interfaces.TreeModel#g:method:foreach"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [iterChildren]("GI.Gtk.Interfaces.TreeModel#g:method:iterChildren"), [iterHasChild]("GI.Gtk.Interfaces.TreeModel#g:method:iterHasChild"), [iterNChildren]("GI.Gtk.Interfaces.TreeModel#g:method:iterNChildren"), [iterNext]("GI.Gtk.Interfaces.TreeModel#g:method:iterNext"), [iterNthChild]("GI.Gtk.Interfaces.TreeModel#g:method:iterNthChild"), [iterParent]("GI.Gtk.Interfaces.TreeModel#g:method:iterParent"), [iterPrevious]("GI.Gtk.Interfaces.TreeModel#g:method:iterPrevious"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refNode]("GI.Gtk.Interfaces.TreeModel#g:method:refNode"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [refilter]("GI.Gtk.Objects.TreeModelFilter#g:method:refilter"), [rowChanged]("GI.Gtk.Interfaces.TreeModel#g:method:rowChanged"), [rowDeleted]("GI.Gtk.Interfaces.TreeModel#g:method:rowDeleted"), [rowDraggable]("GI.Gtk.Interfaces.TreeDragSource#g:method:rowDraggable"), [rowHasChildToggled]("GI.Gtk.Interfaces.TreeModel#g:method:rowHasChildToggled"), [rowInserted]("GI.Gtk.Interfaces.TreeModel#g:method:rowInserted"), [rowsReordered]("GI.Gtk.Interfaces.TreeModel#g:method:rowsReordered"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [unrefNode]("GI.Gtk.Interfaces.TreeModel#g:method:unrefNode"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getColumnType]("GI.Gtk.Interfaces.TreeModel#g:method:getColumnType"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getFlags]("GI.Gtk.Interfaces.TreeModel#g:method:getFlags"), [getIter]("GI.Gtk.Interfaces.TreeModel#g:method:getIter"), [getIterFirst]("GI.Gtk.Interfaces.TreeModel#g:method:getIterFirst"), [getIterFromString]("GI.Gtk.Interfaces.TreeModel#g:method:getIterFromString"), [getModel]("GI.Gtk.Objects.TreeModelFilter#g:method:getModel"), [getNColumns]("GI.Gtk.Interfaces.TreeModel#g:method:getNColumns"), [getPath]("GI.Gtk.Interfaces.TreeModel#g:method:getPath"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getStringFromIter]("GI.Gtk.Interfaces.TreeModel#g:method:getStringFromIter"), [getValue]("GI.Gtk.Interfaces.TreeModel#g:method:getValue").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setVisibleColumn]("GI.Gtk.Objects.TreeModelFilter#g:method:setVisibleColumn"), [setVisibleFunc]("GI.Gtk.Objects.TreeModelFilter#g:method:setVisibleFunc").

#if defined(ENABLE_OVERLOADING)
    ResolveTreeModelFilterMethod            ,
#endif

-- ** clearCache #method:clearCache#

#if defined(ENABLE_OVERLOADING)
    TreeModelFilterClearCacheMethodInfo     ,
#endif
    treeModelFilterClearCache               ,


-- ** convertChildIterToIter #method:convertChildIterToIter#

#if defined(ENABLE_OVERLOADING)
    TreeModelFilterConvertChildIterToIterMethodInfo,
#endif
    treeModelFilterConvertChildIterToIter   ,


-- ** convertChildPathToPath #method:convertChildPathToPath#

#if defined(ENABLE_OVERLOADING)
    TreeModelFilterConvertChildPathToPathMethodInfo,
#endif
    treeModelFilterConvertChildPathToPath   ,


-- ** convertIterToChildIter #method:convertIterToChildIter#

#if defined(ENABLE_OVERLOADING)
    TreeModelFilterConvertIterToChildIterMethodInfo,
#endif
    treeModelFilterConvertIterToChildIter   ,


-- ** convertPathToChildPath #method:convertPathToChildPath#

#if defined(ENABLE_OVERLOADING)
    TreeModelFilterConvertPathToChildPathMethodInfo,
#endif
    treeModelFilterConvertPathToChildPath   ,


-- ** getModel #method:getModel#

#if defined(ENABLE_OVERLOADING)
    TreeModelFilterGetModelMethodInfo       ,
#endif
    treeModelFilterGetModel                 ,


-- ** refilter #method:refilter#

#if defined(ENABLE_OVERLOADING)
    TreeModelFilterRefilterMethodInfo       ,
#endif
    treeModelFilterRefilter                 ,


-- ** setVisibleColumn #method:setVisibleColumn#

#if defined(ENABLE_OVERLOADING)
    TreeModelFilterSetVisibleColumnMethodInfo,
#endif
    treeModelFilterSetVisibleColumn         ,


-- ** setVisibleFunc #method:setVisibleFunc#

#if defined(ENABLE_OVERLOADING)
    TreeModelFilterSetVisibleFuncMethodInfo ,
#endif
    treeModelFilterSetVisibleFunc           ,




 -- * Properties


-- ** childModel #attr:childModel#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeModelFilterChildModelPropertyInfo   ,
#endif
    constructTreeModelFilterChildModel      ,
    getTreeModelFilterChildModel            ,
#if defined(ENABLE_OVERLOADING)
    treeModelFilterChildModel               ,
#endif


-- ** virtualRoot #attr:virtualRoot#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    TreeModelFilterVirtualRootPropertyInfo  ,
#endif
    constructTreeModelFilterVirtualRoot     ,
    getTreeModelFilterVirtualRoot           ,
#if defined(ENABLE_OVERLOADING)
    treeModelFilterVirtualRoot              ,
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

import qualified GI.GLib.Callbacks as GLib.Callbacks
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gtk.Callbacks as Gtk.Callbacks
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeDragSource as Gtk.TreeDragSource
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeModel as Gtk.TreeModel
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreeIter as Gtk.TreeIter
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreePath as Gtk.TreePath

-- | Memory-managed wrapper type.
newtype TreeModelFilter = TreeModelFilter (SP.ManagedPtr TreeModelFilter)
    deriving (Eq)

instance SP.ManagedPtrNewtype TreeModelFilter where
    toManagedPtr (TreeModelFilter p) = p

foreign import ccall "gtk_tree_model_filter_get_type"
    c_gtk_tree_model_filter_get_type :: IO B.Types.GType

instance B.Types.TypedObject TreeModelFilter where
    glibType = c_gtk_tree_model_filter_get_type

instance B.Types.GObject TreeModelFilter

-- | Type class for types which can be safely cast to `TreeModelFilter`, for instance with `toTreeModelFilter`.
class (SP.GObject o, O.IsDescendantOf TreeModelFilter o) => IsTreeModelFilter o
instance (SP.GObject o, O.IsDescendantOf TreeModelFilter o) => IsTreeModelFilter o

instance O.HasParentTypes TreeModelFilter
type instance O.ParentTypes TreeModelFilter = '[GObject.Object.Object, Gtk.TreeDragSource.TreeDragSource, Gtk.TreeModel.TreeModel]

-- | Cast to `TreeModelFilter`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toTreeModelFilter :: (MIO.MonadIO m, IsTreeModelFilter o) => o -> m TreeModelFilter
toTreeModelFilter = MIO.liftIO . B.ManagedPtr.unsafeCastTo TreeModelFilter

-- | Convert 'TreeModelFilter' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TreeModelFilter) where
    gvalueGType_ = c_gtk_tree_model_filter_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr TreeModelFilter)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr TreeModelFilter)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject TreeModelFilter ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveTreeModelFilterMethod (t :: Symbol) (o :: *) :: * where
    ResolveTreeModelFilterMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveTreeModelFilterMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveTreeModelFilterMethod "clearCache" o = TreeModelFilterClearCacheMethodInfo
    ResolveTreeModelFilterMethod "convertChildIterToIter" o = TreeModelFilterConvertChildIterToIterMethodInfo
    ResolveTreeModelFilterMethod "convertChildPathToPath" o = TreeModelFilterConvertChildPathToPathMethodInfo
    ResolveTreeModelFilterMethod "convertIterToChildIter" o = TreeModelFilterConvertIterToChildIterMethodInfo
    ResolveTreeModelFilterMethod "convertPathToChildPath" o = TreeModelFilterConvertPathToChildPathMethodInfo
    ResolveTreeModelFilterMethod "dragDataDelete" o = Gtk.TreeDragSource.TreeDragSourceDragDataDeleteMethodInfo
    ResolveTreeModelFilterMethod "dragDataGet" o = Gtk.TreeDragSource.TreeDragSourceDragDataGetMethodInfo
    ResolveTreeModelFilterMethod "filterNew" o = Gtk.TreeModel.TreeModelFilterNewMethodInfo
    ResolveTreeModelFilterMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveTreeModelFilterMethod "foreach" o = Gtk.TreeModel.TreeModelForeachMethodInfo
    ResolveTreeModelFilterMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveTreeModelFilterMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveTreeModelFilterMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveTreeModelFilterMethod "iterChildren" o = Gtk.TreeModel.TreeModelIterChildrenMethodInfo
    ResolveTreeModelFilterMethod "iterHasChild" o = Gtk.TreeModel.TreeModelIterHasChildMethodInfo
    ResolveTreeModelFilterMethod "iterNChildren" o = Gtk.TreeModel.TreeModelIterNChildrenMethodInfo
    ResolveTreeModelFilterMethod "iterNext" o = Gtk.TreeModel.TreeModelIterNextMethodInfo
    ResolveTreeModelFilterMethod "iterNthChild" o = Gtk.TreeModel.TreeModelIterNthChildMethodInfo
    ResolveTreeModelFilterMethod "iterParent" o = Gtk.TreeModel.TreeModelIterParentMethodInfo
    ResolveTreeModelFilterMethod "iterPrevious" o = Gtk.TreeModel.TreeModelIterPreviousMethodInfo
    ResolveTreeModelFilterMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveTreeModelFilterMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveTreeModelFilterMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveTreeModelFilterMethod "refNode" o = Gtk.TreeModel.TreeModelRefNodeMethodInfo
    ResolveTreeModelFilterMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveTreeModelFilterMethod "refilter" o = TreeModelFilterRefilterMethodInfo
    ResolveTreeModelFilterMethod "rowChanged" o = Gtk.TreeModel.TreeModelRowChangedMethodInfo
    ResolveTreeModelFilterMethod "rowDeleted" o = Gtk.TreeModel.TreeModelRowDeletedMethodInfo
    ResolveTreeModelFilterMethod "rowDraggable" o = Gtk.TreeDragSource.TreeDragSourceRowDraggableMethodInfo
    ResolveTreeModelFilterMethod "rowHasChildToggled" o = Gtk.TreeModel.TreeModelRowHasChildToggledMethodInfo
    ResolveTreeModelFilterMethod "rowInserted" o = Gtk.TreeModel.TreeModelRowInsertedMethodInfo
    ResolveTreeModelFilterMethod "rowsReordered" o = Gtk.TreeModel.TreeModelRowsReorderedMethodInfo
    ResolveTreeModelFilterMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveTreeModelFilterMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveTreeModelFilterMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveTreeModelFilterMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveTreeModelFilterMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveTreeModelFilterMethod "unrefNode" o = Gtk.TreeModel.TreeModelUnrefNodeMethodInfo
    ResolveTreeModelFilterMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveTreeModelFilterMethod "getColumnType" o = Gtk.TreeModel.TreeModelGetColumnTypeMethodInfo
    ResolveTreeModelFilterMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveTreeModelFilterMethod "getFlags" o = Gtk.TreeModel.TreeModelGetFlagsMethodInfo
    ResolveTreeModelFilterMethod "getIter" o = Gtk.TreeModel.TreeModelGetIterMethodInfo
    ResolveTreeModelFilterMethod "getIterFirst" o = Gtk.TreeModel.TreeModelGetIterFirstMethodInfo
    ResolveTreeModelFilterMethod "getIterFromString" o = Gtk.TreeModel.TreeModelGetIterFromStringMethodInfo
    ResolveTreeModelFilterMethod "getModel" o = TreeModelFilterGetModelMethodInfo
    ResolveTreeModelFilterMethod "getNColumns" o = Gtk.TreeModel.TreeModelGetNColumnsMethodInfo
    ResolveTreeModelFilterMethod "getPath" o = Gtk.TreeModel.TreeModelGetPathMethodInfo
    ResolveTreeModelFilterMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveTreeModelFilterMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveTreeModelFilterMethod "getStringFromIter" o = Gtk.TreeModel.TreeModelGetStringFromIterMethodInfo
    ResolveTreeModelFilterMethod "getValue" o = Gtk.TreeModel.TreeModelGetValueMethodInfo
    ResolveTreeModelFilterMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveTreeModelFilterMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveTreeModelFilterMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveTreeModelFilterMethod "setVisibleColumn" o = TreeModelFilterSetVisibleColumnMethodInfo
    ResolveTreeModelFilterMethod "setVisibleFunc" o = TreeModelFilterSetVisibleFuncMethodInfo
    ResolveTreeModelFilterMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTreeModelFilterMethod t TreeModelFilter, O.OverloadedMethod info TreeModelFilter p) => OL.IsLabel t (TreeModelFilter -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTreeModelFilterMethod t TreeModelFilter, O.OverloadedMethod info TreeModelFilter p, R.HasField t TreeModelFilter p) => R.HasField t TreeModelFilter p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTreeModelFilterMethod t TreeModelFilter, O.OverloadedMethodInfo info TreeModelFilter) => OL.IsLabel t (O.MethodProxy info TreeModelFilter) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "child-model"
   -- Type: TInterface (Name {namespace = "Gtk", name = "TreeModel"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@child-model@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeModelFilter #childModel
-- @
getTreeModelFilterChildModel :: (MonadIO m, IsTreeModelFilter o) => o -> m (Maybe Gtk.TreeModel.TreeModel)
getTreeModelFilterChildModel obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "child-model" Gtk.TreeModel.TreeModel

-- | Construct a `GValueConstruct` with valid value for the “@child-model@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeModelFilterChildModel :: (IsTreeModelFilter o, MIO.MonadIO m, Gtk.TreeModel.IsTreeModel a) => a -> m (GValueConstruct o)
constructTreeModelFilterChildModel val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "child-model" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data TreeModelFilterChildModelPropertyInfo
instance AttrInfo TreeModelFilterChildModelPropertyInfo where
    type AttrAllowedOps TreeModelFilterChildModelPropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TreeModelFilterChildModelPropertyInfo = IsTreeModelFilter
    type AttrSetTypeConstraint TreeModelFilterChildModelPropertyInfo = Gtk.TreeModel.IsTreeModel
    type AttrTransferTypeConstraint TreeModelFilterChildModelPropertyInfo = Gtk.TreeModel.IsTreeModel
    type AttrTransferType TreeModelFilterChildModelPropertyInfo = Gtk.TreeModel.TreeModel
    type AttrGetType TreeModelFilterChildModelPropertyInfo = (Maybe Gtk.TreeModel.TreeModel)
    type AttrLabel TreeModelFilterChildModelPropertyInfo = "child-model"
    type AttrOrigin TreeModelFilterChildModelPropertyInfo = TreeModelFilter
    attrGet = getTreeModelFilterChildModel
    attrSet = undefined
    attrTransfer _ v = do
        unsafeCastTo Gtk.TreeModel.TreeModel v
    attrConstruct = constructTreeModelFilterChildModel
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelFilter.childModel"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelFilter.html#g:attr:childModel"
        })
#endif

-- VVV Prop "virtual-root"
   -- Type: TInterface (Name {namespace = "Gtk", name = "TreePath"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@virtual-root@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeModelFilter #virtualRoot
-- @
getTreeModelFilterVirtualRoot :: (MonadIO m, IsTreeModelFilter o) => o -> m (Maybe Gtk.TreePath.TreePath)
getTreeModelFilterVirtualRoot obj = MIO.liftIO $ B.Properties.getObjectPropertyBoxed obj "virtual-root" Gtk.TreePath.TreePath

-- | Construct a `GValueConstruct` with valid value for the “@virtual-root@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructTreeModelFilterVirtualRoot :: (IsTreeModelFilter o, MIO.MonadIO m) => Gtk.TreePath.TreePath -> m (GValueConstruct o)
constructTreeModelFilterVirtualRoot val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyBoxed "virtual-root" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data TreeModelFilterVirtualRootPropertyInfo
instance AttrInfo TreeModelFilterVirtualRootPropertyInfo where
    type AttrAllowedOps TreeModelFilterVirtualRootPropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint TreeModelFilterVirtualRootPropertyInfo = IsTreeModelFilter
    type AttrSetTypeConstraint TreeModelFilterVirtualRootPropertyInfo = (~) Gtk.TreePath.TreePath
    type AttrTransferTypeConstraint TreeModelFilterVirtualRootPropertyInfo = (~) Gtk.TreePath.TreePath
    type AttrTransferType TreeModelFilterVirtualRootPropertyInfo = Gtk.TreePath.TreePath
    type AttrGetType TreeModelFilterVirtualRootPropertyInfo = (Maybe Gtk.TreePath.TreePath)
    type AttrLabel TreeModelFilterVirtualRootPropertyInfo = "virtual-root"
    type AttrOrigin TreeModelFilterVirtualRootPropertyInfo = TreeModelFilter
    attrGet = getTreeModelFilterVirtualRoot
    attrSet = undefined
    attrTransfer _ v = do
        return v
    attrConstruct = constructTreeModelFilterVirtualRoot
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelFilter.virtualRoot"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelFilter.html#g:attr:virtualRoot"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TreeModelFilter
type instance O.AttributeList TreeModelFilter = TreeModelFilterAttributeList
type TreeModelFilterAttributeList = ('[ '("childModel", TreeModelFilterChildModelPropertyInfo), '("virtualRoot", TreeModelFilterVirtualRootPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
treeModelFilterChildModel :: AttrLabelProxy "childModel"
treeModelFilterChildModel = AttrLabelProxy

treeModelFilterVirtualRoot :: AttrLabelProxy "virtualRoot"
treeModelFilterVirtualRoot = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TreeModelFilter = TreeModelFilterSignalList
type TreeModelFilterSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo), '("rowChanged", Gtk.TreeModel.TreeModelRowChangedSignalInfo), '("rowDeleted", Gtk.TreeModel.TreeModelRowDeletedSignalInfo), '("rowHasChildToggled", Gtk.TreeModel.TreeModelRowHasChildToggledSignalInfo), '("rowInserted", Gtk.TreeModel.TreeModelRowInsertedSignalInfo)] :: [(Symbol, *)])

#endif

-- method TreeModelFilter::clear_cache
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModelFilter."
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

foreign import ccall "gtk_tree_model_filter_clear_cache" gtk_tree_model_filter_clear_cache :: 
    Ptr TreeModelFilter ->                  -- filter : TInterface (Name {namespace = "Gtk", name = "TreeModelFilter"})
    IO ()

-- | This function should almost never be called. It clears the /@filter@/
-- of any cached iterators that haven’t been reffed with
-- 'GI.Gtk.Interfaces.TreeModel.treeModelRefNode'. This might be useful if the child model
-- being filtered is static (and doesn’t change often) and there has been
-- a lot of unreffed access to nodes. As a side effect of this function,
-- all unreffed iters will be invalid.
-- 
-- /Since: 2.4/
treeModelFilterClearCache ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelFilter a) =>
    a
    -- ^ /@filter@/: A t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter'.
    -> m ()
treeModelFilterClearCache filter = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    gtk_tree_model_filter_clear_cache filter'
    touchManagedPtr filter
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeModelFilterClearCacheMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTreeModelFilter a) => O.OverloadedMethod TreeModelFilterClearCacheMethodInfo a signature where
    overloadedMethod = treeModelFilterClearCache

instance O.OverloadedMethodInfo TreeModelFilterClearCacheMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelFilter.treeModelFilterClearCache",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelFilter.html#v:treeModelFilterClearCache"
        })


#endif

-- method TreeModelFilter::convert_child_iter_to_iter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModelFilter."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filter_iter"
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
--                     Just "A valid #GtkTreeIter pointing to a row on the child model."
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

foreign import ccall "gtk_tree_model_filter_convert_child_iter_to_iter" gtk_tree_model_filter_convert_child_iter_to_iter :: 
    Ptr TreeModelFilter ->                  -- filter : TInterface (Name {namespace = "Gtk", name = "TreeModelFilter"})
    Ptr Gtk.TreeIter.TreeIter ->            -- filter_iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Ptr Gtk.TreeIter.TreeIter ->            -- child_iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO CInt

-- | Sets /@filterIter@/ to point to the row in /@filter@/ that corresponds to the
-- row pointed at by /@childIter@/.  If /@filterIter@/ was not set, 'P.False' is
-- returned.
-- 
-- /Since: 2.4/
treeModelFilterConvertChildIterToIter ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelFilter a) =>
    a
    -- ^ /@filter@/: A t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter'.
    -> Gtk.TreeIter.TreeIter
    -- ^ /@childIter@/: A valid t'GI.Gtk.Structs.TreeIter.TreeIter' pointing to a row on the child model.
    -> m ((Bool, Gtk.TreeIter.TreeIter))
    -- ^ __Returns:__ 'P.True', if /@filterIter@/ was set, i.e. if /@childIter@/ is a
    -- valid iterator pointing to a visible row in child model.
treeModelFilterConvertChildIterToIter filter childIter = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    filterIter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    childIter' <- unsafeManagedPtrGetPtr childIter
    result <- gtk_tree_model_filter_convert_child_iter_to_iter filter' filterIter childIter'
    let result' = (/= 0) result
    filterIter' <- (wrapBoxed Gtk.TreeIter.TreeIter) filterIter
    touchManagedPtr filter
    touchManagedPtr childIter
    return (result', filterIter')

#if defined(ENABLE_OVERLOADING)
data TreeModelFilterConvertChildIterToIterMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m ((Bool, Gtk.TreeIter.TreeIter))), MonadIO m, IsTreeModelFilter a) => O.OverloadedMethod TreeModelFilterConvertChildIterToIterMethodInfo a signature where
    overloadedMethod = treeModelFilterConvertChildIterToIter

instance O.OverloadedMethodInfo TreeModelFilterConvertChildIterToIterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelFilter.treeModelFilterConvertChildIterToIter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelFilter.html#v:treeModelFilterConvertChildIterToIter"
        })


#endif

-- method TreeModelFilter::convert_child_path_to_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModelFilter."
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
--                 { rawDocText = Just "A #GtkTreePath to convert."
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

foreign import ccall "gtk_tree_model_filter_convert_child_path_to_path" gtk_tree_model_filter_convert_child_path_to_path :: 
    Ptr TreeModelFilter ->                  -- filter : TInterface (Name {namespace = "Gtk", name = "TreeModelFilter"})
    Ptr Gtk.TreePath.TreePath ->            -- child_path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO (Ptr Gtk.TreePath.TreePath)

-- | Converts /@childPath@/ to a path relative to /@filter@/. That is, /@childPath@/
-- points to a path in the child model. The rerturned path will point to the
-- same row in the filtered model. If /@childPath@/ isn’t a valid path on the
-- child model or points to a row which is not visible in /@filter@/, then 'P.Nothing'
-- is returned.
-- 
-- /Since: 2.4/
treeModelFilterConvertChildPathToPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelFilter a) =>
    a
    -- ^ /@filter@/: A t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter'.
    -> Gtk.TreePath.TreePath
    -- ^ /@childPath@/: A t'GI.Gtk.Structs.TreePath.TreePath' to convert.
    -> m (Maybe Gtk.TreePath.TreePath)
    -- ^ __Returns:__ A newly allocated t'GI.Gtk.Structs.TreePath.TreePath', or 'P.Nothing'.
treeModelFilterConvertChildPathToPath filter childPath = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    childPath' <- unsafeManagedPtrGetPtr childPath
    result <- gtk_tree_model_filter_convert_child_path_to_path filter' childPath'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapBoxed Gtk.TreePath.TreePath) result'
        return result''
    touchManagedPtr filter
    touchManagedPtr childPath
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data TreeModelFilterConvertChildPathToPathMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> m (Maybe Gtk.TreePath.TreePath)), MonadIO m, IsTreeModelFilter a) => O.OverloadedMethod TreeModelFilterConvertChildPathToPathMethodInfo a signature where
    overloadedMethod = treeModelFilterConvertChildPathToPath

instance O.OverloadedMethodInfo TreeModelFilterConvertChildPathToPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelFilter.treeModelFilterConvertChildPathToPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelFilter.html#v:treeModelFilterConvertChildPathToPath"
        })


#endif

-- method TreeModelFilter::convert_iter_to_child_iter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModelFilter."
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
--           { argCName = "filter_iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "A valid #GtkTreeIter pointing to a row on @filter."
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

foreign import ccall "gtk_tree_model_filter_convert_iter_to_child_iter" gtk_tree_model_filter_convert_iter_to_child_iter :: 
    Ptr TreeModelFilter ->                  -- filter : TInterface (Name {namespace = "Gtk", name = "TreeModelFilter"})
    Ptr Gtk.TreeIter.TreeIter ->            -- child_iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    Ptr Gtk.TreeIter.TreeIter ->            -- filter_iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Sets /@childIter@/ to point to the row pointed to by /@filterIter@/.
-- 
-- /Since: 2.4/
treeModelFilterConvertIterToChildIter ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelFilter a) =>
    a
    -- ^ /@filter@/: A t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter'.
    -> Gtk.TreeIter.TreeIter
    -- ^ /@filterIter@/: A valid t'GI.Gtk.Structs.TreeIter.TreeIter' pointing to a row on /@filter@/.
    -> m (Gtk.TreeIter.TreeIter)
treeModelFilterConvertIterToChildIter filter filterIter = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    childIter <- SP.callocBoxedBytes 32 :: IO (Ptr Gtk.TreeIter.TreeIter)
    filterIter' <- unsafeManagedPtrGetPtr filterIter
    gtk_tree_model_filter_convert_iter_to_child_iter filter' childIter filterIter'
    childIter' <- (wrapBoxed Gtk.TreeIter.TreeIter) childIter
    touchManagedPtr filter
    touchManagedPtr filterIter
    return childIter'

#if defined(ENABLE_OVERLOADING)
data TreeModelFilterConvertIterToChildIterMethodInfo
instance (signature ~ (Gtk.TreeIter.TreeIter -> m (Gtk.TreeIter.TreeIter)), MonadIO m, IsTreeModelFilter a) => O.OverloadedMethod TreeModelFilterConvertIterToChildIterMethodInfo a signature where
    overloadedMethod = treeModelFilterConvertIterToChildIter

instance O.OverloadedMethodInfo TreeModelFilterConvertIterToChildIterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelFilter.treeModelFilterConvertIterToChildIter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelFilter.html#v:treeModelFilterConvertIterToChildIter"
        })


#endif

-- method TreeModelFilter::convert_path_to_child_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModelFilter."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "filter_path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreePath to convert."
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

foreign import ccall "gtk_tree_model_filter_convert_path_to_child_path" gtk_tree_model_filter_convert_path_to_child_path :: 
    Ptr TreeModelFilter ->                  -- filter : TInterface (Name {namespace = "Gtk", name = "TreeModelFilter"})
    Ptr Gtk.TreePath.TreePath ->            -- filter_path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO (Ptr Gtk.TreePath.TreePath)

-- | Converts /@filterPath@/ to a path on the child model of /@filter@/. That is,
-- /@filterPath@/ points to a location in /@filter@/. The returned path will
-- point to the same location in the model not being filtered. If /@filterPath@/
-- does not point to a location in the child model, 'P.Nothing' is returned.
-- 
-- /Since: 2.4/
treeModelFilterConvertPathToChildPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelFilter a) =>
    a
    -- ^ /@filter@/: A t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter'.
    -> Gtk.TreePath.TreePath
    -- ^ /@filterPath@/: A t'GI.Gtk.Structs.TreePath.TreePath' to convert.
    -> m (Maybe Gtk.TreePath.TreePath)
    -- ^ __Returns:__ A newly allocated t'GI.Gtk.Structs.TreePath.TreePath', or 'P.Nothing'.
treeModelFilterConvertPathToChildPath filter filterPath = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    filterPath' <- unsafeManagedPtrGetPtr filterPath
    result <- gtk_tree_model_filter_convert_path_to_child_path filter' filterPath'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapBoxed Gtk.TreePath.TreePath) result'
        return result''
    touchManagedPtr filter
    touchManagedPtr filterPath
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data TreeModelFilterConvertPathToChildPathMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> m (Maybe Gtk.TreePath.TreePath)), MonadIO m, IsTreeModelFilter a) => O.OverloadedMethod TreeModelFilterConvertPathToChildPathMethodInfo a signature where
    overloadedMethod = treeModelFilterConvertPathToChildPath

instance O.OverloadedMethodInfo TreeModelFilterConvertPathToChildPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelFilter.treeModelFilterConvertPathToChildPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelFilter.html#v:treeModelFilterConvertPathToChildPath"
        })


#endif

-- method TreeModelFilter::get_model
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModelFilter."
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

foreign import ccall "gtk_tree_model_filter_get_model" gtk_tree_model_filter_get_model :: 
    Ptr TreeModelFilter ->                  -- filter : TInterface (Name {namespace = "Gtk", name = "TreeModelFilter"})
    IO (Ptr Gtk.TreeModel.TreeModel)

-- | Returns a pointer to the child model of /@filter@/.
-- 
-- /Since: 2.4/
treeModelFilterGetModel ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelFilter a) =>
    a
    -- ^ /@filter@/: A t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter'.
    -> m Gtk.TreeModel.TreeModel
    -- ^ __Returns:__ A pointer to a t'GI.Gtk.Interfaces.TreeModel.TreeModel'.
treeModelFilterGetModel filter = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    result <- gtk_tree_model_filter_get_model filter'
    checkUnexpectedReturnNULL "treeModelFilterGetModel" result
    result' <- (newObject Gtk.TreeModel.TreeModel) result
    touchManagedPtr filter
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeModelFilterGetModelMethodInfo
instance (signature ~ (m Gtk.TreeModel.TreeModel), MonadIO m, IsTreeModelFilter a) => O.OverloadedMethod TreeModelFilterGetModelMethodInfo a signature where
    overloadedMethod = treeModelFilterGetModel

instance O.OverloadedMethodInfo TreeModelFilterGetModelMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelFilter.treeModelFilterGetModel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelFilter.html#v:treeModelFilterGetModel"
        })


#endif

-- method TreeModelFilter::refilter
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModelFilter."
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

foreign import ccall "gtk_tree_model_filter_refilter" gtk_tree_model_filter_refilter :: 
    Ptr TreeModelFilter ->                  -- filter : TInterface (Name {namespace = "Gtk", name = "TreeModelFilter"})
    IO ()

-- | Emits [row_changed](#g:signal:row_changed) for each row in the child model, which causes
-- the filter to re-evaluate whether a row is visible or not.
-- 
-- /Since: 2.4/
treeModelFilterRefilter ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelFilter a) =>
    a
    -- ^ /@filter@/: A t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter'.
    -> m ()
treeModelFilterRefilter filter = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    gtk_tree_model_filter_refilter filter'
    touchManagedPtr filter
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeModelFilterRefilterMethodInfo
instance (signature ~ (m ()), MonadIO m, IsTreeModelFilter a) => O.OverloadedMethod TreeModelFilterRefilterMethodInfo a signature where
    overloadedMethod = treeModelFilterRefilter

instance O.OverloadedMethodInfo TreeModelFilterRefilterMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelFilter.treeModelFilterRefilter",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelFilter.html#v:treeModelFilterRefilter"
        })


#endif

-- method TreeModelFilter::set_visible_column
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModelFilter"
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
--                     Just
--                       "A #gint which is the column containing the visible information"
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

foreign import ccall "gtk_tree_model_filter_set_visible_column" gtk_tree_model_filter_set_visible_column :: 
    Ptr TreeModelFilter ->                  -- filter : TInterface (Name {namespace = "Gtk", name = "TreeModelFilter"})
    Int32 ->                                -- column : TBasicType TInt
    IO ()

-- | Sets /@column@/ of the child_model to be the column where /@filter@/ should
-- look for visibility information. /@columns@/ should be a column of type
-- @/G_TYPE_BOOLEAN/@, where 'P.True' means that a row is visible, and 'P.False'
-- if not.
-- 
-- Note that 'GI.Gtk.Objects.TreeModelFilter.treeModelFilterSetVisibleFunc' or
-- 'GI.Gtk.Objects.TreeModelFilter.treeModelFilterSetVisibleColumn' can only be called
-- once for a given filter model.
-- 
-- /Since: 2.4/
treeModelFilterSetVisibleColumn ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelFilter a) =>
    a
    -- ^ /@filter@/: A t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter'
    -> Int32
    -- ^ /@column@/: A @/gint/@ which is the column containing the visible information
    -> m ()
treeModelFilterSetVisibleColumn filter column = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    gtk_tree_model_filter_set_visible_column filter' column
    touchManagedPtr filter
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeModelFilterSetVisibleColumnMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m, IsTreeModelFilter a) => O.OverloadedMethod TreeModelFilterSetVisibleColumnMethodInfo a signature where
    overloadedMethod = treeModelFilterSetVisibleColumn

instance O.OverloadedMethodInfo TreeModelFilterSetVisibleColumnMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelFilter.treeModelFilterSetVisibleColumn",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelFilter.html#v:treeModelFilterSetVisibleColumn"
        })


#endif

-- method TreeModelFilter::set_visible_func
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "filter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeModelFilter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreeModelFilter"
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
--                 Name { namespace = "Gtk" , name = "TreeModelFilterVisibleFunc" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "A #GtkTreeModelFilterVisibleFunc, the visible function"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeNotified
--           , argClosure = 2
--           , argDestroy = 3
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "User data to pass to the visible function, or %NULL"
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
--                 { rawDocText = Just "Destroy notifier of @data, or %NULL"
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

foreign import ccall "gtk_tree_model_filter_set_visible_func" gtk_tree_model_filter_set_visible_func :: 
    Ptr TreeModelFilter ->                  -- filter : TInterface (Name {namespace = "Gtk", name = "TreeModelFilter"})
    FunPtr Gtk.Callbacks.C_TreeModelFilterVisibleFunc -> -- func : TInterface (Name {namespace = "Gtk", name = "TreeModelFilterVisibleFunc"})
    Ptr () ->                               -- data : TBasicType TPtr
    FunPtr GLib.Callbacks.C_DestroyNotify -> -- destroy : TInterface (Name {namespace = "GLib", name = "DestroyNotify"})
    IO ()

-- | Sets the visible function used when filtering the /@filter@/ to be /@func@/.
-- The function should return 'P.True' if the given row should be visible and
-- 'P.False' otherwise.
-- 
-- If the condition calculated by the function changes over time (e.g.
-- because it depends on some global parameters), you must call
-- 'GI.Gtk.Objects.TreeModelFilter.treeModelFilterRefilter' to keep the visibility information
-- of the model up-to-date.
-- 
-- Note that /@func@/ is called whenever a row is inserted, when it may still
-- be empty. The visible function should therefore take special care of empty
-- rows, like in the example below.
-- 
-- 
-- === /C code/
-- >
-- >static gboolean
-- >visible_func (GtkTreeModel *model,
-- >              GtkTreeIter  *iter,
-- >              gpointer      data)
-- >{
-- >  // Visible if row is non-empty and first column is “HI”
-- >  gchar *str;
-- >  gboolean visible = FALSE;
-- >
-- >  gtk_tree_model_get (model, iter, 0, &str, -1);
-- >  if (str && strcmp (str, "HI") == 0)
-- >    visible = TRUE;
-- >  g_free (str);
-- >
-- >  return visible;
-- >}
-- 
-- 
-- Note that 'GI.Gtk.Objects.TreeModelFilter.treeModelFilterSetVisibleFunc' or
-- 'GI.Gtk.Objects.TreeModelFilter.treeModelFilterSetVisibleColumn' can only be called
-- once for a given filter model.
-- 
-- /Since: 2.4/
treeModelFilterSetVisibleFunc ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeModelFilter a) =>
    a
    -- ^ /@filter@/: A t'GI.Gtk.Objects.TreeModelFilter.TreeModelFilter'
    -> Gtk.Callbacks.TreeModelFilterVisibleFunc
    -- ^ /@func@/: A t'GI.Gtk.Callbacks.TreeModelFilterVisibleFunc', the visible function
    -> m ()
treeModelFilterSetVisibleFunc filter func = liftIO $ do
    filter' <- unsafeManagedPtrCastPtr filter
    func' <- Gtk.Callbacks.mk_TreeModelFilterVisibleFunc (Gtk.Callbacks.wrap_TreeModelFilterVisibleFunc Nothing (Gtk.Callbacks.drop_closures_TreeModelFilterVisibleFunc func))
    let data_ = castFunPtrToPtr func'
    let destroy = SP.safeFreeFunPtrPtr
    gtk_tree_model_filter_set_visible_func filter' func' data_ destroy
    touchManagedPtr filter
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeModelFilterSetVisibleFuncMethodInfo
instance (signature ~ (Gtk.Callbacks.TreeModelFilterVisibleFunc -> m ()), MonadIO m, IsTreeModelFilter a) => O.OverloadedMethod TreeModelFilterSetVisibleFuncMethodInfo a signature where
    overloadedMethod = treeModelFilterSetVisibleFunc

instance O.OverloadedMethodInfo TreeModelFilterSetVisibleFuncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.TreeModelFilter.treeModelFilterSetVisibleFunc",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-TreeModelFilter.html#v:treeModelFilterSetVisibleFunc"
        })


#endif


