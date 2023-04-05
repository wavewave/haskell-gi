{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Interfaces.TreeDragSource
    ( 

-- * Exported types
    TreeDragSource(..)                      ,
    IsTreeDragSource                        ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [dragDataDelete]("GI.Gtk.Interfaces.TreeDragSource#g:method:dragDataDelete"), [dragDataGet]("GI.Gtk.Interfaces.TreeDragSource#g:method:dragDataGet"), [rowDraggable]("GI.Gtk.Interfaces.TreeDragSource#g:method:rowDraggable").
-- 
-- ==== Getters
-- /None/.
-- 
-- ==== Setters
-- /None/.

#if defined(ENABLE_OVERLOADING)
    ResolveTreeDragSourceMethod             ,
#endif

-- ** dragDataDelete #method:dragDataDelete#

#if defined(ENABLE_OVERLOADING)
    TreeDragSourceDragDataDeleteMethodInfo  ,
#endif
    treeDragSourceDragDataDelete            ,


-- ** dragDataGet #method:dragDataGet#

#if defined(ENABLE_OVERLOADING)
    TreeDragSourceDragDataGetMethodInfo     ,
#endif
    treeDragSourceDragDataGet               ,


-- ** rowDraggable #method:rowDraggable#

#if defined(ENABLE_OVERLOADING)
    TreeDragSourceRowDraggableMethodInfo    ,
#endif
    treeDragSourceRowDraggable              ,




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

import {-# SOURCE #-} qualified GI.Gtk.Structs.SelectionData as Gtk.SelectionData
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreePath as Gtk.TreePath

-- interface TreeDragSource 
-- | Memory-managed wrapper type.
newtype TreeDragSource = TreeDragSource (SP.ManagedPtr TreeDragSource)
    deriving (Eq)

instance SP.ManagedPtrNewtype TreeDragSource where
    toManagedPtr (TreeDragSource p) = p

-- | Type class for types which implement `TreeDragSource`.
class (ManagedPtrNewtype o, O.IsDescendantOf TreeDragSource o) => IsTreeDragSource o
instance (ManagedPtrNewtype o, O.IsDescendantOf TreeDragSource o) => IsTreeDragSource o
-- XXX Wrapping a foreign struct/union with no known destructor or size, leak?
instance BoxedPtr TreeDragSource where
    boxedPtrCopy = return
    boxedPtrFree = \_x -> return ()


#if defined(ENABLE_OVERLOADING)
type family ResolveTreeDragSourceMethod (t :: Symbol) (o :: *) :: * where
    ResolveTreeDragSourceMethod "dragDataDelete" o = TreeDragSourceDragDataDeleteMethodInfo
    ResolveTreeDragSourceMethod "dragDataGet" o = TreeDragSourceDragDataGetMethodInfo
    ResolveTreeDragSourceMethod "rowDraggable" o = TreeDragSourceRowDraggableMethodInfo
    ResolveTreeDragSourceMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTreeDragSourceMethod t TreeDragSource, O.OverloadedMethod info TreeDragSource p) => OL.IsLabel t (TreeDragSource -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTreeDragSourceMethod t TreeDragSource, O.OverloadedMethod info TreeDragSource p, R.HasField t TreeDragSource p) => R.HasField t TreeDragSource p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTreeDragSourceMethod t TreeDragSource, O.OverloadedMethodInfo info TreeDragSource) => OL.IsLabel t (O.MethodProxy info TreeDragSource) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- method TreeDragSource::drag_data_delete
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "drag_source"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeDragSource" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeDragSource"
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
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "row that was being dragged"
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

foreign import ccall "gtk_tree_drag_source_drag_data_delete" gtk_tree_drag_source_drag_data_delete :: 
    Ptr TreeDragSource ->                   -- drag_source : TInterface (Name {namespace = "Gtk", name = "TreeDragSource"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO CInt

-- | Asks the t'GI.Gtk.Interfaces.TreeDragSource.TreeDragSource' to delete the row at /@path@/, because
-- it was moved somewhere else via drag-and-drop. Returns 'P.False'
-- if the deletion fails because /@path@/ no longer exists, or for
-- some model-specific reason. Should robustly handle a /@path@/ no
-- longer found in the model!
treeDragSourceDragDataDelete ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeDragSource a) =>
    a
    -- ^ /@dragSource@/: a t'GI.Gtk.Interfaces.TreeDragSource.TreeDragSource'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: row that was being dragged
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the row was successfully deleted
treeDragSourceDragDataDelete dragSource path = liftIO $ do
    dragSource' <- unsafeManagedPtrCastPtr dragSource
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_tree_drag_source_drag_data_delete dragSource' path'
    let result' = (/= 0) result
    touchManagedPtr dragSource
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeDragSourceDragDataDeleteMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> m Bool), MonadIO m, IsTreeDragSource a) => O.OverloadedMethod TreeDragSourceDragDataDeleteMethodInfo a signature where
    overloadedMethod = treeDragSourceDragDataDelete

instance O.OverloadedMethodInfo TreeDragSourceDragDataDeleteMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeDragSource.treeDragSourceDragDataDelete",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeDragSource.html#v:treeDragSourceDragDataDelete"
        })


#endif

-- method TreeDragSource::drag_data_get
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "drag_source"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeDragSource" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeDragSource"
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
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "row that was dragged"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "selection_data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "SelectionData" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GtkSelectionData to fill with data\n                 from the dragged row"
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

foreign import ccall "gtk_tree_drag_source_drag_data_get" gtk_tree_drag_source_drag_data_get :: 
    Ptr TreeDragSource ->                   -- drag_source : TInterface (Name {namespace = "Gtk", name = "TreeDragSource"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.SelectionData.SelectionData ->  -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO CInt

-- | Asks the t'GI.Gtk.Interfaces.TreeDragSource.TreeDragSource' to fill in /@selectionData@/ with a
-- representation of the row at /@path@/. /@selectionData@/->target gives
-- the required type of the data.  Should robustly handle a /@path@/ no
-- longer found in the model!
treeDragSourceDragDataGet ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeDragSource a) =>
    a
    -- ^ /@dragSource@/: a t'GI.Gtk.Interfaces.TreeDragSource.TreeDragSource'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: row that was dragged
    -> Gtk.SelectionData.SelectionData
    -- ^ /@selectionData@/: a t'GI.Gtk.Structs.SelectionData.SelectionData' to fill with data
    --                  from the dragged row
    -> m Bool
    -- ^ __Returns:__ 'P.True' if data of the required type was provided
treeDragSourceDragDataGet dragSource path selectionData = liftIO $ do
    dragSource' <- unsafeManagedPtrCastPtr dragSource
    path' <- unsafeManagedPtrGetPtr path
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    result <- gtk_tree_drag_source_drag_data_get dragSource' path' selectionData'
    let result' = (/= 0) result
    touchManagedPtr dragSource
    touchManagedPtr path
    touchManagedPtr selectionData
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeDragSourceDragDataGetMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> Gtk.SelectionData.SelectionData -> m Bool), MonadIO m, IsTreeDragSource a) => O.OverloadedMethod TreeDragSourceDragDataGetMethodInfo a signature where
    overloadedMethod = treeDragSourceDragDataGet

instance O.OverloadedMethodInfo TreeDragSourceDragDataGetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeDragSource.treeDragSourceDragDataGet",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeDragSource.html#v:treeDragSourceDragDataGet"
        })


#endif

-- method TreeDragSource::row_draggable
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "drag_source"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeDragSource" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeDragSource"
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
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "row on which user is initiating a drag"
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

foreign import ccall "gtk_tree_drag_source_row_draggable" gtk_tree_drag_source_row_draggable :: 
    Ptr TreeDragSource ->                   -- drag_source : TInterface (Name {namespace = "Gtk", name = "TreeDragSource"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO CInt

-- | Asks the t'GI.Gtk.Interfaces.TreeDragSource.TreeDragSource' whether a particular row can be used as
-- the source of a DND operation. If the source doesn’t implement
-- this interface, the row is assumed draggable.
treeDragSourceRowDraggable ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeDragSource a) =>
    a
    -- ^ /@dragSource@/: a t'GI.Gtk.Interfaces.TreeDragSource.TreeDragSource'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: row on which user is initiating a drag
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the row can be dragged
treeDragSourceRowDraggable dragSource path = liftIO $ do
    dragSource' <- unsafeManagedPtrCastPtr dragSource
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_tree_drag_source_row_draggable dragSource' path'
    let result' = (/= 0) result
    touchManagedPtr dragSource
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeDragSourceRowDraggableMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> m Bool), MonadIO m, IsTreeDragSource a) => O.OverloadedMethod TreeDragSourceRowDraggableMethodInfo a signature where
    overloadedMethod = treeDragSourceRowDraggable

instance O.OverloadedMethodInfo TreeDragSourceRowDraggableMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeDragSource.treeDragSourceRowDraggable",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeDragSource.html#v:treeDragSourceRowDraggable"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TreeDragSource = TreeDragSourceSignalList
type TreeDragSourceSignalList = ('[ ] :: [(Symbol, *)])

#endif


