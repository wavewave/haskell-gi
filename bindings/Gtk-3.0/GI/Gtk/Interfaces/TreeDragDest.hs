{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Interfaces.TreeDragDest
    ( 

-- * Exported types
    TreeDragDest(..)                        ,
    IsTreeDragDest                          ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [dragDataReceived]("GI.Gtk.Interfaces.TreeDragDest#g:method:dragDataReceived"), [rowDropPossible]("GI.Gtk.Interfaces.TreeDragDest#g:method:rowDropPossible").
-- 
-- ==== Getters
-- /None/.
-- 
-- ==== Setters
-- /None/.

#if defined(ENABLE_OVERLOADING)
    ResolveTreeDragDestMethod               ,
#endif

-- ** dragDataReceived #method:dragDataReceived#

#if defined(ENABLE_OVERLOADING)
    TreeDragDestDragDataReceivedMethodInfo  ,
#endif
    treeDragDestDragDataReceived            ,


-- ** rowDropPossible #method:rowDropPossible#

#if defined(ENABLE_OVERLOADING)
    TreeDragDestRowDropPossibleMethodInfo   ,
#endif
    treeDragDestRowDropPossible             ,




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

-- interface TreeDragDest 
-- | Memory-managed wrapper type.
newtype TreeDragDest = TreeDragDest (SP.ManagedPtr TreeDragDest)
    deriving (Eq)

instance SP.ManagedPtrNewtype TreeDragDest where
    toManagedPtr (TreeDragDest p) = p

-- | Type class for types which implement `TreeDragDest`.
class (ManagedPtrNewtype o, O.IsDescendantOf TreeDragDest o) => IsTreeDragDest o
instance (ManagedPtrNewtype o, O.IsDescendantOf TreeDragDest o) => IsTreeDragDest o
-- XXX Wrapping a foreign struct/union with no known destructor or size, leak?
instance BoxedPtr TreeDragDest where
    boxedPtrCopy = return
    boxedPtrFree = \_x -> return ()


#if defined(ENABLE_OVERLOADING)
type family ResolveTreeDragDestMethod (t :: Symbol) (o :: *) :: * where
    ResolveTreeDragDestMethod "dragDataReceived" o = TreeDragDestDragDataReceivedMethodInfo
    ResolveTreeDragDestMethod "rowDropPossible" o = TreeDragDestRowDropPossibleMethodInfo
    ResolveTreeDragDestMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTreeDragDestMethod t TreeDragDest, O.OverloadedMethod info TreeDragDest p) => OL.IsLabel t (TreeDragDest -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTreeDragDestMethod t TreeDragDest, O.OverloadedMethod info TreeDragDest p, R.HasField t TreeDragDest p) => R.HasField t TreeDragDest p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTreeDragDestMethod t TreeDragDest, O.OverloadedMethodInfo info TreeDragDest) => OL.IsLabel t (O.MethodProxy info TreeDragDest) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- method TreeDragDest::drag_data_received
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "drag_dest"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeDragDest" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeDragDest" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "dest"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "row to drop in front of"
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
--                 { rawDocText = Just "data to drop" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_drag_dest_drag_data_received" gtk_tree_drag_dest_drag_data_received :: 
    Ptr TreeDragDest ->                     -- drag_dest : TInterface (Name {namespace = "Gtk", name = "TreeDragDest"})
    Ptr Gtk.TreePath.TreePath ->            -- dest : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.SelectionData.SelectionData ->  -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO CInt

-- | Asks the t'GI.Gtk.Interfaces.TreeDragDest.TreeDragDest' to insert a row before the path /@dest@/,
-- deriving the contents of the row from /@selectionData@/. If /@dest@/ is
-- outside the tree so that inserting before it is impossible, 'P.False'
-- will be returned. Also, 'P.False' may be returned if the new row is
-- not created for some model-specific reason.  Should robustly handle
-- a /@dest@/ no longer found in the model!
treeDragDestDragDataReceived ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeDragDest a) =>
    a
    -- ^ /@dragDest@/: a t'GI.Gtk.Interfaces.TreeDragDest.TreeDragDest'
    -> Gtk.TreePath.TreePath
    -- ^ /@dest@/: row to drop in front of
    -> Gtk.SelectionData.SelectionData
    -- ^ /@selectionData@/: data to drop
    -> m Bool
    -- ^ __Returns:__ whether a new row was created before position /@dest@/
treeDragDestDragDataReceived dragDest dest selectionData = liftIO $ do
    dragDest' <- unsafeManagedPtrCastPtr dragDest
    dest' <- unsafeManagedPtrGetPtr dest
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    result <- gtk_tree_drag_dest_drag_data_received dragDest' dest' selectionData'
    let result' = (/= 0) result
    touchManagedPtr dragDest
    touchManagedPtr dest
    touchManagedPtr selectionData
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeDragDestDragDataReceivedMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> Gtk.SelectionData.SelectionData -> m Bool), MonadIO m, IsTreeDragDest a) => O.OverloadedMethod TreeDragDestDragDataReceivedMethodInfo a signature where
    overloadedMethod = treeDragDestDragDataReceived

instance O.OverloadedMethodInfo TreeDragDestDragDataReceivedMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeDragDest.treeDragDestDragDataReceived",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeDragDest.html#v:treeDragDestDragDataReceived"
        })


#endif

-- method TreeDragDest::row_drop_possible
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "drag_dest"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeDragDest" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeDragDest" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "dest_path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "destination row" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the data being dragged"
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

foreign import ccall "gtk_tree_drag_dest_row_drop_possible" gtk_tree_drag_dest_row_drop_possible :: 
    Ptr TreeDragDest ->                     -- drag_dest : TInterface (Name {namespace = "Gtk", name = "TreeDragDest"})
    Ptr Gtk.TreePath.TreePath ->            -- dest_path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Gtk.SelectionData.SelectionData ->  -- selection_data : TInterface (Name {namespace = "Gtk", name = "SelectionData"})
    IO CInt

-- | Determines whether a drop is possible before the given /@destPath@/,
-- at the same depth as /@destPath@/. i.e., can we drop the data in
-- /@selectionData@/ at that location. /@destPath@/ does not have to
-- exist; the return value will almost certainly be 'P.False' if the
-- parent of /@destPath@/ doesn’t exist, though.
treeDragDestRowDropPossible ::
    (B.CallStack.HasCallStack, MonadIO m, IsTreeDragDest a) =>
    a
    -- ^ /@dragDest@/: a t'GI.Gtk.Interfaces.TreeDragDest.TreeDragDest'
    -> Gtk.TreePath.TreePath
    -- ^ /@destPath@/: destination row
    -> Gtk.SelectionData.SelectionData
    -- ^ /@selectionData@/: the data being dragged
    -> m Bool
    -- ^ __Returns:__ 'P.True' if a drop is possible before /@destPath@/
treeDragDestRowDropPossible dragDest destPath selectionData = liftIO $ do
    dragDest' <- unsafeManagedPtrCastPtr dragDest
    destPath' <- unsafeManagedPtrGetPtr destPath
    selectionData' <- unsafeManagedPtrGetPtr selectionData
    result <- gtk_tree_drag_dest_row_drop_possible dragDest' destPath' selectionData'
    let result' = (/= 0) result
    touchManagedPtr dragDest
    touchManagedPtr destPath
    touchManagedPtr selectionData
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeDragDestRowDropPossibleMethodInfo
instance (signature ~ (Gtk.TreePath.TreePath -> Gtk.SelectionData.SelectionData -> m Bool), MonadIO m, IsTreeDragDest a) => O.OverloadedMethod TreeDragDestRowDropPossibleMethodInfo a signature where
    overloadedMethod = treeDragDestRowDropPossible

instance O.OverloadedMethodInfo TreeDragDestRowDropPossibleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.TreeDragDest.treeDragDestRowDropPossible",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-TreeDragDest.html#v:treeDragDestRowDropPossible"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList TreeDragDest = TreeDragDestSignalList
type TreeDragDestSignalList = ('[ ] :: [(Symbol, *)])

#endif


