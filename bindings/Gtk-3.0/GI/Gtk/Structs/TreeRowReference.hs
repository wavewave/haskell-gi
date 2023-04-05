{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A GtkTreeRowReference tracks model changes so that it always refers to the
-- same row (a t'GI.Gtk.Structs.TreePath.TreePath' refers to a position, not a fixed row). Create a
-- new GtkTreeRowReference with 'GI.Gtk.Structs.TreeRowReference.treeRowReferenceNew'.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.TreeRowReference
    ( 

-- * Exported types
    TreeRowReference(..)                    ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [copy]("GI.Gtk.Structs.TreeRowReference#g:method:copy"), [free]("GI.Gtk.Structs.TreeRowReference#g:method:free"), [valid]("GI.Gtk.Structs.TreeRowReference#g:method:valid").
-- 
-- ==== Getters
-- [getModel]("GI.Gtk.Structs.TreeRowReference#g:method:getModel"), [getPath]("GI.Gtk.Structs.TreeRowReference#g:method:getPath").
-- 
-- ==== Setters
-- /None/.

#if defined(ENABLE_OVERLOADING)
    ResolveTreeRowReferenceMethod           ,
#endif

-- ** copy #method:copy#

#if defined(ENABLE_OVERLOADING)
    TreeRowReferenceCopyMethodInfo          ,
#endif
    treeRowReferenceCopy                    ,


-- ** deleted #method:deleted#

    treeRowReferenceDeleted                 ,


-- ** free #method:free#

#if defined(ENABLE_OVERLOADING)
    TreeRowReferenceFreeMethodInfo          ,
#endif
    treeRowReferenceFree                    ,


-- ** getModel #method:getModel#

#if defined(ENABLE_OVERLOADING)
    TreeRowReferenceGetModelMethodInfo      ,
#endif
    treeRowReferenceGetModel                ,


-- ** getPath #method:getPath#

#if defined(ENABLE_OVERLOADING)
    TreeRowReferenceGetPathMethodInfo       ,
#endif
    treeRowReferenceGetPath                 ,


-- ** inserted #method:inserted#

    treeRowReferenceInserted                ,


-- ** new #method:new#

    treeRowReferenceNew                     ,


-- ** newProxy #method:newProxy#

    treeRowReferenceNewProxy                ,


-- ** valid #method:valid#

#if defined(ENABLE_OVERLOADING)
    TreeRowReferenceValidMethodInfo         ,
#endif
    treeRowReferenceValid                   ,




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
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.TreeModel as Gtk.TreeModel
import {-# SOURCE #-} qualified GI.Gtk.Structs.TreePath as Gtk.TreePath

-- | Memory-managed wrapper type.
newtype TreeRowReference = TreeRowReference (SP.ManagedPtr TreeRowReference)
    deriving (Eq)

instance SP.ManagedPtrNewtype TreeRowReference where
    toManagedPtr (TreeRowReference p) = p

foreign import ccall "gtk_tree_row_reference_get_type" c_gtk_tree_row_reference_get_type :: 
    IO GType

type instance O.ParentTypes TreeRowReference = '[]
instance O.HasParentTypes TreeRowReference

instance B.Types.TypedObject TreeRowReference where
    glibType = c_gtk_tree_row_reference_get_type

instance B.Types.GBoxed TreeRowReference

-- | Convert 'TreeRowReference' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TreeRowReference) where
    gvalueGType_ = c_gtk_tree_row_reference_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_boxed gv (FP.nullPtr :: FP.Ptr TreeRowReference)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_boxed gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_boxed gv :: IO (Ptr TreeRowReference)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newBoxed TreeRowReference ptr
        else return P.Nothing
        
    


#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TreeRowReference
type instance O.AttributeList TreeRowReference = TreeRowReferenceAttributeList
type TreeRowReferenceAttributeList = ('[ ] :: [(Symbol, *)])
#endif

-- method TreeRowReference::new
-- method type : Constructor
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
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a valid #GtkTreePath-struct to monitor"
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
--               (TInterface Name { namespace = "Gtk" , name = "TreeRowReference" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_row_reference_new" gtk_tree_row_reference_new :: 
    Ptr Gtk.TreeModel.TreeModel ->          -- model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO (Ptr TreeRowReference)

-- | Creates a row reference based on /@path@/.
-- 
-- This reference will keep pointing to the node pointed to
-- by /@path@/, so long as it exists. Any changes that occur on /@model@/ are
-- propagated, and the path is updated appropriately. If
-- /@path@/ isn’t a valid path in /@model@/, then 'P.Nothing' is returned.
treeRowReferenceNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.TreeModel.IsTreeModel a) =>
    a
    -- ^ /@model@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: a valid t'GI.Gtk.Structs.TreePath.TreePath'-struct to monitor
    -> m TreeRowReference
    -- ^ __Returns:__ a newly allocated t'GI.Gtk.Structs.TreeRowReference.TreeRowReference', or 'P.Nothing'
treeRowReferenceNew model path = liftIO $ do
    model' <- unsafeManagedPtrCastPtr model
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_tree_row_reference_new model' path'
    checkUnexpectedReturnNULL "treeRowReferenceNew" result
    result' <- (wrapBoxed TreeRowReference) result
    touchManagedPtr model
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TreeRowReference::new_proxy
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "proxy"
--           , argType =
--               TInterface Name { namespace = "GObject" , name = "Object" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a proxy #GObject" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a valid #GtkTreePath-struct to monitor"
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
--               (TInterface Name { namespace = "Gtk" , name = "TreeRowReference" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_row_reference_new_proxy" gtk_tree_row_reference_new_proxy :: 
    Ptr GObject.Object.Object ->            -- proxy : TInterface (Name {namespace = "GObject", name = "Object"})
    Ptr Gtk.TreeModel.TreeModel ->          -- model : TInterface (Name {namespace = "Gtk", name = "TreeModel"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO (Ptr TreeRowReference)

-- | You do not need to use this function.
-- 
-- Creates a row reference based on /@path@/.
-- 
-- This reference will keep pointing to the node pointed to
-- by /@path@/, so long as it exists. If /@path@/ isn’t a valid
-- path in /@model@/, then 'P.Nothing' is returned. However, unlike
-- references created with 'GI.Gtk.Structs.TreeRowReference.treeRowReferenceNew', it
-- does not listen to the model for changes. The creator of
-- the row reference must do this explicitly using
-- 'GI.Gtk.Functions.treeRowReferenceInserted', 'GI.Gtk.Functions.treeRowReferenceDeleted',
-- @/gtk_tree_row_reference_reordered()/@.
-- 
-- These functions must be called exactly once per proxy when the
-- corresponding signal on the model is emitted. This single call
-- updates all row references for that proxy. Since built-in GTK+
-- objects like t'GI.Gtk.Objects.TreeView.TreeView' already use this mechanism internally,
-- using them as the proxy object will produce unpredictable results.
-- Further more, passing the same object as /@model@/ and /@proxy@/
-- doesn’t work for reasons of internal implementation.
-- 
-- This type of row reference is primarily meant by structures that
-- need to carefully monitor exactly when a row reference updates
-- itself, and is not generally needed by most applications.
treeRowReferenceNewProxy ::
    (B.CallStack.HasCallStack, MonadIO m, GObject.Object.IsObject a, Gtk.TreeModel.IsTreeModel b) =>
    a
    -- ^ /@proxy@/: a proxy t'GI.GObject.Objects.Object.Object'
    -> b
    -- ^ /@model@/: a t'GI.Gtk.Interfaces.TreeModel.TreeModel'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: a valid t'GI.Gtk.Structs.TreePath.TreePath'-struct to monitor
    -> m TreeRowReference
    -- ^ __Returns:__ a newly allocated t'GI.Gtk.Structs.TreeRowReference.TreeRowReference', or 'P.Nothing'
treeRowReferenceNewProxy proxy model path = liftIO $ do
    proxy' <- unsafeManagedPtrCastPtr proxy
    model' <- unsafeManagedPtrCastPtr model
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_tree_row_reference_new_proxy proxy' model' path'
    checkUnexpectedReturnNULL "treeRowReferenceNewProxy" result
    result' <- (wrapBoxed TreeRowReference) result
    touchManagedPtr proxy
    touchManagedPtr model
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TreeRowReference::copy
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "reference"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeRowReference" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeRowReference"
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
--               (TInterface Name { namespace = "Gtk" , name = "TreeRowReference" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_row_reference_copy" gtk_tree_row_reference_copy :: 
    Ptr TreeRowReference ->                 -- reference : TInterface (Name {namespace = "Gtk", name = "TreeRowReference"})
    IO (Ptr TreeRowReference)

-- | Copies a t'GI.Gtk.Structs.TreeRowReference.TreeRowReference'.
-- 
-- /Since: 2.2/
treeRowReferenceCopy ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreeRowReference
    -- ^ /@reference@/: a t'GI.Gtk.Structs.TreeRowReference.TreeRowReference'
    -> m TreeRowReference
    -- ^ __Returns:__ a copy of /@reference@/
treeRowReferenceCopy reference = liftIO $ do
    reference' <- unsafeManagedPtrGetPtr reference
    result <- gtk_tree_row_reference_copy reference'
    checkUnexpectedReturnNULL "treeRowReferenceCopy" result
    result' <- (wrapBoxed TreeRowReference) result
    touchManagedPtr reference
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeRowReferenceCopyMethodInfo
instance (signature ~ (m TreeRowReference), MonadIO m) => O.OverloadedMethod TreeRowReferenceCopyMethodInfo TreeRowReference signature where
    overloadedMethod = treeRowReferenceCopy

instance O.OverloadedMethodInfo TreeRowReferenceCopyMethodInfo TreeRowReference where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreeRowReference.treeRowReferenceCopy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreeRowReference.html#v:treeRowReferenceCopy"
        })


#endif

-- method TreeRowReference::free
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "reference"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeRowReference" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeRowReference, or %NULL"
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

foreign import ccall "gtk_tree_row_reference_free" gtk_tree_row_reference_free :: 
    Ptr TreeRowReference ->                 -- reference : TInterface (Name {namespace = "Gtk", name = "TreeRowReference"})
    IO ()

-- | Free’s /@reference@/. /@reference@/ may be 'P.Nothing'
treeRowReferenceFree ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreeRowReference
    -- ^ /@reference@/: a t'GI.Gtk.Structs.TreeRowReference.TreeRowReference', or 'P.Nothing'
    -> m ()
treeRowReferenceFree reference = liftIO $ do
    reference' <- unsafeManagedPtrGetPtr reference
    gtk_tree_row_reference_free reference'
    touchManagedPtr reference
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeRowReferenceFreeMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod TreeRowReferenceFreeMethodInfo TreeRowReference signature where
    overloadedMethod = treeRowReferenceFree

instance O.OverloadedMethodInfo TreeRowReferenceFreeMethodInfo TreeRowReference where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreeRowReference.treeRowReferenceFree",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreeRowReference.html#v:treeRowReferenceFree"
        })


#endif

-- method TreeRowReference::get_model
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "reference"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeRowReference" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeRowReference"
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

foreign import ccall "gtk_tree_row_reference_get_model" gtk_tree_row_reference_get_model :: 
    Ptr TreeRowReference ->                 -- reference : TInterface (Name {namespace = "Gtk", name = "TreeRowReference"})
    IO (Ptr Gtk.TreeModel.TreeModel)

-- | Returns the model that the row reference is monitoring.
-- 
-- /Since: 2.8/
treeRowReferenceGetModel ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreeRowReference
    -- ^ /@reference@/: a t'GI.Gtk.Structs.TreeRowReference.TreeRowReference'
    -> m Gtk.TreeModel.TreeModel
    -- ^ __Returns:__ the model
treeRowReferenceGetModel reference = liftIO $ do
    reference' <- unsafeManagedPtrGetPtr reference
    result <- gtk_tree_row_reference_get_model reference'
    checkUnexpectedReturnNULL "treeRowReferenceGetModel" result
    result' <- (newObject Gtk.TreeModel.TreeModel) result
    touchManagedPtr reference
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeRowReferenceGetModelMethodInfo
instance (signature ~ (m Gtk.TreeModel.TreeModel), MonadIO m) => O.OverloadedMethod TreeRowReferenceGetModelMethodInfo TreeRowReference signature where
    overloadedMethod = treeRowReferenceGetModel

instance O.OverloadedMethodInfo TreeRowReferenceGetModelMethodInfo TreeRowReference where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreeRowReference.treeRowReferenceGetModel",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreeRowReference.html#v:treeRowReferenceGetModel"
        })


#endif

-- method TreeRowReference::get_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "reference"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeRowReference" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeRowReference"
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

foreign import ccall "gtk_tree_row_reference_get_path" gtk_tree_row_reference_get_path :: 
    Ptr TreeRowReference ->                 -- reference : TInterface (Name {namespace = "Gtk", name = "TreeRowReference"})
    IO (Ptr Gtk.TreePath.TreePath)

-- | Returns a path that the row reference currently points to,
-- or 'P.Nothing' if the path pointed to is no longer valid.
treeRowReferenceGetPath ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreeRowReference
    -- ^ /@reference@/: a t'GI.Gtk.Structs.TreeRowReference.TreeRowReference'
    -> m (Maybe Gtk.TreePath.TreePath)
    -- ^ __Returns:__ a current path, or 'P.Nothing'
treeRowReferenceGetPath reference = liftIO $ do
    reference' <- unsafeManagedPtrGetPtr reference
    result <- gtk_tree_row_reference_get_path reference'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapBoxed Gtk.TreePath.TreePath) result'
        return result''
    touchManagedPtr reference
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data TreeRowReferenceGetPathMethodInfo
instance (signature ~ (m (Maybe Gtk.TreePath.TreePath)), MonadIO m) => O.OverloadedMethod TreeRowReferenceGetPathMethodInfo TreeRowReference signature where
    overloadedMethod = treeRowReferenceGetPath

instance O.OverloadedMethodInfo TreeRowReferenceGetPathMethodInfo TreeRowReference where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreeRowReference.treeRowReferenceGetPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreeRowReference.html#v:treeRowReferenceGetPath"
        })


#endif

-- method TreeRowReference::valid
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "reference"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeRowReference" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreeRowReference, or %NULL"
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

foreign import ccall "gtk_tree_row_reference_valid" gtk_tree_row_reference_valid :: 
    Ptr TreeRowReference ->                 -- reference : TInterface (Name {namespace = "Gtk", name = "TreeRowReference"})
    IO CInt

-- | Returns 'P.True' if the /@reference@/ is non-'P.Nothing' and refers to
-- a current valid path.
treeRowReferenceValid ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreeRowReference
    -- ^ /@reference@/: a t'GI.Gtk.Structs.TreeRowReference.TreeRowReference', or 'P.Nothing'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@reference@/ points to a valid path
treeRowReferenceValid reference = liftIO $ do
    reference' <- unsafeManagedPtrGetPtr reference
    result <- gtk_tree_row_reference_valid reference'
    let result' = (/= 0) result
    touchManagedPtr reference
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeRowReferenceValidMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TreeRowReferenceValidMethodInfo TreeRowReference signature where
    overloadedMethod = treeRowReferenceValid

instance O.OverloadedMethodInfo TreeRowReferenceValidMethodInfo TreeRowReference where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreeRowReference.treeRowReferenceValid",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreeRowReference.html#v:treeRowReferenceValid"
        })


#endif

-- method TreeRowReference::deleted
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "proxy"
--           , argType =
--               TInterface Name { namespace = "GObject" , name = "Object" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GObject" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the path position that was deleted"
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

foreign import ccall "gtk_tree_row_reference_deleted" gtk_tree_row_reference_deleted :: 
    Ptr GObject.Object.Object ->            -- proxy : TInterface (Name {namespace = "GObject", name = "Object"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO ()

-- | Lets a set of row reference created by
-- 'GI.Gtk.Structs.TreeRowReference.treeRowReferenceNewProxy' know that the
-- model emitted the [TreeModel::rowDeleted]("GI.Gtk.Interfaces.TreeModel#g:signal:rowDeleted") signal.
treeRowReferenceDeleted ::
    (B.CallStack.HasCallStack, MonadIO m, GObject.Object.IsObject a) =>
    a
    -- ^ /@proxy@/: a t'GI.GObject.Objects.Object.Object'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: the path position that was deleted
    -> m ()
treeRowReferenceDeleted proxy path = liftIO $ do
    proxy' <- unsafeManagedPtrCastPtr proxy
    path' <- unsafeManagedPtrGetPtr path
    gtk_tree_row_reference_deleted proxy' path'
    touchManagedPtr proxy
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method TreeRowReference::inserted
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "proxy"
--           , argType =
--               TInterface Name { namespace = "GObject" , name = "Object" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GObject" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the row position that was inserted"
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

foreign import ccall "gtk_tree_row_reference_inserted" gtk_tree_row_reference_inserted :: 
    Ptr GObject.Object.Object ->            -- proxy : TInterface (Name {namespace = "GObject", name = "Object"})
    Ptr Gtk.TreePath.TreePath ->            -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO ()

-- | Lets a set of row reference created by
-- 'GI.Gtk.Structs.TreeRowReference.treeRowReferenceNewProxy' know that the
-- model emitted the [TreeModel::rowInserted]("GI.Gtk.Interfaces.TreeModel#g:signal:rowInserted") signal.
treeRowReferenceInserted ::
    (B.CallStack.HasCallStack, MonadIO m, GObject.Object.IsObject a) =>
    a
    -- ^ /@proxy@/: a t'GI.GObject.Objects.Object.Object'
    -> Gtk.TreePath.TreePath
    -- ^ /@path@/: the row position that was inserted
    -> m ()
treeRowReferenceInserted proxy path = liftIO $ do
    proxy' <- unsafeManagedPtrCastPtr proxy
    path' <- unsafeManagedPtrGetPtr path
    gtk_tree_row_reference_inserted proxy' path'
    touchManagedPtr proxy
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveTreeRowReferenceMethod (t :: Symbol) (o :: *) :: * where
    ResolveTreeRowReferenceMethod "copy" o = TreeRowReferenceCopyMethodInfo
    ResolveTreeRowReferenceMethod "free" o = TreeRowReferenceFreeMethodInfo
    ResolveTreeRowReferenceMethod "valid" o = TreeRowReferenceValidMethodInfo
    ResolveTreeRowReferenceMethod "getModel" o = TreeRowReferenceGetModelMethodInfo
    ResolveTreeRowReferenceMethod "getPath" o = TreeRowReferenceGetPathMethodInfo
    ResolveTreeRowReferenceMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTreeRowReferenceMethod t TreeRowReference, O.OverloadedMethod info TreeRowReference p) => OL.IsLabel t (TreeRowReference -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTreeRowReferenceMethod t TreeRowReference, O.OverloadedMethod info TreeRowReference p, R.HasField t TreeRowReference p) => R.HasField t TreeRowReference p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTreeRowReferenceMethod t TreeRowReference, O.OverloadedMethodInfo info TreeRowReference) => OL.IsLabel t (O.MethodProxy info TreeRowReference) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


