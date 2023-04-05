{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.TreePath
    ( 

-- * Exported types
    TreePath(..)                            ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [appendIndex]("GI.Gtk.Structs.TreePath#g:method:appendIndex"), [compare]("GI.Gtk.Structs.TreePath#g:method:compare"), [copy]("GI.Gtk.Structs.TreePath#g:method:copy"), [down]("GI.Gtk.Structs.TreePath#g:method:down"), [free]("GI.Gtk.Structs.TreePath#g:method:free"), [isAncestor]("GI.Gtk.Structs.TreePath#g:method:isAncestor"), [isDescendant]("GI.Gtk.Structs.TreePath#g:method:isDescendant"), [next]("GI.Gtk.Structs.TreePath#g:method:next"), [prependIndex]("GI.Gtk.Structs.TreePath#g:method:prependIndex"), [prev]("GI.Gtk.Structs.TreePath#g:method:prev"), [toString]("GI.Gtk.Structs.TreePath#g:method:toString"), [up]("GI.Gtk.Structs.TreePath#g:method:up").
-- 
-- ==== Getters
-- [getDepth]("GI.Gtk.Structs.TreePath#g:method:getDepth"), [getIndices]("GI.Gtk.Structs.TreePath#g:method:getIndices").
-- 
-- ==== Setters
-- /None/.

#if defined(ENABLE_OVERLOADING)
    ResolveTreePathMethod                   ,
#endif

-- ** appendIndex #method:appendIndex#

#if defined(ENABLE_OVERLOADING)
    TreePathAppendIndexMethodInfo           ,
#endif
    treePathAppendIndex                     ,


-- ** compare #method:compare#

#if defined(ENABLE_OVERLOADING)
    TreePathCompareMethodInfo               ,
#endif
    treePathCompare                         ,


-- ** copy #method:copy#

#if defined(ENABLE_OVERLOADING)
    TreePathCopyMethodInfo                  ,
#endif
    treePathCopy                            ,


-- ** down #method:down#

#if defined(ENABLE_OVERLOADING)
    TreePathDownMethodInfo                  ,
#endif
    treePathDown                            ,


-- ** free #method:free#

#if defined(ENABLE_OVERLOADING)
    TreePathFreeMethodInfo                  ,
#endif
    treePathFree                            ,


-- ** getDepth #method:getDepth#

#if defined(ENABLE_OVERLOADING)
    TreePathGetDepthMethodInfo              ,
#endif
    treePathGetDepth                        ,


-- ** getIndices #method:getIndices#

#if defined(ENABLE_OVERLOADING)
    TreePathGetIndicesMethodInfo            ,
#endif
    treePathGetIndices                      ,


-- ** isAncestor #method:isAncestor#

#if defined(ENABLE_OVERLOADING)
    TreePathIsAncestorMethodInfo            ,
#endif
    treePathIsAncestor                      ,


-- ** isDescendant #method:isDescendant#

#if defined(ENABLE_OVERLOADING)
    TreePathIsDescendantMethodInfo          ,
#endif
    treePathIsDescendant                    ,


-- ** new #method:new#

    treePathNew                             ,


-- ** newFirst #method:newFirst#

    treePathNewFirst                        ,


-- ** newFromIndices #method:newFromIndices#

    treePathNewFromIndices                  ,


-- ** newFromString #method:newFromString#

    treePathNewFromString                   ,


-- ** next #method:next#

#if defined(ENABLE_OVERLOADING)
    TreePathNextMethodInfo                  ,
#endif
    treePathNext                            ,


-- ** prependIndex #method:prependIndex#

#if defined(ENABLE_OVERLOADING)
    TreePathPrependIndexMethodInfo          ,
#endif
    treePathPrependIndex                    ,


-- ** prev #method:prev#

#if defined(ENABLE_OVERLOADING)
    TreePathPrevMethodInfo                  ,
#endif
    treePathPrev                            ,


-- ** toString #method:toString#

#if defined(ENABLE_OVERLOADING)
    TreePathToStringMethodInfo              ,
#endif
    treePathToString                        ,


-- ** up #method:up#

#if defined(ENABLE_OVERLOADING)
    TreePathUpMethodInfo                    ,
#endif
    treePathUp                              ,




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


-- | Memory-managed wrapper type.
newtype TreePath = TreePath (SP.ManagedPtr TreePath)
    deriving (Eq)

instance SP.ManagedPtrNewtype TreePath where
    toManagedPtr (TreePath p) = p

foreign import ccall "gtk_tree_path_get_type" c_gtk_tree_path_get_type :: 
    IO GType

type instance O.ParentTypes TreePath = '[]
instance O.HasParentTypes TreePath

instance B.Types.TypedObject TreePath where
    glibType = c_gtk_tree_path_get_type

instance B.Types.GBoxed TreePath

-- | Convert 'TreePath' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TreePath) where
    gvalueGType_ = c_gtk_tree_path_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_boxed gv (FP.nullPtr :: FP.Ptr TreePath)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_boxed gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_boxed gv :: IO (Ptr TreePath)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newBoxed TreePath ptr
        else return P.Nothing
        
    


#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TreePath
type instance O.AttributeList TreePath = TreePathAttributeList
type TreePathAttributeList = ('[ ] :: [(Symbol, *)])
#endif

-- method TreePath::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TreePath" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_path_new" gtk_tree_path_new :: 
    IO (Ptr TreePath)

-- | Creates a new t'GI.Gtk.Structs.TreePath.TreePath'-struct.
-- This refers to a row.
treePathNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m TreePath
    -- ^ __Returns:__ A newly created t'GI.Gtk.Structs.TreePath.TreePath'-struct.
treePathNew  = liftIO $ do
    result <- gtk_tree_path_new
    checkUnexpectedReturnNULL "treePathNew" result
    result' <- (wrapBoxed TreePath) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TreePath::new_first
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TreePath" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_path_new_first" gtk_tree_path_new_first :: 
    IO (Ptr TreePath)

-- | Creates a new t'GI.Gtk.Structs.TreePath.TreePath'-struct.
-- 
-- The string representation of this path is “0”.
treePathNewFirst ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m TreePath
    -- ^ __Returns:__ A new t'GI.Gtk.Structs.TreePath.TreePath'-struct
treePathNewFirst  = liftIO $ do
    result <- gtk_tree_path_new_first
    checkUnexpectedReturnNULL "treePathNewFirst" result
    result' <- (wrapBoxed TreePath) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TreePath::new_from_indices
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "indices"
--           , argType = TCArray False (-1) 1 (TBasicType TInt)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "array of indices" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "length"
--           , argType = TBasicType TUInt64
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "length of @indices array"
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
--              , argType = TBasicType TUInt64
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "length of @indices array"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TreePath" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_path_new_from_indicesv" gtk_tree_path_new_from_indicesv :: 
    Ptr Int32 ->                            -- indices : TCArray False (-1) 1 (TBasicType TInt)
    Word64 ->                               -- length : TBasicType TUInt64
    IO (Ptr TreePath)

-- | Creates a new path with the given /@indices@/ array of /@length@/.
-- 
-- /Since: 3.12/
treePathNewFromIndices ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    [Int32]
    -- ^ /@indices@/: array of indices
    -> m TreePath
    -- ^ __Returns:__ A newly created t'GI.Gtk.Structs.TreePath.TreePath'-struct
treePathNewFromIndices indices = liftIO $ do
    let length_ = fromIntegral $ P.length indices
    indices' <- packStorableArray indices
    result <- gtk_tree_path_new_from_indicesv indices' length_
    checkUnexpectedReturnNULL "treePathNewFromIndices" result
    result' <- (wrapBoxed TreePath) result
    freeMem indices'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TreePath::new_from_string
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The string representation of a path"
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

foreign import ccall "gtk_tree_path_new_from_string" gtk_tree_path_new_from_string :: 
    CString ->                              -- path : TBasicType TUTF8
    IO (Ptr TreePath)

-- | Creates a new t'GI.Gtk.Structs.TreePath.TreePath'-struct initialized to /@path@/.
-- 
-- /@path@/ is expected to be a colon separated list of numbers.
-- For example, the string “10:4:0” would create a path of depth
-- 3 pointing to the 11th child of the root node, the 5th
-- child of that 11th child, and the 1st child of that 5th child.
-- If an invalid path string is passed in, 'P.Nothing' is returned.
treePathNewFromString ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@path@/: The string representation of a path
    -> m TreePath
    -- ^ __Returns:__ A newly-created t'GI.Gtk.Structs.TreePath.TreePath'-struct, or 'P.Nothing'
treePathNewFromString path = liftIO $ do
    path' <- textToCString path
    result <- gtk_tree_path_new_from_string path'
    checkUnexpectedReturnNULL "treePathNewFromString" result
    result' <- (wrapBoxed TreePath) result
    freeMem path'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TreePath::append_index
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath-struct"
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
--                 { rawDocText = Just "the index" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_path_append_index" gtk_tree_path_append_index :: 
    Ptr TreePath ->                         -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Int32 ->                                -- index_ : TBasicType TInt
    IO ()

-- | Appends a new index to a path.
-- 
-- As a result, the depth of the path is increased.
treePathAppendIndex ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> Int32
    -- ^ /@index_@/: the index
    -> m ()
treePathAppendIndex path index_ = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    gtk_tree_path_append_index path' index_
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data TreePathAppendIndexMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m) => O.OverloadedMethod TreePathAppendIndexMethodInfo TreePath signature where
    overloadedMethod = treePathAppendIndex

instance O.OverloadedMethodInfo TreePathAppendIndexMethodInfo TreePath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreePath.treePathAppendIndex",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreePath.html#v:treePathAppendIndex"
        })


#endif

-- method TreePath::compare
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "a"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath-struct"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "b"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath-struct to compare with"
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

foreign import ccall "gtk_tree_path_compare" gtk_tree_path_compare :: 
    Ptr TreePath ->                         -- a : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr TreePath ->                         -- b : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO Int32

-- | Compares two paths.
-- 
-- If /@a@/ appears before /@b@/ in a tree, then -1 is returned.
-- If /@b@/ appears before /@a@/, then 1 is returned.
-- If the two nodes are equal, then 0 is returned.
treePathCompare ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreePath
    -- ^ /@a@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> TreePath
    -- ^ /@b@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct to compare with
    -> m Int32
    -- ^ __Returns:__ the relative positions of /@a@/ and /@b@/
treePathCompare a b = liftIO $ do
    a' <- unsafeManagedPtrGetPtr a
    b' <- unsafeManagedPtrGetPtr b
    result <- gtk_tree_path_compare a' b'
    touchManagedPtr a
    touchManagedPtr b
    return result

#if defined(ENABLE_OVERLOADING)
data TreePathCompareMethodInfo
instance (signature ~ (TreePath -> m Int32), MonadIO m) => O.OverloadedMethod TreePathCompareMethodInfo TreePath signature where
    overloadedMethod = treePathCompare

instance O.OverloadedMethodInfo TreePathCompareMethodInfo TreePath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreePath.treePathCompare",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreePath.html#v:treePathCompare"
        })


#endif

-- method TreePath::copy
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath-struct"
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

foreign import ccall "gtk_tree_path_copy" gtk_tree_path_copy :: 
    Ptr TreePath ->                         -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO (Ptr TreePath)

-- | Creates a new t'GI.Gtk.Structs.TreePath.TreePath'-struct as a copy of /@path@/.
treePathCopy ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> m TreePath
    -- ^ __Returns:__ a new t'GI.Gtk.Structs.TreePath.TreePath'-struct
treePathCopy path = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_tree_path_copy path'
    checkUnexpectedReturnNULL "treePathCopy" result
    result' <- (wrapBoxed TreePath) result
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data TreePathCopyMethodInfo
instance (signature ~ (m TreePath), MonadIO m) => O.OverloadedMethod TreePathCopyMethodInfo TreePath signature where
    overloadedMethod = treePathCopy

instance O.OverloadedMethodInfo TreePathCopyMethodInfo TreePath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreePath.treePathCopy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreePath.html#v:treePathCopy"
        })


#endif

-- method TreePath::down
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath-struct"
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

foreign import ccall "gtk_tree_path_down" gtk_tree_path_down :: 
    Ptr TreePath ->                         -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO ()

-- | Moves /@path@/ to point to the first child of the current path.
treePathDown ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> m ()
treePathDown path = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    gtk_tree_path_down path'
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data TreePathDownMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod TreePathDownMethodInfo TreePath signature where
    overloadedMethod = treePathDown

instance O.OverloadedMethodInfo TreePathDownMethodInfo TreePath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreePath.treePathDown",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreePath.html#v:treePathDown"
        })


#endif

-- method TreePath::free
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath-struct"
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

foreign import ccall "gtk_tree_path_free" gtk_tree_path_free :: 
    Ptr TreePath ->                         -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO ()

-- | Frees /@path@/. If /@path@/ is 'P.Nothing', it simply returns.
treePathFree ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> m ()
treePathFree path = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    gtk_tree_path_free path'
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data TreePathFreeMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod TreePathFreeMethodInfo TreePath signature where
    overloadedMethod = treePathFree

instance O.OverloadedMethodInfo TreePathFreeMethodInfo TreePath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreePath.treePathFree",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreePath.html#v:treePathFree"
        })


#endif

-- method TreePath::get_depth
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath-struct"
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

foreign import ccall "gtk_tree_path_get_depth" gtk_tree_path_get_depth :: 
    Ptr TreePath ->                         -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO Int32

-- | Returns the current depth of /@path@/.
treePathGetDepth ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> m Int32
    -- ^ __Returns:__ The depth of /@path@/
treePathGetDepth path = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_tree_path_get_depth path'
    touchManagedPtr path
    return result

#if defined(ENABLE_OVERLOADING)
data TreePathGetDepthMethodInfo
instance (signature ~ (m Int32), MonadIO m) => O.OverloadedMethod TreePathGetDepthMethodInfo TreePath signature where
    overloadedMethod = treePathGetDepth

instance O.OverloadedMethodInfo TreePathGetDepthMethodInfo TreePath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreePath.treePathGetDepth",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreePath.html#v:treePathGetDepth"
        })


#endif

-- method TreePath::get_indices
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath-struct"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "depth"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "return location for number of elements\n    returned in the integer array, or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "depth"
--              , argType = TBasicType TInt
--              , direction = DirectionOut
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText =
--                        Just
--                          "return location for number of elements\n    returned in the integer array, or %NULL"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferEverything
--              }
--          ]
-- returnType: Just (TCArray False (-1) 1 (TBasicType TInt))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_path_get_indices_with_depth" gtk_tree_path_get_indices_with_depth :: 
    Ptr TreePath ->                         -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr Int32 ->                            -- depth : TBasicType TInt
    IO (Ptr Int32)

-- | Returns the current indices of /@path@/.
-- 
-- This is an array of integers, each representing a node in a tree.
-- It also returns the number of elements in the array.
-- The array should not be freed.
-- 
-- /Since: 3.0/
treePathGetIndices ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> m (Maybe [Int32])
    -- ^ __Returns:__ The current
    --     indices, or 'P.Nothing'
treePathGetIndices path = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    depth <- allocMem :: IO (Ptr Int32)
    result <- gtk_tree_path_get_indices_with_depth path' depth
    depth' <- peek depth
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (unpackStorableArrayWithLength depth') result'
        return result''
    touchManagedPtr path
    freeMem depth
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data TreePathGetIndicesMethodInfo
instance (signature ~ (m (Maybe [Int32])), MonadIO m) => O.OverloadedMethod TreePathGetIndicesMethodInfo TreePath signature where
    overloadedMethod = treePathGetIndices

instance O.OverloadedMethodInfo TreePathGetIndicesMethodInfo TreePath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreePath.treePathGetIndices",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreePath.html#v:treePathGetIndices"
        })


#endif

-- method TreePath::is_ancestor
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath-struct"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "descendant"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "another #GtkTreePath-struct"
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

foreign import ccall "gtk_tree_path_is_ancestor" gtk_tree_path_is_ancestor :: 
    Ptr TreePath ->                         -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr TreePath ->                         -- descendant : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO CInt

-- | Returns 'P.True' if /@descendant@/ is a descendant of /@path@/.
treePathIsAncestor ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> TreePath
    -- ^ /@descendant@/: another t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@descendant@/ is contained inside /@path@/
treePathIsAncestor path descendant = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    descendant' <- unsafeManagedPtrGetPtr descendant
    result <- gtk_tree_path_is_ancestor path' descendant'
    let result' = (/= 0) result
    touchManagedPtr path
    touchManagedPtr descendant
    return result'

#if defined(ENABLE_OVERLOADING)
data TreePathIsAncestorMethodInfo
instance (signature ~ (TreePath -> m Bool), MonadIO m) => O.OverloadedMethod TreePathIsAncestorMethodInfo TreePath signature where
    overloadedMethod = treePathIsAncestor

instance O.OverloadedMethodInfo TreePathIsAncestorMethodInfo TreePath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreePath.treePathIsAncestor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreePath.html#v:treePathIsAncestor"
        })


#endif

-- method TreePath::is_descendant
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath-struct"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "ancestor"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "another #GtkTreePath-struct"
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

foreign import ccall "gtk_tree_path_is_descendant" gtk_tree_path_is_descendant :: 
    Ptr TreePath ->                         -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Ptr TreePath ->                         -- ancestor : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO CInt

-- | Returns 'P.True' if /@path@/ is a descendant of /@ancestor@/.
treePathIsDescendant ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> TreePath
    -- ^ /@ancestor@/: another t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@ancestor@/ contains /@path@/ somewhere below it
treePathIsDescendant path ancestor = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    ancestor' <- unsafeManagedPtrGetPtr ancestor
    result <- gtk_tree_path_is_descendant path' ancestor'
    let result' = (/= 0) result
    touchManagedPtr path
    touchManagedPtr ancestor
    return result'

#if defined(ENABLE_OVERLOADING)
data TreePathIsDescendantMethodInfo
instance (signature ~ (TreePath -> m Bool), MonadIO m) => O.OverloadedMethod TreePathIsDescendantMethodInfo TreePath signature where
    overloadedMethod = treePathIsDescendant

instance O.OverloadedMethodInfo TreePathIsDescendantMethodInfo TreePath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreePath.treePathIsDescendant",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreePath.html#v:treePathIsDescendant"
        })


#endif

-- method TreePath::next
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath-struct"
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

foreign import ccall "gtk_tree_path_next" gtk_tree_path_next :: 
    Ptr TreePath ->                         -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO ()

-- | Moves the /@path@/ to point to the next node at the current depth.
treePathNext ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> m ()
treePathNext path = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    gtk_tree_path_next path'
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data TreePathNextMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod TreePathNextMethodInfo TreePath signature where
    overloadedMethod = treePathNext

instance O.OverloadedMethodInfo TreePathNextMethodInfo TreePath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreePath.treePathNext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreePath.html#v:treePathNext"
        })


#endif

-- method TreePath::prepend_index
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath-struct"
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
--                 { rawDocText = Just "the index" , sinceVersion = Nothing }
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

foreign import ccall "gtk_tree_path_prepend_index" gtk_tree_path_prepend_index :: 
    Ptr TreePath ->                         -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    Int32 ->                                -- index_ : TBasicType TInt
    IO ()

-- | Prepends a new index to a path.
-- 
-- As a result, the depth of the path is increased.
treePathPrependIndex ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> Int32
    -- ^ /@index_@/: the index
    -> m ()
treePathPrependIndex path index_ = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    gtk_tree_path_prepend_index path' index_
    touchManagedPtr path
    return ()

#if defined(ENABLE_OVERLOADING)
data TreePathPrependIndexMethodInfo
instance (signature ~ (Int32 -> m ()), MonadIO m) => O.OverloadedMethod TreePathPrependIndexMethodInfo TreePath signature where
    overloadedMethod = treePathPrependIndex

instance O.OverloadedMethodInfo TreePathPrependIndexMethodInfo TreePath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreePath.treePathPrependIndex",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreePath.html#v:treePathPrependIndex"
        })


#endif

-- method TreePath::prev
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath-struct"
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

foreign import ccall "gtk_tree_path_prev" gtk_tree_path_prev :: 
    Ptr TreePath ->                         -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO CInt

-- | Moves the /@path@/ to point to the previous node at the
-- current depth, if it exists.
treePathPrev ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@path@/ has a previous node, and
    --     the move was made
treePathPrev path = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_tree_path_prev path'
    let result' = (/= 0) result
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data TreePathPrevMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TreePathPrevMethodInfo TreePath signature where
    overloadedMethod = treePathPrev

instance O.OverloadedMethodInfo TreePathPrevMethodInfo TreePath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreePath.treePathPrev",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreePath.html#v:treePathPrev"
        })


#endif

-- method TreePath::to_string
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A #GtkTreePath-struct"
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

foreign import ccall "gtk_tree_path_to_string" gtk_tree_path_to_string :: 
    Ptr TreePath ->                         -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO CString

-- | Generates a string representation of the path.
-- 
-- This string is a “:” separated list of numbers.
-- For example, “4:10:0:3” would be an acceptable
-- return value for this string.
treePathToString ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreePath
    -- ^ /@path@/: A t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> m T.Text
    -- ^ __Returns:__ A newly-allocated string.
    --     Must be freed with 'GI.GLib.Functions.free'.
treePathToString path = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_tree_path_to_string path'
    checkUnexpectedReturnNULL "treePathToString" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data TreePathToStringMethodInfo
instance (signature ~ (m T.Text), MonadIO m) => O.OverloadedMethod TreePathToStringMethodInfo TreePath signature where
    overloadedMethod = treePathToString

instance O.OverloadedMethodInfo TreePathToStringMethodInfo TreePath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreePath.treePathToString",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreePath.html#v:treePathToString"
        })


#endif

-- method TreePath::up
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreePath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkTreePath-struct"
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

foreign import ccall "gtk_tree_path_up" gtk_tree_path_up :: 
    Ptr TreePath ->                         -- path : TInterface (Name {namespace = "Gtk", name = "TreePath"})
    IO CInt

-- | Moves the /@path@/ to point to its parent node, if it has a parent.
treePathUp ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreePath
    -- ^ /@path@/: a t'GI.Gtk.Structs.TreePath.TreePath'-struct
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@path@/ has a parent, and the move was made
treePathUp path = liftIO $ do
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_tree_path_up path'
    let result' = (/= 0) result
    touchManagedPtr path
    return result'

#if defined(ENABLE_OVERLOADING)
data TreePathUpMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod TreePathUpMethodInfo TreePath signature where
    overloadedMethod = treePathUp

instance O.OverloadedMethodInfo TreePathUpMethodInfo TreePath where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreePath.treePathUp",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreePath.html#v:treePathUp"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveTreePathMethod (t :: Symbol) (o :: *) :: * where
    ResolveTreePathMethod "appendIndex" o = TreePathAppendIndexMethodInfo
    ResolveTreePathMethod "compare" o = TreePathCompareMethodInfo
    ResolveTreePathMethod "copy" o = TreePathCopyMethodInfo
    ResolveTreePathMethod "down" o = TreePathDownMethodInfo
    ResolveTreePathMethod "free" o = TreePathFreeMethodInfo
    ResolveTreePathMethod "isAncestor" o = TreePathIsAncestorMethodInfo
    ResolveTreePathMethod "isDescendant" o = TreePathIsDescendantMethodInfo
    ResolveTreePathMethod "next" o = TreePathNextMethodInfo
    ResolveTreePathMethod "prependIndex" o = TreePathPrependIndexMethodInfo
    ResolveTreePathMethod "prev" o = TreePathPrevMethodInfo
    ResolveTreePathMethod "toString" o = TreePathToStringMethodInfo
    ResolveTreePathMethod "up" o = TreePathUpMethodInfo
    ResolveTreePathMethod "getDepth" o = TreePathGetDepthMethodInfo
    ResolveTreePathMethod "getIndices" o = TreePathGetIndicesMethodInfo
    ResolveTreePathMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTreePathMethod t TreePath, O.OverloadedMethod info TreePath p) => OL.IsLabel t (TreePath -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTreePathMethod t TreePath, O.OverloadedMethod info TreePath p, R.HasField t TreePath p) => R.HasField t TreePath p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTreePathMethod t TreePath, O.OverloadedMethodInfo info TreePath) => OL.IsLabel t (O.MethodProxy info TreePath) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


