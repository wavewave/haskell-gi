{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- The t'GI.Gtk.Structs.TreeIter.TreeIter' is the primary structure
-- for accessing a t'GI.Gtk.Interfaces.TreeModel.TreeModel'. Models are expected to put a unique
-- integer in the /@stamp@/ member, and put
-- model-specific data in the three /@userData@/
-- members.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.TreeIter
    ( 

-- * Exported types
    TreeIter(..)                            ,
    newZeroTreeIter                         ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [copy]("GI.Gtk.Structs.TreeIter#g:method:copy"), [free]("GI.Gtk.Structs.TreeIter#g:method:free").
-- 
-- ==== Getters
-- /None/.
-- 
-- ==== Setters
-- /None/.

#if defined(ENABLE_OVERLOADING)
    ResolveTreeIterMethod                   ,
#endif

-- ** copy #method:copy#

#if defined(ENABLE_OVERLOADING)
    TreeIterCopyMethodInfo                  ,
#endif
    treeIterCopy                            ,


-- ** free #method:free#

#if defined(ENABLE_OVERLOADING)
    TreeIterFreeMethodInfo                  ,
#endif
    treeIterFree                            ,




 -- * Properties


-- ** stamp #attr:stamp#
-- | a unique stamp to catch invalid iterators

    getTreeIterStamp                        ,
    setTreeIterStamp                        ,
#if defined(ENABLE_OVERLOADING)
    treeIter_stamp                          ,
#endif


-- ** userData #attr:userData#
-- | model-specific data

    clearTreeIterUserData                   ,
    getTreeIterUserData                     ,
    setTreeIterUserData                     ,
#if defined(ENABLE_OVERLOADING)
    treeIter_userData                       ,
#endif


-- ** userData2 #attr:userData2#
-- | model-specific data

    clearTreeIterUserData2                  ,
    getTreeIterUserData2                    ,
    setTreeIterUserData2                    ,
#if defined(ENABLE_OVERLOADING)
    treeIter_userData2                      ,
#endif


-- ** userData3 #attr:userData3#
-- | model-specific data

    clearTreeIterUserData3                  ,
    getTreeIterUserData3                    ,
    setTreeIterUserData3                    ,
#if defined(ENABLE_OVERLOADING)
    treeIter_userData3                      ,
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


-- | Memory-managed wrapper type.
newtype TreeIter = TreeIter (SP.ManagedPtr TreeIter)
    deriving (Eq)

instance SP.ManagedPtrNewtype TreeIter where
    toManagedPtr (TreeIter p) = p

foreign import ccall "gtk_tree_iter_get_type" c_gtk_tree_iter_get_type :: 
    IO GType

type instance O.ParentTypes TreeIter = '[]
instance O.HasParentTypes TreeIter

instance B.Types.TypedObject TreeIter where
    glibType = c_gtk_tree_iter_get_type

instance B.Types.GBoxed TreeIter

-- | Convert 'TreeIter' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TreeIter) where
    gvalueGType_ = c_gtk_tree_iter_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_boxed gv (FP.nullPtr :: FP.Ptr TreeIter)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_boxed gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_boxed gv :: IO (Ptr TreeIter)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newBoxed TreeIter ptr
        else return P.Nothing
        
    

-- | Construct a `TreeIter` struct initialized to zero.
newZeroTreeIter :: MonadIO m => m TreeIter
newZeroTreeIter = liftIO $ callocBoxedBytes 32 >>= wrapBoxed TreeIter

instance tag ~ 'AttrSet => Constructible TreeIter tag where
    new _ attrs = do
        o <- newZeroTreeIter
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@stamp@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeIter #stamp
-- @
getTreeIterStamp :: MonadIO m => TreeIter -> m Int32
getTreeIterStamp s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO Int32
    return val

-- | Set the value of the “@stamp@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeIter [ #stamp 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeIterStamp :: MonadIO m => TreeIter -> Int32 -> m ()
setTreeIterStamp s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: Int32)

#if defined(ENABLE_OVERLOADING)
data TreeIterStampFieldInfo
instance AttrInfo TreeIterStampFieldInfo where
    type AttrBaseTypeConstraint TreeIterStampFieldInfo = (~) TreeIter
    type AttrAllowedOps TreeIterStampFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TreeIterStampFieldInfo = (~) Int32
    type AttrTransferTypeConstraint TreeIterStampFieldInfo = (~)Int32
    type AttrTransferType TreeIterStampFieldInfo = Int32
    type AttrGetType TreeIterStampFieldInfo = Int32
    type AttrLabel TreeIterStampFieldInfo = "stamp"
    type AttrOrigin TreeIterStampFieldInfo = TreeIter
    attrGet = getTreeIterStamp
    attrSet = setTreeIterStamp
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreeIter.stamp"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreeIter.html#g:attr:stamp"
        })

treeIter_stamp :: AttrLabelProxy "stamp"
treeIter_stamp = AttrLabelProxy

#endif


-- | Get the value of the “@user_data@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeIter #userData
-- @
getTreeIterUserData :: MonadIO m => TreeIter -> m (Ptr ())
getTreeIterUserData s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO (Ptr ())
    return val

-- | Set the value of the “@user_data@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeIter [ #userData 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeIterUserData :: MonadIO m => TreeIter -> Ptr () -> m ()
setTreeIterUserData s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: Ptr ())

-- | Set the value of the “@user_data@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #userData
-- @
clearTreeIterUserData :: MonadIO m => TreeIter -> m ()
clearTreeIterUserData s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (FP.nullPtr :: Ptr ())

#if defined(ENABLE_OVERLOADING)
data TreeIterUserDataFieldInfo
instance AttrInfo TreeIterUserDataFieldInfo where
    type AttrBaseTypeConstraint TreeIterUserDataFieldInfo = (~) TreeIter
    type AttrAllowedOps TreeIterUserDataFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint TreeIterUserDataFieldInfo = (~) (Ptr ())
    type AttrTransferTypeConstraint TreeIterUserDataFieldInfo = (~)(Ptr ())
    type AttrTransferType TreeIterUserDataFieldInfo = (Ptr ())
    type AttrGetType TreeIterUserDataFieldInfo = Ptr ()
    type AttrLabel TreeIterUserDataFieldInfo = "user_data"
    type AttrOrigin TreeIterUserDataFieldInfo = TreeIter
    attrGet = getTreeIterUserData
    attrSet = setTreeIterUserData
    attrConstruct = undefined
    attrClear = clearTreeIterUserData
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreeIter.userData"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreeIter.html#g:attr:userData"
        })

treeIter_userData :: AttrLabelProxy "userData"
treeIter_userData = AttrLabelProxy

#endif


-- | Get the value of the “@user_data2@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeIter #userData2
-- @
getTreeIterUserData2 :: MonadIO m => TreeIter -> m (Ptr ())
getTreeIterUserData2 s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 16) :: IO (Ptr ())
    return val

-- | Set the value of the “@user_data2@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeIter [ #userData2 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeIterUserData2 :: MonadIO m => TreeIter -> Ptr () -> m ()
setTreeIterUserData2 s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (val :: Ptr ())

-- | Set the value of the “@user_data2@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #userData2
-- @
clearTreeIterUserData2 :: MonadIO m => TreeIter -> m ()
clearTreeIterUserData2 s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (FP.nullPtr :: Ptr ())

#if defined(ENABLE_OVERLOADING)
data TreeIterUserData2FieldInfo
instance AttrInfo TreeIterUserData2FieldInfo where
    type AttrBaseTypeConstraint TreeIterUserData2FieldInfo = (~) TreeIter
    type AttrAllowedOps TreeIterUserData2FieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint TreeIterUserData2FieldInfo = (~) (Ptr ())
    type AttrTransferTypeConstraint TreeIterUserData2FieldInfo = (~)(Ptr ())
    type AttrTransferType TreeIterUserData2FieldInfo = (Ptr ())
    type AttrGetType TreeIterUserData2FieldInfo = Ptr ()
    type AttrLabel TreeIterUserData2FieldInfo = "user_data2"
    type AttrOrigin TreeIterUserData2FieldInfo = TreeIter
    attrGet = getTreeIterUserData2
    attrSet = setTreeIterUserData2
    attrConstruct = undefined
    attrClear = clearTreeIterUserData2
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreeIter.userData2"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreeIter.html#g:attr:userData2"
        })

treeIter_userData2 :: AttrLabelProxy "userData2"
treeIter_userData2 = AttrLabelProxy

#endif


-- | Get the value of the “@user_data3@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' treeIter #userData3
-- @
getTreeIterUserData3 :: MonadIO m => TreeIter -> m (Ptr ())
getTreeIterUserData3 s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 24) :: IO (Ptr ())
    return val

-- | Set the value of the “@user_data3@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' treeIter [ #userData3 'Data.GI.Base.Attributes.:=' value ]
-- @
setTreeIterUserData3 :: MonadIO m => TreeIter -> Ptr () -> m ()
setTreeIterUserData3 s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (val :: Ptr ())

-- | Set the value of the “@user_data3@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #userData3
-- @
clearTreeIterUserData3 :: MonadIO m => TreeIter -> m ()
clearTreeIterUserData3 s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (FP.nullPtr :: Ptr ())

#if defined(ENABLE_OVERLOADING)
data TreeIterUserData3FieldInfo
instance AttrInfo TreeIterUserData3FieldInfo where
    type AttrBaseTypeConstraint TreeIterUserData3FieldInfo = (~) TreeIter
    type AttrAllowedOps TreeIterUserData3FieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint TreeIterUserData3FieldInfo = (~) (Ptr ())
    type AttrTransferTypeConstraint TreeIterUserData3FieldInfo = (~)(Ptr ())
    type AttrTransferType TreeIterUserData3FieldInfo = (Ptr ())
    type AttrGetType TreeIterUserData3FieldInfo = Ptr ()
    type AttrLabel TreeIterUserData3FieldInfo = "user_data3"
    type AttrOrigin TreeIterUserData3FieldInfo = TreeIter
    attrGet = getTreeIterUserData3
    attrSet = setTreeIterUserData3
    attrConstruct = undefined
    attrClear = clearTreeIterUserData3
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreeIter.userData3"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreeIter.html#g:attr:userData3"
        })

treeIter_userData3 :: AttrLabelProxy "userData3"
treeIter_userData3 = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TreeIter
type instance O.AttributeList TreeIter = TreeIterAttributeList
type TreeIterAttributeList = ('[ '("stamp", TreeIterStampFieldInfo), '("userData", TreeIterUserDataFieldInfo), '("userData2", TreeIterUserData2FieldInfo), '("userData3", TreeIterUserData3FieldInfo)] :: [(Symbol, *)])
#endif

-- method TreeIter::copy
-- method type : OrdinaryMethod
-- Args: [ Arg
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TreeIter" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_tree_iter_copy" gtk_tree_iter_copy :: 
    Ptr TreeIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO (Ptr TreeIter)

-- | Creates a dynamically allocated tree iterator as a copy of /@iter@/.
-- 
-- This function is not intended for use in applications,
-- because you can just copy the structs by value
-- (@GtkTreeIter new_iter = iter;@).
-- You must free this iter with 'GI.Gtk.Structs.TreeIter.treeIterFree'.
treeIterCopy ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreeIter
    -- ^ /@iter@/: a t'GI.Gtk.Structs.TreeIter.TreeIter'-struct
    -> m TreeIter
    -- ^ __Returns:__ a newly-allocated copy of /@iter@/
treeIterCopy iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    result <- gtk_tree_iter_copy iter'
    checkUnexpectedReturnNULL "treeIterCopy" result
    result' <- (wrapBoxed TreeIter) result
    touchManagedPtr iter
    return result'

#if defined(ENABLE_OVERLOADING)
data TreeIterCopyMethodInfo
instance (signature ~ (m TreeIter), MonadIO m) => O.OverloadedMethod TreeIterCopyMethodInfo TreeIter signature where
    overloadedMethod = treeIterCopy

instance O.OverloadedMethodInfo TreeIterCopyMethodInfo TreeIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreeIter.treeIterCopy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreeIter.html#v:treeIterCopy"
        })


#endif

-- method TreeIter::free
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "iter"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TreeIter" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a dynamically allocated tree iterator"
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

foreign import ccall "gtk_tree_iter_free" gtk_tree_iter_free :: 
    Ptr TreeIter ->                         -- iter : TInterface (Name {namespace = "Gtk", name = "TreeIter"})
    IO ()

-- | Frees an iterator that has been allocated by 'GI.Gtk.Structs.TreeIter.treeIterCopy'.
-- 
-- This function is mainly used for language bindings.
treeIterFree ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TreeIter
    -- ^ /@iter@/: a dynamically allocated tree iterator
    -> m ()
treeIterFree iter = liftIO $ do
    iter' <- unsafeManagedPtrGetPtr iter
    gtk_tree_iter_free iter'
    touchManagedPtr iter
    return ()

#if defined(ENABLE_OVERLOADING)
data TreeIterFreeMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod TreeIterFreeMethodInfo TreeIter signature where
    overloadedMethod = treeIterFree

instance O.OverloadedMethodInfo TreeIterFreeMethodInfo TreeIter where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TreeIter.treeIterFree",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TreeIter.html#v:treeIterFree"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveTreeIterMethod (t :: Symbol) (o :: *) :: * where
    ResolveTreeIterMethod "copy" o = TreeIterCopyMethodInfo
    ResolveTreeIterMethod "free" o = TreeIterFreeMethodInfo
    ResolveTreeIterMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTreeIterMethod t TreeIter, O.OverloadedMethod info TreeIter p) => OL.IsLabel t (TreeIter -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTreeIterMethod t TreeIter, O.OverloadedMethod info TreeIter p, R.HasField t TreeIter p) => R.HasField t TreeIter p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTreeIterMethod t TreeIter, O.OverloadedMethodInfo info TreeIter) => OL.IsLabel t (O.MethodProxy info TreeIter) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


