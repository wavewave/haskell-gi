{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Structs.TargetEntry.TargetEntry' represents a single type of
-- data than can be supplied for by a widget for a selection
-- or for supplied or received during drag-and-drop.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.TargetEntry
    ( 

-- * Exported types
    TargetEntry(..)                         ,
    newZeroTargetEntry                      ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [copy]("GI.Gtk.Structs.TargetEntry#g:method:copy"), [free]("GI.Gtk.Structs.TargetEntry#g:method:free").
-- 
-- ==== Getters
-- /None/.
-- 
-- ==== Setters
-- /None/.

#if defined(ENABLE_OVERLOADING)
    ResolveTargetEntryMethod                ,
#endif

-- ** copy #method:copy#

#if defined(ENABLE_OVERLOADING)
    TargetEntryCopyMethodInfo               ,
#endif
    targetEntryCopy                         ,


-- ** free #method:free#

#if defined(ENABLE_OVERLOADING)
    TargetEntryFreeMethodInfo               ,
#endif
    targetEntryFree                         ,


-- ** new #method:new#

    targetEntryNew                          ,




 -- * Properties


-- ** flags #attr:flags#
-- | t'GI.Gtk.Flags.TargetFlags' for DND

    getTargetEntryFlags                     ,
    setTargetEntryFlags                     ,
#if defined(ENABLE_OVERLOADING)
    targetEntry_flags                       ,
#endif


-- ** info #attr:info#
-- | an application-assigned integer ID which will
--     get passed as a parameter to e.g the [Widget::selectionGet]("GI.Gtk.Objects.Widget#g:signal:selectionGet")
--     signal. It allows the application to identify the target
--     type without extensive string compares.

    getTargetEntryInfo                      ,
    setTargetEntryInfo                      ,
#if defined(ENABLE_OVERLOADING)
    targetEntry_info                        ,
#endif


-- ** target #attr:target#
-- | a string representation of the target type

    clearTargetEntryTarget                  ,
    getTargetEntryTarget                    ,
    setTargetEntryTarget                    ,
#if defined(ENABLE_OVERLOADING)
    targetEntry_target                      ,
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
newtype TargetEntry = TargetEntry (SP.ManagedPtr TargetEntry)
    deriving (Eq)

instance SP.ManagedPtrNewtype TargetEntry where
    toManagedPtr (TargetEntry p) = p

foreign import ccall "gtk_target_entry_get_type" c_gtk_target_entry_get_type :: 
    IO GType

type instance O.ParentTypes TargetEntry = '[]
instance O.HasParentTypes TargetEntry

instance B.Types.TypedObject TargetEntry where
    glibType = c_gtk_target_entry_get_type

instance B.Types.GBoxed TargetEntry

-- | Convert 'TargetEntry' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe TargetEntry) where
    gvalueGType_ = c_gtk_target_entry_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_boxed gv (FP.nullPtr :: FP.Ptr TargetEntry)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_boxed gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_boxed gv :: IO (Ptr TargetEntry)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newBoxed TargetEntry ptr
        else return P.Nothing
        
    

-- | Construct a `TargetEntry` struct initialized to zero.
newZeroTargetEntry :: MonadIO m => m TargetEntry
newZeroTargetEntry = liftIO $ callocBoxedBytes 16 >>= wrapBoxed TargetEntry

instance tag ~ 'AttrSet => Constructible TargetEntry tag where
    new _ attrs = do
        o <- newZeroTargetEntry
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@target@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' targetEntry #target
-- @
getTargetEntryTarget :: MonadIO m => TargetEntry -> m (Maybe T.Text)
getTargetEntryTarget s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@target@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' targetEntry [ #target 'Data.GI.Base.Attributes.:=' value ]
-- @
setTargetEntryTarget :: MonadIO m => TargetEntry -> CString -> m ()
setTargetEntryTarget s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: CString)

-- | Set the value of the “@target@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #target
-- @
clearTargetEntryTarget :: MonadIO m => TargetEntry -> m ()
clearTargetEntryTarget s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data TargetEntryTargetFieldInfo
instance AttrInfo TargetEntryTargetFieldInfo where
    type AttrBaseTypeConstraint TargetEntryTargetFieldInfo = (~) TargetEntry
    type AttrAllowedOps TargetEntryTargetFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint TargetEntryTargetFieldInfo = (~) CString
    type AttrTransferTypeConstraint TargetEntryTargetFieldInfo = (~)CString
    type AttrTransferType TargetEntryTargetFieldInfo = CString
    type AttrGetType TargetEntryTargetFieldInfo = Maybe T.Text
    type AttrLabel TargetEntryTargetFieldInfo = "target"
    type AttrOrigin TargetEntryTargetFieldInfo = TargetEntry
    attrGet = getTargetEntryTarget
    attrSet = setTargetEntryTarget
    attrConstruct = undefined
    attrClear = clearTargetEntryTarget
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TargetEntry.target"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TargetEntry.html#g:attr:target"
        })

targetEntry_target :: AttrLabelProxy "target"
targetEntry_target = AttrLabelProxy

#endif


-- | Get the value of the “@flags@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' targetEntry #flags
-- @
getTargetEntryFlags :: MonadIO m => TargetEntry -> m Word32
getTargetEntryFlags s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO Word32
    return val

-- | Set the value of the “@flags@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' targetEntry [ #flags 'Data.GI.Base.Attributes.:=' value ]
-- @
setTargetEntryFlags :: MonadIO m => TargetEntry -> Word32 -> m ()
setTargetEntryFlags s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TargetEntryFlagsFieldInfo
instance AttrInfo TargetEntryFlagsFieldInfo where
    type AttrBaseTypeConstraint TargetEntryFlagsFieldInfo = (~) TargetEntry
    type AttrAllowedOps TargetEntryFlagsFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TargetEntryFlagsFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TargetEntryFlagsFieldInfo = (~)Word32
    type AttrTransferType TargetEntryFlagsFieldInfo = Word32
    type AttrGetType TargetEntryFlagsFieldInfo = Word32
    type AttrLabel TargetEntryFlagsFieldInfo = "flags"
    type AttrOrigin TargetEntryFlagsFieldInfo = TargetEntry
    attrGet = getTargetEntryFlags
    attrSet = setTargetEntryFlags
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TargetEntry.flags"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TargetEntry.html#g:attr:flags"
        })

targetEntry_flags :: AttrLabelProxy "flags"
targetEntry_flags = AttrLabelProxy

#endif


-- | Get the value of the “@info@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' targetEntry #info
-- @
getTargetEntryInfo :: MonadIO m => TargetEntry -> m Word32
getTargetEntryInfo s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 12) :: IO Word32
    return val

-- | Set the value of the “@info@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' targetEntry [ #info 'Data.GI.Base.Attributes.:=' value ]
-- @
setTargetEntryInfo :: MonadIO m => TargetEntry -> Word32 -> m ()
setTargetEntryInfo s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 12) (val :: Word32)

#if defined(ENABLE_OVERLOADING)
data TargetEntryInfoFieldInfo
instance AttrInfo TargetEntryInfoFieldInfo where
    type AttrBaseTypeConstraint TargetEntryInfoFieldInfo = (~) TargetEntry
    type AttrAllowedOps TargetEntryInfoFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint TargetEntryInfoFieldInfo = (~) Word32
    type AttrTransferTypeConstraint TargetEntryInfoFieldInfo = (~)Word32
    type AttrTransferType TargetEntryInfoFieldInfo = Word32
    type AttrGetType TargetEntryInfoFieldInfo = Word32
    type AttrLabel TargetEntryInfoFieldInfo = "info"
    type AttrOrigin TargetEntryInfoFieldInfo = TargetEntry
    attrGet = getTargetEntryInfo
    attrSet = setTargetEntryInfo
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TargetEntry.info"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TargetEntry.html#g:attr:info"
        })

targetEntry_info :: AttrLabelProxy "info"
targetEntry_info = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList TargetEntry
type instance O.AttributeList TargetEntry = TargetEntryAttributeList
type TargetEntryAttributeList = ('[ '("target", TargetEntryTargetFieldInfo), '("flags", TargetEntryFlagsFieldInfo), '("info", TargetEntryInfoFieldInfo)] :: [(Symbol, *)])
#endif

-- method TargetEntry::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "target"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "String identifier for target"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Set of flags, see #GtkTargetFlags"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "info"
--           , argType = TBasicType TUInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "an ID that will be passed back to the application"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TargetEntry" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_target_entry_new" gtk_target_entry_new :: 
    CString ->                              -- target : TBasicType TUTF8
    Word32 ->                               -- flags : TBasicType TUInt
    Word32 ->                               -- info : TBasicType TUInt
    IO (Ptr TargetEntry)

-- | Makes a new t'GI.Gtk.Structs.TargetEntry.TargetEntry'.
targetEntryNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@target@/: String identifier for target
    -> Word32
    -- ^ /@flags@/: Set of flags, see t'GI.Gtk.Flags.TargetFlags'
    -> Word32
    -- ^ /@info@/: an ID that will be passed back to the application
    -> m TargetEntry
    -- ^ __Returns:__ a pointer to a new t'GI.Gtk.Structs.TargetEntry.TargetEntry'.
    --     Free with 'GI.Gtk.Structs.TargetEntry.targetEntryFree'
targetEntryNew target flags info = liftIO $ do
    target' <- textToCString target
    result <- gtk_target_entry_new target' flags info
    checkUnexpectedReturnNULL "targetEntryNew" result
    result' <- (wrapBoxed TargetEntry) result
    freeMem target'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method TargetEntry::copy
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TargetEntry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a pointer to a #GtkTargetEntry"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "TargetEntry" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_target_entry_copy" gtk_target_entry_copy :: 
    Ptr TargetEntry ->                      -- data : TInterface (Name {namespace = "Gtk", name = "TargetEntry"})
    IO (Ptr TargetEntry)

-- | Makes a copy of a t'GI.Gtk.Structs.TargetEntry.TargetEntry' and its data.
targetEntryCopy ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TargetEntry
    -- ^ /@data@/: a pointer to a t'GI.Gtk.Structs.TargetEntry.TargetEntry'
    -> m TargetEntry
    -- ^ __Returns:__ a pointer to a copy of /@data@/.
    --     Free with 'GI.Gtk.Structs.TargetEntry.targetEntryFree'
targetEntryCopy data_ = liftIO $ do
    data_' <- unsafeManagedPtrGetPtr data_
    result <- gtk_target_entry_copy data_'
    checkUnexpectedReturnNULL "targetEntryCopy" result
    result' <- (wrapBoxed TargetEntry) result
    touchManagedPtr data_
    return result'

#if defined(ENABLE_OVERLOADING)
data TargetEntryCopyMethodInfo
instance (signature ~ (m TargetEntry), MonadIO m) => O.OverloadedMethod TargetEntryCopyMethodInfo TargetEntry signature where
    overloadedMethod = targetEntryCopy

instance O.OverloadedMethodInfo TargetEntryCopyMethodInfo TargetEntry where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TargetEntry.targetEntryCopy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TargetEntry.html#v:targetEntryCopy"
        })


#endif

-- method TargetEntry::free
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "data"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TargetEntry" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a pointer to a #GtkTargetEntry."
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

foreign import ccall "gtk_target_entry_free" gtk_target_entry_free :: 
    Ptr TargetEntry ->                      -- data : TInterface (Name {namespace = "Gtk", name = "TargetEntry"})
    IO ()

-- | Frees a t'GI.Gtk.Structs.TargetEntry.TargetEntry' returned from
-- 'GI.Gtk.Structs.TargetEntry.targetEntryNew' or 'GI.Gtk.Structs.TargetEntry.targetEntryCopy'.
targetEntryFree ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    TargetEntry
    -- ^ /@data@/: a pointer to a t'GI.Gtk.Structs.TargetEntry.TargetEntry'.
    -> m ()
targetEntryFree data_ = liftIO $ do
    data_' <- unsafeManagedPtrGetPtr data_
    gtk_target_entry_free data_'
    touchManagedPtr data_
    return ()

#if defined(ENABLE_OVERLOADING)
data TargetEntryFreeMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod TargetEntryFreeMethodInfo TargetEntry signature where
    overloadedMethod = targetEntryFree

instance O.OverloadedMethodInfo TargetEntryFreeMethodInfo TargetEntry where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.TargetEntry.targetEntryFree",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-TargetEntry.html#v:targetEntryFree"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveTargetEntryMethod (t :: Symbol) (o :: *) :: * where
    ResolveTargetEntryMethod "copy" o = TargetEntryCopyMethodInfo
    ResolveTargetEntryMethod "free" o = TargetEntryFreeMethodInfo
    ResolveTargetEntryMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveTargetEntryMethod t TargetEntry, O.OverloadedMethod info TargetEntry p) => OL.IsLabel t (TargetEntry -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveTargetEntryMethod t TargetEntry, O.OverloadedMethod info TargetEntry p, R.HasField t TargetEntry p) => R.HasField t TargetEntry p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveTargetEntryMethod t TargetEntry, O.OverloadedMethodInfo info TargetEntry) => OL.IsLabel t (O.MethodProxy info TargetEntry) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


