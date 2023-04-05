{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A struct that specifies a border around a rectangular area
-- that can be of different width on each side.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.Border
    ( 

-- * Exported types
    Border(..)                              ,
    newZeroBorder                           ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [copy]("GI.Gtk.Structs.Border#g:method:copy"), [free]("GI.Gtk.Structs.Border#g:method:free").
-- 
-- ==== Getters
-- /None/.
-- 
-- ==== Setters
-- /None/.

#if defined(ENABLE_OVERLOADING)
    ResolveBorderMethod                     ,
#endif

-- ** copy #method:copy#

#if defined(ENABLE_OVERLOADING)
    BorderCopyMethodInfo                    ,
#endif
    borderCopy                              ,


-- ** free #method:free#

#if defined(ENABLE_OVERLOADING)
    BorderFreeMethodInfo                    ,
#endif
    borderFree                              ,


-- ** new #method:new#

    borderNew                               ,




 -- * Properties


-- ** bottom #attr:bottom#
-- | The width of the bottom border

#if defined(ENABLE_OVERLOADING)
    border_bottom                           ,
#endif
    getBorderBottom                         ,
    setBorderBottom                         ,


-- ** left #attr:left#
-- | The width of the left border

#if defined(ENABLE_OVERLOADING)
    border_left                             ,
#endif
    getBorderLeft                           ,
    setBorderLeft                           ,


-- ** right #attr:right#
-- | The width of the right border

#if defined(ENABLE_OVERLOADING)
    border_right                            ,
#endif
    getBorderRight                          ,
    setBorderRight                          ,


-- ** top #attr:top#
-- | The width of the top border

#if defined(ENABLE_OVERLOADING)
    border_top                              ,
#endif
    getBorderTop                            ,
    setBorderTop                            ,




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
newtype Border = Border (SP.ManagedPtr Border)
    deriving (Eq)

instance SP.ManagedPtrNewtype Border where
    toManagedPtr (Border p) = p

foreign import ccall "gtk_border_get_type" c_gtk_border_get_type :: 
    IO GType

type instance O.ParentTypes Border = '[]
instance O.HasParentTypes Border

instance B.Types.TypedObject Border where
    glibType = c_gtk_border_get_type

instance B.Types.GBoxed Border

-- | Convert 'Border' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Border) where
    gvalueGType_ = c_gtk_border_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_boxed gv (FP.nullPtr :: FP.Ptr Border)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_boxed gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_boxed gv :: IO (Ptr Border)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newBoxed Border ptr
        else return P.Nothing
        
    

-- | Construct a `Border` struct initialized to zero.
newZeroBorder :: MonadIO m => m Border
newZeroBorder = liftIO $ callocBoxedBytes 8 >>= wrapBoxed Border

instance tag ~ 'AttrSet => Constructible Border tag where
    new _ attrs = do
        o <- newZeroBorder
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@left@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' border #left
-- @
getBorderLeft :: MonadIO m => Border -> m Int16
getBorderLeft s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO Int16
    return val

-- | Set the value of the “@left@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' border [ #left 'Data.GI.Base.Attributes.:=' value ]
-- @
setBorderLeft :: MonadIO m => Border -> Int16 -> m ()
setBorderLeft s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: Int16)

#if defined(ENABLE_OVERLOADING)
data BorderLeftFieldInfo
instance AttrInfo BorderLeftFieldInfo where
    type AttrBaseTypeConstraint BorderLeftFieldInfo = (~) Border
    type AttrAllowedOps BorderLeftFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint BorderLeftFieldInfo = (~) Int16
    type AttrTransferTypeConstraint BorderLeftFieldInfo = (~)Int16
    type AttrTransferType BorderLeftFieldInfo = Int16
    type AttrGetType BorderLeftFieldInfo = Int16
    type AttrLabel BorderLeftFieldInfo = "left"
    type AttrOrigin BorderLeftFieldInfo = Border
    attrGet = getBorderLeft
    attrSet = setBorderLeft
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.Border.left"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-Border.html#g:attr:left"
        })

border_left :: AttrLabelProxy "left"
border_left = AttrLabelProxy

#endif


-- | Get the value of the “@right@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' border #right
-- @
getBorderRight :: MonadIO m => Border -> m Int16
getBorderRight s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 2) :: IO Int16
    return val

-- | Set the value of the “@right@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' border [ #right 'Data.GI.Base.Attributes.:=' value ]
-- @
setBorderRight :: MonadIO m => Border -> Int16 -> m ()
setBorderRight s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 2) (val :: Int16)

#if defined(ENABLE_OVERLOADING)
data BorderRightFieldInfo
instance AttrInfo BorderRightFieldInfo where
    type AttrBaseTypeConstraint BorderRightFieldInfo = (~) Border
    type AttrAllowedOps BorderRightFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint BorderRightFieldInfo = (~) Int16
    type AttrTransferTypeConstraint BorderRightFieldInfo = (~)Int16
    type AttrTransferType BorderRightFieldInfo = Int16
    type AttrGetType BorderRightFieldInfo = Int16
    type AttrLabel BorderRightFieldInfo = "right"
    type AttrOrigin BorderRightFieldInfo = Border
    attrGet = getBorderRight
    attrSet = setBorderRight
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.Border.right"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-Border.html#g:attr:right"
        })

border_right :: AttrLabelProxy "right"
border_right = AttrLabelProxy

#endif


-- | Get the value of the “@top@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' border #top
-- @
getBorderTop :: MonadIO m => Border -> m Int16
getBorderTop s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 4) :: IO Int16
    return val

-- | Set the value of the “@top@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' border [ #top 'Data.GI.Base.Attributes.:=' value ]
-- @
setBorderTop :: MonadIO m => Border -> Int16 -> m ()
setBorderTop s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 4) (val :: Int16)

#if defined(ENABLE_OVERLOADING)
data BorderTopFieldInfo
instance AttrInfo BorderTopFieldInfo where
    type AttrBaseTypeConstraint BorderTopFieldInfo = (~) Border
    type AttrAllowedOps BorderTopFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint BorderTopFieldInfo = (~) Int16
    type AttrTransferTypeConstraint BorderTopFieldInfo = (~)Int16
    type AttrTransferType BorderTopFieldInfo = Int16
    type AttrGetType BorderTopFieldInfo = Int16
    type AttrLabel BorderTopFieldInfo = "top"
    type AttrOrigin BorderTopFieldInfo = Border
    attrGet = getBorderTop
    attrSet = setBorderTop
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.Border.top"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-Border.html#g:attr:top"
        })

border_top :: AttrLabelProxy "top"
border_top = AttrLabelProxy

#endif


-- | Get the value of the “@bottom@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' border #bottom
-- @
getBorderBottom :: MonadIO m => Border -> m Int16
getBorderBottom s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 6) :: IO Int16
    return val

-- | Set the value of the “@bottom@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' border [ #bottom 'Data.GI.Base.Attributes.:=' value ]
-- @
setBorderBottom :: MonadIO m => Border -> Int16 -> m ()
setBorderBottom s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 6) (val :: Int16)

#if defined(ENABLE_OVERLOADING)
data BorderBottomFieldInfo
instance AttrInfo BorderBottomFieldInfo where
    type AttrBaseTypeConstraint BorderBottomFieldInfo = (~) Border
    type AttrAllowedOps BorderBottomFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint BorderBottomFieldInfo = (~) Int16
    type AttrTransferTypeConstraint BorderBottomFieldInfo = (~)Int16
    type AttrTransferType BorderBottomFieldInfo = Int16
    type AttrGetType BorderBottomFieldInfo = Int16
    type AttrLabel BorderBottomFieldInfo = "bottom"
    type AttrOrigin BorderBottomFieldInfo = Border
    attrGet = getBorderBottom
    attrSet = setBorderBottom
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.Border.bottom"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-Border.html#g:attr:bottom"
        })

border_bottom :: AttrLabelProxy "bottom"
border_bottom = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Border
type instance O.AttributeList Border = BorderAttributeList
type BorderAttributeList = ('[ '("left", BorderLeftFieldInfo), '("right", BorderRightFieldInfo), '("top", BorderTopFieldInfo), '("bottom", BorderBottomFieldInfo)] :: [(Symbol, *)])
#endif

-- method Border::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Border" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_border_new" gtk_border_new :: 
    IO (Ptr Border)

-- | Allocates a new t'GI.Gtk.Structs.Border.Border'-struct and initializes its elements to zero.
-- 
-- /Since: 2.14/
borderNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Border
    -- ^ __Returns:__ a newly allocated t'GI.Gtk.Structs.Border.Border'-struct.
    --  Free with 'GI.Gtk.Structs.Border.borderFree'
borderNew  = liftIO $ do
    result <- gtk_border_new
    checkUnexpectedReturnNULL "borderNew" result
    result' <- (wrapBoxed Border) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Border::copy
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "border_"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Border" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBorder-struct"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Border" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_border_copy" gtk_border_copy :: 
    Ptr Border ->                           -- border_ : TInterface (Name {namespace = "Gtk", name = "Border"})
    IO (Ptr Border)

-- | Copies a t'GI.Gtk.Structs.Border.Border'-struct.
borderCopy ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Border
    -- ^ /@border_@/: a t'GI.Gtk.Structs.Border.Border'-struct
    -> m Border
    -- ^ __Returns:__ a copy of /@border_@/.
borderCopy border_ = liftIO $ do
    border_' <- unsafeManagedPtrGetPtr border_
    result <- gtk_border_copy border_'
    checkUnexpectedReturnNULL "borderCopy" result
    result' <- (wrapBoxed Border) result
    touchManagedPtr border_
    return result'

#if defined(ENABLE_OVERLOADING)
data BorderCopyMethodInfo
instance (signature ~ (m Border), MonadIO m) => O.OverloadedMethod BorderCopyMethodInfo Border signature where
    overloadedMethod = borderCopy

instance O.OverloadedMethodInfo BorderCopyMethodInfo Border where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.Border.borderCopy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-Border.html#v:borderCopy"
        })


#endif

-- method Border::free
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "border_"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Border" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkBorder-struct"
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

foreign import ccall "gtk_border_free" gtk_border_free :: 
    Ptr Border ->                           -- border_ : TInterface (Name {namespace = "Gtk", name = "Border"})
    IO ()

-- | Frees a t'GI.Gtk.Structs.Border.Border'-struct.
borderFree ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    Border
    -- ^ /@border_@/: a t'GI.Gtk.Structs.Border.Border'-struct
    -> m ()
borderFree border_ = liftIO $ do
    border_' <- unsafeManagedPtrGetPtr border_
    gtk_border_free border_'
    touchManagedPtr border_
    return ()

#if defined(ENABLE_OVERLOADING)
data BorderFreeMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod BorderFreeMethodInfo Border signature where
    overloadedMethod = borderFree

instance O.OverloadedMethodInfo BorderFreeMethodInfo Border where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.Border.borderFree",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-Border.html#v:borderFree"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveBorderMethod (t :: Symbol) (o :: *) :: * where
    ResolveBorderMethod "copy" o = BorderCopyMethodInfo
    ResolveBorderMethod "free" o = BorderFreeMethodInfo
    ResolveBorderMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveBorderMethod t Border, O.OverloadedMethod info Border p) => OL.IsLabel t (Border -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveBorderMethod t Border, O.OverloadedMethod info Border p, R.HasField t Border p) => R.HasField t Border p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveBorderMethod t Border, O.OverloadedMethodInfo info Border) => OL.IsLabel t (O.MethodProxy info Border) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


