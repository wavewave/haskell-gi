{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.FixedChild
    ( 

-- * Exported types
    FixedChild(..)                          ,
    newZeroFixedChild                       ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveFixedChildMethod                 ,
#endif



 -- * Properties


-- ** widget #attr:widget#
-- | /No description available in the introspection data./

    clearFixedChildWidget                   ,
#if defined(ENABLE_OVERLOADING)
    fixedChild_widget                       ,
#endif
    getFixedChildWidget                     ,
    setFixedChildWidget                     ,


-- ** x #attr:x#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    fixedChild_x                            ,
#endif
    getFixedChildX                          ,
    setFixedChildX                          ,


-- ** y #attr:y#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    fixedChild_y                            ,
#endif
    getFixedChildY                          ,
    setFixedChildY                          ,




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

import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype FixedChild = FixedChild (SP.ManagedPtr FixedChild)
    deriving (Eq)

instance SP.ManagedPtrNewtype FixedChild where
    toManagedPtr (FixedChild p) = p

instance BoxedPtr FixedChild where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 16 >=> B.ManagedPtr.wrapPtr FixedChild)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr FixedChild where
    boxedPtrCalloc = callocBytes 16


-- | Construct a `FixedChild` struct initialized to zero.
newZeroFixedChild :: MonadIO m => m FixedChild
newZeroFixedChild = liftIO $ boxedPtrCalloc >>= wrapPtr FixedChild

instance tag ~ 'AttrSet => Constructible FixedChild tag where
    new _ attrs = do
        o <- newZeroFixedChild
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@widget@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fixedChild #widget
-- @
getFixedChildWidget :: MonadIO m => FixedChild -> m (Maybe Gtk.Widget.Widget)
getFixedChildWidget s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO (Ptr Gtk.Widget.Widget)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- (newObject Gtk.Widget.Widget) val'
        return val''
    return result

-- | Set the value of the “@widget@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fixedChild [ #widget 'Data.GI.Base.Attributes.:=' value ]
-- @
setFixedChildWidget :: MonadIO m => FixedChild -> Ptr Gtk.Widget.Widget -> m ()
setFixedChildWidget s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: Ptr Gtk.Widget.Widget)

-- | Set the value of the “@widget@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #widget
-- @
clearFixedChildWidget :: MonadIO m => FixedChild -> m ()
clearFixedChildWidget s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (FP.nullPtr :: Ptr Gtk.Widget.Widget)

#if defined(ENABLE_OVERLOADING)
data FixedChildWidgetFieldInfo
instance AttrInfo FixedChildWidgetFieldInfo where
    type AttrBaseTypeConstraint FixedChildWidgetFieldInfo = (~) FixedChild
    type AttrAllowedOps FixedChildWidgetFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint FixedChildWidgetFieldInfo = (~) (Ptr Gtk.Widget.Widget)
    type AttrTransferTypeConstraint FixedChildWidgetFieldInfo = (~)(Ptr Gtk.Widget.Widget)
    type AttrTransferType FixedChildWidgetFieldInfo = (Ptr Gtk.Widget.Widget)
    type AttrGetType FixedChildWidgetFieldInfo = Maybe Gtk.Widget.Widget
    type AttrLabel FixedChildWidgetFieldInfo = "widget"
    type AttrOrigin FixedChildWidgetFieldInfo = FixedChild
    attrGet = getFixedChildWidget
    attrSet = setFixedChildWidget
    attrConstruct = undefined
    attrClear = clearFixedChildWidget
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.FixedChild.widget"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-FixedChild.html#g:attr:widget"
        })

fixedChild_widget :: AttrLabelProxy "widget"
fixedChild_widget = AttrLabelProxy

#endif


-- | Get the value of the “@x@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fixedChild #x
-- @
getFixedChildX :: MonadIO m => FixedChild -> m Int32
getFixedChildX s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO Int32
    return val

-- | Set the value of the “@x@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fixedChild [ #x 'Data.GI.Base.Attributes.:=' value ]
-- @
setFixedChildX :: MonadIO m => FixedChild -> Int32 -> m ()
setFixedChildX s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: Int32)

#if defined(ENABLE_OVERLOADING)
data FixedChildXFieldInfo
instance AttrInfo FixedChildXFieldInfo where
    type AttrBaseTypeConstraint FixedChildXFieldInfo = (~) FixedChild
    type AttrAllowedOps FixedChildXFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint FixedChildXFieldInfo = (~) Int32
    type AttrTransferTypeConstraint FixedChildXFieldInfo = (~)Int32
    type AttrTransferType FixedChildXFieldInfo = Int32
    type AttrGetType FixedChildXFieldInfo = Int32
    type AttrLabel FixedChildXFieldInfo = "x"
    type AttrOrigin FixedChildXFieldInfo = FixedChild
    attrGet = getFixedChildX
    attrSet = setFixedChildX
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.FixedChild.x"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-FixedChild.html#g:attr:x"
        })

fixedChild_x :: AttrLabelProxy "x"
fixedChild_x = AttrLabelProxy

#endif


-- | Get the value of the “@y@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fixedChild #y
-- @
getFixedChildY :: MonadIO m => FixedChild -> m Int32
getFixedChildY s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 12) :: IO Int32
    return val

-- | Set the value of the “@y@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fixedChild [ #y 'Data.GI.Base.Attributes.:=' value ]
-- @
setFixedChildY :: MonadIO m => FixedChild -> Int32 -> m ()
setFixedChildY s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 12) (val :: Int32)

#if defined(ENABLE_OVERLOADING)
data FixedChildYFieldInfo
instance AttrInfo FixedChildYFieldInfo where
    type AttrBaseTypeConstraint FixedChildYFieldInfo = (~) FixedChild
    type AttrAllowedOps FixedChildYFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint FixedChildYFieldInfo = (~) Int32
    type AttrTransferTypeConstraint FixedChildYFieldInfo = (~)Int32
    type AttrTransferType FixedChildYFieldInfo = Int32
    type AttrGetType FixedChildYFieldInfo = Int32
    type AttrLabel FixedChildYFieldInfo = "y"
    type AttrOrigin FixedChildYFieldInfo = FixedChild
    attrGet = getFixedChildY
    attrSet = setFixedChildY
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.FixedChild.y"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-FixedChild.html#g:attr:y"
        })

fixedChild_y :: AttrLabelProxy "y"
fixedChild_y = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList FixedChild
type instance O.AttributeList FixedChild = FixedChildAttributeList
type FixedChildAttributeList = ('[ '("widget", FixedChildWidgetFieldInfo), '("x", FixedChildXFieldInfo), '("y", FixedChildYFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveFixedChildMethod (t :: Symbol) (o :: *) :: * where
    ResolveFixedChildMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveFixedChildMethod t FixedChild, O.OverloadedMethod info FixedChild p) => OL.IsLabel t (FixedChild -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveFixedChildMethod t FixedChild, O.OverloadedMethod info FixedChild p, R.HasField t FixedChild p) => R.HasField t FixedChild p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveFixedChildMethod t FixedChild, O.OverloadedMethodInfo info FixedChild) => OL.IsLabel t (O.MethodProxy info FixedChild) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


