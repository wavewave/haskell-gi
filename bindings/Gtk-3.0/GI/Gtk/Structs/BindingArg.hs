{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Structs.BindingArg.BindingArg' holds the data associated with
-- an argument for a key binding signal emission as
-- stored in t'GI.Gtk.Structs.BindingSignal.BindingSignal'.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.BindingArg
    ( 

-- * Exported types
    BindingArg(..)                          ,
    newZeroBindingArg                       ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveBindingArgMethod                 ,
#endif



 -- * Properties


-- ** argType #attr:argType#
-- | implementation detail

#if defined(ENABLE_OVERLOADING)
    bindingArg_argType                      ,
#endif
    getBindingArgArgType                    ,
    setBindingArgArgType                    ,




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
newtype BindingArg = BindingArg (SP.ManagedPtr BindingArg)
    deriving (Eq)

instance SP.ManagedPtrNewtype BindingArg where
    toManagedPtr (BindingArg p) = p

instance BoxedPtr BindingArg where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 8 >=> B.ManagedPtr.wrapPtr BindingArg)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr BindingArg where
    boxedPtrCalloc = callocBytes 8


-- | Construct a `BindingArg` struct initialized to zero.
newZeroBindingArg :: MonadIO m => m BindingArg
newZeroBindingArg = liftIO $ boxedPtrCalloc >>= wrapPtr BindingArg

instance tag ~ 'AttrSet => Constructible BindingArg tag where
    new _ attrs = do
        o <- newZeroBindingArg
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@arg_type@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' bindingArg #argType
-- @
getBindingArgArgType :: MonadIO m => BindingArg -> m GType
getBindingArgArgType s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO CGType
    let val' = GType val
    return val'

-- | Set the value of the “@arg_type@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' bindingArg [ #argType 'Data.GI.Base.Attributes.:=' value ]
-- @
setBindingArgArgType :: MonadIO m => BindingArg -> GType -> m ()
setBindingArgArgType s val = liftIO $ withManagedPtr s $ \ptr -> do
    let val' = gtypeToCGType val
    poke (ptr `plusPtr` 0) (val' :: CGType)

#if defined(ENABLE_OVERLOADING)
data BindingArgArgTypeFieldInfo
instance AttrInfo BindingArgArgTypeFieldInfo where
    type AttrBaseTypeConstraint BindingArgArgTypeFieldInfo = (~) BindingArg
    type AttrAllowedOps BindingArgArgTypeFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint BindingArgArgTypeFieldInfo = (~) GType
    type AttrTransferTypeConstraint BindingArgArgTypeFieldInfo = (~)GType
    type AttrTransferType BindingArgArgTypeFieldInfo = GType
    type AttrGetType BindingArgArgTypeFieldInfo = GType
    type AttrLabel BindingArgArgTypeFieldInfo = "arg_type"
    type AttrOrigin BindingArgArgTypeFieldInfo = BindingArg
    attrGet = getBindingArgArgType
    attrSet = setBindingArgArgType
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.BindingArg.argType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-BindingArg.html#g:attr:argType"
        })

bindingArg_argType :: AttrLabelProxy "argType"
bindingArg_argType = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList BindingArg
type instance O.AttributeList BindingArg = BindingArgAttributeList
type BindingArgAttributeList = ('[ '("argType", BindingArgArgTypeFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveBindingArgMethod (t :: Symbol) (o :: *) :: * where
    ResolveBindingArgMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveBindingArgMethod t BindingArg, O.OverloadedMethod info BindingArg p) => OL.IsLabel t (BindingArg -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveBindingArgMethod t BindingArg, O.OverloadedMethod info BindingArg p, R.HasField t BindingArg p) => R.HasField t BindingArg p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveBindingArgMethod t BindingArg, O.OverloadedMethodInfo info BindingArg) => OL.IsLabel t (O.MethodProxy info BindingArg) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


