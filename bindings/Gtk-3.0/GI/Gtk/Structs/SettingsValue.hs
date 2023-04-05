{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- /No description available in the introspection data./

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.SettingsValue
    ( 

-- * Exported types
    SettingsValue(..)                       ,
    newZeroSettingsValue                    ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveSettingsValueMethod              ,
#endif



 -- * Properties


-- ** origin #attr:origin#
-- | Origin should be something like “filename:linenumber” for
--    rc files, or e.g. “XProperty” for other sources.

    clearSettingsValueOrigin                ,
    getSettingsValueOrigin                  ,
    setSettingsValueOrigin                  ,
#if defined(ENABLE_OVERLOADING)
    settingsValue_origin                    ,
#endif


-- ** value #attr:value#
-- | Valid types are LONG, DOUBLE and STRING corresponding to
--    the token parsed, or a GSTRING holding an unparsed statement

    clearSettingsValueValue                 ,
    getSettingsValueValue                   ,
    setSettingsValueValue                   ,
#if defined(ENABLE_OVERLOADING)
    settingsValue_value                     ,
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
newtype SettingsValue = SettingsValue (SP.ManagedPtr SettingsValue)
    deriving (Eq)

instance SP.ManagedPtrNewtype SettingsValue where
    toManagedPtr (SettingsValue p) = p

instance BoxedPtr SettingsValue where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 32 >=> B.ManagedPtr.wrapPtr SettingsValue)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr SettingsValue where
    boxedPtrCalloc = callocBytes 32


-- | Construct a `SettingsValue` struct initialized to zero.
newZeroSettingsValue :: MonadIO m => m SettingsValue
newZeroSettingsValue = liftIO $ boxedPtrCalloc >>= wrapPtr SettingsValue

instance tag ~ 'AttrSet => Constructible SettingsValue tag where
    new _ attrs = do
        o <- newZeroSettingsValue
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@origin@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settingsValue #origin
-- @
getSettingsValueOrigin :: MonadIO m => SettingsValue -> m (Maybe T.Text)
getSettingsValueOrigin s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@origin@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settingsValue [ #origin 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsValueOrigin :: MonadIO m => SettingsValue -> CString -> m ()
setSettingsValueOrigin s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: CString)

-- | Set the value of the “@origin@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #origin
-- @
clearSettingsValueOrigin :: MonadIO m => SettingsValue -> m ()
clearSettingsValueOrigin s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data SettingsValueOriginFieldInfo
instance AttrInfo SettingsValueOriginFieldInfo where
    type AttrBaseTypeConstraint SettingsValueOriginFieldInfo = (~) SettingsValue
    type AttrAllowedOps SettingsValueOriginFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint SettingsValueOriginFieldInfo = (~) CString
    type AttrTransferTypeConstraint SettingsValueOriginFieldInfo = (~)CString
    type AttrTransferType SettingsValueOriginFieldInfo = CString
    type AttrGetType SettingsValueOriginFieldInfo = Maybe T.Text
    type AttrLabel SettingsValueOriginFieldInfo = "origin"
    type AttrOrigin SettingsValueOriginFieldInfo = SettingsValue
    attrGet = getSettingsValueOrigin
    attrSet = setSettingsValueOrigin
    attrConstruct = undefined
    attrClear = clearSettingsValueOrigin
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SettingsValue.origin"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SettingsValue.html#g:attr:origin"
        })

settingsValue_origin :: AttrLabelProxy "origin"
settingsValue_origin = AttrLabelProxy

#endif


-- | Get the value of the “@value@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' settingsValue #value
-- @
getSettingsValueValue :: MonadIO m => SettingsValue -> m (Maybe GValue)
getSettingsValueValue s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO (Ptr GValue)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- B.GValue.newGValueFromPtr val'
        return val''
    return result

-- | Set the value of the “@value@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' settingsValue [ #value 'Data.GI.Base.Attributes.:=' value ]
-- @
setSettingsValueValue :: MonadIO m => SettingsValue -> Ptr GValue -> m ()
setSettingsValueValue s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: Ptr GValue)

-- | Set the value of the “@value@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #value
-- @
clearSettingsValueValue :: MonadIO m => SettingsValue -> m ()
clearSettingsValueValue s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (FP.nullPtr :: Ptr GValue)

#if defined(ENABLE_OVERLOADING)
data SettingsValueValueFieldInfo
instance AttrInfo SettingsValueValueFieldInfo where
    type AttrBaseTypeConstraint SettingsValueValueFieldInfo = (~) SettingsValue
    type AttrAllowedOps SettingsValueValueFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint SettingsValueValueFieldInfo = (~) (Ptr GValue)
    type AttrTransferTypeConstraint SettingsValueValueFieldInfo = (~)(Ptr GValue)
    type AttrTransferType SettingsValueValueFieldInfo = (Ptr GValue)
    type AttrGetType SettingsValueValueFieldInfo = Maybe GValue
    type AttrLabel SettingsValueValueFieldInfo = "value"
    type AttrOrigin SettingsValueValueFieldInfo = SettingsValue
    attrGet = getSettingsValueValue
    attrSet = setSettingsValueValue
    attrConstruct = undefined
    attrClear = clearSettingsValueValue
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.SettingsValue.value"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-SettingsValue.html#g:attr:value"
        })

settingsValue_value :: AttrLabelProxy "value"
settingsValue_value = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList SettingsValue
type instance O.AttributeList SettingsValue = SettingsValueAttributeList
type SettingsValueAttributeList = ('[ '("origin", SettingsValueOriginFieldInfo), '("value", SettingsValueValueFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveSettingsValueMethod (t :: Symbol) (o :: *) :: * where
    ResolveSettingsValueMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveSettingsValueMethod t SettingsValue, O.OverloadedMethod info SettingsValue p) => OL.IsLabel t (SettingsValue -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveSettingsValueMethod t SettingsValue, O.OverloadedMethod info SettingsValue p, R.HasField t SettingsValue p) => R.HasField t SettingsValue p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveSettingsValueMethod t SettingsValue, O.OverloadedMethodInfo info SettingsValue) => OL.IsLabel t (O.MethodProxy info SettingsValue) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


