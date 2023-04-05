{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Bookkeeping information about a loadable input method.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.IMContextInfo
    ( 

-- * Exported types
    IMContextInfo(..)                       ,
    newZeroIMContextInfo                    ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveIMContextInfoMethod              ,
#endif



 -- * Properties


-- ** contextId #attr:contextId#
-- | The unique identification string of the input method.

    clearIMContextInfoContextId             ,
    getIMContextInfoContextId               ,
#if defined(ENABLE_OVERLOADING)
    iMContextInfo_contextId                 ,
#endif
    setIMContextInfoContextId               ,


-- ** contextName #attr:contextName#
-- | The human-readable name of the input method.

    clearIMContextInfoContextName           ,
    getIMContextInfoContextName             ,
#if defined(ENABLE_OVERLOADING)
    iMContextInfo_contextName               ,
#endif
    setIMContextInfoContextName             ,


-- ** defaultLocales #attr:defaultLocales#
-- | A colon-separated list of locales where this input method
--   should be the default. The asterisk “*” sets the default for all locales.

    clearIMContextInfoDefaultLocales        ,
    getIMContextInfoDefaultLocales          ,
#if defined(ENABLE_OVERLOADING)
    iMContextInfo_defaultLocales            ,
#endif
    setIMContextInfoDefaultLocales          ,


-- ** domain #attr:domain#
-- | Translation domain to be used with @/dgettext()/@

    clearIMContextInfoDomain                ,
    getIMContextInfoDomain                  ,
#if defined(ENABLE_OVERLOADING)
    iMContextInfo_domain                    ,
#endif
    setIMContextInfoDomain                  ,


-- ** domainDirname #attr:domainDirname#
-- | Name of locale directory for use with @/bindtextdomain()/@

    clearIMContextInfoDomainDirname         ,
    getIMContextInfoDomainDirname           ,
#if defined(ENABLE_OVERLOADING)
    iMContextInfo_domainDirname             ,
#endif
    setIMContextInfoDomainDirname           ,




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
newtype IMContextInfo = IMContextInfo (SP.ManagedPtr IMContextInfo)
    deriving (Eq)

instance SP.ManagedPtrNewtype IMContextInfo where
    toManagedPtr (IMContextInfo p) = p

instance BoxedPtr IMContextInfo where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 40 >=> B.ManagedPtr.wrapPtr IMContextInfo)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr IMContextInfo where
    boxedPtrCalloc = callocBytes 40


-- | Construct a `IMContextInfo` struct initialized to zero.
newZeroIMContextInfo :: MonadIO m => m IMContextInfo
newZeroIMContextInfo = liftIO $ boxedPtrCalloc >>= wrapPtr IMContextInfo

instance tag ~ 'AttrSet => Constructible IMContextInfo tag where
    new _ attrs = do
        o <- newZeroIMContextInfo
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@context_id@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' iMContextInfo #contextId
-- @
getIMContextInfoContextId :: MonadIO m => IMContextInfo -> m (Maybe T.Text)
getIMContextInfoContextId s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@context_id@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' iMContextInfo [ #contextId 'Data.GI.Base.Attributes.:=' value ]
-- @
setIMContextInfoContextId :: MonadIO m => IMContextInfo -> CString -> m ()
setIMContextInfoContextId s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: CString)

-- | Set the value of the “@context_id@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #contextId
-- @
clearIMContextInfoContextId :: MonadIO m => IMContextInfo -> m ()
clearIMContextInfoContextId s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data IMContextInfoContextIdFieldInfo
instance AttrInfo IMContextInfoContextIdFieldInfo where
    type AttrBaseTypeConstraint IMContextInfoContextIdFieldInfo = (~) IMContextInfo
    type AttrAllowedOps IMContextInfoContextIdFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint IMContextInfoContextIdFieldInfo = (~) CString
    type AttrTransferTypeConstraint IMContextInfoContextIdFieldInfo = (~)CString
    type AttrTransferType IMContextInfoContextIdFieldInfo = CString
    type AttrGetType IMContextInfoContextIdFieldInfo = Maybe T.Text
    type AttrLabel IMContextInfoContextIdFieldInfo = "context_id"
    type AttrOrigin IMContextInfoContextIdFieldInfo = IMContextInfo
    attrGet = getIMContextInfoContextId
    attrSet = setIMContextInfoContextId
    attrConstruct = undefined
    attrClear = clearIMContextInfoContextId
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.IMContextInfo.contextId"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-IMContextInfo.html#g:attr:contextId"
        })

iMContextInfo_contextId :: AttrLabelProxy "contextId"
iMContextInfo_contextId = AttrLabelProxy

#endif


-- | Get the value of the “@context_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' iMContextInfo #contextName
-- @
getIMContextInfoContextName :: MonadIO m => IMContextInfo -> m (Maybe T.Text)
getIMContextInfoContextName s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@context_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' iMContextInfo [ #contextName 'Data.GI.Base.Attributes.:=' value ]
-- @
setIMContextInfoContextName :: MonadIO m => IMContextInfo -> CString -> m ()
setIMContextInfoContextName s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: CString)

-- | Set the value of the “@context_name@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #contextName
-- @
clearIMContextInfoContextName :: MonadIO m => IMContextInfo -> m ()
clearIMContextInfoContextName s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data IMContextInfoContextNameFieldInfo
instance AttrInfo IMContextInfoContextNameFieldInfo where
    type AttrBaseTypeConstraint IMContextInfoContextNameFieldInfo = (~) IMContextInfo
    type AttrAllowedOps IMContextInfoContextNameFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint IMContextInfoContextNameFieldInfo = (~) CString
    type AttrTransferTypeConstraint IMContextInfoContextNameFieldInfo = (~)CString
    type AttrTransferType IMContextInfoContextNameFieldInfo = CString
    type AttrGetType IMContextInfoContextNameFieldInfo = Maybe T.Text
    type AttrLabel IMContextInfoContextNameFieldInfo = "context_name"
    type AttrOrigin IMContextInfoContextNameFieldInfo = IMContextInfo
    attrGet = getIMContextInfoContextName
    attrSet = setIMContextInfoContextName
    attrConstruct = undefined
    attrClear = clearIMContextInfoContextName
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.IMContextInfo.contextName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-IMContextInfo.html#g:attr:contextName"
        })

iMContextInfo_contextName :: AttrLabelProxy "contextName"
iMContextInfo_contextName = AttrLabelProxy

#endif


-- | Get the value of the “@domain@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' iMContextInfo #domain
-- @
getIMContextInfoDomain :: MonadIO m => IMContextInfo -> m (Maybe T.Text)
getIMContextInfoDomain s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 16) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@domain@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' iMContextInfo [ #domain 'Data.GI.Base.Attributes.:=' value ]
-- @
setIMContextInfoDomain :: MonadIO m => IMContextInfo -> CString -> m ()
setIMContextInfoDomain s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (val :: CString)

-- | Set the value of the “@domain@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #domain
-- @
clearIMContextInfoDomain :: MonadIO m => IMContextInfo -> m ()
clearIMContextInfoDomain s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data IMContextInfoDomainFieldInfo
instance AttrInfo IMContextInfoDomainFieldInfo where
    type AttrBaseTypeConstraint IMContextInfoDomainFieldInfo = (~) IMContextInfo
    type AttrAllowedOps IMContextInfoDomainFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint IMContextInfoDomainFieldInfo = (~) CString
    type AttrTransferTypeConstraint IMContextInfoDomainFieldInfo = (~)CString
    type AttrTransferType IMContextInfoDomainFieldInfo = CString
    type AttrGetType IMContextInfoDomainFieldInfo = Maybe T.Text
    type AttrLabel IMContextInfoDomainFieldInfo = "domain"
    type AttrOrigin IMContextInfoDomainFieldInfo = IMContextInfo
    attrGet = getIMContextInfoDomain
    attrSet = setIMContextInfoDomain
    attrConstruct = undefined
    attrClear = clearIMContextInfoDomain
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.IMContextInfo.domain"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-IMContextInfo.html#g:attr:domain"
        })

iMContextInfo_domain :: AttrLabelProxy "domain"
iMContextInfo_domain = AttrLabelProxy

#endif


-- | Get the value of the “@domain_dirname@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' iMContextInfo #domainDirname
-- @
getIMContextInfoDomainDirname :: MonadIO m => IMContextInfo -> m (Maybe T.Text)
getIMContextInfoDomainDirname s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 24) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@domain_dirname@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' iMContextInfo [ #domainDirname 'Data.GI.Base.Attributes.:=' value ]
-- @
setIMContextInfoDomainDirname :: MonadIO m => IMContextInfo -> CString -> m ()
setIMContextInfoDomainDirname s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (val :: CString)

-- | Set the value of the “@domain_dirname@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #domainDirname
-- @
clearIMContextInfoDomainDirname :: MonadIO m => IMContextInfo -> m ()
clearIMContextInfoDomainDirname s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data IMContextInfoDomainDirnameFieldInfo
instance AttrInfo IMContextInfoDomainDirnameFieldInfo where
    type AttrBaseTypeConstraint IMContextInfoDomainDirnameFieldInfo = (~) IMContextInfo
    type AttrAllowedOps IMContextInfoDomainDirnameFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint IMContextInfoDomainDirnameFieldInfo = (~) CString
    type AttrTransferTypeConstraint IMContextInfoDomainDirnameFieldInfo = (~)CString
    type AttrTransferType IMContextInfoDomainDirnameFieldInfo = CString
    type AttrGetType IMContextInfoDomainDirnameFieldInfo = Maybe T.Text
    type AttrLabel IMContextInfoDomainDirnameFieldInfo = "domain_dirname"
    type AttrOrigin IMContextInfoDomainDirnameFieldInfo = IMContextInfo
    attrGet = getIMContextInfoDomainDirname
    attrSet = setIMContextInfoDomainDirname
    attrConstruct = undefined
    attrClear = clearIMContextInfoDomainDirname
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.IMContextInfo.domainDirname"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-IMContextInfo.html#g:attr:domainDirname"
        })

iMContextInfo_domainDirname :: AttrLabelProxy "domainDirname"
iMContextInfo_domainDirname = AttrLabelProxy

#endif


-- | Get the value of the “@default_locales@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' iMContextInfo #defaultLocales
-- @
getIMContextInfoDefaultLocales :: MonadIO m => IMContextInfo -> m (Maybe T.Text)
getIMContextInfoDefaultLocales s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 32) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@default_locales@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' iMContextInfo [ #defaultLocales 'Data.GI.Base.Attributes.:=' value ]
-- @
setIMContextInfoDefaultLocales :: MonadIO m => IMContextInfo -> CString -> m ()
setIMContextInfoDefaultLocales s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (val :: CString)

-- | Set the value of the “@default_locales@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #defaultLocales
-- @
clearIMContextInfoDefaultLocales :: MonadIO m => IMContextInfo -> m ()
clearIMContextInfoDefaultLocales s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data IMContextInfoDefaultLocalesFieldInfo
instance AttrInfo IMContextInfoDefaultLocalesFieldInfo where
    type AttrBaseTypeConstraint IMContextInfoDefaultLocalesFieldInfo = (~) IMContextInfo
    type AttrAllowedOps IMContextInfoDefaultLocalesFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint IMContextInfoDefaultLocalesFieldInfo = (~) CString
    type AttrTransferTypeConstraint IMContextInfoDefaultLocalesFieldInfo = (~)CString
    type AttrTransferType IMContextInfoDefaultLocalesFieldInfo = CString
    type AttrGetType IMContextInfoDefaultLocalesFieldInfo = Maybe T.Text
    type AttrLabel IMContextInfoDefaultLocalesFieldInfo = "default_locales"
    type AttrOrigin IMContextInfoDefaultLocalesFieldInfo = IMContextInfo
    attrGet = getIMContextInfoDefaultLocales
    attrSet = setIMContextInfoDefaultLocales
    attrConstruct = undefined
    attrClear = clearIMContextInfoDefaultLocales
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.IMContextInfo.defaultLocales"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-IMContextInfo.html#g:attr:defaultLocales"
        })

iMContextInfo_defaultLocales :: AttrLabelProxy "defaultLocales"
iMContextInfo_defaultLocales = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList IMContextInfo
type instance O.AttributeList IMContextInfo = IMContextInfoAttributeList
type IMContextInfoAttributeList = ('[ '("contextId", IMContextInfoContextIdFieldInfo), '("contextName", IMContextInfoContextNameFieldInfo), '("domain", IMContextInfoDomainFieldInfo), '("domainDirname", IMContextInfoDomainDirnameFieldInfo), '("defaultLocales", IMContextInfoDefaultLocalesFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveIMContextInfoMethod (t :: Symbol) (o :: *) :: * where
    ResolveIMContextInfoMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveIMContextInfoMethod t IMContextInfo, O.OverloadedMethod info IMContextInfo p) => OL.IsLabel t (IMContextInfo -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveIMContextInfoMethod t IMContextInfo, O.OverloadedMethod info IMContextInfo p, R.HasField t IMContextInfo p) => R.HasField t IMContextInfo p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveIMContextInfoMethod t IMContextInfo, O.OverloadedMethodInfo info IMContextInfo) => OL.IsLabel t (O.MethodProxy info IMContextInfo) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


