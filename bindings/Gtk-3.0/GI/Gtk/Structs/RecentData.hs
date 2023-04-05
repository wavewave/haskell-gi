{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Meta-data to be passed to 'GI.Gtk.Objects.RecentManager.recentManagerAddFull' when
-- registering a recently used resource.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.RecentData
    ( 

-- * Exported types
    RecentData(..)                          ,
    newZeroRecentData                       ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveRecentDataMethod                 ,
#endif



 -- * Properties


-- ** appExec #attr:appExec#
-- | command line used to launch this resource; may contain the
--   “%f” and “%u” escape characters which will be expanded
--   to the resource file path and URI respectively when the command line
--   is retrieved;

    clearRecentDataAppExec                  ,
    getRecentDataAppExec                    ,
#if defined(ENABLE_OVERLOADING)
    recentData_appExec                      ,
#endif
    setRecentDataAppExec                    ,


-- ** appName #attr:appName#
-- | the name of the application that is registering this recently
--   used resource;

    clearRecentDataAppName                  ,
    getRecentDataAppName                    ,
#if defined(ENABLE_OVERLOADING)
    recentData_appName                      ,
#endif
    setRecentDataAppName                    ,


-- ** description #attr:description#
-- | a UTF-8 encoded string, containing a short description of
--   the resource, or 'P.Nothing';

    clearRecentDataDescription              ,
    getRecentDataDescription                ,
#if defined(ENABLE_OVERLOADING)
    recentData_description                  ,
#endif
    setRecentDataDescription                ,


-- ** displayName #attr:displayName#
-- | a UTF-8 encoded string, containing the name of the recently
--   used resource to be displayed, or 'P.Nothing';

    clearRecentDataDisplayName              ,
    getRecentDataDisplayName                ,
#if defined(ENABLE_OVERLOADING)
    recentData_displayName                  ,
#endif
    setRecentDataDisplayName                ,


-- ** groups #attr:groups#
-- | a vector of strings containing
--   groups names;

    clearRecentDataGroups                   ,
    getRecentDataGroups                     ,
#if defined(ENABLE_OVERLOADING)
    recentData_groups                       ,
#endif
    setRecentDataGroups                     ,


-- ** isPrivate #attr:isPrivate#
-- | whether this resource should be displayed only by the
--   applications that have registered it or not.

    getRecentDataIsPrivate                  ,
#if defined(ENABLE_OVERLOADING)
    recentData_isPrivate                    ,
#endif
    setRecentDataIsPrivate                  ,


-- ** mimeType #attr:mimeType#
-- | the MIME type of the resource;

    clearRecentDataMimeType                 ,
    getRecentDataMimeType                   ,
#if defined(ENABLE_OVERLOADING)
    recentData_mimeType                     ,
#endif
    setRecentDataMimeType                   ,




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
newtype RecentData = RecentData (SP.ManagedPtr RecentData)
    deriving (Eq)

instance SP.ManagedPtrNewtype RecentData where
    toManagedPtr (RecentData p) = p

instance BoxedPtr RecentData where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 56 >=> B.ManagedPtr.wrapPtr RecentData)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr RecentData where
    boxedPtrCalloc = callocBytes 56


-- | Construct a `RecentData` struct initialized to zero.
newZeroRecentData :: MonadIO m => m RecentData
newZeroRecentData = liftIO $ boxedPtrCalloc >>= wrapPtr RecentData

instance tag ~ 'AttrSet => Constructible RecentData tag where
    new _ attrs = do
        o <- newZeroRecentData
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@display_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentData #displayName
-- @
getRecentDataDisplayName :: MonadIO m => RecentData -> m (Maybe T.Text)
getRecentDataDisplayName s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@display_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentData [ #displayName 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentDataDisplayName :: MonadIO m => RecentData -> CString -> m ()
setRecentDataDisplayName s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: CString)

-- | Set the value of the “@display_name@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #displayName
-- @
clearRecentDataDisplayName :: MonadIO m => RecentData -> m ()
clearRecentDataDisplayName s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data RecentDataDisplayNameFieldInfo
instance AttrInfo RecentDataDisplayNameFieldInfo where
    type AttrBaseTypeConstraint RecentDataDisplayNameFieldInfo = (~) RecentData
    type AttrAllowedOps RecentDataDisplayNameFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RecentDataDisplayNameFieldInfo = (~) CString
    type AttrTransferTypeConstraint RecentDataDisplayNameFieldInfo = (~)CString
    type AttrTransferType RecentDataDisplayNameFieldInfo = CString
    type AttrGetType RecentDataDisplayNameFieldInfo = Maybe T.Text
    type AttrLabel RecentDataDisplayNameFieldInfo = "display_name"
    type AttrOrigin RecentDataDisplayNameFieldInfo = RecentData
    attrGet = getRecentDataDisplayName
    attrSet = setRecentDataDisplayName
    attrConstruct = undefined
    attrClear = clearRecentDataDisplayName
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentData.displayName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentData.html#g:attr:displayName"
        })

recentData_displayName :: AttrLabelProxy "displayName"
recentData_displayName = AttrLabelProxy

#endif


-- | Get the value of the “@description@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentData #description
-- @
getRecentDataDescription :: MonadIO m => RecentData -> m (Maybe T.Text)
getRecentDataDescription s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@description@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentData [ #description 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentDataDescription :: MonadIO m => RecentData -> CString -> m ()
setRecentDataDescription s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: CString)

-- | Set the value of the “@description@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #description
-- @
clearRecentDataDescription :: MonadIO m => RecentData -> m ()
clearRecentDataDescription s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data RecentDataDescriptionFieldInfo
instance AttrInfo RecentDataDescriptionFieldInfo where
    type AttrBaseTypeConstraint RecentDataDescriptionFieldInfo = (~) RecentData
    type AttrAllowedOps RecentDataDescriptionFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RecentDataDescriptionFieldInfo = (~) CString
    type AttrTransferTypeConstraint RecentDataDescriptionFieldInfo = (~)CString
    type AttrTransferType RecentDataDescriptionFieldInfo = CString
    type AttrGetType RecentDataDescriptionFieldInfo = Maybe T.Text
    type AttrLabel RecentDataDescriptionFieldInfo = "description"
    type AttrOrigin RecentDataDescriptionFieldInfo = RecentData
    attrGet = getRecentDataDescription
    attrSet = setRecentDataDescription
    attrConstruct = undefined
    attrClear = clearRecentDataDescription
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentData.description"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentData.html#g:attr:description"
        })

recentData_description :: AttrLabelProxy "description"
recentData_description = AttrLabelProxy

#endif


-- | Get the value of the “@mime_type@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentData #mimeType
-- @
getRecentDataMimeType :: MonadIO m => RecentData -> m (Maybe T.Text)
getRecentDataMimeType s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 16) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@mime_type@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentData [ #mimeType 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentDataMimeType :: MonadIO m => RecentData -> CString -> m ()
setRecentDataMimeType s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (val :: CString)

-- | Set the value of the “@mime_type@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #mimeType
-- @
clearRecentDataMimeType :: MonadIO m => RecentData -> m ()
clearRecentDataMimeType s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data RecentDataMimeTypeFieldInfo
instance AttrInfo RecentDataMimeTypeFieldInfo where
    type AttrBaseTypeConstraint RecentDataMimeTypeFieldInfo = (~) RecentData
    type AttrAllowedOps RecentDataMimeTypeFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RecentDataMimeTypeFieldInfo = (~) CString
    type AttrTransferTypeConstraint RecentDataMimeTypeFieldInfo = (~)CString
    type AttrTransferType RecentDataMimeTypeFieldInfo = CString
    type AttrGetType RecentDataMimeTypeFieldInfo = Maybe T.Text
    type AttrLabel RecentDataMimeTypeFieldInfo = "mime_type"
    type AttrOrigin RecentDataMimeTypeFieldInfo = RecentData
    attrGet = getRecentDataMimeType
    attrSet = setRecentDataMimeType
    attrConstruct = undefined
    attrClear = clearRecentDataMimeType
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentData.mimeType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentData.html#g:attr:mimeType"
        })

recentData_mimeType :: AttrLabelProxy "mimeType"
recentData_mimeType = AttrLabelProxy

#endif


-- | Get the value of the “@app_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentData #appName
-- @
getRecentDataAppName :: MonadIO m => RecentData -> m (Maybe T.Text)
getRecentDataAppName s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 24) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@app_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentData [ #appName 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentDataAppName :: MonadIO m => RecentData -> CString -> m ()
setRecentDataAppName s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (val :: CString)

-- | Set the value of the “@app_name@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #appName
-- @
clearRecentDataAppName :: MonadIO m => RecentData -> m ()
clearRecentDataAppName s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data RecentDataAppNameFieldInfo
instance AttrInfo RecentDataAppNameFieldInfo where
    type AttrBaseTypeConstraint RecentDataAppNameFieldInfo = (~) RecentData
    type AttrAllowedOps RecentDataAppNameFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RecentDataAppNameFieldInfo = (~) CString
    type AttrTransferTypeConstraint RecentDataAppNameFieldInfo = (~)CString
    type AttrTransferType RecentDataAppNameFieldInfo = CString
    type AttrGetType RecentDataAppNameFieldInfo = Maybe T.Text
    type AttrLabel RecentDataAppNameFieldInfo = "app_name"
    type AttrOrigin RecentDataAppNameFieldInfo = RecentData
    attrGet = getRecentDataAppName
    attrSet = setRecentDataAppName
    attrConstruct = undefined
    attrClear = clearRecentDataAppName
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentData.appName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentData.html#g:attr:appName"
        })

recentData_appName :: AttrLabelProxy "appName"
recentData_appName = AttrLabelProxy

#endif


-- | Get the value of the “@app_exec@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentData #appExec
-- @
getRecentDataAppExec :: MonadIO m => RecentData -> m (Maybe T.Text)
getRecentDataAppExec s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 32) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@app_exec@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentData [ #appExec 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentDataAppExec :: MonadIO m => RecentData -> CString -> m ()
setRecentDataAppExec s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (val :: CString)

-- | Set the value of the “@app_exec@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #appExec
-- @
clearRecentDataAppExec :: MonadIO m => RecentData -> m ()
clearRecentDataAppExec s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data RecentDataAppExecFieldInfo
instance AttrInfo RecentDataAppExecFieldInfo where
    type AttrBaseTypeConstraint RecentDataAppExecFieldInfo = (~) RecentData
    type AttrAllowedOps RecentDataAppExecFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RecentDataAppExecFieldInfo = (~) CString
    type AttrTransferTypeConstraint RecentDataAppExecFieldInfo = (~)CString
    type AttrTransferType RecentDataAppExecFieldInfo = CString
    type AttrGetType RecentDataAppExecFieldInfo = Maybe T.Text
    type AttrLabel RecentDataAppExecFieldInfo = "app_exec"
    type AttrOrigin RecentDataAppExecFieldInfo = RecentData
    attrGet = getRecentDataAppExec
    attrSet = setRecentDataAppExec
    attrConstruct = undefined
    attrClear = clearRecentDataAppExec
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentData.appExec"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentData.html#g:attr:appExec"
        })

recentData_appExec :: AttrLabelProxy "appExec"
recentData_appExec = AttrLabelProxy

#endif


-- | Get the value of the “@groups@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentData #groups
-- @
getRecentDataGroups :: MonadIO m => RecentData -> m (Maybe [T.Text])
getRecentDataGroups s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 40) :: IO (Ptr CString)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- unpackZeroTerminatedUTF8CArray val'
        return val''
    return result

-- | Set the value of the “@groups@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentData [ #groups 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentDataGroups :: MonadIO m => RecentData -> Ptr CString -> m ()
setRecentDataGroups s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 40) (val :: Ptr CString)

-- | Set the value of the “@groups@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #groups
-- @
clearRecentDataGroups :: MonadIO m => RecentData -> m ()
clearRecentDataGroups s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 40) (FP.nullPtr :: Ptr CString)

#if defined(ENABLE_OVERLOADING)
data RecentDataGroupsFieldInfo
instance AttrInfo RecentDataGroupsFieldInfo where
    type AttrBaseTypeConstraint RecentDataGroupsFieldInfo = (~) RecentData
    type AttrAllowedOps RecentDataGroupsFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RecentDataGroupsFieldInfo = (~) (Ptr CString)
    type AttrTransferTypeConstraint RecentDataGroupsFieldInfo = (~)(Ptr CString)
    type AttrTransferType RecentDataGroupsFieldInfo = (Ptr CString)
    type AttrGetType RecentDataGroupsFieldInfo = Maybe [T.Text]
    type AttrLabel RecentDataGroupsFieldInfo = "groups"
    type AttrOrigin RecentDataGroupsFieldInfo = RecentData
    attrGet = getRecentDataGroups
    attrSet = setRecentDataGroups
    attrConstruct = undefined
    attrClear = clearRecentDataGroups
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentData.groups"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentData.html#g:attr:groups"
        })

recentData_groups :: AttrLabelProxy "groups"
recentData_groups = AttrLabelProxy

#endif


-- | Get the value of the “@is_private@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentData #isPrivate
-- @
getRecentDataIsPrivate :: MonadIO m => RecentData -> m Bool
getRecentDataIsPrivate s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 48) :: IO CInt
    let val' = (/= 0) val
    return val'

-- | Set the value of the “@is_private@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentData [ #isPrivate 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentDataIsPrivate :: MonadIO m => RecentData -> Bool -> m ()
setRecentDataIsPrivate s val = liftIO $ withManagedPtr s $ \ptr -> do
    let val' = (fromIntegral . fromEnum) val
    poke (ptr `plusPtr` 48) (val' :: CInt)

#if defined(ENABLE_OVERLOADING)
data RecentDataIsPrivateFieldInfo
instance AttrInfo RecentDataIsPrivateFieldInfo where
    type AttrBaseTypeConstraint RecentDataIsPrivateFieldInfo = (~) RecentData
    type AttrAllowedOps RecentDataIsPrivateFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint RecentDataIsPrivateFieldInfo = (~) Bool
    type AttrTransferTypeConstraint RecentDataIsPrivateFieldInfo = (~)Bool
    type AttrTransferType RecentDataIsPrivateFieldInfo = Bool
    type AttrGetType RecentDataIsPrivateFieldInfo = Bool
    type AttrLabel RecentDataIsPrivateFieldInfo = "is_private"
    type AttrOrigin RecentDataIsPrivateFieldInfo = RecentData
    attrGet = getRecentDataIsPrivate
    attrSet = setRecentDataIsPrivate
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentData.isPrivate"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentData.html#g:attr:isPrivate"
        })

recentData_isPrivate :: AttrLabelProxy "isPrivate"
recentData_isPrivate = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList RecentData
type instance O.AttributeList RecentData = RecentDataAttributeList
type RecentDataAttributeList = ('[ '("displayName", RecentDataDisplayNameFieldInfo), '("description", RecentDataDescriptionFieldInfo), '("mimeType", RecentDataMimeTypeFieldInfo), '("appName", RecentDataAppNameFieldInfo), '("appExec", RecentDataAppExecFieldInfo), '("groups", RecentDataGroupsFieldInfo), '("isPrivate", RecentDataIsPrivateFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveRecentDataMethod (t :: Symbol) (o :: *) :: * where
    ResolveRecentDataMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveRecentDataMethod t RecentData, O.OverloadedMethod info RecentData p) => OL.IsLabel t (RecentData -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveRecentDataMethod t RecentData, O.OverloadedMethod info RecentData p, R.HasField t RecentData p) => R.HasField t RecentData p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveRecentDataMethod t RecentData, O.OverloadedMethodInfo info RecentData) => OL.IsLabel t (O.MethodProxy info RecentData) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


