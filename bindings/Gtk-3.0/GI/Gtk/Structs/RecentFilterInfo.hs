{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A GtkRecentFilterInfo struct is used
-- to pass information about the tested file to 'GI.Gtk.Objects.RecentFilter.recentFilterFilter'.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.RecentFilterInfo
    ( 

-- * Exported types
    RecentFilterInfo(..)                    ,
    newZeroRecentFilterInfo                 ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveRecentFilterInfoMethod           ,
#endif



 -- * Properties


-- ** age #attr:age#
-- | The number of days elapsed since the file has been
--    registered.

    getRecentFilterInfoAge                  ,
#if defined(ENABLE_OVERLOADING)
    recentFilterInfo_age                    ,
#endif
    setRecentFilterInfoAge                  ,


-- ** applications #attr:applications#
-- | The list of
--    applications that have registered the file.

    clearRecentFilterInfoApplications       ,
    getRecentFilterInfoApplications         ,
#if defined(ENABLE_OVERLOADING)
    recentFilterInfo_applications           ,
#endif
    setRecentFilterInfoApplications         ,


-- ** contains #attr:contains#
-- | t'GI.Gtk.Flags.RecentFilterFlags' to indicate which fields are set.

    getRecentFilterInfoContains             ,
#if defined(ENABLE_OVERLOADING)
    recentFilterInfo_contains               ,
#endif
    setRecentFilterInfoContains             ,


-- ** displayName #attr:displayName#
-- | The string that will be used to display
--    the file in the recent chooser.

    clearRecentFilterInfoDisplayName        ,
    getRecentFilterInfoDisplayName          ,
#if defined(ENABLE_OVERLOADING)
    recentFilterInfo_displayName            ,
#endif
    setRecentFilterInfoDisplayName          ,


-- ** groups #attr:groups#
-- | The groups to which
--    the file belongs to.

    clearRecentFilterInfoGroups             ,
    getRecentFilterInfoGroups               ,
#if defined(ENABLE_OVERLOADING)
    recentFilterInfo_groups                 ,
#endif
    setRecentFilterInfoGroups               ,


-- ** mimeType #attr:mimeType#
-- | MIME type of the file.

    clearRecentFilterInfoMimeType           ,
    getRecentFilterInfoMimeType             ,
#if defined(ENABLE_OVERLOADING)
    recentFilterInfo_mimeType               ,
#endif
    setRecentFilterInfoMimeType             ,


-- ** uri #attr:uri#
-- | The URI of the file being tested.

    clearRecentFilterInfoUri                ,
    getRecentFilterInfoUri                  ,
#if defined(ENABLE_OVERLOADING)
    recentFilterInfo_uri                    ,
#endif
    setRecentFilterInfoUri                  ,




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

import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags

-- | Memory-managed wrapper type.
newtype RecentFilterInfo = RecentFilterInfo (SP.ManagedPtr RecentFilterInfo)
    deriving (Eq)

instance SP.ManagedPtrNewtype RecentFilterInfo where
    toManagedPtr (RecentFilterInfo p) = p

instance BoxedPtr RecentFilterInfo where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 56 >=> B.ManagedPtr.wrapPtr RecentFilterInfo)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr RecentFilterInfo where
    boxedPtrCalloc = callocBytes 56


-- | Construct a `RecentFilterInfo` struct initialized to zero.
newZeroRecentFilterInfo :: MonadIO m => m RecentFilterInfo
newZeroRecentFilterInfo = liftIO $ boxedPtrCalloc >>= wrapPtr RecentFilterInfo

instance tag ~ 'AttrSet => Constructible RecentFilterInfo tag where
    new _ attrs = do
        o <- newZeroRecentFilterInfo
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@contains@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentFilterInfo #contains
-- @
getRecentFilterInfoContains :: MonadIO m => RecentFilterInfo -> m [Gtk.Flags.RecentFilterFlags]
getRecentFilterInfoContains s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO CUInt
    let val' = wordToGFlags val
    return val'

-- | Set the value of the “@contains@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentFilterInfo [ #contains 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentFilterInfoContains :: MonadIO m => RecentFilterInfo -> [Gtk.Flags.RecentFilterFlags] -> m ()
setRecentFilterInfoContains s val = liftIO $ withManagedPtr s $ \ptr -> do
    let val' = gflagsToWord val
    poke (ptr `plusPtr` 0) (val' :: CUInt)

#if defined(ENABLE_OVERLOADING)
data RecentFilterInfoContainsFieldInfo
instance AttrInfo RecentFilterInfoContainsFieldInfo where
    type AttrBaseTypeConstraint RecentFilterInfoContainsFieldInfo = (~) RecentFilterInfo
    type AttrAllowedOps RecentFilterInfoContainsFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint RecentFilterInfoContainsFieldInfo = (~) [Gtk.Flags.RecentFilterFlags]
    type AttrTransferTypeConstraint RecentFilterInfoContainsFieldInfo = (~)[Gtk.Flags.RecentFilterFlags]
    type AttrTransferType RecentFilterInfoContainsFieldInfo = [Gtk.Flags.RecentFilterFlags]
    type AttrGetType RecentFilterInfoContainsFieldInfo = [Gtk.Flags.RecentFilterFlags]
    type AttrLabel RecentFilterInfoContainsFieldInfo = "contains"
    type AttrOrigin RecentFilterInfoContainsFieldInfo = RecentFilterInfo
    attrGet = getRecentFilterInfoContains
    attrSet = setRecentFilterInfoContains
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentFilterInfo.contains"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentFilterInfo.html#g:attr:contains"
        })

recentFilterInfo_contains :: AttrLabelProxy "contains"
recentFilterInfo_contains = AttrLabelProxy

#endif


-- | Get the value of the “@uri@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentFilterInfo #uri
-- @
getRecentFilterInfoUri :: MonadIO m => RecentFilterInfo -> m (Maybe T.Text)
getRecentFilterInfoUri s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@uri@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentFilterInfo [ #uri 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentFilterInfoUri :: MonadIO m => RecentFilterInfo -> CString -> m ()
setRecentFilterInfoUri s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: CString)

-- | Set the value of the “@uri@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #uri
-- @
clearRecentFilterInfoUri :: MonadIO m => RecentFilterInfo -> m ()
clearRecentFilterInfoUri s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data RecentFilterInfoUriFieldInfo
instance AttrInfo RecentFilterInfoUriFieldInfo where
    type AttrBaseTypeConstraint RecentFilterInfoUriFieldInfo = (~) RecentFilterInfo
    type AttrAllowedOps RecentFilterInfoUriFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RecentFilterInfoUriFieldInfo = (~) CString
    type AttrTransferTypeConstraint RecentFilterInfoUriFieldInfo = (~)CString
    type AttrTransferType RecentFilterInfoUriFieldInfo = CString
    type AttrGetType RecentFilterInfoUriFieldInfo = Maybe T.Text
    type AttrLabel RecentFilterInfoUriFieldInfo = "uri"
    type AttrOrigin RecentFilterInfoUriFieldInfo = RecentFilterInfo
    attrGet = getRecentFilterInfoUri
    attrSet = setRecentFilterInfoUri
    attrConstruct = undefined
    attrClear = clearRecentFilterInfoUri
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentFilterInfo.uri"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentFilterInfo.html#g:attr:uri"
        })

recentFilterInfo_uri :: AttrLabelProxy "uri"
recentFilterInfo_uri = AttrLabelProxy

#endif


-- | Get the value of the “@display_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentFilterInfo #displayName
-- @
getRecentFilterInfoDisplayName :: MonadIO m => RecentFilterInfo -> m (Maybe T.Text)
getRecentFilterInfoDisplayName s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 16) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@display_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentFilterInfo [ #displayName 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentFilterInfoDisplayName :: MonadIO m => RecentFilterInfo -> CString -> m ()
setRecentFilterInfoDisplayName s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (val :: CString)

-- | Set the value of the “@display_name@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #displayName
-- @
clearRecentFilterInfoDisplayName :: MonadIO m => RecentFilterInfo -> m ()
clearRecentFilterInfoDisplayName s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data RecentFilterInfoDisplayNameFieldInfo
instance AttrInfo RecentFilterInfoDisplayNameFieldInfo where
    type AttrBaseTypeConstraint RecentFilterInfoDisplayNameFieldInfo = (~) RecentFilterInfo
    type AttrAllowedOps RecentFilterInfoDisplayNameFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RecentFilterInfoDisplayNameFieldInfo = (~) CString
    type AttrTransferTypeConstraint RecentFilterInfoDisplayNameFieldInfo = (~)CString
    type AttrTransferType RecentFilterInfoDisplayNameFieldInfo = CString
    type AttrGetType RecentFilterInfoDisplayNameFieldInfo = Maybe T.Text
    type AttrLabel RecentFilterInfoDisplayNameFieldInfo = "display_name"
    type AttrOrigin RecentFilterInfoDisplayNameFieldInfo = RecentFilterInfo
    attrGet = getRecentFilterInfoDisplayName
    attrSet = setRecentFilterInfoDisplayName
    attrConstruct = undefined
    attrClear = clearRecentFilterInfoDisplayName
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentFilterInfo.displayName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentFilterInfo.html#g:attr:displayName"
        })

recentFilterInfo_displayName :: AttrLabelProxy "displayName"
recentFilterInfo_displayName = AttrLabelProxy

#endif


-- | Get the value of the “@mime_type@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentFilterInfo #mimeType
-- @
getRecentFilterInfoMimeType :: MonadIO m => RecentFilterInfo -> m (Maybe T.Text)
getRecentFilterInfoMimeType s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 24) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@mime_type@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentFilterInfo [ #mimeType 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentFilterInfoMimeType :: MonadIO m => RecentFilterInfo -> CString -> m ()
setRecentFilterInfoMimeType s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (val :: CString)

-- | Set the value of the “@mime_type@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #mimeType
-- @
clearRecentFilterInfoMimeType :: MonadIO m => RecentFilterInfo -> m ()
clearRecentFilterInfoMimeType s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data RecentFilterInfoMimeTypeFieldInfo
instance AttrInfo RecentFilterInfoMimeTypeFieldInfo where
    type AttrBaseTypeConstraint RecentFilterInfoMimeTypeFieldInfo = (~) RecentFilterInfo
    type AttrAllowedOps RecentFilterInfoMimeTypeFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RecentFilterInfoMimeTypeFieldInfo = (~) CString
    type AttrTransferTypeConstraint RecentFilterInfoMimeTypeFieldInfo = (~)CString
    type AttrTransferType RecentFilterInfoMimeTypeFieldInfo = CString
    type AttrGetType RecentFilterInfoMimeTypeFieldInfo = Maybe T.Text
    type AttrLabel RecentFilterInfoMimeTypeFieldInfo = "mime_type"
    type AttrOrigin RecentFilterInfoMimeTypeFieldInfo = RecentFilterInfo
    attrGet = getRecentFilterInfoMimeType
    attrSet = setRecentFilterInfoMimeType
    attrConstruct = undefined
    attrClear = clearRecentFilterInfoMimeType
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentFilterInfo.mimeType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentFilterInfo.html#g:attr:mimeType"
        })

recentFilterInfo_mimeType :: AttrLabelProxy "mimeType"
recentFilterInfo_mimeType = AttrLabelProxy

#endif


-- | Get the value of the “@applications@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentFilterInfo #applications
-- @
getRecentFilterInfoApplications :: MonadIO m => RecentFilterInfo -> m (Maybe [T.Text])
getRecentFilterInfoApplications s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 32) :: IO (Ptr CString)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- unpackZeroTerminatedUTF8CArray val'
        return val''
    return result

-- | Set the value of the “@applications@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentFilterInfo [ #applications 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentFilterInfoApplications :: MonadIO m => RecentFilterInfo -> Ptr CString -> m ()
setRecentFilterInfoApplications s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (val :: Ptr CString)

-- | Set the value of the “@applications@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #applications
-- @
clearRecentFilterInfoApplications :: MonadIO m => RecentFilterInfo -> m ()
clearRecentFilterInfoApplications s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (FP.nullPtr :: Ptr CString)

#if defined(ENABLE_OVERLOADING)
data RecentFilterInfoApplicationsFieldInfo
instance AttrInfo RecentFilterInfoApplicationsFieldInfo where
    type AttrBaseTypeConstraint RecentFilterInfoApplicationsFieldInfo = (~) RecentFilterInfo
    type AttrAllowedOps RecentFilterInfoApplicationsFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RecentFilterInfoApplicationsFieldInfo = (~) (Ptr CString)
    type AttrTransferTypeConstraint RecentFilterInfoApplicationsFieldInfo = (~)(Ptr CString)
    type AttrTransferType RecentFilterInfoApplicationsFieldInfo = (Ptr CString)
    type AttrGetType RecentFilterInfoApplicationsFieldInfo = Maybe [T.Text]
    type AttrLabel RecentFilterInfoApplicationsFieldInfo = "applications"
    type AttrOrigin RecentFilterInfoApplicationsFieldInfo = RecentFilterInfo
    attrGet = getRecentFilterInfoApplications
    attrSet = setRecentFilterInfoApplications
    attrConstruct = undefined
    attrClear = clearRecentFilterInfoApplications
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentFilterInfo.applications"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentFilterInfo.html#g:attr:applications"
        })

recentFilterInfo_applications :: AttrLabelProxy "applications"
recentFilterInfo_applications = AttrLabelProxy

#endif


-- | Get the value of the “@groups@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentFilterInfo #groups
-- @
getRecentFilterInfoGroups :: MonadIO m => RecentFilterInfo -> m (Maybe [T.Text])
getRecentFilterInfoGroups s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 40) :: IO (Ptr CString)
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- unpackZeroTerminatedUTF8CArray val'
        return val''
    return result

-- | Set the value of the “@groups@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentFilterInfo [ #groups 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentFilterInfoGroups :: MonadIO m => RecentFilterInfo -> Ptr CString -> m ()
setRecentFilterInfoGroups s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 40) (val :: Ptr CString)

-- | Set the value of the “@groups@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #groups
-- @
clearRecentFilterInfoGroups :: MonadIO m => RecentFilterInfo -> m ()
clearRecentFilterInfoGroups s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 40) (FP.nullPtr :: Ptr CString)

#if defined(ENABLE_OVERLOADING)
data RecentFilterInfoGroupsFieldInfo
instance AttrInfo RecentFilterInfoGroupsFieldInfo where
    type AttrBaseTypeConstraint RecentFilterInfoGroupsFieldInfo = (~) RecentFilterInfo
    type AttrAllowedOps RecentFilterInfoGroupsFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint RecentFilterInfoGroupsFieldInfo = (~) (Ptr CString)
    type AttrTransferTypeConstraint RecentFilterInfoGroupsFieldInfo = (~)(Ptr CString)
    type AttrTransferType RecentFilterInfoGroupsFieldInfo = (Ptr CString)
    type AttrGetType RecentFilterInfoGroupsFieldInfo = Maybe [T.Text]
    type AttrLabel RecentFilterInfoGroupsFieldInfo = "groups"
    type AttrOrigin RecentFilterInfoGroupsFieldInfo = RecentFilterInfo
    attrGet = getRecentFilterInfoGroups
    attrSet = setRecentFilterInfoGroups
    attrConstruct = undefined
    attrClear = clearRecentFilterInfoGroups
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentFilterInfo.groups"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentFilterInfo.html#g:attr:groups"
        })

recentFilterInfo_groups :: AttrLabelProxy "groups"
recentFilterInfo_groups = AttrLabelProxy

#endif


-- | Get the value of the “@age@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' recentFilterInfo #age
-- @
getRecentFilterInfoAge :: MonadIO m => RecentFilterInfo -> m Int32
getRecentFilterInfoAge s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 48) :: IO Int32
    return val

-- | Set the value of the “@age@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' recentFilterInfo [ #age 'Data.GI.Base.Attributes.:=' value ]
-- @
setRecentFilterInfoAge :: MonadIO m => RecentFilterInfo -> Int32 -> m ()
setRecentFilterInfoAge s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 48) (val :: Int32)

#if defined(ENABLE_OVERLOADING)
data RecentFilterInfoAgeFieldInfo
instance AttrInfo RecentFilterInfoAgeFieldInfo where
    type AttrBaseTypeConstraint RecentFilterInfoAgeFieldInfo = (~) RecentFilterInfo
    type AttrAllowedOps RecentFilterInfoAgeFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint RecentFilterInfoAgeFieldInfo = (~) Int32
    type AttrTransferTypeConstraint RecentFilterInfoAgeFieldInfo = (~)Int32
    type AttrTransferType RecentFilterInfoAgeFieldInfo = Int32
    type AttrGetType RecentFilterInfoAgeFieldInfo = Int32
    type AttrLabel RecentFilterInfoAgeFieldInfo = "age"
    type AttrOrigin RecentFilterInfoAgeFieldInfo = RecentFilterInfo
    attrGet = getRecentFilterInfoAge
    attrSet = setRecentFilterInfoAge
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentFilterInfo.age"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentFilterInfo.html#g:attr:age"
        })

recentFilterInfo_age :: AttrLabelProxy "age"
recentFilterInfo_age = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList RecentFilterInfo
type instance O.AttributeList RecentFilterInfo = RecentFilterInfoAttributeList
type RecentFilterInfoAttributeList = ('[ '("contains", RecentFilterInfoContainsFieldInfo), '("uri", RecentFilterInfoUriFieldInfo), '("displayName", RecentFilterInfoDisplayNameFieldInfo), '("mimeType", RecentFilterInfoMimeTypeFieldInfo), '("applications", RecentFilterInfoApplicationsFieldInfo), '("groups", RecentFilterInfoGroupsFieldInfo), '("age", RecentFilterInfoAgeFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveRecentFilterInfoMethod (t :: Symbol) (o :: *) :: * where
    ResolveRecentFilterInfoMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveRecentFilterInfoMethod t RecentFilterInfo, O.OverloadedMethod info RecentFilterInfo p) => OL.IsLabel t (RecentFilterInfo -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveRecentFilterInfoMethod t RecentFilterInfo, O.OverloadedMethod info RecentFilterInfo p, R.HasField t RecentFilterInfo p) => R.HasField t RecentFilterInfo p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveRecentFilterInfoMethod t RecentFilterInfo, O.OverloadedMethodInfo info RecentFilterInfo) => OL.IsLabel t (O.MethodProxy info RecentFilterInfo) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


