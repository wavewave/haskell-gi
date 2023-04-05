{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Structs.FileFilterInfo.FileFilterInfo'-struct is used to pass information about the
-- tested file to 'GI.Gtk.Objects.FileFilter.fileFilterFilter'.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.FileFilterInfo
    ( 

-- * Exported types
    FileFilterInfo(..)                      ,
    newZeroFileFilterInfo                   ,


 -- * Methods

#if defined(ENABLE_OVERLOADING)
    ResolveFileFilterInfoMethod             ,
#endif



 -- * Properties


-- ** contains #attr:contains#
-- | Flags indicating which of the following fields need
--   are filled

#if defined(ENABLE_OVERLOADING)
    fileFilterInfo_contains                 ,
#endif
    getFileFilterInfoContains               ,
    setFileFilterInfoContains               ,


-- ** displayName #attr:displayName#
-- | the string that will be used to display the file
--   in the file chooser

    clearFileFilterInfoDisplayName          ,
#if defined(ENABLE_OVERLOADING)
    fileFilterInfo_displayName              ,
#endif
    getFileFilterInfoDisplayName            ,
    setFileFilterInfoDisplayName            ,


-- ** filename #attr:filename#
-- | the filename of the file being tested

    clearFileFilterInfoFilename             ,
#if defined(ENABLE_OVERLOADING)
    fileFilterInfo_filename                 ,
#endif
    getFileFilterInfoFilename               ,
    setFileFilterInfoFilename               ,


-- ** mimeType #attr:mimeType#
-- | the mime type of the file

    clearFileFilterInfoMimeType             ,
#if defined(ENABLE_OVERLOADING)
    fileFilterInfo_mimeType                 ,
#endif
    getFileFilterInfoMimeType               ,
    setFileFilterInfoMimeType               ,


-- ** uri #attr:uri#
-- | the URI for the file being tested

    clearFileFilterInfoUri                  ,
#if defined(ENABLE_OVERLOADING)
    fileFilterInfo_uri                      ,
#endif
    getFileFilterInfoUri                    ,
    setFileFilterInfoUri                    ,




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
newtype FileFilterInfo = FileFilterInfo (SP.ManagedPtr FileFilterInfo)
    deriving (Eq)

instance SP.ManagedPtrNewtype FileFilterInfo where
    toManagedPtr (FileFilterInfo p) = p

instance BoxedPtr FileFilterInfo where
    boxedPtrCopy = \p -> B.ManagedPtr.withManagedPtr p (copyBytes 40 >=> B.ManagedPtr.wrapPtr FileFilterInfo)
    boxedPtrFree = \x -> SP.withManagedPtr x SP.freeMem
instance CallocPtr FileFilterInfo where
    boxedPtrCalloc = callocBytes 40


-- | Construct a `FileFilterInfo` struct initialized to zero.
newZeroFileFilterInfo :: MonadIO m => m FileFilterInfo
newZeroFileFilterInfo = liftIO $ boxedPtrCalloc >>= wrapPtr FileFilterInfo

instance tag ~ 'AttrSet => Constructible FileFilterInfo tag where
    new _ attrs = do
        o <- newZeroFileFilterInfo
        GI.Attributes.set o attrs
        return o


-- | Get the value of the “@contains@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fileFilterInfo #contains
-- @
getFileFilterInfoContains :: MonadIO m => FileFilterInfo -> m [Gtk.Flags.FileFilterFlags]
getFileFilterInfoContains s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 0) :: IO CUInt
    let val' = wordToGFlags val
    return val'

-- | Set the value of the “@contains@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fileFilterInfo [ #contains 'Data.GI.Base.Attributes.:=' value ]
-- @
setFileFilterInfoContains :: MonadIO m => FileFilterInfo -> [Gtk.Flags.FileFilterFlags] -> m ()
setFileFilterInfoContains s val = liftIO $ withManagedPtr s $ \ptr -> do
    let val' = gflagsToWord val
    poke (ptr `plusPtr` 0) (val' :: CUInt)

#if defined(ENABLE_OVERLOADING)
data FileFilterInfoContainsFieldInfo
instance AttrInfo FileFilterInfoContainsFieldInfo where
    type AttrBaseTypeConstraint FileFilterInfoContainsFieldInfo = (~) FileFilterInfo
    type AttrAllowedOps FileFilterInfoContainsFieldInfo = '[ 'AttrSet, 'AttrGet]
    type AttrSetTypeConstraint FileFilterInfoContainsFieldInfo = (~) [Gtk.Flags.FileFilterFlags]
    type AttrTransferTypeConstraint FileFilterInfoContainsFieldInfo = (~)[Gtk.Flags.FileFilterFlags]
    type AttrTransferType FileFilterInfoContainsFieldInfo = [Gtk.Flags.FileFilterFlags]
    type AttrGetType FileFilterInfoContainsFieldInfo = [Gtk.Flags.FileFilterFlags]
    type AttrLabel FileFilterInfoContainsFieldInfo = "contains"
    type AttrOrigin FileFilterInfoContainsFieldInfo = FileFilterInfo
    attrGet = getFileFilterInfoContains
    attrSet = setFileFilterInfoContains
    attrConstruct = undefined
    attrClear = undefined
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.FileFilterInfo.contains"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-FileFilterInfo.html#g:attr:contains"
        })

fileFilterInfo_contains :: AttrLabelProxy "contains"
fileFilterInfo_contains = AttrLabelProxy

#endif


-- | Get the value of the “@filename@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fileFilterInfo #filename
-- @
getFileFilterInfoFilename :: MonadIO m => FileFilterInfo -> m (Maybe T.Text)
getFileFilterInfoFilename s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 8) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@filename@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fileFilterInfo [ #filename 'Data.GI.Base.Attributes.:=' value ]
-- @
setFileFilterInfoFilename :: MonadIO m => FileFilterInfo -> CString -> m ()
setFileFilterInfoFilename s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: CString)

-- | Set the value of the “@filename@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #filename
-- @
clearFileFilterInfoFilename :: MonadIO m => FileFilterInfo -> m ()
clearFileFilterInfoFilename s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data FileFilterInfoFilenameFieldInfo
instance AttrInfo FileFilterInfoFilenameFieldInfo where
    type AttrBaseTypeConstraint FileFilterInfoFilenameFieldInfo = (~) FileFilterInfo
    type AttrAllowedOps FileFilterInfoFilenameFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint FileFilterInfoFilenameFieldInfo = (~) CString
    type AttrTransferTypeConstraint FileFilterInfoFilenameFieldInfo = (~)CString
    type AttrTransferType FileFilterInfoFilenameFieldInfo = CString
    type AttrGetType FileFilterInfoFilenameFieldInfo = Maybe T.Text
    type AttrLabel FileFilterInfoFilenameFieldInfo = "filename"
    type AttrOrigin FileFilterInfoFilenameFieldInfo = FileFilterInfo
    attrGet = getFileFilterInfoFilename
    attrSet = setFileFilterInfoFilename
    attrConstruct = undefined
    attrClear = clearFileFilterInfoFilename
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.FileFilterInfo.filename"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-FileFilterInfo.html#g:attr:filename"
        })

fileFilterInfo_filename :: AttrLabelProxy "filename"
fileFilterInfo_filename = AttrLabelProxy

#endif


-- | Get the value of the “@uri@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fileFilterInfo #uri
-- @
getFileFilterInfoUri :: MonadIO m => FileFilterInfo -> m (Maybe T.Text)
getFileFilterInfoUri s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 16) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@uri@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fileFilterInfo [ #uri 'Data.GI.Base.Attributes.:=' value ]
-- @
setFileFilterInfoUri :: MonadIO m => FileFilterInfo -> CString -> m ()
setFileFilterInfoUri s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (val :: CString)

-- | Set the value of the “@uri@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #uri
-- @
clearFileFilterInfoUri :: MonadIO m => FileFilterInfo -> m ()
clearFileFilterInfoUri s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 16) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data FileFilterInfoUriFieldInfo
instance AttrInfo FileFilterInfoUriFieldInfo where
    type AttrBaseTypeConstraint FileFilterInfoUriFieldInfo = (~) FileFilterInfo
    type AttrAllowedOps FileFilterInfoUriFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint FileFilterInfoUriFieldInfo = (~) CString
    type AttrTransferTypeConstraint FileFilterInfoUriFieldInfo = (~)CString
    type AttrTransferType FileFilterInfoUriFieldInfo = CString
    type AttrGetType FileFilterInfoUriFieldInfo = Maybe T.Text
    type AttrLabel FileFilterInfoUriFieldInfo = "uri"
    type AttrOrigin FileFilterInfoUriFieldInfo = FileFilterInfo
    attrGet = getFileFilterInfoUri
    attrSet = setFileFilterInfoUri
    attrConstruct = undefined
    attrClear = clearFileFilterInfoUri
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.FileFilterInfo.uri"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-FileFilterInfo.html#g:attr:uri"
        })

fileFilterInfo_uri :: AttrLabelProxy "uri"
fileFilterInfo_uri = AttrLabelProxy

#endif


-- | Get the value of the “@display_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fileFilterInfo #displayName
-- @
getFileFilterInfoDisplayName :: MonadIO m => FileFilterInfo -> m (Maybe T.Text)
getFileFilterInfoDisplayName s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 24) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@display_name@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fileFilterInfo [ #displayName 'Data.GI.Base.Attributes.:=' value ]
-- @
setFileFilterInfoDisplayName :: MonadIO m => FileFilterInfo -> CString -> m ()
setFileFilterInfoDisplayName s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (val :: CString)

-- | Set the value of the “@display_name@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #displayName
-- @
clearFileFilterInfoDisplayName :: MonadIO m => FileFilterInfo -> m ()
clearFileFilterInfoDisplayName s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 24) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data FileFilterInfoDisplayNameFieldInfo
instance AttrInfo FileFilterInfoDisplayNameFieldInfo where
    type AttrBaseTypeConstraint FileFilterInfoDisplayNameFieldInfo = (~) FileFilterInfo
    type AttrAllowedOps FileFilterInfoDisplayNameFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint FileFilterInfoDisplayNameFieldInfo = (~) CString
    type AttrTransferTypeConstraint FileFilterInfoDisplayNameFieldInfo = (~)CString
    type AttrTransferType FileFilterInfoDisplayNameFieldInfo = CString
    type AttrGetType FileFilterInfoDisplayNameFieldInfo = Maybe T.Text
    type AttrLabel FileFilterInfoDisplayNameFieldInfo = "display_name"
    type AttrOrigin FileFilterInfoDisplayNameFieldInfo = FileFilterInfo
    attrGet = getFileFilterInfoDisplayName
    attrSet = setFileFilterInfoDisplayName
    attrConstruct = undefined
    attrClear = clearFileFilterInfoDisplayName
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.FileFilterInfo.displayName"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-FileFilterInfo.html#g:attr:displayName"
        })

fileFilterInfo_displayName :: AttrLabelProxy "displayName"
fileFilterInfo_displayName = AttrLabelProxy

#endif


-- | Get the value of the “@mime_type@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' fileFilterInfo #mimeType
-- @
getFileFilterInfoMimeType :: MonadIO m => FileFilterInfo -> m (Maybe T.Text)
getFileFilterInfoMimeType s = liftIO $ withManagedPtr s $ \ptr -> do
    val <- peek (ptr `plusPtr` 32) :: IO CString
    result <- SP.convertIfNonNull val $ \val' -> do
        val'' <- cstringToText val'
        return val''
    return result

-- | Set the value of the “@mime_type@” field.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' fileFilterInfo [ #mimeType 'Data.GI.Base.Attributes.:=' value ]
-- @
setFileFilterInfoMimeType :: MonadIO m => FileFilterInfo -> CString -> m ()
setFileFilterInfoMimeType s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (val :: CString)

-- | Set the value of the “@mime_type@” field to `Nothing`.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.clear' #mimeType
-- @
clearFileFilterInfoMimeType :: MonadIO m => FileFilterInfo -> m ()
clearFileFilterInfoMimeType s = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 32) (FP.nullPtr :: CString)

#if defined(ENABLE_OVERLOADING)
data FileFilterInfoMimeTypeFieldInfo
instance AttrInfo FileFilterInfoMimeTypeFieldInfo where
    type AttrBaseTypeConstraint FileFilterInfoMimeTypeFieldInfo = (~) FileFilterInfo
    type AttrAllowedOps FileFilterInfoMimeTypeFieldInfo = '[ 'AttrSet, 'AttrGet, 'AttrClear]
    type AttrSetTypeConstraint FileFilterInfoMimeTypeFieldInfo = (~) CString
    type AttrTransferTypeConstraint FileFilterInfoMimeTypeFieldInfo = (~)CString
    type AttrTransferType FileFilterInfoMimeTypeFieldInfo = CString
    type AttrGetType FileFilterInfoMimeTypeFieldInfo = Maybe T.Text
    type AttrLabel FileFilterInfoMimeTypeFieldInfo = "mime_type"
    type AttrOrigin FileFilterInfoMimeTypeFieldInfo = FileFilterInfo
    attrGet = getFileFilterInfoMimeType
    attrSet = setFileFilterInfoMimeType
    attrConstruct = undefined
    attrClear = clearFileFilterInfoMimeType
    attrTransfer _ v = do
        return v
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.FileFilterInfo.mimeType"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-FileFilterInfo.html#g:attr:mimeType"
        })

fileFilterInfo_mimeType :: AttrLabelProxy "mimeType"
fileFilterInfo_mimeType = AttrLabelProxy

#endif



#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList FileFilterInfo
type instance O.AttributeList FileFilterInfo = FileFilterInfoAttributeList
type FileFilterInfoAttributeList = ('[ '("contains", FileFilterInfoContainsFieldInfo), '("filename", FileFilterInfoFilenameFieldInfo), '("uri", FileFilterInfoUriFieldInfo), '("displayName", FileFilterInfoDisplayNameFieldInfo), '("mimeType", FileFilterInfoMimeTypeFieldInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveFileFilterInfoMethod (t :: Symbol) (o :: *) :: * where
    ResolveFileFilterInfoMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveFileFilterInfoMethod t FileFilterInfo, O.OverloadedMethod info FileFilterInfo p) => OL.IsLabel t (FileFilterInfo -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveFileFilterInfoMethod t FileFilterInfo, O.OverloadedMethod info FileFilterInfo p, R.HasField t FileFilterInfo p) => R.HasField t FileFilterInfo p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveFileFilterInfoMethod t FileFilterInfo, O.OverloadedMethodInfo info FileFilterInfo) => OL.IsLabel t (O.MethodProxy info FileFilterInfo) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


