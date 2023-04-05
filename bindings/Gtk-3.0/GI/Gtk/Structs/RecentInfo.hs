{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Structs.RecentInfo.RecentInfo'-struct contains private data only, and should
-- be accessed using the provided API.
-- 
-- t'GI.Gtk.Structs.RecentInfo.RecentInfo' constains all the meta-data
-- associated with an entry in the recently used files list.
-- 
-- /Since: 2.10/

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Structs.RecentInfo
    ( 

-- * Exported types
    RecentInfo(..)                          ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [createAppInfo]("GI.Gtk.Structs.RecentInfo#g:method:createAppInfo"), [exists]("GI.Gtk.Structs.RecentInfo#g:method:exists"), [hasApplication]("GI.Gtk.Structs.RecentInfo#g:method:hasApplication"), [hasGroup]("GI.Gtk.Structs.RecentInfo#g:method:hasGroup"), [isLocal]("GI.Gtk.Structs.RecentInfo#g:method:isLocal"), [lastApplication]("GI.Gtk.Structs.RecentInfo#g:method:lastApplication"), [match]("GI.Gtk.Structs.RecentInfo#g:method:match"), [ref]("GI.Gtk.Structs.RecentInfo#g:method:ref"), [unref]("GI.Gtk.Structs.RecentInfo#g:method:unref").
-- 
-- ==== Getters
-- [getAdded]("GI.Gtk.Structs.RecentInfo#g:method:getAdded"), [getAge]("GI.Gtk.Structs.RecentInfo#g:method:getAge"), [getApplicationInfo]("GI.Gtk.Structs.RecentInfo#g:method:getApplicationInfo"), [getApplications]("GI.Gtk.Structs.RecentInfo#g:method:getApplications"), [getDescription]("GI.Gtk.Structs.RecentInfo#g:method:getDescription"), [getDisplayName]("GI.Gtk.Structs.RecentInfo#g:method:getDisplayName"), [getGicon]("GI.Gtk.Structs.RecentInfo#g:method:getGicon"), [getGroups]("GI.Gtk.Structs.RecentInfo#g:method:getGroups"), [getIcon]("GI.Gtk.Structs.RecentInfo#g:method:getIcon"), [getMimeType]("GI.Gtk.Structs.RecentInfo#g:method:getMimeType"), [getModified]("GI.Gtk.Structs.RecentInfo#g:method:getModified"), [getPrivateHint]("GI.Gtk.Structs.RecentInfo#g:method:getPrivateHint"), [getShortName]("GI.Gtk.Structs.RecentInfo#g:method:getShortName"), [getUri]("GI.Gtk.Structs.RecentInfo#g:method:getUri"), [getUriDisplay]("GI.Gtk.Structs.RecentInfo#g:method:getUriDisplay"), [getVisited]("GI.Gtk.Structs.RecentInfo#g:method:getVisited").
-- 
-- ==== Setters
-- /None/.

#if defined(ENABLE_OVERLOADING)
    ResolveRecentInfoMethod                 ,
#endif

-- ** createAppInfo #method:createAppInfo#

#if defined(ENABLE_OVERLOADING)
    RecentInfoCreateAppInfoMethodInfo       ,
#endif
    recentInfoCreateAppInfo                 ,


-- ** exists #method:exists#

#if defined(ENABLE_OVERLOADING)
    RecentInfoExistsMethodInfo              ,
#endif
    recentInfoExists                        ,


-- ** getAdded #method:getAdded#

#if defined(ENABLE_OVERLOADING)
    RecentInfoGetAddedMethodInfo            ,
#endif
    recentInfoGetAdded                      ,


-- ** getAge #method:getAge#

#if defined(ENABLE_OVERLOADING)
    RecentInfoGetAgeMethodInfo              ,
#endif
    recentInfoGetAge                        ,


-- ** getApplicationInfo #method:getApplicationInfo#

#if defined(ENABLE_OVERLOADING)
    RecentInfoGetApplicationInfoMethodInfo  ,
#endif
    recentInfoGetApplicationInfo            ,


-- ** getApplications #method:getApplications#

#if defined(ENABLE_OVERLOADING)
    RecentInfoGetApplicationsMethodInfo     ,
#endif
    recentInfoGetApplications               ,


-- ** getDescription #method:getDescription#

#if defined(ENABLE_OVERLOADING)
    RecentInfoGetDescriptionMethodInfo      ,
#endif
    recentInfoGetDescription                ,


-- ** getDisplayName #method:getDisplayName#

#if defined(ENABLE_OVERLOADING)
    RecentInfoGetDisplayNameMethodInfo      ,
#endif
    recentInfoGetDisplayName                ,


-- ** getGicon #method:getGicon#

#if defined(ENABLE_OVERLOADING)
    RecentInfoGetGiconMethodInfo            ,
#endif
    recentInfoGetGicon                      ,


-- ** getGroups #method:getGroups#

#if defined(ENABLE_OVERLOADING)
    RecentInfoGetGroupsMethodInfo           ,
#endif
    recentInfoGetGroups                     ,


-- ** getIcon #method:getIcon#

#if defined(ENABLE_OVERLOADING)
    RecentInfoGetIconMethodInfo             ,
#endif
    recentInfoGetIcon                       ,


-- ** getMimeType #method:getMimeType#

#if defined(ENABLE_OVERLOADING)
    RecentInfoGetMimeTypeMethodInfo         ,
#endif
    recentInfoGetMimeType                   ,


-- ** getModified #method:getModified#

#if defined(ENABLE_OVERLOADING)
    RecentInfoGetModifiedMethodInfo         ,
#endif
    recentInfoGetModified                   ,


-- ** getPrivateHint #method:getPrivateHint#

#if defined(ENABLE_OVERLOADING)
    RecentInfoGetPrivateHintMethodInfo      ,
#endif
    recentInfoGetPrivateHint                ,


-- ** getShortName #method:getShortName#

#if defined(ENABLE_OVERLOADING)
    RecentInfoGetShortNameMethodInfo        ,
#endif
    recentInfoGetShortName                  ,


-- ** getUri #method:getUri#

#if defined(ENABLE_OVERLOADING)
    RecentInfoGetUriMethodInfo              ,
#endif
    recentInfoGetUri                        ,


-- ** getUriDisplay #method:getUriDisplay#

#if defined(ENABLE_OVERLOADING)
    RecentInfoGetUriDisplayMethodInfo       ,
#endif
    recentInfoGetUriDisplay                 ,


-- ** getVisited #method:getVisited#

#if defined(ENABLE_OVERLOADING)
    RecentInfoGetVisitedMethodInfo          ,
#endif
    recentInfoGetVisited                    ,


-- ** hasApplication #method:hasApplication#

#if defined(ENABLE_OVERLOADING)
    RecentInfoHasApplicationMethodInfo      ,
#endif
    recentInfoHasApplication                ,


-- ** hasGroup #method:hasGroup#

#if defined(ENABLE_OVERLOADING)
    RecentInfoHasGroupMethodInfo            ,
#endif
    recentInfoHasGroup                      ,


-- ** isLocal #method:isLocal#

#if defined(ENABLE_OVERLOADING)
    RecentInfoIsLocalMethodInfo             ,
#endif
    recentInfoIsLocal                       ,


-- ** lastApplication #method:lastApplication#

#if defined(ENABLE_OVERLOADING)
    RecentInfoLastApplicationMethodInfo     ,
#endif
    recentInfoLastApplication               ,


-- ** match #method:match#

#if defined(ENABLE_OVERLOADING)
    RecentInfoMatchMethodInfo               ,
#endif
    recentInfoMatch                         ,


-- ** ref #method:ref#

#if defined(ENABLE_OVERLOADING)
    RecentInfoRefMethodInfo                 ,
#endif
    recentInfoRef                           ,


-- ** unref #method:unref#

#if defined(ENABLE_OVERLOADING)
    RecentInfoUnrefMethodInfo               ,
#endif
    recentInfoUnref                         ,




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

import qualified GI.GdkPixbuf.Objects.Pixbuf as GdkPixbuf.Pixbuf
import qualified GI.Gio.Interfaces.AppInfo as Gio.AppInfo
import qualified GI.Gio.Interfaces.Icon as Gio.Icon

-- | Memory-managed wrapper type.
newtype RecentInfo = RecentInfo (SP.ManagedPtr RecentInfo)
    deriving (Eq)

instance SP.ManagedPtrNewtype RecentInfo where
    toManagedPtr (RecentInfo p) = p

foreign import ccall "gtk_recent_info_get_type" c_gtk_recent_info_get_type :: 
    IO GType

type instance O.ParentTypes RecentInfo = '[]
instance O.HasParentTypes RecentInfo

instance B.Types.TypedObject RecentInfo where
    glibType = c_gtk_recent_info_get_type

instance B.Types.GBoxed RecentInfo

-- | Convert 'RecentInfo' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe RecentInfo) where
    gvalueGType_ = c_gtk_recent_info_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_boxed gv (FP.nullPtr :: FP.Ptr RecentInfo)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_boxed gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_boxed gv :: IO (Ptr RecentInfo)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newBoxed RecentInfo ptr
        else return P.Nothing
        
    


#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList RecentInfo
type instance O.AttributeList RecentInfo = RecentInfoAttributeList
type RecentInfoAttributeList = ('[ ] :: [(Symbol, *)])
#endif

-- method RecentInfo::create_app_info
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "app_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the name of the application that should\n  be mapped to a #GAppInfo; if %NULL is used then the default\n  application for the MIME type is used"
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
-- returnType: Just (TInterface Name { namespace = "Gio" , name = "AppInfo" })
-- throws : True
-- Skip return : False

foreign import ccall "gtk_recent_info_create_app_info" gtk_recent_info_create_app_info :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    CString ->                              -- app_name : TBasicType TUTF8
    Ptr (Ptr GError) ->                     -- error
    IO (Ptr Gio.AppInfo.AppInfo)

-- | Creates a t'GI.Gio.Interfaces.AppInfo.AppInfo' for the specified t'GI.Gtk.Structs.RecentInfo.RecentInfo'
recentInfoCreateAppInfo ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> Maybe (T.Text)
    -- ^ /@appName@/: the name of the application that should
    --   be mapped to a t'GI.Gio.Interfaces.AppInfo.AppInfo'; if 'P.Nothing' is used then the default
    --   application for the MIME type is used
    -> m (Maybe Gio.AppInfo.AppInfo)
    -- ^ __Returns:__ the newly created t'GI.Gio.Interfaces.AppInfo.AppInfo', or 'P.Nothing'.
    --   In case of error, /@error@/ will be set either with a
    --   @/GTK_RECENT_MANAGER_ERROR/@ or a @/G_IO_ERROR/@ /(Can throw 'Data.GI.Base.GError.GError')/
recentInfoCreateAppInfo info appName = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    maybeAppName <- case appName of
        Nothing -> return nullPtr
        Just jAppName -> do
            jAppName' <- textToCString jAppName
            return jAppName'
    onException (do
        result <- propagateGError $ gtk_recent_info_create_app_info info' maybeAppName
        maybeResult <- convertIfNonNull result $ \result' -> do
            result'' <- (wrapObject Gio.AppInfo.AppInfo) result'
            return result''
        touchManagedPtr info
        freeMem maybeAppName
        return maybeResult
     ) (do
        freeMem maybeAppName
     )

#if defined(ENABLE_OVERLOADING)
data RecentInfoCreateAppInfoMethodInfo
instance (signature ~ (Maybe (T.Text) -> m (Maybe Gio.AppInfo.AppInfo)), MonadIO m) => O.OverloadedMethod RecentInfoCreateAppInfoMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoCreateAppInfo

instance O.OverloadedMethodInfo RecentInfoCreateAppInfoMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoCreateAppInfo",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoCreateAppInfo"
        })


#endif

-- method RecentInfo::exists
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_exists" gtk_recent_info_exists :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO CInt

-- | Checks whether the resource pointed by /@info@/ still exists.
-- At the moment this check is done only on resources pointing
-- to local files.
-- 
-- /Since: 2.10/
recentInfoExists ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the resource exists
recentInfoExists info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_exists info'
    let result' = (/= 0) result
    touchManagedPtr info
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentInfoExistsMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod RecentInfoExistsMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoExists

instance O.OverloadedMethodInfo RecentInfoExistsMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoExists",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoExists"
        })


#endif

-- method RecentInfo::get_added
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TLong)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_get_added" gtk_recent_info_get_added :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO CLong

-- | Gets the timestamp (seconds from system’s Epoch) when the resource
-- was added to the recently used resources list.
-- 
-- /Since: 2.10/
recentInfoGetAdded ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m CLong
    -- ^ __Returns:__ the number of seconds elapsed from system’s Epoch when
    --   the resource was added to the list, or -1 on failure.
recentInfoGetAdded info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_get_added info'
    touchManagedPtr info
    return result

#if defined(ENABLE_OVERLOADING)
data RecentInfoGetAddedMethodInfo
instance (signature ~ (m CLong), MonadIO m) => O.OverloadedMethod RecentInfoGetAddedMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoGetAdded

instance O.OverloadedMethodInfo RecentInfoGetAddedMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoGetAdded",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoGetAdded"
        })


#endif

-- method RecentInfo::get_age
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TInt)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_get_age" gtk_recent_info_get_age :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO Int32

-- | Gets the number of days elapsed since the last update
-- of the resource pointed by /@info@/.
-- 
-- /Since: 2.10/
recentInfoGetAge ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m Int32
    -- ^ __Returns:__ a positive integer containing the number of days
    --   elapsed since the time this resource was last modified
recentInfoGetAge info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_get_age info'
    touchManagedPtr info
    return result

#if defined(ENABLE_OVERLOADING)
data RecentInfoGetAgeMethodInfo
instance (signature ~ (m Int32), MonadIO m) => O.OverloadedMethod RecentInfoGetAgeMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoGetAge

instance O.OverloadedMethodInfo RecentInfoGetAgeMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoGetAge",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoGetAge"
        })


#endif

-- method RecentInfo::get_application_info
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "app_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the name of the application that has registered this item"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "app_exec"
--           , argType = TBasicType TUTF8
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "return location for the string containing\n   the command line"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "count"
--           , argType = TBasicType TUInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "return location for the number of times this item was registered"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "time_"
--           , argType = TBasicType TLong
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "return location for the timestamp this item was last registered\n   for this application"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_get_application_info" gtk_recent_info_get_application_info :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    CString ->                              -- app_name : TBasicType TUTF8
    Ptr CString ->                          -- app_exec : TBasicType TUTF8
    Ptr Word32 ->                           -- count : TBasicType TUInt
    Ptr CLong ->                            -- time_ : TBasicType TLong
    IO CInt

-- | Gets the data regarding the application that has registered the resource
-- pointed by /@info@/.
-- 
-- If the command line contains any escape characters defined inside the
-- storage specification, they will be expanded.
-- 
-- /Since: 2.10/
recentInfoGetApplicationInfo ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> T.Text
    -- ^ /@appName@/: the name of the application that has registered this item
    -> m ((Bool, T.Text, Word32, CLong))
    -- ^ __Returns:__ 'P.True' if an application with /@appName@/ has registered this
    --   resource inside the recently used list, or 'P.False' otherwise. The
    --   /@appExec@/ string is owned by the t'GI.Gtk.Structs.RecentInfo.RecentInfo' and should not be
    --   modified or freed
recentInfoGetApplicationInfo info appName = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    appName' <- textToCString appName
    appExec <- callocMem :: IO (Ptr CString)
    count <- allocMem :: IO (Ptr Word32)
    time_ <- allocMem :: IO (Ptr CLong)
    result <- gtk_recent_info_get_application_info info' appName' appExec count time_
    let result' = (/= 0) result
    appExec' <- peek appExec
    appExec'' <- cstringToText appExec'
    count' <- peek count
    time_' <- peek time_
    touchManagedPtr info
    freeMem appName'
    freeMem appExec
    freeMem count
    freeMem time_
    return (result', appExec'', count', time_')

#if defined(ENABLE_OVERLOADING)
data RecentInfoGetApplicationInfoMethodInfo
instance (signature ~ (T.Text -> m ((Bool, T.Text, Word32, CLong))), MonadIO m) => O.OverloadedMethod RecentInfoGetApplicationInfoMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoGetApplicationInfo

instance O.OverloadedMethodInfo RecentInfoGetApplicationInfoMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoGetApplicationInfo",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoGetApplicationInfo"
        })


#endif

-- method RecentInfo::get_applications
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "length"
--           , argType = TBasicType TUInt64
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for the length of the returned list"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Just (TCArray True (-1) 1 (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_get_applications" gtk_recent_info_get_applications :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    Ptr Word64 ->                           -- length : TBasicType TUInt64
    IO (Ptr CString)

-- | Retrieves the list of applications that have registered this resource.
-- 
-- /Since: 2.10/
recentInfoGetApplications ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m (([T.Text], Word64))
    -- ^ __Returns:__ 
    --     a newly allocated 'P.Nothing'-terminated array of strings.
    --     Use 'GI.GLib.Functions.strfreev' to free it.
recentInfoGetApplications info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    length_ <- allocMem :: IO (Ptr Word64)
    result <- gtk_recent_info_get_applications info' length_
    checkUnexpectedReturnNULL "recentInfoGetApplications" result
    result' <- unpackZeroTerminatedUTF8CArray result
    mapZeroTerminatedCArray freeMem result
    freeMem result
    length_' <- peek length_
    touchManagedPtr info
    freeMem length_
    return (result', length_')

#if defined(ENABLE_OVERLOADING)
data RecentInfoGetApplicationsMethodInfo
instance (signature ~ (m (([T.Text], Word64))), MonadIO m) => O.OverloadedMethod RecentInfoGetApplicationsMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoGetApplications

instance O.OverloadedMethodInfo RecentInfoGetApplicationsMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoGetApplications",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoGetApplications"
        })


#endif

-- method RecentInfo::get_description
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_get_description" gtk_recent_info_get_description :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO CString

-- | Gets the (short) description of the resource.
-- 
-- /Since: 2.10/
recentInfoGetDescription ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m T.Text
    -- ^ __Returns:__ the description of the resource. The returned string
    --   is owned by the recent manager, and should not be freed.
recentInfoGetDescription info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_get_description info'
    checkUnexpectedReturnNULL "recentInfoGetDescription" result
    result' <- cstringToText result
    touchManagedPtr info
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentInfoGetDescriptionMethodInfo
instance (signature ~ (m T.Text), MonadIO m) => O.OverloadedMethod RecentInfoGetDescriptionMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoGetDescription

instance O.OverloadedMethodInfo RecentInfoGetDescriptionMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoGetDescription",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoGetDescription"
        })


#endif

-- method RecentInfo::get_display_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_get_display_name" gtk_recent_info_get_display_name :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO CString

-- | Gets the name of the resource. If none has been defined, the basename
-- of the resource is obtained.
-- 
-- /Since: 2.10/
recentInfoGetDisplayName ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m T.Text
    -- ^ __Returns:__ the display name of the resource. The returned string
    --   is owned by the recent manager, and should not be freed.
recentInfoGetDisplayName info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_get_display_name info'
    checkUnexpectedReturnNULL "recentInfoGetDisplayName" result
    result' <- cstringToText result
    touchManagedPtr info
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentInfoGetDisplayNameMethodInfo
instance (signature ~ (m T.Text), MonadIO m) => O.OverloadedMethod RecentInfoGetDisplayNameMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoGetDisplayName

instance O.OverloadedMethodInfo RecentInfoGetDisplayNameMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoGetDisplayName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoGetDisplayName"
        })


#endif

-- method RecentInfo::get_gicon
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gio" , name = "Icon" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_get_gicon" gtk_recent_info_get_gicon :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO (Ptr Gio.Icon.Icon)

-- | Retrieves the icon associated to the resource MIME type.
-- 
-- /Since: 2.22/
recentInfoGetGicon ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m (Maybe Gio.Icon.Icon)
    -- ^ __Returns:__ a t'GI.Gio.Interfaces.Icon.Icon' containing the icon, or 'P.Nothing'.
    --   Use 'GI.GObject.Objects.Object.objectUnref' when finished using the icon
recentInfoGetGicon info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_get_gicon info'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapObject Gio.Icon.Icon) result'
        return result''
    touchManagedPtr info
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data RecentInfoGetGiconMethodInfo
instance (signature ~ (m (Maybe Gio.Icon.Icon)), MonadIO m) => O.OverloadedMethod RecentInfoGetGiconMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoGetGicon

instance O.OverloadedMethodInfo RecentInfoGetGiconMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoGetGicon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoGetGicon"
        })


#endif

-- method RecentInfo::get_groups
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "length"
--           , argType = TBasicType TUInt64
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "return location for the number of groups returned"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Just (TCArray True (-1) 1 (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_get_groups" gtk_recent_info_get_groups :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    Ptr Word64 ->                           -- length : TBasicType TUInt64
    IO (Ptr CString)

-- | Returns all groups registered for the recently used item /@info@/.
-- The array of returned group names will be 'P.Nothing' terminated, so
-- length might optionally be 'P.Nothing'.
-- 
-- /Since: 2.10/
recentInfoGetGroups ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m (([T.Text], Word64))
    -- ^ __Returns:__ 
    --   a newly allocated 'P.Nothing' terminated array of strings.
    --   Use 'GI.GLib.Functions.strfreev' to free it.
recentInfoGetGroups info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    length_ <- allocMem :: IO (Ptr Word64)
    result <- gtk_recent_info_get_groups info' length_
    checkUnexpectedReturnNULL "recentInfoGetGroups" result
    result' <- unpackZeroTerminatedUTF8CArray result
    mapZeroTerminatedCArray freeMem result
    freeMem result
    length_' <- peek length_
    touchManagedPtr info
    freeMem length_
    return (result', length_')

#if defined(ENABLE_OVERLOADING)
data RecentInfoGetGroupsMethodInfo
instance (signature ~ (m (([T.Text], Word64))), MonadIO m) => O.OverloadedMethod RecentInfoGetGroupsMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoGetGroups

instance O.OverloadedMethodInfo RecentInfoGetGroupsMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoGetGroups",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoGetGroups"
        })


#endif

-- method RecentInfo::get_icon
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "size"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the size of the icon in pixels"
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
-- returnType: Just
--               (TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_get_icon" gtk_recent_info_get_icon :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    Int32 ->                                -- size : TBasicType TInt
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

-- | Retrieves the icon of size /@size@/ associated to the resource MIME type.
-- 
-- /Since: 2.10/
recentInfoGetIcon ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> Int32
    -- ^ /@size@/: the size of the icon in pixels
    -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)
    -- ^ __Returns:__ a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf' containing the icon,
    --     or 'P.Nothing'. Use 'GI.GObject.Objects.Object.objectUnref' when finished using the icon.
recentInfoGetIcon info size = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_get_icon info' size
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapObject GdkPixbuf.Pixbuf.Pixbuf) result'
        return result''
    touchManagedPtr info
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data RecentInfoGetIconMethodInfo
instance (signature ~ (Int32 -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)), MonadIO m) => O.OverloadedMethod RecentInfoGetIconMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoGetIcon

instance O.OverloadedMethodInfo RecentInfoGetIconMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoGetIcon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoGetIcon"
        })


#endif

-- method RecentInfo::get_mime_type
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_get_mime_type" gtk_recent_info_get_mime_type :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO CString

-- | Gets the MIME type of the resource.
-- 
-- /Since: 2.10/
recentInfoGetMimeType ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m T.Text
    -- ^ __Returns:__ the MIME type of the resource. The returned string
    --   is owned by the recent manager, and should not be freed.
recentInfoGetMimeType info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_get_mime_type info'
    checkUnexpectedReturnNULL "recentInfoGetMimeType" result
    result' <- cstringToText result
    touchManagedPtr info
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentInfoGetMimeTypeMethodInfo
instance (signature ~ (m T.Text), MonadIO m) => O.OverloadedMethod RecentInfoGetMimeTypeMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoGetMimeType

instance O.OverloadedMethodInfo RecentInfoGetMimeTypeMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoGetMimeType",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoGetMimeType"
        })


#endif

-- method RecentInfo::get_modified
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TLong)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_get_modified" gtk_recent_info_get_modified :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO CLong

-- | Gets the timestamp (seconds from system’s Epoch) when the meta-data
-- for the resource was last modified.
-- 
-- /Since: 2.10/
recentInfoGetModified ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m CLong
    -- ^ __Returns:__ the number of seconds elapsed from system’s Epoch when
    --   the resource was last modified, or -1 on failure.
recentInfoGetModified info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_get_modified info'
    touchManagedPtr info
    return result

#if defined(ENABLE_OVERLOADING)
data RecentInfoGetModifiedMethodInfo
instance (signature ~ (m CLong), MonadIO m) => O.OverloadedMethod RecentInfoGetModifiedMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoGetModified

instance O.OverloadedMethodInfo RecentInfoGetModifiedMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoGetModified",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoGetModified"
        })


#endif

-- method RecentInfo::get_private_hint
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_get_private_hint" gtk_recent_info_get_private_hint :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO CInt

-- | Gets the value of the “private” flag. Resources in the recently used
-- list that have this flag set to 'P.True' should only be displayed by the
-- applications that have registered them.
-- 
-- /Since: 2.10/
recentInfoGetPrivateHint ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the private flag was found, 'P.False' otherwise
recentInfoGetPrivateHint info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_get_private_hint info'
    let result' = (/= 0) result
    touchManagedPtr info
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentInfoGetPrivateHintMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod RecentInfoGetPrivateHintMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoGetPrivateHint

instance O.OverloadedMethodInfo RecentInfoGetPrivateHintMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoGetPrivateHint",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoGetPrivateHint"
        })


#endif

-- method RecentInfo::get_short_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_get_short_name" gtk_recent_info_get_short_name :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO CString

-- | Computes a valid UTF-8 string that can be used as the
-- name of the item in a menu or list. For example, calling
-- this function on an item that refers to
-- “file:\/\/\/foo\/bar.txt” will yield “bar.txt”.
-- 
-- /Since: 2.10/
recentInfoGetShortName ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: an t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m T.Text
    -- ^ __Returns:__ A newly-allocated string in UTF-8 encoding
    --   free it with 'GI.GLib.Functions.free'
recentInfoGetShortName info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_get_short_name info'
    checkUnexpectedReturnNULL "recentInfoGetShortName" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr info
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentInfoGetShortNameMethodInfo
instance (signature ~ (m T.Text), MonadIO m) => O.OverloadedMethod RecentInfoGetShortNameMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoGetShortName

instance O.OverloadedMethodInfo RecentInfoGetShortNameMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoGetShortName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoGetShortName"
        })


#endif

-- method RecentInfo::get_uri
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_get_uri" gtk_recent_info_get_uri :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO CString

-- | Gets the URI of the resource.
-- 
-- /Since: 2.10/
recentInfoGetUri ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m T.Text
    -- ^ __Returns:__ the URI of the resource. The returned string is
    --   owned by the recent manager, and should not be freed.
recentInfoGetUri info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_get_uri info'
    checkUnexpectedReturnNULL "recentInfoGetUri" result
    result' <- cstringToText result
    touchManagedPtr info
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentInfoGetUriMethodInfo
instance (signature ~ (m T.Text), MonadIO m) => O.OverloadedMethod RecentInfoGetUriMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoGetUri

instance O.OverloadedMethodInfo RecentInfoGetUriMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoGetUri",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoGetUri"
        })


#endif

-- method RecentInfo::get_uri_display
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_get_uri_display" gtk_recent_info_get_uri_display :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO CString

-- | Gets a displayable version of the resource’s URI. If the resource
-- is local, it returns a local path; if the resource is not local,
-- it returns the UTF-8 encoded content of 'GI.Gtk.Structs.RecentInfo.recentInfoGetUri'.
-- 
-- /Since: 2.10/
recentInfoGetUriDisplay ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ a newly allocated UTF-8 string containing the
    --   resource’s URI or 'P.Nothing'. Use 'GI.GLib.Functions.free' when done using it.
recentInfoGetUriDisplay info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_get_uri_display info'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        freeMem result'
        return result''
    touchManagedPtr info
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data RecentInfoGetUriDisplayMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m) => O.OverloadedMethod RecentInfoGetUriDisplayMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoGetUriDisplay

instance O.OverloadedMethodInfo RecentInfoGetUriDisplayMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoGetUriDisplay",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoGetUriDisplay"
        })


#endif

-- method RecentInfo::get_visited
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TLong)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_get_visited" gtk_recent_info_get_visited :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO CLong

-- | Gets the timestamp (seconds from system’s Epoch) when the meta-data
-- for the resource was last visited.
-- 
-- /Since: 2.10/
recentInfoGetVisited ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m CLong
    -- ^ __Returns:__ the number of seconds elapsed from system’s Epoch when
    --   the resource was last visited, or -1 on failure.
recentInfoGetVisited info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_get_visited info'
    touchManagedPtr info
    return result

#if defined(ENABLE_OVERLOADING)
data RecentInfoGetVisitedMethodInfo
instance (signature ~ (m CLong), MonadIO m) => O.OverloadedMethod RecentInfoGetVisitedMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoGetVisited

instance O.OverloadedMethodInfo RecentInfoGetVisitedMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoGetVisited",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoGetVisited"
        })


#endif

-- method RecentInfo::has_application
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "app_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a string containing an application name"
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
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_has_application" gtk_recent_info_has_application :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    CString ->                              -- app_name : TBasicType TUTF8
    IO CInt

-- | Checks whether an application registered this resource using /@appName@/.
-- 
-- /Since: 2.10/
recentInfoHasApplication ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> T.Text
    -- ^ /@appName@/: a string containing an application name
    -> m Bool
    -- ^ __Returns:__ 'P.True' if an application with name /@appName@/ was found,
    --   'P.False' otherwise
recentInfoHasApplication info appName = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    appName' <- textToCString appName
    result <- gtk_recent_info_has_application info' appName'
    let result' = (/= 0) result
    touchManagedPtr info
    freeMem appName'
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentInfoHasApplicationMethodInfo
instance (signature ~ (T.Text -> m Bool), MonadIO m) => O.OverloadedMethod RecentInfoHasApplicationMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoHasApplication

instance O.OverloadedMethodInfo RecentInfoHasApplicationMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoHasApplication",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoHasApplication"
        })


#endif

-- method RecentInfo::has_group
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "group_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "name of a group" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_has_group" gtk_recent_info_has_group :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    CString ->                              -- group_name : TBasicType TUTF8
    IO CInt

-- | Checks whether /@groupName@/ appears inside the groups
-- registered for the recently used item /@info@/.
-- 
-- /Since: 2.10/
recentInfoHasGroup ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> T.Text
    -- ^ /@groupName@/: name of a group
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the group was found
recentInfoHasGroup info groupName = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    groupName' <- textToCString groupName
    result <- gtk_recent_info_has_group info' groupName'
    let result' = (/= 0) result
    touchManagedPtr info
    freeMem groupName'
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentInfoHasGroupMethodInfo
instance (signature ~ (T.Text -> m Bool), MonadIO m) => O.OverloadedMethod RecentInfoHasGroupMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoHasGroup

instance O.OverloadedMethodInfo RecentInfoHasGroupMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoHasGroup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoHasGroup"
        })


#endif

-- method RecentInfo::is_local
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_is_local" gtk_recent_info_is_local :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO CInt

-- | Checks whether the resource is local or not by looking at the
-- scheme of its URI.
-- 
-- /Since: 2.10/
recentInfoIsLocal ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the resource is local
recentInfoIsLocal info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_is_local info'
    let result' = (/= 0) result
    touchManagedPtr info
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentInfoIsLocalMethodInfo
instance (signature ~ (m Bool), MonadIO m) => O.OverloadedMethod RecentInfoIsLocalMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoIsLocal

instance O.OverloadedMethodInfo RecentInfoIsLocalMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoIsLocal",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoIsLocal"
        })


#endif

-- method RecentInfo::last_application
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_last_application" gtk_recent_info_last_application :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO CString

-- | Gets the name of the last application that have registered the
-- recently used resource represented by /@info@/.
-- 
-- /Since: 2.10/
recentInfoLastApplication ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m T.Text
    -- ^ __Returns:__ an application name. Use 'GI.GLib.Functions.free' to free it.
recentInfoLastApplication info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_last_application info'
    checkUnexpectedReturnNULL "recentInfoLastApplication" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr info
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentInfoLastApplicationMethodInfo
instance (signature ~ (m T.Text), MonadIO m) => O.OverloadedMethod RecentInfoLastApplicationMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoLastApplication

instance O.OverloadedMethodInfo RecentInfoLastApplicationMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoLastApplication",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoLastApplication"
        })


#endif

-- method RecentInfo::match
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info_a"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "info_b"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_match" gtk_recent_info_match :: 
    Ptr RecentInfo ->                       -- info_a : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    Ptr RecentInfo ->                       -- info_b : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO CInt

-- | Checks whether two t'GI.Gtk.Structs.RecentInfo.RecentInfo'-struct point to the same
-- resource.
-- 
-- /Since: 2.10/
recentInfoMatch ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@infoA@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> RecentInfo
    -- ^ /@infoB@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if both t'GI.Gtk.Structs.RecentInfo.RecentInfo'-struct point to the same
    --   resource, 'P.False' otherwise
recentInfoMatch infoA infoB = liftIO $ do
    infoA' <- unsafeManagedPtrGetPtr infoA
    infoB' <- unsafeManagedPtrGetPtr infoB
    result <- gtk_recent_info_match infoA' infoB'
    let result' = (/= 0) result
    touchManagedPtr infoA
    touchManagedPtr infoB
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentInfoMatchMethodInfo
instance (signature ~ (RecentInfo -> m Bool), MonadIO m) => O.OverloadedMethod RecentInfoMatchMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoMatch

instance O.OverloadedMethodInfo RecentInfoMatchMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoMatch",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoMatch"
        })


#endif

-- method RecentInfo::ref
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "RecentInfo" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_recent_info_ref" gtk_recent_info_ref :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO (Ptr RecentInfo)

-- | Increases the reference count of /@recentInfo@/ by one.
-- 
-- /Since: 2.10/
recentInfoRef ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m RecentInfo
    -- ^ __Returns:__ the recent info object with its reference count
    --     increased by one
recentInfoRef info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    result <- gtk_recent_info_ref info'
    checkUnexpectedReturnNULL "recentInfoRef" result
    result' <- (wrapBoxed RecentInfo) result
    touchManagedPtr info
    return result'

#if defined(ENABLE_OVERLOADING)
data RecentInfoRefMethodInfo
instance (signature ~ (m RecentInfo), MonadIO m) => O.OverloadedMethod RecentInfoRefMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoRef

instance O.OverloadedMethodInfo RecentInfoRefMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoRef",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoRef"
        })


#endif

-- method RecentInfo::unref
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RecentInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkRecentInfo" , sinceVersion = Nothing }
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

foreign import ccall "gtk_recent_info_unref" gtk_recent_info_unref :: 
    Ptr RecentInfo ->                       -- info : TInterface (Name {namespace = "Gtk", name = "RecentInfo"})
    IO ()

-- | Decreases the reference count of /@info@/ by one. If the reference
-- count reaches zero, /@info@/ is deallocated, and the memory freed.
-- 
-- /Since: 2.10/
recentInfoUnref ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    RecentInfo
    -- ^ /@info@/: a t'GI.Gtk.Structs.RecentInfo.RecentInfo'
    -> m ()
recentInfoUnref info = liftIO $ do
    info' <- unsafeManagedPtrGetPtr info
    gtk_recent_info_unref info'
    touchManagedPtr info
    return ()

#if defined(ENABLE_OVERLOADING)
data RecentInfoUnrefMethodInfo
instance (signature ~ (m ()), MonadIO m) => O.OverloadedMethod RecentInfoUnrefMethodInfo RecentInfo signature where
    overloadedMethod = recentInfoUnref

instance O.OverloadedMethodInfo RecentInfoUnrefMethodInfo RecentInfo where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Structs.RecentInfo.recentInfoUnref",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Structs-RecentInfo.html#v:recentInfoUnref"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type family ResolveRecentInfoMethod (t :: Symbol) (o :: *) :: * where
    ResolveRecentInfoMethod "createAppInfo" o = RecentInfoCreateAppInfoMethodInfo
    ResolveRecentInfoMethod "exists" o = RecentInfoExistsMethodInfo
    ResolveRecentInfoMethod "hasApplication" o = RecentInfoHasApplicationMethodInfo
    ResolveRecentInfoMethod "hasGroup" o = RecentInfoHasGroupMethodInfo
    ResolveRecentInfoMethod "isLocal" o = RecentInfoIsLocalMethodInfo
    ResolveRecentInfoMethod "lastApplication" o = RecentInfoLastApplicationMethodInfo
    ResolveRecentInfoMethod "match" o = RecentInfoMatchMethodInfo
    ResolveRecentInfoMethod "ref" o = RecentInfoRefMethodInfo
    ResolveRecentInfoMethod "unref" o = RecentInfoUnrefMethodInfo
    ResolveRecentInfoMethod "getAdded" o = RecentInfoGetAddedMethodInfo
    ResolveRecentInfoMethod "getAge" o = RecentInfoGetAgeMethodInfo
    ResolveRecentInfoMethod "getApplicationInfo" o = RecentInfoGetApplicationInfoMethodInfo
    ResolveRecentInfoMethod "getApplications" o = RecentInfoGetApplicationsMethodInfo
    ResolveRecentInfoMethod "getDescription" o = RecentInfoGetDescriptionMethodInfo
    ResolveRecentInfoMethod "getDisplayName" o = RecentInfoGetDisplayNameMethodInfo
    ResolveRecentInfoMethod "getGicon" o = RecentInfoGetGiconMethodInfo
    ResolveRecentInfoMethod "getGroups" o = RecentInfoGetGroupsMethodInfo
    ResolveRecentInfoMethod "getIcon" o = RecentInfoGetIconMethodInfo
    ResolveRecentInfoMethod "getMimeType" o = RecentInfoGetMimeTypeMethodInfo
    ResolveRecentInfoMethod "getModified" o = RecentInfoGetModifiedMethodInfo
    ResolveRecentInfoMethod "getPrivateHint" o = RecentInfoGetPrivateHintMethodInfo
    ResolveRecentInfoMethod "getShortName" o = RecentInfoGetShortNameMethodInfo
    ResolveRecentInfoMethod "getUri" o = RecentInfoGetUriMethodInfo
    ResolveRecentInfoMethod "getUriDisplay" o = RecentInfoGetUriDisplayMethodInfo
    ResolveRecentInfoMethod "getVisited" o = RecentInfoGetVisitedMethodInfo
    ResolveRecentInfoMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveRecentInfoMethod t RecentInfo, O.OverloadedMethod info RecentInfo p) => OL.IsLabel t (RecentInfo -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveRecentInfoMethod t RecentInfo, O.OverloadedMethod info RecentInfo p, R.HasField t RecentInfo p) => R.HasField t RecentInfo p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveRecentInfoMethod t RecentInfo, O.OverloadedMethodInfo info RecentInfo) => OL.IsLabel t (O.MethodProxy info RecentInfo) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif


