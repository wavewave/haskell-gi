{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- Contains information found when looking up an icon in
-- an icon theme.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.IconInfo
    ( 

-- * Exported types
    IconInfo(..)                            ,
    IsIconInfo                              ,
    toIconInfo                              ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [isSymbolic]("GI.Gtk.Objects.IconInfo#g:method:isSymbolic"), [loadIcon]("GI.Gtk.Objects.IconInfo#g:method:loadIcon"), [loadIconAsync]("GI.Gtk.Objects.IconInfo#g:method:loadIconAsync"), [loadIconFinish]("GI.Gtk.Objects.IconInfo#g:method:loadIconFinish"), [loadSurface]("GI.Gtk.Objects.IconInfo#g:method:loadSurface"), [loadSymbolic]("GI.Gtk.Objects.IconInfo#g:method:loadSymbolic"), [loadSymbolicAsync]("GI.Gtk.Objects.IconInfo#g:method:loadSymbolicAsync"), [loadSymbolicFinish]("GI.Gtk.Objects.IconInfo#g:method:loadSymbolicFinish"), [loadSymbolicForContext]("GI.Gtk.Objects.IconInfo#g:method:loadSymbolicForContext"), [loadSymbolicForContextAsync]("GI.Gtk.Objects.IconInfo#g:method:loadSymbolicForContextAsync"), [loadSymbolicForContextFinish]("GI.Gtk.Objects.IconInfo#g:method:loadSymbolicForContextFinish"), [loadSymbolicForStyle]("GI.Gtk.Objects.IconInfo#g:method:loadSymbolicForStyle"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getAttachPoints]("GI.Gtk.Objects.IconInfo#g:method:getAttachPoints"), [getBaseScale]("GI.Gtk.Objects.IconInfo#g:method:getBaseScale"), [getBaseSize]("GI.Gtk.Objects.IconInfo#g:method:getBaseSize"), [getBuiltinPixbuf]("GI.Gtk.Objects.IconInfo#g:method:getBuiltinPixbuf"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDisplayName]("GI.Gtk.Objects.IconInfo#g:method:getDisplayName"), [getEmbeddedRect]("GI.Gtk.Objects.IconInfo#g:method:getEmbeddedRect"), [getFilename]("GI.Gtk.Objects.IconInfo#g:method:getFilename"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setRawCoordinates]("GI.Gtk.Objects.IconInfo#g:method:setRawCoordinates").

#if defined(ENABLE_OVERLOADING)
    ResolveIconInfoMethod                   ,
#endif

-- ** getAttachPoints #method:getAttachPoints#

#if defined(ENABLE_OVERLOADING)
    IconInfoGetAttachPointsMethodInfo       ,
#endif
    iconInfoGetAttachPoints                 ,


-- ** getBaseScale #method:getBaseScale#

#if defined(ENABLE_OVERLOADING)
    IconInfoGetBaseScaleMethodInfo          ,
#endif
    iconInfoGetBaseScale                    ,


-- ** getBaseSize #method:getBaseSize#

#if defined(ENABLE_OVERLOADING)
    IconInfoGetBaseSizeMethodInfo           ,
#endif
    iconInfoGetBaseSize                     ,


-- ** getBuiltinPixbuf #method:getBuiltinPixbuf#

#if defined(ENABLE_OVERLOADING)
    IconInfoGetBuiltinPixbufMethodInfo      ,
#endif
    iconInfoGetBuiltinPixbuf                ,


-- ** getDisplayName #method:getDisplayName#

#if defined(ENABLE_OVERLOADING)
    IconInfoGetDisplayNameMethodInfo        ,
#endif
    iconInfoGetDisplayName                  ,


-- ** getEmbeddedRect #method:getEmbeddedRect#

#if defined(ENABLE_OVERLOADING)
    IconInfoGetEmbeddedRectMethodInfo       ,
#endif
    iconInfoGetEmbeddedRect                 ,


-- ** getFilename #method:getFilename#

#if defined(ENABLE_OVERLOADING)
    IconInfoGetFilenameMethodInfo           ,
#endif
    iconInfoGetFilename                     ,


-- ** isSymbolic #method:isSymbolic#

#if defined(ENABLE_OVERLOADING)
    IconInfoIsSymbolicMethodInfo            ,
#endif
    iconInfoIsSymbolic                      ,


-- ** loadIcon #method:loadIcon#

#if defined(ENABLE_OVERLOADING)
    IconInfoLoadIconMethodInfo              ,
#endif
    iconInfoLoadIcon                        ,


-- ** loadIconAsync #method:loadIconAsync#

#if defined(ENABLE_OVERLOADING)
    IconInfoLoadIconAsyncMethodInfo         ,
#endif
    iconInfoLoadIconAsync                   ,


-- ** loadIconFinish #method:loadIconFinish#

#if defined(ENABLE_OVERLOADING)
    IconInfoLoadIconFinishMethodInfo        ,
#endif
    iconInfoLoadIconFinish                  ,


-- ** loadSurface #method:loadSurface#

#if defined(ENABLE_OVERLOADING)
    IconInfoLoadSurfaceMethodInfo           ,
#endif
    iconInfoLoadSurface                     ,


-- ** loadSymbolic #method:loadSymbolic#

#if defined(ENABLE_OVERLOADING)
    IconInfoLoadSymbolicMethodInfo          ,
#endif
    iconInfoLoadSymbolic                    ,


-- ** loadSymbolicAsync #method:loadSymbolicAsync#

#if defined(ENABLE_OVERLOADING)
    IconInfoLoadSymbolicAsyncMethodInfo     ,
#endif
    iconInfoLoadSymbolicAsync               ,


-- ** loadSymbolicFinish #method:loadSymbolicFinish#

#if defined(ENABLE_OVERLOADING)
    IconInfoLoadSymbolicFinishMethodInfo    ,
#endif
    iconInfoLoadSymbolicFinish              ,


-- ** loadSymbolicForContext #method:loadSymbolicForContext#

#if defined(ENABLE_OVERLOADING)
    IconInfoLoadSymbolicForContextMethodInfo,
#endif
    iconInfoLoadSymbolicForContext          ,


-- ** loadSymbolicForContextAsync #method:loadSymbolicForContextAsync#

#if defined(ENABLE_OVERLOADING)
    IconInfoLoadSymbolicForContextAsyncMethodInfo,
#endif
    iconInfoLoadSymbolicForContextAsync     ,


-- ** loadSymbolicForContextFinish #method:loadSymbolicForContextFinish#

#if defined(ENABLE_OVERLOADING)
    IconInfoLoadSymbolicForContextFinishMethodInfo,
#endif
    iconInfoLoadSymbolicForContextFinish    ,


-- ** loadSymbolicForStyle #method:loadSymbolicForStyle#

#if defined(ENABLE_OVERLOADING)
    IconInfoLoadSymbolicForStyleMethodInfo  ,
#endif
    iconInfoLoadSymbolicForStyle            ,


-- ** newForPixbuf #method:newForPixbuf#

    iconInfoNewForPixbuf                    ,


-- ** setRawCoordinates #method:setRawCoordinates#

#if defined(ENABLE_OVERLOADING)
    IconInfoSetRawCoordinatesMethodInfo     ,
#endif
    iconInfoSetRawCoordinates               ,




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

import qualified GI.Cairo.Structs.Surface as Cairo.Surface
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gdk.Objects.Window as Gdk.Window
import qualified GI.Gdk.Structs.Point as Gdk.Point
import qualified GI.Gdk.Structs.RGBA as Gdk.RGBA
import qualified GI.Gdk.Structs.Rectangle as Gdk.Rectangle
import qualified GI.GdkPixbuf.Objects.Pixbuf as GdkPixbuf.Pixbuf
import qualified GI.Gio.Callbacks as Gio.Callbacks
import qualified GI.Gio.Interfaces.AsyncResult as Gio.AsyncResult
import qualified GI.Gio.Objects.Cancellable as Gio.Cancellable
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Objects.IconTheme as Gtk.IconTheme
import {-# SOURCE #-} qualified GI.Gtk.Objects.Style as Gtk.Style
import {-# SOURCE #-} qualified GI.Gtk.Objects.StyleContext as Gtk.StyleContext

-- | Memory-managed wrapper type.
newtype IconInfo = IconInfo (SP.ManagedPtr IconInfo)
    deriving (Eq)

instance SP.ManagedPtrNewtype IconInfo where
    toManagedPtr (IconInfo p) = p

foreign import ccall "gtk_icon_info_get_type"
    c_gtk_icon_info_get_type :: IO B.Types.GType

instance B.Types.TypedObject IconInfo where
    glibType = c_gtk_icon_info_get_type

instance B.Types.GObject IconInfo

-- | Type class for types which can be safely cast to `IconInfo`, for instance with `toIconInfo`.
class (SP.GObject o, O.IsDescendantOf IconInfo o) => IsIconInfo o
instance (SP.GObject o, O.IsDescendantOf IconInfo o) => IsIconInfo o

instance O.HasParentTypes IconInfo
type instance O.ParentTypes IconInfo = '[GObject.Object.Object]

-- | Cast to `IconInfo`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toIconInfo :: (MIO.MonadIO m, IsIconInfo o) => o -> m IconInfo
toIconInfo = MIO.liftIO . B.ManagedPtr.unsafeCastTo IconInfo

-- | Convert 'IconInfo' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe IconInfo) where
    gvalueGType_ = c_gtk_icon_info_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr IconInfo)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr IconInfo)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject IconInfo ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveIconInfoMethod (t :: Symbol) (o :: *) :: * where
    ResolveIconInfoMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveIconInfoMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveIconInfoMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveIconInfoMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveIconInfoMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveIconInfoMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveIconInfoMethod "isSymbolic" o = IconInfoIsSymbolicMethodInfo
    ResolveIconInfoMethod "loadIcon" o = IconInfoLoadIconMethodInfo
    ResolveIconInfoMethod "loadIconAsync" o = IconInfoLoadIconAsyncMethodInfo
    ResolveIconInfoMethod "loadIconFinish" o = IconInfoLoadIconFinishMethodInfo
    ResolveIconInfoMethod "loadSurface" o = IconInfoLoadSurfaceMethodInfo
    ResolveIconInfoMethod "loadSymbolic" o = IconInfoLoadSymbolicMethodInfo
    ResolveIconInfoMethod "loadSymbolicAsync" o = IconInfoLoadSymbolicAsyncMethodInfo
    ResolveIconInfoMethod "loadSymbolicFinish" o = IconInfoLoadSymbolicFinishMethodInfo
    ResolveIconInfoMethod "loadSymbolicForContext" o = IconInfoLoadSymbolicForContextMethodInfo
    ResolveIconInfoMethod "loadSymbolicForContextAsync" o = IconInfoLoadSymbolicForContextAsyncMethodInfo
    ResolveIconInfoMethod "loadSymbolicForContextFinish" o = IconInfoLoadSymbolicForContextFinishMethodInfo
    ResolveIconInfoMethod "loadSymbolicForStyle" o = IconInfoLoadSymbolicForStyleMethodInfo
    ResolveIconInfoMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveIconInfoMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveIconInfoMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveIconInfoMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveIconInfoMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveIconInfoMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveIconInfoMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveIconInfoMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveIconInfoMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveIconInfoMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveIconInfoMethod "getAttachPoints" o = IconInfoGetAttachPointsMethodInfo
    ResolveIconInfoMethod "getBaseScale" o = IconInfoGetBaseScaleMethodInfo
    ResolveIconInfoMethod "getBaseSize" o = IconInfoGetBaseSizeMethodInfo
    ResolveIconInfoMethod "getBuiltinPixbuf" o = IconInfoGetBuiltinPixbufMethodInfo
    ResolveIconInfoMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveIconInfoMethod "getDisplayName" o = IconInfoGetDisplayNameMethodInfo
    ResolveIconInfoMethod "getEmbeddedRect" o = IconInfoGetEmbeddedRectMethodInfo
    ResolveIconInfoMethod "getFilename" o = IconInfoGetFilenameMethodInfo
    ResolveIconInfoMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveIconInfoMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveIconInfoMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveIconInfoMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveIconInfoMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveIconInfoMethod "setRawCoordinates" o = IconInfoSetRawCoordinatesMethodInfo
    ResolveIconInfoMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveIconInfoMethod t IconInfo, O.OverloadedMethod info IconInfo p) => OL.IsLabel t (IconInfo -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveIconInfoMethod t IconInfo, O.OverloadedMethod info IconInfo p, R.HasField t IconInfo p) => R.HasField t IconInfo p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveIconInfoMethod t IconInfo, O.OverloadedMethodInfo info IconInfo) => OL.IsLabel t (O.MethodProxy info IconInfo) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList IconInfo
type instance O.AttributeList IconInfo = IconInfoAttributeList
type IconInfoAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList IconInfo = IconInfoSignalList
type IconInfoSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method IconInfo::new_for_pixbuf
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "icon_theme"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconTheme" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconTheme" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pixbuf"
--           , argType =
--               TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the pixbuf to wrap in a #GtkIconInfo"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "IconInfo" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_info_new_for_pixbuf" gtk_icon_info_new_for_pixbuf :: 
    Ptr Gtk.IconTheme.IconTheme ->          -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    Ptr GdkPixbuf.Pixbuf.Pixbuf ->          -- pixbuf : TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
    IO (Ptr IconInfo)

-- | Creates a t'GI.Gtk.Objects.IconInfo.IconInfo' for a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf'.
-- 
-- /Since: 2.14/
iconInfoNewForPixbuf ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.IconTheme.IsIconTheme a, GdkPixbuf.Pixbuf.IsPixbuf b) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> b
    -- ^ /@pixbuf@/: the pixbuf to wrap in a t'GI.Gtk.Objects.IconInfo.IconInfo'
    -> m IconInfo
    -- ^ __Returns:__ a t'GI.Gtk.Objects.IconInfo.IconInfo'
iconInfoNewForPixbuf iconTheme pixbuf = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    pixbuf' <- unsafeManagedPtrCastPtr pixbuf
    result <- gtk_icon_info_new_for_pixbuf iconTheme' pixbuf'
    checkUnexpectedReturnNULL "iconInfoNewForPixbuf" result
    result' <- (wrapObject IconInfo) result
    touchManagedPtr iconTheme
    touchManagedPtr pixbuf
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method IconInfo::get_attach_points
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "points"
--           , argType =
--               TCArray
--                 False
--                 (-1)
--                 2
--                 (TInterface Name { namespace = "Gdk" , name = "Point" })
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to store pointer\n    to an array of points, or %NULL free the array of points with g_free()."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "n_points"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "location to store the number of points in @points,\n    or %NULL"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "n_points"
--              , argType = TBasicType TInt
--              , direction = DirectionOut
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText =
--                        Just
--                          "location to store the number of points in @points,\n    or %NULL"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferEverything
--              }
--          ]
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_info_get_attach_points" gtk_icon_info_get_attach_points :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    Ptr (Ptr Gdk.Point.Point) ->            -- points : TCArray False (-1) 2 (TInterface (Name {namespace = "Gdk", name = "Point"}))
    Ptr Int32 ->                            -- n_points : TBasicType TInt
    IO CInt

{-# DEPRECATED iconInfoGetAttachPoints ["(Since version 3.14)","Attachment points are deprecated"] #-}
-- | This function is deprecated and always returns 'P.False'.
-- 
-- /Since: 2.4/
iconInfoGetAttachPoints ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo'
    -> m ((Bool, [Gdk.Point.Point]))
    -- ^ __Returns:__ 'P.False'
iconInfoGetAttachPoints iconInfo = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    points <- callocMem :: IO (Ptr (Ptr Gdk.Point.Point))
    nPoints <- allocMem :: IO (Ptr Int32)
    result <- gtk_icon_info_get_attach_points iconInfo' points nPoints
    nPoints' <- peek nPoints
    let result' = (/= 0) result
    points' <- peek points
    points'' <- (unpackBlockArrayWithLength 8 nPoints') points'
    points''' <- mapM (wrapPtr Gdk.Point.Point) points''
    freeMem points'
    touchManagedPtr iconInfo
    freeMem points
    freeMem nPoints
    return (result', points''')

#if defined(ENABLE_OVERLOADING)
data IconInfoGetAttachPointsMethodInfo
instance (signature ~ (m ((Bool, [Gdk.Point.Point]))), MonadIO m, IsIconInfo a) => O.OverloadedMethod IconInfoGetAttachPointsMethodInfo a signature where
    overloadedMethod = iconInfoGetAttachPoints

instance O.OverloadedMethodInfo IconInfoGetAttachPointsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoGetAttachPoints",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoGetAttachPoints"
        })


#endif

-- method IconInfo::get_base_scale
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconInfo" , sinceVersion = Nothing }
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

foreign import ccall "gtk_icon_info_get_base_scale" gtk_icon_info_get_base_scale :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    IO Int32

-- | Gets the base scale for the icon. The base scale is a scale
-- for the icon that was specified by the icon theme creator.
-- For instance an icon drawn for a high-dpi screen with window
-- scale 2 for a base size of 32 will be 64 pixels tall and have
-- a base scale of 2.
-- 
-- /Since: 3.10/
iconInfoGetBaseScale ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo'
    -> m Int32
    -- ^ __Returns:__ the base scale
iconInfoGetBaseScale iconInfo = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    result <- gtk_icon_info_get_base_scale iconInfo'
    touchManagedPtr iconInfo
    return result

#if defined(ENABLE_OVERLOADING)
data IconInfoGetBaseScaleMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsIconInfo a) => O.OverloadedMethod IconInfoGetBaseScaleMethodInfo a signature where
    overloadedMethod = iconInfoGetBaseScale

instance O.OverloadedMethodInfo IconInfoGetBaseScaleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoGetBaseScale",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoGetBaseScale"
        })


#endif

-- method IconInfo::get_base_size
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconInfo" , sinceVersion = Nothing }
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

foreign import ccall "gtk_icon_info_get_base_size" gtk_icon_info_get_base_size :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    IO Int32

-- | Gets the base size for the icon. The base size
-- is a size for the icon that was specified by
-- the icon theme creator. This may be different
-- than the actual size of image; an example of
-- this is small emblem icons that can be attached
-- to a larger icon. These icons will be given
-- the same base size as the larger icons to which
-- they are attached.
-- 
-- Note that for scaled icons the base size does
-- not include the base scale.
-- 
-- /Since: 2.4/
iconInfoGetBaseSize ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo'
    -> m Int32
    -- ^ __Returns:__ the base size, or 0, if no base
    --     size is known for the icon.
iconInfoGetBaseSize iconInfo = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    result <- gtk_icon_info_get_base_size iconInfo'
    touchManagedPtr iconInfo
    return result

#if defined(ENABLE_OVERLOADING)
data IconInfoGetBaseSizeMethodInfo
instance (signature ~ (m Int32), MonadIO m, IsIconInfo a) => O.OverloadedMethod IconInfoGetBaseSizeMethodInfo a signature where
    overloadedMethod = iconInfoGetBaseSize

instance O.OverloadedMethodInfo IconInfoGetBaseSizeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoGetBaseSize",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoGetBaseSize"
        })


#endif

-- method IconInfo::get_builtin_pixbuf
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconInfo" , sinceVersion = Nothing }
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

foreign import ccall "gtk_icon_info_get_builtin_pixbuf" gtk_icon_info_get_builtin_pixbuf :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

{-# DEPRECATED iconInfoGetBuiltinPixbuf ["(Since version 3.14)","This function is deprecated, use","    'GI.Gtk.Objects.IconTheme.iconThemeAddResourcePath' instead of builtin icons."] #-}
-- | Gets the built-in image for this icon, if any. To allow GTK+ to use
-- built in icon images, you must pass the 'GI.Gtk.Flags.IconLookupFlagsUseBuiltin'
-- to 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'.
-- 
-- /Since: 2.4/
iconInfoGetBuiltinPixbuf ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo'
    -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)
    -- ^ __Returns:__ the built-in image pixbuf, or 'P.Nothing'.
    --     No extra reference is added to the returned pixbuf, so if
    --     you want to keep it around, you must use 'GI.GObject.Objects.Object.objectRef'.
    --     The returned image must not be modified.
iconInfoGetBuiltinPixbuf iconInfo = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    result <- gtk_icon_info_get_builtin_pixbuf iconInfo'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject GdkPixbuf.Pixbuf.Pixbuf) result'
        return result''
    touchManagedPtr iconInfo
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data IconInfoGetBuiltinPixbufMethodInfo
instance (signature ~ (m (Maybe GdkPixbuf.Pixbuf.Pixbuf)), MonadIO m, IsIconInfo a) => O.OverloadedMethod IconInfoGetBuiltinPixbufMethodInfo a signature where
    overloadedMethod = iconInfoGetBuiltinPixbuf

instance O.OverloadedMethodInfo IconInfoGetBuiltinPixbufMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoGetBuiltinPixbuf",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoGetBuiltinPixbuf"
        })


#endif

-- method IconInfo::get_display_name
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconInfo" , sinceVersion = Nothing }
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

foreign import ccall "gtk_icon_info_get_display_name" gtk_icon_info_get_display_name :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    IO CString

{-# DEPRECATED iconInfoGetDisplayName ["(Since version 3.14)","Display names are deprecated"] #-}
-- | This function is deprecated and always returns 'P.Nothing'.
-- 
-- /Since: 2.4/
iconInfoGetDisplayName ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo'
    -> m T.Text
    -- ^ __Returns:__ 'P.Nothing'
iconInfoGetDisplayName iconInfo = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    result <- gtk_icon_info_get_display_name iconInfo'
    checkUnexpectedReturnNULL "iconInfoGetDisplayName" result
    result' <- cstringToText result
    touchManagedPtr iconInfo
    return result'

#if defined(ENABLE_OVERLOADING)
data IconInfoGetDisplayNameMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsIconInfo a) => O.OverloadedMethod IconInfoGetDisplayNameMethodInfo a signature where
    overloadedMethod = iconInfoGetDisplayName

instance O.OverloadedMethodInfo IconInfoGetDisplayNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoGetDisplayName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoGetDisplayName"
        })


#endif

-- method IconInfo::get_embedded_rect
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "rectangle"
--           , argType =
--               TInterface Name { namespace = "Gdk" , name = "Rectangle" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "#GdkRectangle in which to store embedded\n  rectangle coordinates; coordinates are only stored\n  when this function returns %TRUE."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_info_get_embedded_rect" gtk_icon_info_get_embedded_rect :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    Ptr Gdk.Rectangle.Rectangle ->          -- rectangle : TInterface (Name {namespace = "Gdk", name = "Rectangle"})
    IO CInt

{-# DEPRECATED iconInfoGetEmbeddedRect ["(Since version 3.14)","Embedded rectangles are deprecated"] #-}
-- | This function is deprecated and always returns 'P.False'.
-- 
-- /Since: 2.4/
iconInfoGetEmbeddedRect ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo'
    -> m ((Bool, Gdk.Rectangle.Rectangle))
    -- ^ __Returns:__ 'P.False'
iconInfoGetEmbeddedRect iconInfo = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    rectangle <- SP.callocBoxedBytes 16 :: IO (Ptr Gdk.Rectangle.Rectangle)
    result <- gtk_icon_info_get_embedded_rect iconInfo' rectangle
    let result' = (/= 0) result
    rectangle' <- (wrapBoxed Gdk.Rectangle.Rectangle) rectangle
    touchManagedPtr iconInfo
    return (result', rectangle')

#if defined(ENABLE_OVERLOADING)
data IconInfoGetEmbeddedRectMethodInfo
instance (signature ~ (m ((Bool, Gdk.Rectangle.Rectangle))), MonadIO m, IsIconInfo a) => O.OverloadedMethod IconInfoGetEmbeddedRectMethodInfo a signature where
    overloadedMethod = iconInfoGetEmbeddedRect

instance O.OverloadedMethodInfo IconInfoGetEmbeddedRectMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoGetEmbeddedRect",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoGetEmbeddedRect"
        })


#endif

-- method IconInfo::get_filename
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TFileName)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_info_get_filename" gtk_icon_info_get_filename :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    IO CString

-- | Gets the filename for the icon. If the 'GI.Gtk.Flags.IconLookupFlagsUseBuiltin'
-- flag was passed to 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon', there may be no
-- filename if a builtin icon is returned; in this case, you should
-- use 'GI.Gtk.Objects.IconInfo.iconInfoGetBuiltinPixbuf'.
-- 
-- /Since: 2.4/
iconInfoGetFilename ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo'
    -> m (Maybe [Char])
    -- ^ __Returns:__ the filename for the icon, or 'P.Nothing'
    --     if 'GI.Gtk.Objects.IconInfo.iconInfoGetBuiltinPixbuf' should be used instead.
    --     The return value is owned by GTK+ and should not be modified
    --     or freed.
iconInfoGetFilename iconInfo = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    result <- gtk_icon_info_get_filename iconInfo'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToString result'
        return result''
    touchManagedPtr iconInfo
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data IconInfoGetFilenameMethodInfo
instance (signature ~ (m (Maybe [Char])), MonadIO m, IsIconInfo a) => O.OverloadedMethod IconInfoGetFilenameMethodInfo a signature where
    overloadedMethod = iconInfoGetFilename

instance O.OverloadedMethodInfo IconInfoGetFilenameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoGetFilename",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoGetFilename"
        })


#endif

-- method IconInfo::is_symbolic
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconInfo" , sinceVersion = Nothing }
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

foreign import ccall "gtk_icon_info_is_symbolic" gtk_icon_info_is_symbolic :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    IO CInt

-- | Checks if the icon is symbolic or not. This currently uses only
-- the file name and not the file contents for determining this.
-- This behaviour may change in the future.
-- 
-- /Since: 3.12/
iconInfoIsSymbolic ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the icon is symbolic, 'P.False' otherwise
iconInfoIsSymbolic iconInfo = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    result <- gtk_icon_info_is_symbolic iconInfo'
    let result' = (/= 0) result
    touchManagedPtr iconInfo
    return result'

#if defined(ENABLE_OVERLOADING)
data IconInfoIsSymbolicMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsIconInfo a) => O.OverloadedMethod IconInfoIsSymbolicMethodInfo a signature where
    overloadedMethod = iconInfoIsSymbolic

instance O.OverloadedMethodInfo IconInfoIsSymbolicMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoIsSymbolic",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoIsSymbolic"
        })


#endif

-- method IconInfo::load_icon
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkIconInfo from gtk_icon_theme_lookup_icon()"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_icon_info_load_icon" gtk_icon_info_load_icon :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    Ptr (Ptr GError) ->                     -- error
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

-- | Renders an icon previously looked up in an icon theme using
-- 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'; the size will be based on the size
-- passed to 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'. Note that the resulting
-- pixbuf may not be exactly this size; an icon theme may have icons
-- that differ slightly from their nominal sizes, and in addition GTK+
-- will avoid scaling icons that it considers sufficiently close to the
-- requested size or for which the source image would have to be scaled
-- up too far. (This maintains sharpness.). This behaviour can be changed
-- by passing the 'GI.Gtk.Flags.IconLookupFlagsForceSize' flag when obtaining
-- the t'GI.Gtk.Objects.IconInfo.IconInfo'. If this flag has been specified, the pixbuf
-- returned by this function will be scaled to the exact size.
-- 
-- /Since: 2.4/
iconInfoLoadIcon ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo' from 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'
    -> m GdkPixbuf.Pixbuf.Pixbuf
    -- ^ __Returns:__ the rendered icon; this may be a newly
    --     created icon or a new reference to an internal icon, so you must
    --     not modify the icon. Use 'GI.GObject.Objects.Object.objectUnref' to release your reference
    --     to the icon. /(Can throw 'Data.GI.Base.GError.GError')/
iconInfoLoadIcon iconInfo = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    onException (do
        result <- propagateGError $ gtk_icon_info_load_icon iconInfo'
        checkUnexpectedReturnNULL "iconInfoLoadIcon" result
        result' <- (wrapObject GdkPixbuf.Pixbuf.Pixbuf) result
        touchManagedPtr iconInfo
        return result'
     ) (do
        return ()
     )

#if defined(ENABLE_OVERLOADING)
data IconInfoLoadIconMethodInfo
instance (signature ~ (m GdkPixbuf.Pixbuf.Pixbuf), MonadIO m, IsIconInfo a) => O.OverloadedMethod IconInfoLoadIconMethodInfo a signature where
    overloadedMethod = iconInfoLoadIcon

instance O.OverloadedMethodInfo IconInfoLoadIconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoLoadIcon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoLoadIcon"
        })


#endif

-- method IconInfo::load_icon_async
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkIconInfo from gtk_icon_theme_lookup_icon()"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cancellable"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "Cancellable" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "optional #GCancellable object, %NULL to ignore"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "callback"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "AsyncReadyCallback" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GAsyncReadyCallback to call when the\n    request is satisfied"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeAsync
--           , argClosure = 3
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the data to pass to callback function"
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

foreign import ccall "gtk_icon_info_load_icon_async" gtk_icon_info_load_icon_async :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    Ptr Gio.Cancellable.Cancellable ->      -- cancellable : TInterface (Name {namespace = "Gio", name = "Cancellable"})
    FunPtr Gio.Callbacks.C_AsyncReadyCallback -> -- callback : TInterface (Name {namespace = "Gio", name = "AsyncReadyCallback"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    IO ()

-- | Asynchronously load, render and scale an icon previously looked up
-- from the icon theme using 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'.
-- 
-- For more details, see 'GI.Gtk.Objects.IconInfo.iconInfoLoadIcon' which is the synchronous
-- version of this call.
-- 
-- /Since: 3.8/
iconInfoLoadIconAsync ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a, Gio.Cancellable.IsCancellable b) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo' from 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'
    -> Maybe (b)
    -- ^ /@cancellable@/: optional t'GI.Gio.Objects.Cancellable.Cancellable' object, 'P.Nothing' to ignore
    -> Maybe (Gio.Callbacks.AsyncReadyCallback)
    -- ^ /@callback@/: a t'GI.Gio.Callbacks.AsyncReadyCallback' to call when the
    --     request is satisfied
    -> m ()
iconInfoLoadIconAsync iconInfo cancellable callback = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    maybeCancellable <- case cancellable of
        Nothing -> return nullPtr
        Just jCancellable -> do
            jCancellable' <- unsafeManagedPtrCastPtr jCancellable
            return jCancellable'
    maybeCallback <- case callback of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jCallback -> do
            ptrcallback <- callocMem :: IO (Ptr (FunPtr Gio.Callbacks.C_AsyncReadyCallback))
            jCallback' <- Gio.Callbacks.mk_AsyncReadyCallback (Gio.Callbacks.wrap_AsyncReadyCallback (Just ptrcallback) (Gio.Callbacks.drop_closures_AsyncReadyCallback jCallback))
            poke ptrcallback jCallback'
            return jCallback'
    let userData = nullPtr
    gtk_icon_info_load_icon_async iconInfo' maybeCancellable maybeCallback userData
    touchManagedPtr iconInfo
    whenJust cancellable touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data IconInfoLoadIconAsyncMethodInfo
instance (signature ~ (Maybe (b) -> Maybe (Gio.Callbacks.AsyncReadyCallback) -> m ()), MonadIO m, IsIconInfo a, Gio.Cancellable.IsCancellable b) => O.OverloadedMethod IconInfoLoadIconAsyncMethodInfo a signature where
    overloadedMethod = iconInfoLoadIconAsync

instance O.OverloadedMethodInfo IconInfoLoadIconAsyncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoLoadIconAsync",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoLoadIconAsync"
        })


#endif

-- method IconInfo::load_icon_finish
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkIconInfo from gtk_icon_theme_lookup_icon()"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "res"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "AsyncResult" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GAsyncResult" , sinceVersion = Nothing }
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_icon_info_load_icon_finish" gtk_icon_info_load_icon_finish :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    Ptr Gio.AsyncResult.AsyncResult ->      -- res : TInterface (Name {namespace = "Gio", name = "AsyncResult"})
    Ptr (Ptr GError) ->                     -- error
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

-- | Finishes an async icon load, see 'GI.Gtk.Objects.IconInfo.iconInfoLoadIconAsync'.
-- 
-- /Since: 3.8/
iconInfoLoadIconFinish ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a, Gio.AsyncResult.IsAsyncResult b) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo' from 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'
    -> b
    -- ^ /@res@/: a t'GI.Gio.Interfaces.AsyncResult.AsyncResult'
    -> m GdkPixbuf.Pixbuf.Pixbuf
    -- ^ __Returns:__ the rendered icon; this may be a newly
    --     created icon or a new reference to an internal icon, so you must
    --     not modify the icon. Use 'GI.GObject.Objects.Object.objectUnref' to release your reference
    --     to the icon. /(Can throw 'Data.GI.Base.GError.GError')/
iconInfoLoadIconFinish iconInfo res = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    res' <- unsafeManagedPtrCastPtr res
    onException (do
        result <- propagateGError $ gtk_icon_info_load_icon_finish iconInfo' res'
        checkUnexpectedReturnNULL "iconInfoLoadIconFinish" result
        result' <- (wrapObject GdkPixbuf.Pixbuf.Pixbuf) result
        touchManagedPtr iconInfo
        touchManagedPtr res
        return result'
     ) (do
        return ()
     )

#if defined(ENABLE_OVERLOADING)
data IconInfoLoadIconFinishMethodInfo
instance (signature ~ (b -> m GdkPixbuf.Pixbuf.Pixbuf), MonadIO m, IsIconInfo a, Gio.AsyncResult.IsAsyncResult b) => O.OverloadedMethod IconInfoLoadIconFinishMethodInfo a signature where
    overloadedMethod = iconInfoLoadIconFinish

instance O.OverloadedMethodInfo IconInfoLoadIconFinishMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoLoadIconFinish",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoLoadIconFinish"
        })


#endif

-- method IconInfo::load_surface
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkIconInfo from gtk_icon_theme_lookup_icon()"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "for_window"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "#GdkWindow to optimize drawing for, or %NULL"
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
-- returnType: Just (TInterface Name { namespace = "cairo" , name = "Surface" })
-- throws : True
-- Skip return : False

foreign import ccall "gtk_icon_info_load_surface" gtk_icon_info_load_surface :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    Ptr Gdk.Window.Window ->                -- for_window : TInterface (Name {namespace = "Gdk", name = "Window"})
    Ptr (Ptr GError) ->                     -- error
    IO (Ptr Cairo.Surface.Surface)

-- | Renders an icon previously looked up in an icon theme using
-- 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'; the size will be based on the size
-- passed to 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'. Note that the resulting
-- surface may not be exactly this size; an icon theme may have icons
-- that differ slightly from their nominal sizes, and in addition GTK+
-- will avoid scaling icons that it considers sufficiently close to the
-- requested size or for which the source image would have to be scaled
-- up too far. (This maintains sharpness.). This behaviour can be changed
-- by passing the 'GI.Gtk.Flags.IconLookupFlagsForceSize' flag when obtaining
-- the t'GI.Gtk.Objects.IconInfo.IconInfo'. If this flag has been specified, the pixbuf
-- returned by this function will be scaled to the exact size.
-- 
-- /Since: 3.10/
iconInfoLoadSurface ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a, Gdk.Window.IsWindow b) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo' from 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'
    -> Maybe (b)
    -- ^ /@forWindow@/: t'GI.Gdk.Objects.Window.Window' to optimize drawing for, or 'P.Nothing'
    -> m Cairo.Surface.Surface
    -- ^ __Returns:__ the rendered icon; this may be a newly
    --     created icon or a new reference to an internal icon, so you must
    --     not modify the icon. Use @/cairo_surface_destroy()/@ to release your
    --     reference to the icon. /(Can throw 'Data.GI.Base.GError.GError')/
iconInfoLoadSurface iconInfo forWindow = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    maybeForWindow <- case forWindow of
        Nothing -> return nullPtr
        Just jForWindow -> do
            jForWindow' <- unsafeManagedPtrCastPtr jForWindow
            return jForWindow'
    onException (do
        result <- propagateGError $ gtk_icon_info_load_surface iconInfo' maybeForWindow
        checkUnexpectedReturnNULL "iconInfoLoadSurface" result
        result' <- (wrapBoxed Cairo.Surface.Surface) result
        touchManagedPtr iconInfo
        whenJust forWindow touchManagedPtr
        return result'
     ) (do
        return ()
     )

#if defined(ENABLE_OVERLOADING)
data IconInfoLoadSurfaceMethodInfo
instance (signature ~ (Maybe (b) -> m Cairo.Surface.Surface), MonadIO m, IsIconInfo a, Gdk.Window.IsWindow b) => O.OverloadedMethod IconInfoLoadSurfaceMethodInfo a signature where
    overloadedMethod = iconInfoLoadSurface

instance O.OverloadedMethodInfo IconInfoLoadSurfaceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoLoadSurface",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoLoadSurface"
        })


#endif

-- method IconInfo::load_symbolic
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "fg"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GdkRGBA representing the foreground color of the icon"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "success_color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GdkRGBA representing the warning color\n    of the icon or %NULL to use the default color"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "warning_color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GdkRGBA representing the warning color\n    of the icon or %NULL to use the default color"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "error_color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GdkRGBA representing the error color\n    of the icon or %NULL to use the default color (allow-none)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "was_symbolic"
--           , argType = TBasicType TBoolean
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #gboolean, returns whether the\n    loaded icon was a symbolic one and whether the @fg color was\n    applied to it."
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
-- returnType: Just
--               (TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" })
-- throws : True
-- Skip return : False

foreign import ccall "gtk_icon_info_load_symbolic" gtk_icon_info_load_symbolic :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    Ptr Gdk.RGBA.RGBA ->                    -- fg : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    Ptr Gdk.RGBA.RGBA ->                    -- success_color : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    Ptr Gdk.RGBA.RGBA ->                    -- warning_color : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    Ptr Gdk.RGBA.RGBA ->                    -- error_color : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    Ptr CInt ->                             -- was_symbolic : TBasicType TBoolean
    Ptr (Ptr GError) ->                     -- error
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

-- | Loads an icon, modifying it to match the system colours for the foreground,
-- success, warning and error colors provided. If the icon is not a symbolic
-- one, the function will return the result from 'GI.Gtk.Objects.IconInfo.iconInfoLoadIcon'.
-- 
-- This allows loading symbolic icons that will match the system theme.
-- 
-- Unless you are implementing a widget, you will want to use
-- 'GI.Gio.Objects.ThemedIcon.themedIconNewWithDefaultFallbacks' to load the icon.
-- 
-- As implementation details, the icon loaded needs to be of SVG type,
-- contain the “symbolic” term as the last component of the icon name,
-- and use the “fg”, “success”, “warning” and “error” CSS styles in the
-- SVG file itself.
-- 
-- See the <http://www.freedesktop.org/wiki/SymbolicIcons Symbolic Icons Specification>
-- for more information about symbolic icons.
-- 
-- /Since: 3.0/
iconInfoLoadSymbolic ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo'
    -> Gdk.RGBA.RGBA
    -- ^ /@fg@/: a t'GI.Gdk.Structs.RGBA.RGBA' representing the foreground color of the icon
    -> Maybe (Gdk.RGBA.RGBA)
    -- ^ /@successColor@/: a t'GI.Gdk.Structs.RGBA.RGBA' representing the warning color
    --     of the icon or 'P.Nothing' to use the default color
    -> Maybe (Gdk.RGBA.RGBA)
    -- ^ /@warningColor@/: a t'GI.Gdk.Structs.RGBA.RGBA' representing the warning color
    --     of the icon or 'P.Nothing' to use the default color
    -> Maybe (Gdk.RGBA.RGBA)
    -- ^ /@errorColor@/: a t'GI.Gdk.Structs.RGBA.RGBA' representing the error color
    --     of the icon or 'P.Nothing' to use the default color (allow-none)
    -> m ((GdkPixbuf.Pixbuf.Pixbuf, Bool))
    -- ^ __Returns:__ a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf' representing the loaded icon /(Can throw 'Data.GI.Base.GError.GError')/
iconInfoLoadSymbolic iconInfo fg successColor warningColor errorColor = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    fg' <- unsafeManagedPtrGetPtr fg
    maybeSuccessColor <- case successColor of
        Nothing -> return nullPtr
        Just jSuccessColor -> do
            jSuccessColor' <- unsafeManagedPtrGetPtr jSuccessColor
            return jSuccessColor'
    maybeWarningColor <- case warningColor of
        Nothing -> return nullPtr
        Just jWarningColor -> do
            jWarningColor' <- unsafeManagedPtrGetPtr jWarningColor
            return jWarningColor'
    maybeErrorColor <- case errorColor of
        Nothing -> return nullPtr
        Just jErrorColor -> do
            jErrorColor' <- unsafeManagedPtrGetPtr jErrorColor
            return jErrorColor'
    wasSymbolic <- allocMem :: IO (Ptr CInt)
    onException (do
        result <- propagateGError $ gtk_icon_info_load_symbolic iconInfo' fg' maybeSuccessColor maybeWarningColor maybeErrorColor wasSymbolic
        checkUnexpectedReturnNULL "iconInfoLoadSymbolic" result
        result' <- (wrapObject GdkPixbuf.Pixbuf.Pixbuf) result
        wasSymbolic' <- peek wasSymbolic
        let wasSymbolic'' = (/= 0) wasSymbolic'
        touchManagedPtr iconInfo
        touchManagedPtr fg
        whenJust successColor touchManagedPtr
        whenJust warningColor touchManagedPtr
        whenJust errorColor touchManagedPtr
        freeMem wasSymbolic
        return (result', wasSymbolic'')
     ) (do
        freeMem wasSymbolic
     )

#if defined(ENABLE_OVERLOADING)
data IconInfoLoadSymbolicMethodInfo
instance (signature ~ (Gdk.RGBA.RGBA -> Maybe (Gdk.RGBA.RGBA) -> Maybe (Gdk.RGBA.RGBA) -> Maybe (Gdk.RGBA.RGBA) -> m ((GdkPixbuf.Pixbuf.Pixbuf, Bool))), MonadIO m, IsIconInfo a) => O.OverloadedMethod IconInfoLoadSymbolicMethodInfo a signature where
    overloadedMethod = iconInfoLoadSymbolic

instance O.OverloadedMethodInfo IconInfoLoadSymbolicMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoLoadSymbolic",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoLoadSymbolic"
        })


#endif

-- method IconInfo::load_symbolic_async
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkIconInfo from gtk_icon_theme_lookup_icon()"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "fg"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GdkRGBA representing the foreground color of the icon"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "success_color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GdkRGBA representing the warning color\n    of the icon or %NULL to use the default color"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "warning_color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GdkRGBA representing the warning color\n    of the icon or %NULL to use the default color"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "error_color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GdkRGBA representing the error color\n    of the icon or %NULL to use the default color (allow-none)"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cancellable"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "Cancellable" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "optional #GCancellable object,\n    %NULL to ignore"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "callback"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "AsyncReadyCallback" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GAsyncReadyCallback to call when the\n    request is satisfied"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeAsync
--           , argClosure = 7
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the data to pass to callback function"
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

foreign import ccall "gtk_icon_info_load_symbolic_async" gtk_icon_info_load_symbolic_async :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    Ptr Gdk.RGBA.RGBA ->                    -- fg : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    Ptr Gdk.RGBA.RGBA ->                    -- success_color : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    Ptr Gdk.RGBA.RGBA ->                    -- warning_color : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    Ptr Gdk.RGBA.RGBA ->                    -- error_color : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    Ptr Gio.Cancellable.Cancellable ->      -- cancellable : TInterface (Name {namespace = "Gio", name = "Cancellable"})
    FunPtr Gio.Callbacks.C_AsyncReadyCallback -> -- callback : TInterface (Name {namespace = "Gio", name = "AsyncReadyCallback"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    IO ()

-- | Asynchronously load, render and scale a symbolic icon previously looked up
-- from the icon theme using 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'.
-- 
-- For more details, see 'GI.Gtk.Objects.IconInfo.iconInfoLoadSymbolic' which is the synchronous
-- version of this call.
-- 
-- /Since: 3.8/
iconInfoLoadSymbolicAsync ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a, Gio.Cancellable.IsCancellable b) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo' from 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'
    -> Gdk.RGBA.RGBA
    -- ^ /@fg@/: a t'GI.Gdk.Structs.RGBA.RGBA' representing the foreground color of the icon
    -> Maybe (Gdk.RGBA.RGBA)
    -- ^ /@successColor@/: a t'GI.Gdk.Structs.RGBA.RGBA' representing the warning color
    --     of the icon or 'P.Nothing' to use the default color
    -> Maybe (Gdk.RGBA.RGBA)
    -- ^ /@warningColor@/: a t'GI.Gdk.Structs.RGBA.RGBA' representing the warning color
    --     of the icon or 'P.Nothing' to use the default color
    -> Maybe (Gdk.RGBA.RGBA)
    -- ^ /@errorColor@/: a t'GI.Gdk.Structs.RGBA.RGBA' representing the error color
    --     of the icon or 'P.Nothing' to use the default color (allow-none)
    -> Maybe (b)
    -- ^ /@cancellable@/: optional t'GI.Gio.Objects.Cancellable.Cancellable' object,
    --     'P.Nothing' to ignore
    -> Maybe (Gio.Callbacks.AsyncReadyCallback)
    -- ^ /@callback@/: a t'GI.Gio.Callbacks.AsyncReadyCallback' to call when the
    --     request is satisfied
    -> m ()
iconInfoLoadSymbolicAsync iconInfo fg successColor warningColor errorColor cancellable callback = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    fg' <- unsafeManagedPtrGetPtr fg
    maybeSuccessColor <- case successColor of
        Nothing -> return nullPtr
        Just jSuccessColor -> do
            jSuccessColor' <- unsafeManagedPtrGetPtr jSuccessColor
            return jSuccessColor'
    maybeWarningColor <- case warningColor of
        Nothing -> return nullPtr
        Just jWarningColor -> do
            jWarningColor' <- unsafeManagedPtrGetPtr jWarningColor
            return jWarningColor'
    maybeErrorColor <- case errorColor of
        Nothing -> return nullPtr
        Just jErrorColor -> do
            jErrorColor' <- unsafeManagedPtrGetPtr jErrorColor
            return jErrorColor'
    maybeCancellable <- case cancellable of
        Nothing -> return nullPtr
        Just jCancellable -> do
            jCancellable' <- unsafeManagedPtrCastPtr jCancellable
            return jCancellable'
    maybeCallback <- case callback of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jCallback -> do
            ptrcallback <- callocMem :: IO (Ptr (FunPtr Gio.Callbacks.C_AsyncReadyCallback))
            jCallback' <- Gio.Callbacks.mk_AsyncReadyCallback (Gio.Callbacks.wrap_AsyncReadyCallback (Just ptrcallback) (Gio.Callbacks.drop_closures_AsyncReadyCallback jCallback))
            poke ptrcallback jCallback'
            return jCallback'
    let userData = nullPtr
    gtk_icon_info_load_symbolic_async iconInfo' fg' maybeSuccessColor maybeWarningColor maybeErrorColor maybeCancellable maybeCallback userData
    touchManagedPtr iconInfo
    touchManagedPtr fg
    whenJust successColor touchManagedPtr
    whenJust warningColor touchManagedPtr
    whenJust errorColor touchManagedPtr
    whenJust cancellable touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data IconInfoLoadSymbolicAsyncMethodInfo
instance (signature ~ (Gdk.RGBA.RGBA -> Maybe (Gdk.RGBA.RGBA) -> Maybe (Gdk.RGBA.RGBA) -> Maybe (Gdk.RGBA.RGBA) -> Maybe (b) -> Maybe (Gio.Callbacks.AsyncReadyCallback) -> m ()), MonadIO m, IsIconInfo a, Gio.Cancellable.IsCancellable b) => O.OverloadedMethod IconInfoLoadSymbolicAsyncMethodInfo a signature where
    overloadedMethod = iconInfoLoadSymbolicAsync

instance O.OverloadedMethodInfo IconInfoLoadSymbolicAsyncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoLoadSymbolicAsync",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoLoadSymbolicAsync"
        })


#endif

-- method IconInfo::load_symbolic_finish
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkIconInfo from gtk_icon_theme_lookup_icon()"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "res"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "AsyncResult" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GAsyncResult" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "was_symbolic"
--           , argType = TBasicType TBoolean
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #gboolean, returns whether the\n    loaded icon was a symbolic one and whether the @fg color was\n    applied to it."
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
-- returnType: Just
--               (TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" })
-- throws : True
-- Skip return : False

foreign import ccall "gtk_icon_info_load_symbolic_finish" gtk_icon_info_load_symbolic_finish :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    Ptr Gio.AsyncResult.AsyncResult ->      -- res : TInterface (Name {namespace = "Gio", name = "AsyncResult"})
    Ptr CInt ->                             -- was_symbolic : TBasicType TBoolean
    Ptr (Ptr GError) ->                     -- error
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

-- | Finishes an async icon load, see 'GI.Gtk.Objects.IconInfo.iconInfoLoadSymbolicAsync'.
-- 
-- /Since: 3.8/
iconInfoLoadSymbolicFinish ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a, Gio.AsyncResult.IsAsyncResult b) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo' from 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'
    -> b
    -- ^ /@res@/: a t'GI.Gio.Interfaces.AsyncResult.AsyncResult'
    -> m ((GdkPixbuf.Pixbuf.Pixbuf, Bool))
    -- ^ __Returns:__ the rendered icon; this may be a newly
    --     created icon or a new reference to an internal icon, so you must
    --     not modify the icon. Use 'GI.GObject.Objects.Object.objectUnref' to release your reference
    --     to the icon. /(Can throw 'Data.GI.Base.GError.GError')/
iconInfoLoadSymbolicFinish iconInfo res = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    res' <- unsafeManagedPtrCastPtr res
    wasSymbolic <- allocMem :: IO (Ptr CInt)
    onException (do
        result <- propagateGError $ gtk_icon_info_load_symbolic_finish iconInfo' res' wasSymbolic
        checkUnexpectedReturnNULL "iconInfoLoadSymbolicFinish" result
        result' <- (wrapObject GdkPixbuf.Pixbuf.Pixbuf) result
        wasSymbolic' <- peek wasSymbolic
        let wasSymbolic'' = (/= 0) wasSymbolic'
        touchManagedPtr iconInfo
        touchManagedPtr res
        freeMem wasSymbolic
        return (result', wasSymbolic'')
     ) (do
        freeMem wasSymbolic
     )

#if defined(ENABLE_OVERLOADING)
data IconInfoLoadSymbolicFinishMethodInfo
instance (signature ~ (b -> m ((GdkPixbuf.Pixbuf.Pixbuf, Bool))), MonadIO m, IsIconInfo a, Gio.AsyncResult.IsAsyncResult b) => O.OverloadedMethod IconInfoLoadSymbolicFinishMethodInfo a signature where
    overloadedMethod = iconInfoLoadSymbolicFinish

instance O.OverloadedMethodInfo IconInfoLoadSymbolicFinishMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoLoadSymbolicFinish",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoLoadSymbolicFinish"
        })


#endif

-- method IconInfo::load_symbolic_for_context
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "was_symbolic"
--           , argType = TBasicType TBoolean
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #gboolean, returns whether the\n    loaded icon was a symbolic one and whether the @fg color was\n    applied to it."
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
-- returnType: Just
--               (TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" })
-- throws : True
-- Skip return : False

foreign import ccall "gtk_icon_info_load_symbolic_for_context" gtk_icon_info_load_symbolic_for_context :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr CInt ->                             -- was_symbolic : TBasicType TBoolean
    Ptr (Ptr GError) ->                     -- error
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

-- | Loads an icon, modifying it to match the system colors for the foreground,
-- success, warning and error colors provided. If the icon is not a symbolic
-- one, the function will return the result from 'GI.Gtk.Objects.IconInfo.iconInfoLoadIcon'.
-- This function uses the regular foreground color and the symbolic colors
-- with the names “success_color”, “warning_color” and “error_color” from
-- the context.
-- 
-- This allows loading symbolic icons that will match the system theme.
-- 
-- See 'GI.Gtk.Objects.IconInfo.iconInfoLoadSymbolic' for more details.
-- 
-- /Since: 3.0/
iconInfoLoadSymbolicForContext ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a, Gtk.StyleContext.IsStyleContext b) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo'
    -> b
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> m ((GdkPixbuf.Pixbuf.Pixbuf, Bool))
    -- ^ __Returns:__ a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf' representing the loaded icon /(Can throw 'Data.GI.Base.GError.GError')/
iconInfoLoadSymbolicForContext iconInfo context = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    context' <- unsafeManagedPtrCastPtr context
    wasSymbolic <- allocMem :: IO (Ptr CInt)
    onException (do
        result <- propagateGError $ gtk_icon_info_load_symbolic_for_context iconInfo' context' wasSymbolic
        checkUnexpectedReturnNULL "iconInfoLoadSymbolicForContext" result
        result' <- (wrapObject GdkPixbuf.Pixbuf.Pixbuf) result
        wasSymbolic' <- peek wasSymbolic
        let wasSymbolic'' = (/= 0) wasSymbolic'
        touchManagedPtr iconInfo
        touchManagedPtr context
        freeMem wasSymbolic
        return (result', wasSymbolic'')
     ) (do
        freeMem wasSymbolic
     )

#if defined(ENABLE_OVERLOADING)
data IconInfoLoadSymbolicForContextMethodInfo
instance (signature ~ (b -> m ((GdkPixbuf.Pixbuf.Pixbuf, Bool))), MonadIO m, IsIconInfo a, Gtk.StyleContext.IsStyleContext b) => O.OverloadedMethod IconInfoLoadSymbolicForContextMethodInfo a signature where
    overloadedMethod = iconInfoLoadSymbolicForContext

instance O.OverloadedMethodInfo IconInfoLoadSymbolicForContextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoLoadSymbolicForContext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoLoadSymbolicForContext"
        })


#endif

-- method IconInfo::load_symbolic_for_context_async
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkIconInfo from gtk_icon_theme_lookup_icon()"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "context"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleContext" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleContext" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cancellable"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "Cancellable" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "optional #GCancellable object,\n    %NULL to ignore"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "callback"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "AsyncReadyCallback" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GAsyncReadyCallback to call when the\n    request is satisfied"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeAsync
--           , argClosure = 4
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "user_data"
--           , argType = TBasicType TPtr
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the data to pass to callback function"
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

foreign import ccall "gtk_icon_info_load_symbolic_for_context_async" gtk_icon_info_load_symbolic_for_context_async :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    Ptr Gtk.StyleContext.StyleContext ->    -- context : TInterface (Name {namespace = "Gtk", name = "StyleContext"})
    Ptr Gio.Cancellable.Cancellable ->      -- cancellable : TInterface (Name {namespace = "Gio", name = "Cancellable"})
    FunPtr Gio.Callbacks.C_AsyncReadyCallback -> -- callback : TInterface (Name {namespace = "Gio", name = "AsyncReadyCallback"})
    Ptr () ->                               -- user_data : TBasicType TPtr
    IO ()

-- | Asynchronously load, render and scale a symbolic icon previously
-- looked up from the icon theme using 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'.
-- 
-- For more details, see 'GI.Gtk.Objects.IconInfo.iconInfoLoadSymbolicForContext'
-- which is the synchronous version of this call.
-- 
-- /Since: 3.8/
iconInfoLoadSymbolicForContextAsync ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a, Gtk.StyleContext.IsStyleContext b, Gio.Cancellable.IsCancellable c) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo' from 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'
    -> b
    -- ^ /@context@/: a t'GI.Gtk.Objects.StyleContext.StyleContext'
    -> Maybe (c)
    -- ^ /@cancellable@/: optional t'GI.Gio.Objects.Cancellable.Cancellable' object,
    --     'P.Nothing' to ignore
    -> Maybe (Gio.Callbacks.AsyncReadyCallback)
    -- ^ /@callback@/: a t'GI.Gio.Callbacks.AsyncReadyCallback' to call when the
    --     request is satisfied
    -> m ()
iconInfoLoadSymbolicForContextAsync iconInfo context cancellable callback = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    context' <- unsafeManagedPtrCastPtr context
    maybeCancellable <- case cancellable of
        Nothing -> return nullPtr
        Just jCancellable -> do
            jCancellable' <- unsafeManagedPtrCastPtr jCancellable
            return jCancellable'
    maybeCallback <- case callback of
        Nothing -> return (castPtrToFunPtr nullPtr)
        Just jCallback -> do
            ptrcallback <- callocMem :: IO (Ptr (FunPtr Gio.Callbacks.C_AsyncReadyCallback))
            jCallback' <- Gio.Callbacks.mk_AsyncReadyCallback (Gio.Callbacks.wrap_AsyncReadyCallback (Just ptrcallback) (Gio.Callbacks.drop_closures_AsyncReadyCallback jCallback))
            poke ptrcallback jCallback'
            return jCallback'
    let userData = nullPtr
    gtk_icon_info_load_symbolic_for_context_async iconInfo' context' maybeCancellable maybeCallback userData
    touchManagedPtr iconInfo
    touchManagedPtr context
    whenJust cancellable touchManagedPtr
    return ()

#if defined(ENABLE_OVERLOADING)
data IconInfoLoadSymbolicForContextAsyncMethodInfo
instance (signature ~ (b -> Maybe (c) -> Maybe (Gio.Callbacks.AsyncReadyCallback) -> m ()), MonadIO m, IsIconInfo a, Gtk.StyleContext.IsStyleContext b, Gio.Cancellable.IsCancellable c) => O.OverloadedMethod IconInfoLoadSymbolicForContextAsyncMethodInfo a signature where
    overloadedMethod = iconInfoLoadSymbolicForContextAsync

instance O.OverloadedMethodInfo IconInfoLoadSymbolicForContextAsyncMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoLoadSymbolicForContextAsync",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoLoadSymbolicForContextAsync"
        })


#endif

-- method IconInfo::load_symbolic_for_context_finish
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "a #GtkIconInfo from gtk_icon_theme_lookup_icon()"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "res"
--           , argType =
--               TInterface Name { namespace = "Gio" , name = "AsyncResult" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GAsyncResult" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "was_symbolic"
--           , argType = TBasicType TBoolean
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #gboolean, returns whether the\n    loaded icon was a symbolic one and whether the @fg color was\n    applied to it."
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
-- returnType: Just
--               (TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" })
-- throws : True
-- Skip return : False

foreign import ccall "gtk_icon_info_load_symbolic_for_context_finish" gtk_icon_info_load_symbolic_for_context_finish :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    Ptr Gio.AsyncResult.AsyncResult ->      -- res : TInterface (Name {namespace = "Gio", name = "AsyncResult"})
    Ptr CInt ->                             -- was_symbolic : TBasicType TBoolean
    Ptr (Ptr GError) ->                     -- error
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

-- | Finishes an async icon load, see 'GI.Gtk.Objects.IconInfo.iconInfoLoadSymbolicForContextAsync'.
-- 
-- /Since: 3.8/
iconInfoLoadSymbolicForContextFinish ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a, Gio.AsyncResult.IsAsyncResult b) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo' from 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon'
    -> b
    -- ^ /@res@/: a t'GI.Gio.Interfaces.AsyncResult.AsyncResult'
    -> m ((GdkPixbuf.Pixbuf.Pixbuf, Bool))
    -- ^ __Returns:__ the rendered icon; this may be a newly
    --     created icon or a new reference to an internal icon, so you must
    --     not modify the icon. Use 'GI.GObject.Objects.Object.objectUnref' to release your reference
    --     to the icon. /(Can throw 'Data.GI.Base.GError.GError')/
iconInfoLoadSymbolicForContextFinish iconInfo res = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    res' <- unsafeManagedPtrCastPtr res
    wasSymbolic <- allocMem :: IO (Ptr CInt)
    onException (do
        result <- propagateGError $ gtk_icon_info_load_symbolic_for_context_finish iconInfo' res' wasSymbolic
        checkUnexpectedReturnNULL "iconInfoLoadSymbolicForContextFinish" result
        result' <- (wrapObject GdkPixbuf.Pixbuf.Pixbuf) result
        wasSymbolic' <- peek wasSymbolic
        let wasSymbolic'' = (/= 0) wasSymbolic'
        touchManagedPtr iconInfo
        touchManagedPtr res
        freeMem wasSymbolic
        return (result', wasSymbolic'')
     ) (do
        freeMem wasSymbolic
     )

#if defined(ENABLE_OVERLOADING)
data IconInfoLoadSymbolicForContextFinishMethodInfo
instance (signature ~ (b -> m ((GdkPixbuf.Pixbuf.Pixbuf, Bool))), MonadIO m, IsIconInfo a, Gio.AsyncResult.IsAsyncResult b) => O.OverloadedMethod IconInfoLoadSymbolicForContextFinishMethodInfo a signature where
    overloadedMethod = iconInfoLoadSymbolicForContextFinish

instance O.OverloadedMethodInfo IconInfoLoadSymbolicForContextFinishMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoLoadSymbolicForContextFinish",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoLoadSymbolicForContextFinish"
        })


#endif

-- method IconInfo::load_symbolic_for_style
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle to take the colors from"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget state to use for colors"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "was_symbolic"
--           , argType = TBasicType TBoolean
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #gboolean, returns whether the\n    loaded icon was a symbolic one and whether the @fg color was\n    applied to it."
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
-- returnType: Just
--               (TInterface Name { namespace = "GdkPixbuf" , name = "Pixbuf" })
-- throws : True
-- Skip return : False

foreign import ccall "gtk_icon_info_load_symbolic_for_style" gtk_icon_info_load_symbolic_for_style :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    Ptr Gtk.Style.Style ->                  -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateType"})
    Ptr CInt ->                             -- was_symbolic : TBasicType TBoolean
    Ptr (Ptr GError) ->                     -- error
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

{-# DEPRECATED iconInfoLoadSymbolicForStyle ["(Since version 3.0)","Use 'GI.Gtk.Objects.IconInfo.iconInfoLoadSymbolicForContext' instead"] #-}
-- | Loads an icon, modifying it to match the system colours for the foreground,
-- success, warning and error colors provided. If the icon is not a symbolic
-- one, the function will return the result from 'GI.Gtk.Objects.IconInfo.iconInfoLoadIcon'.
-- 
-- This allows loading symbolic icons that will match the system theme.
-- 
-- See 'GI.Gtk.Objects.IconInfo.iconInfoLoadSymbolic' for more details.
-- 
-- /Since: 3.0/
iconInfoLoadSymbolicForStyle ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a, Gtk.Style.IsStyle b) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo'
    -> b
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style' to take the colors from
    -> Gtk.Enums.StateType
    -- ^ /@state@/: the widget state to use for colors
    -> m ((GdkPixbuf.Pixbuf.Pixbuf, Bool))
    -- ^ __Returns:__ a t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf' representing the loaded icon /(Can throw 'Data.GI.Base.GError.GError')/
iconInfoLoadSymbolicForStyle iconInfo style state = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    style' <- unsafeManagedPtrCastPtr style
    let state' = (fromIntegral . fromEnum) state
    wasSymbolic <- allocMem :: IO (Ptr CInt)
    onException (do
        result <- propagateGError $ gtk_icon_info_load_symbolic_for_style iconInfo' style' state' wasSymbolic
        checkUnexpectedReturnNULL "iconInfoLoadSymbolicForStyle" result
        result' <- (wrapObject GdkPixbuf.Pixbuf.Pixbuf) result
        wasSymbolic' <- peek wasSymbolic
        let wasSymbolic'' = (/= 0) wasSymbolic'
        touchManagedPtr iconInfo
        touchManagedPtr style
        freeMem wasSymbolic
        return (result', wasSymbolic'')
     ) (do
        freeMem wasSymbolic
     )

#if defined(ENABLE_OVERLOADING)
data IconInfoLoadSymbolicForStyleMethodInfo
instance (signature ~ (b -> Gtk.Enums.StateType -> m ((GdkPixbuf.Pixbuf.Pixbuf, Bool))), MonadIO m, IsIconInfo a, Gtk.Style.IsStyle b) => O.OverloadedMethod IconInfoLoadSymbolicForStyleMethodInfo a signature where
    overloadedMethod = iconInfoLoadSymbolicForStyle

instance O.OverloadedMethodInfo IconInfoLoadSymbolicForStyleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoLoadSymbolicForStyle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoLoadSymbolicForStyle"
        })


#endif

-- method IconInfo::set_raw_coordinates
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "icon_info"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconInfo" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconInfo" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "raw_coordinates"
--           , argType = TBasicType TBoolean
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "whether the coordinates of embedded rectangles\n    and attached points should be returned in their original\n    (unscaled) form."
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

foreign import ccall "gtk_icon_info_set_raw_coordinates" gtk_icon_info_set_raw_coordinates :: 
    Ptr IconInfo ->                         -- icon_info : TInterface (Name {namespace = "Gtk", name = "IconInfo"})
    CInt ->                                 -- raw_coordinates : TBasicType TBoolean
    IO ()

{-# DEPRECATED iconInfoSetRawCoordinates ["(Since version 3.14)","Embedded rectangles and attachment points are deprecated"] #-}
-- | Sets whether the coordinates returned by 'GI.Gtk.Objects.IconInfo.iconInfoGetEmbeddedRect'
-- and 'GI.Gtk.Objects.IconInfo.iconInfoGetAttachPoints' should be returned in their
-- original form as specified in the icon theme, instead of scaled
-- appropriately for the pixbuf returned by 'GI.Gtk.Objects.IconInfo.iconInfoLoadIcon'.
-- 
-- Raw coordinates are somewhat strange; they are specified to be with
-- respect to the unscaled pixmap for PNG and XPM icons, but for SVG
-- icons, they are in a 1000x1000 coordinate space that is scaled
-- to the final size of the icon.  You can determine if the icon is an SVG
-- icon by using 'GI.Gtk.Objects.IconInfo.iconInfoGetFilename', and seeing if it is non-'P.Nothing'
-- and ends in “.svg”.
-- 
-- This function is provided primarily to allow compatibility wrappers
-- for older API\'s, and is not expected to be useful for applications.
-- 
-- /Since: 2.4/
iconInfoSetRawCoordinates ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconInfo a) =>
    a
    -- ^ /@iconInfo@/: a t'GI.Gtk.Objects.IconInfo.IconInfo'
    -> Bool
    -- ^ /@rawCoordinates@/: whether the coordinates of embedded rectangles
    --     and attached points should be returned in their original
    --     (unscaled) form.
    -> m ()
iconInfoSetRawCoordinates iconInfo rawCoordinates = liftIO $ do
    iconInfo' <- unsafeManagedPtrCastPtr iconInfo
    let rawCoordinates' = (fromIntegral . fromEnum) rawCoordinates
    gtk_icon_info_set_raw_coordinates iconInfo' rawCoordinates'
    touchManagedPtr iconInfo
    return ()

#if defined(ENABLE_OVERLOADING)
data IconInfoSetRawCoordinatesMethodInfo
instance (signature ~ (Bool -> m ()), MonadIO m, IsIconInfo a) => O.OverloadedMethod IconInfoSetRawCoordinatesMethodInfo a signature where
    overloadedMethod = iconInfoSetRawCoordinates

instance O.OverloadedMethodInfo IconInfoSetRawCoordinatesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconInfo.iconInfoSetRawCoordinates",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconInfo.html#v:iconInfoSetRawCoordinates"
        })


#endif


