{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Objects.WindowGroup.WindowGroup' restricts the effect of grabs to windows
-- in the same group, thereby making window groups almost behave
-- like separate applications.
-- 
-- A window can be a member in at most one window group at a time.
-- Windows that have not been explicitly assigned to a group are
-- implicitly treated like windows of the default window group.
-- 
-- GtkWindowGroup objects are referenced by each window in the group,
-- so once you have added all windows to a GtkWindowGroup, you can drop
-- the initial reference to the window group with 'GI.GObject.Objects.Object.objectUnref'. If the
-- windows in the window group are subsequently destroyed, then they will
-- be removed from the window group and drop their references on the window
-- group; when all window have been removed, the window group will be
-- freed.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.WindowGroup
    ( 

-- * Exported types
    WindowGroup(..)                         ,
    IsWindowGroup                           ,
    toWindowGroup                           ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addWindow]("GI.Gtk.Objects.WindowGroup#g:method:addWindow"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [listWindows]("GI.Gtk.Objects.WindowGroup#g:method:listWindows"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [removeWindow]("GI.Gtk.Objects.WindowGroup#g:method:removeWindow"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getCurrentDeviceGrab]("GI.Gtk.Objects.WindowGroup#g:method:getCurrentDeviceGrab"), [getCurrentGrab]("GI.Gtk.Objects.WindowGroup#g:method:getCurrentGrab"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveWindowGroupMethod                ,
#endif

-- ** addWindow #method:addWindow#

#if defined(ENABLE_OVERLOADING)
    WindowGroupAddWindowMethodInfo          ,
#endif
    windowGroupAddWindow                    ,


-- ** getCurrentDeviceGrab #method:getCurrentDeviceGrab#

#if defined(ENABLE_OVERLOADING)
    WindowGroupGetCurrentDeviceGrabMethodInfo,
#endif
    windowGroupGetCurrentDeviceGrab         ,


-- ** getCurrentGrab #method:getCurrentGrab#

#if defined(ENABLE_OVERLOADING)
    WindowGroupGetCurrentGrabMethodInfo     ,
#endif
    windowGroupGetCurrentGrab               ,


-- ** listWindows #method:listWindows#

#if defined(ENABLE_OVERLOADING)
    WindowGroupListWindowsMethodInfo        ,
#endif
    windowGroupListWindows                  ,


-- ** new #method:new#

    windowGroupNew                          ,


-- ** removeWindow #method:removeWindow#

#if defined(ENABLE_OVERLOADING)
    WindowGroupRemoveWindowMethodInfo       ,
#endif
    windowGroupRemoveWindow                 ,




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

import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gdk.Objects.Device as Gdk.Device
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Objects.Window as Gtk.Window

-- | Memory-managed wrapper type.
newtype WindowGroup = WindowGroup (SP.ManagedPtr WindowGroup)
    deriving (Eq)

instance SP.ManagedPtrNewtype WindowGroup where
    toManagedPtr (WindowGroup p) = p

foreign import ccall "gtk_window_group_get_type"
    c_gtk_window_group_get_type :: IO B.Types.GType

instance B.Types.TypedObject WindowGroup where
    glibType = c_gtk_window_group_get_type

instance B.Types.GObject WindowGroup

-- | Type class for types which can be safely cast to `WindowGroup`, for instance with `toWindowGroup`.
class (SP.GObject o, O.IsDescendantOf WindowGroup o) => IsWindowGroup o
instance (SP.GObject o, O.IsDescendantOf WindowGroup o) => IsWindowGroup o

instance O.HasParentTypes WindowGroup
type instance O.ParentTypes WindowGroup = '[GObject.Object.Object]

-- | Cast to `WindowGroup`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toWindowGroup :: (MIO.MonadIO m, IsWindowGroup o) => o -> m WindowGroup
toWindowGroup = MIO.liftIO . B.ManagedPtr.unsafeCastTo WindowGroup

-- | Convert 'WindowGroup' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe WindowGroup) where
    gvalueGType_ = c_gtk_window_group_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr WindowGroup)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr WindowGroup)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject WindowGroup ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveWindowGroupMethod (t :: Symbol) (o :: *) :: * where
    ResolveWindowGroupMethod "addWindow" o = WindowGroupAddWindowMethodInfo
    ResolveWindowGroupMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveWindowGroupMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveWindowGroupMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveWindowGroupMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveWindowGroupMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveWindowGroupMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveWindowGroupMethod "listWindows" o = WindowGroupListWindowsMethodInfo
    ResolveWindowGroupMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveWindowGroupMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveWindowGroupMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveWindowGroupMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveWindowGroupMethod "removeWindow" o = WindowGroupRemoveWindowMethodInfo
    ResolveWindowGroupMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveWindowGroupMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveWindowGroupMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveWindowGroupMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveWindowGroupMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveWindowGroupMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveWindowGroupMethod "getCurrentDeviceGrab" o = WindowGroupGetCurrentDeviceGrabMethodInfo
    ResolveWindowGroupMethod "getCurrentGrab" o = WindowGroupGetCurrentGrabMethodInfo
    ResolveWindowGroupMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveWindowGroupMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveWindowGroupMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveWindowGroupMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveWindowGroupMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveWindowGroupMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveWindowGroupMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveWindowGroupMethod t WindowGroup, O.OverloadedMethod info WindowGroup p) => OL.IsLabel t (WindowGroup -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveWindowGroupMethod t WindowGroup, O.OverloadedMethod info WindowGroup p, R.HasField t WindowGroup p) => R.HasField t WindowGroup p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveWindowGroupMethod t WindowGroup, O.OverloadedMethodInfo info WindowGroup) => OL.IsLabel t (O.MethodProxy info WindowGroup) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList WindowGroup
type instance O.AttributeList WindowGroup = WindowGroupAttributeList
type WindowGroupAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList WindowGroup = WindowGroupSignalList
type WindowGroupSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method WindowGroup::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "WindowGroup" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_group_new" gtk_window_group_new :: 
    IO (Ptr WindowGroup)

-- | Creates a new t'GI.Gtk.Objects.WindowGroup.WindowGroup' object. Grabs added with
-- 'GI.Gtk.Objects.Widget.widgetGrabAdd' only affect windows within the same t'GI.Gtk.Objects.WindowGroup.WindowGroup'.
windowGroupNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m WindowGroup
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.WindowGroup.WindowGroup'.
windowGroupNew  = liftIO $ do
    result <- gtk_window_group_new
    checkUnexpectedReturnNULL "windowGroupNew" result
    result' <- (wrapObject WindowGroup) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method WindowGroup::add_window
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WindowGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindowGroup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkWindow to add"
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

foreign import ccall "gtk_window_group_add_window" gtk_window_group_add_window :: 
    Ptr WindowGroup ->                      -- window_group : TInterface (Name {namespace = "Gtk", name = "WindowGroup"})
    Ptr Gtk.Window.Window ->                -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Adds a window to a t'GI.Gtk.Objects.WindowGroup.WindowGroup'.
windowGroupAddWindow ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindowGroup a, Gtk.Window.IsWindow b) =>
    a
    -- ^ /@windowGroup@/: a t'GI.Gtk.Objects.WindowGroup.WindowGroup'
    -> b
    -- ^ /@window@/: the t'GI.Gtk.Objects.Window.Window' to add
    -> m ()
windowGroupAddWindow windowGroup window = liftIO $ do
    windowGroup' <- unsafeManagedPtrCastPtr windowGroup
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_group_add_window windowGroup' window'
    touchManagedPtr windowGroup
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowGroupAddWindowMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsWindowGroup a, Gtk.Window.IsWindow b) => O.OverloadedMethod WindowGroupAddWindowMethodInfo a signature where
    overloadedMethod = windowGroupAddWindow

instance O.OverloadedMethodInfo WindowGroupAddWindowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.WindowGroup.windowGroupAddWindow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-WindowGroup.html#v:windowGroupAddWindow"
        })


#endif

-- method WindowGroup::get_current_device_grab
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WindowGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindowGroup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "device"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Device" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkDevice" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_group_get_current_device_grab" gtk_window_group_get_current_device_grab :: 
    Ptr WindowGroup ->                      -- window_group : TInterface (Name {namespace = "Gtk", name = "WindowGroup"})
    Ptr Gdk.Device.Device ->                -- device : TInterface (Name {namespace = "Gdk", name = "Device"})
    IO (Ptr Gtk.Widget.Widget)

-- | Returns the current grab widget for /@device@/, or 'P.Nothing' if none.
-- 
-- /Since: 3.0/
windowGroupGetCurrentDeviceGrab ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindowGroup a, Gdk.Device.IsDevice b) =>
    a
    -- ^ /@windowGroup@/: a t'GI.Gtk.Objects.WindowGroup.WindowGroup'
    -> b
    -- ^ /@device@/: a t'GI.Gdk.Objects.Device.Device'
    -> m (Maybe Gtk.Widget.Widget)
    -- ^ __Returns:__ The grab widget, or 'P.Nothing'
windowGroupGetCurrentDeviceGrab windowGroup device = liftIO $ do
    windowGroup' <- unsafeManagedPtrCastPtr windowGroup
    device' <- unsafeManagedPtrCastPtr device
    result <- gtk_window_group_get_current_device_grab windowGroup' device'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.Widget.Widget) result'
        return result''
    touchManagedPtr windowGroup
    touchManagedPtr device
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data WindowGroupGetCurrentDeviceGrabMethodInfo
instance (signature ~ (b -> m (Maybe Gtk.Widget.Widget)), MonadIO m, IsWindowGroup a, Gdk.Device.IsDevice b) => O.OverloadedMethod WindowGroupGetCurrentDeviceGrabMethodInfo a signature where
    overloadedMethod = windowGroupGetCurrentDeviceGrab

instance O.OverloadedMethodInfo WindowGroupGetCurrentDeviceGrabMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.WindowGroup.windowGroupGetCurrentDeviceGrab",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-WindowGroup.html#v:windowGroupGetCurrentDeviceGrab"
        })


#endif

-- method WindowGroup::get_current_grab
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WindowGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindowGroup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Widget" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_group_get_current_grab" gtk_window_group_get_current_grab :: 
    Ptr WindowGroup ->                      -- window_group : TInterface (Name {namespace = "Gtk", name = "WindowGroup"})
    IO (Ptr Gtk.Widget.Widget)

-- | Gets the current grab widget of the given group,
-- see 'GI.Gtk.Objects.Widget.widgetGrabAdd'.
-- 
-- /Since: 2.22/
windowGroupGetCurrentGrab ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindowGroup a) =>
    a
    -- ^ /@windowGroup@/: a t'GI.Gtk.Objects.WindowGroup.WindowGroup'
    -> m Gtk.Widget.Widget
    -- ^ __Returns:__ the current grab widget of the group
windowGroupGetCurrentGrab windowGroup = liftIO $ do
    windowGroup' <- unsafeManagedPtrCastPtr windowGroup
    result <- gtk_window_group_get_current_grab windowGroup'
    checkUnexpectedReturnNULL "windowGroupGetCurrentGrab" result
    result' <- (newObject Gtk.Widget.Widget) result
    touchManagedPtr windowGroup
    return result'

#if defined(ENABLE_OVERLOADING)
data WindowGroupGetCurrentGrabMethodInfo
instance (signature ~ (m Gtk.Widget.Widget), MonadIO m, IsWindowGroup a) => O.OverloadedMethod WindowGroupGetCurrentGrabMethodInfo a signature where
    overloadedMethod = windowGroupGetCurrentGrab

instance O.OverloadedMethodInfo WindowGroupGetCurrentGrabMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.WindowGroup.windowGroupGetCurrentGrab",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-WindowGroup.html#v:windowGroupGetCurrentGrab"
        })


#endif

-- method WindowGroup::list_windows
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WindowGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindowGroup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just
--               (TGList (TInterface Name { namespace = "Gtk" , name = "Window" }))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_window_group_list_windows" gtk_window_group_list_windows :: 
    Ptr WindowGroup ->                      -- window_group : TInterface (Name {namespace = "Gtk", name = "WindowGroup"})
    IO (Ptr (GList (Ptr Gtk.Window.Window)))

-- | Returns a list of the @/GtkWindows/@ that belong to /@windowGroup@/.
-- 
-- /Since: 2.14/
windowGroupListWindows ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindowGroup a) =>
    a
    -- ^ /@windowGroup@/: a t'GI.Gtk.Objects.WindowGroup.WindowGroup'
    -> m [Gtk.Window.Window]
    -- ^ __Returns:__ A
    --   newly-allocated list of windows inside the group.
windowGroupListWindows windowGroup = liftIO $ do
    windowGroup' <- unsafeManagedPtrCastPtr windowGroup
    result <- gtk_window_group_list_windows windowGroup'
    result' <- unpackGList result
    result'' <- mapM (newObject Gtk.Window.Window) result'
    g_list_free result
    touchManagedPtr windowGroup
    return result''

#if defined(ENABLE_OVERLOADING)
data WindowGroupListWindowsMethodInfo
instance (signature ~ (m [Gtk.Window.Window]), MonadIO m, IsWindowGroup a) => O.OverloadedMethod WindowGroupListWindowsMethodInfo a signature where
    overloadedMethod = windowGroupListWindows

instance O.OverloadedMethodInfo WindowGroupListWindowsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.WindowGroup.windowGroupListWindows",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-WindowGroup.html#v:windowGroupListWindows"
        })


#endif

-- method WindowGroup::remove_window
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "window_group"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WindowGroup" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWindowGroup" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GtkWindow to remove"
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

foreign import ccall "gtk_window_group_remove_window" gtk_window_group_remove_window :: 
    Ptr WindowGroup ->                      -- window_group : TInterface (Name {namespace = "Gtk", name = "WindowGroup"})
    Ptr Gtk.Window.Window ->                -- window : TInterface (Name {namespace = "Gtk", name = "Window"})
    IO ()

-- | Removes a window from a t'GI.Gtk.Objects.WindowGroup.WindowGroup'.
windowGroupRemoveWindow ::
    (B.CallStack.HasCallStack, MonadIO m, IsWindowGroup a, Gtk.Window.IsWindow b) =>
    a
    -- ^ /@windowGroup@/: a t'GI.Gtk.Objects.WindowGroup.WindowGroup'
    -> b
    -- ^ /@window@/: the t'GI.Gtk.Objects.Window.Window' to remove
    -> m ()
windowGroupRemoveWindow windowGroup window = liftIO $ do
    windowGroup' <- unsafeManagedPtrCastPtr windowGroup
    window' <- unsafeManagedPtrCastPtr window
    gtk_window_group_remove_window windowGroup' window'
    touchManagedPtr windowGroup
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data WindowGroupRemoveWindowMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsWindowGroup a, Gtk.Window.IsWindow b) => O.OverloadedMethod WindowGroupRemoveWindowMethodInfo a signature where
    overloadedMethod = windowGroupRemoveWindow

instance O.OverloadedMethodInfo WindowGroupRemoveWindowMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.WindowGroup.windowGroupRemoveWindow",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-WindowGroup.html#v:windowGroupRemoveWindow"
        })


#endif


