{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.IconTheme.IconTheme' provides a facility for looking up icons by name
-- and size. The main reason for using a name rather than simply
-- providing a filename is to allow different icons to be used
-- depending on what “icon theme” is selected
-- by the user. The operation of icon themes on Linux and Unix
-- follows the <http://www.freedesktop.org/Standards/icon-theme-spec Icon Theme Specification>
-- There is a fallback icon theme, named @hicolor@, where applications
-- should install their icons, but additional icon themes can be installed
-- as operating system vendors and users choose.
-- 
-- Named icons are similar to the deprecated [Stock Items][gtkstock],
-- and the distinction between the two may be a bit confusing.
-- A few things to keep in mind:
-- 
-- * Stock images usually are used in conjunction with
-- [Stock Items][gtkstock], such as 'GI.Gtk.Constants.STOCK_OK' or
-- 'GI.Gtk.Constants.STOCK_OPEN'. Named icons are easier to set up and therefore
-- are more useful for new icons that an application wants to
-- add, such as application icons or window icons.
-- * Stock images can only be loaded at the symbolic sizes defined
-- by the t'GI.Gtk.Enums.IconSize' enumeration, or by custom sizes defined
-- by 'GI.Gtk.Functions.iconSizeRegister', while named icons are more flexible
-- and any pixel size can be specified.
-- * Because stock images are closely tied to stock items, and thus
-- to actions in the user interface, stock images may come in
-- multiple variants for different widget states or writing
-- directions.
-- 
-- 
-- A good rule of thumb is that if there is a stock image for what
-- you want to use, use it, otherwise use a named icon. It turns
-- out that internally stock images are generally defined in
-- terms of one or more named icons. (An example of the
-- more than one case is icons that depend on writing direction;
-- 'GI.Gtk.Constants.STOCK_GO_FORWARD' uses the two themed icons
-- “gtk-stock-go-forward-ltr” and “gtk-stock-go-forward-rtl”.)
-- 
-- In many cases, named themes are used indirectly, via t'GI.Gtk.Objects.Image.Image'
-- or stock items, rather than directly, but looking up icons
-- directly is also simple. The t'GI.Gtk.Objects.IconTheme.IconTheme' object acts
-- as a database of all the icons in the current theme. You
-- can create new t'GI.Gtk.Objects.IconTheme.IconTheme' objects, but it’s much more
-- efficient to use the standard icon theme for the t'GI.Gdk.Objects.Screen.Screen'
-- so that the icon information is shared with other people
-- looking up icons.
-- 
-- === /C code/
-- >
-- >GError *error = NULL;
-- >GtkIconTheme *icon_theme;
-- >GdkPixbuf *pixbuf;
-- >
-- >icon_theme = gtk_icon_theme_get_default ();
-- >pixbuf = gtk_icon_theme_load_icon (icon_theme,
-- >                                   "my-icon-name", // icon name
-- >                                   48, // icon size
-- >                                   0,  // flags
-- >                                   &error);
-- >if (!pixbuf)
-- >  {
-- >    g_warning ("Couldn’t load icon: %s", error->message);
-- >    g_error_free (error);
-- >  }
-- >else
-- >  {
-- >    // Use the pixbuf
-- >    g_object_unref (pixbuf);
-- >  }
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.IconTheme
    ( 

-- * Exported types
    IconTheme(..)                           ,
    IsIconTheme                             ,
    toIconTheme                             ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [addResourcePath]("GI.Gtk.Objects.IconTheme#g:method:addResourcePath"), [appendSearchPath]("GI.Gtk.Objects.IconTheme#g:method:appendSearchPath"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [chooseIcon]("GI.Gtk.Objects.IconTheme#g:method:chooseIcon"), [chooseIconForScale]("GI.Gtk.Objects.IconTheme#g:method:chooseIconForScale"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [hasIcon]("GI.Gtk.Objects.IconTheme#g:method:hasIcon"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [listContexts]("GI.Gtk.Objects.IconTheme#g:method:listContexts"), [listIcons]("GI.Gtk.Objects.IconTheme#g:method:listIcons"), [loadIcon]("GI.Gtk.Objects.IconTheme#g:method:loadIcon"), [loadIconForScale]("GI.Gtk.Objects.IconTheme#g:method:loadIconForScale"), [loadSurface]("GI.Gtk.Objects.IconTheme#g:method:loadSurface"), [lookupByGicon]("GI.Gtk.Objects.IconTheme#g:method:lookupByGicon"), [lookupByGiconForScale]("GI.Gtk.Objects.IconTheme#g:method:lookupByGiconForScale"), [lookupIcon]("GI.Gtk.Objects.IconTheme#g:method:lookupIcon"), [lookupIconForScale]("GI.Gtk.Objects.IconTheme#g:method:lookupIconForScale"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [prependSearchPath]("GI.Gtk.Objects.IconTheme#g:method:prependSearchPath"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [rescanIfNeeded]("GI.Gtk.Objects.IconTheme#g:method:rescanIfNeeded"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getExampleIconName]("GI.Gtk.Objects.IconTheme#g:method:getExampleIconName"), [getIconSizes]("GI.Gtk.Objects.IconTheme#g:method:getIconSizes"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getSearchPath]("GI.Gtk.Objects.IconTheme#g:method:getSearchPath").
-- 
-- ==== Setters
-- [setCustomTheme]("GI.Gtk.Objects.IconTheme#g:method:setCustomTheme"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty"), [setScreen]("GI.Gtk.Objects.IconTheme#g:method:setScreen"), [setSearchPath]("GI.Gtk.Objects.IconTheme#g:method:setSearchPath").

#if defined(ENABLE_OVERLOADING)
    ResolveIconThemeMethod                  ,
#endif

-- ** addBuiltinIcon #method:addBuiltinIcon#

    iconThemeAddBuiltinIcon                 ,


-- ** addResourcePath #method:addResourcePath#

#if defined(ENABLE_OVERLOADING)
    IconThemeAddResourcePathMethodInfo      ,
#endif
    iconThemeAddResourcePath                ,


-- ** appendSearchPath #method:appendSearchPath#

#if defined(ENABLE_OVERLOADING)
    IconThemeAppendSearchPathMethodInfo     ,
#endif
    iconThemeAppendSearchPath               ,


-- ** chooseIcon #method:chooseIcon#

#if defined(ENABLE_OVERLOADING)
    IconThemeChooseIconMethodInfo           ,
#endif
    iconThemeChooseIcon                     ,


-- ** chooseIconForScale #method:chooseIconForScale#

#if defined(ENABLE_OVERLOADING)
    IconThemeChooseIconForScaleMethodInfo   ,
#endif
    iconThemeChooseIconForScale             ,


-- ** getDefault #method:getDefault#

    iconThemeGetDefault                     ,


-- ** getExampleIconName #method:getExampleIconName#

#if defined(ENABLE_OVERLOADING)
    IconThemeGetExampleIconNameMethodInfo   ,
#endif
    iconThemeGetExampleIconName             ,


-- ** getForScreen #method:getForScreen#

    iconThemeGetForScreen                   ,


-- ** getIconSizes #method:getIconSizes#

#if defined(ENABLE_OVERLOADING)
    IconThemeGetIconSizesMethodInfo         ,
#endif
    iconThemeGetIconSizes                   ,


-- ** getSearchPath #method:getSearchPath#

#if defined(ENABLE_OVERLOADING)
    IconThemeGetSearchPathMethodInfo        ,
#endif
    iconThemeGetSearchPath                  ,


-- ** hasIcon #method:hasIcon#

#if defined(ENABLE_OVERLOADING)
    IconThemeHasIconMethodInfo              ,
#endif
    iconThemeHasIcon                        ,


-- ** listContexts #method:listContexts#

#if defined(ENABLE_OVERLOADING)
    IconThemeListContextsMethodInfo         ,
#endif
    iconThemeListContexts                   ,


-- ** listIcons #method:listIcons#

#if defined(ENABLE_OVERLOADING)
    IconThemeListIconsMethodInfo            ,
#endif
    iconThemeListIcons                      ,


-- ** loadIcon #method:loadIcon#

#if defined(ENABLE_OVERLOADING)
    IconThemeLoadIconMethodInfo             ,
#endif
    iconThemeLoadIcon                       ,


-- ** loadIconForScale #method:loadIconForScale#

#if defined(ENABLE_OVERLOADING)
    IconThemeLoadIconForScaleMethodInfo     ,
#endif
    iconThemeLoadIconForScale               ,


-- ** loadSurface #method:loadSurface#

#if defined(ENABLE_OVERLOADING)
    IconThemeLoadSurfaceMethodInfo          ,
#endif
    iconThemeLoadSurface                    ,


-- ** lookupByGicon #method:lookupByGicon#

#if defined(ENABLE_OVERLOADING)
    IconThemeLookupByGiconMethodInfo        ,
#endif
    iconThemeLookupByGicon                  ,


-- ** lookupByGiconForScale #method:lookupByGiconForScale#

#if defined(ENABLE_OVERLOADING)
    IconThemeLookupByGiconForScaleMethodInfo,
#endif
    iconThemeLookupByGiconForScale          ,


-- ** lookupIcon #method:lookupIcon#

#if defined(ENABLE_OVERLOADING)
    IconThemeLookupIconMethodInfo           ,
#endif
    iconThemeLookupIcon                     ,


-- ** lookupIconForScale #method:lookupIconForScale#

#if defined(ENABLE_OVERLOADING)
    IconThemeLookupIconForScaleMethodInfo   ,
#endif
    iconThemeLookupIconForScale             ,


-- ** new #method:new#

    iconThemeNew                            ,


-- ** prependSearchPath #method:prependSearchPath#

#if defined(ENABLE_OVERLOADING)
    IconThemePrependSearchPathMethodInfo    ,
#endif
    iconThemePrependSearchPath              ,


-- ** rescanIfNeeded #method:rescanIfNeeded#

#if defined(ENABLE_OVERLOADING)
    IconThemeRescanIfNeededMethodInfo       ,
#endif
    iconThemeRescanIfNeeded                 ,


-- ** setCustomTheme #method:setCustomTheme#

#if defined(ENABLE_OVERLOADING)
    IconThemeSetCustomThemeMethodInfo       ,
#endif
    iconThemeSetCustomTheme                 ,


-- ** setScreen #method:setScreen#

#if defined(ENABLE_OVERLOADING)
    IconThemeSetScreenMethodInfo            ,
#endif
    iconThemeSetScreen                      ,


-- ** setSearchPath #method:setSearchPath#

#if defined(ENABLE_OVERLOADING)
    IconThemeSetSearchPathMethodInfo        ,
#endif
    iconThemeSetSearchPath                  ,




 -- * Signals


-- ** changed #signal:changed#

    IconThemeChangedCallback                ,
#if defined(ENABLE_OVERLOADING)
    IconThemeChangedSignalInfo              ,
#endif
    afterIconThemeChanged                   ,
    onIconThemeChanged                      ,




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
import qualified GI.Gdk.Objects.Screen as Gdk.Screen
import qualified GI.Gdk.Objects.Window as Gdk.Window
import qualified GI.GdkPixbuf.Objects.Pixbuf as GdkPixbuf.Pixbuf
import qualified GI.Gio.Interfaces.Icon as Gio.Icon
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Objects.IconInfo as Gtk.IconInfo

-- | Memory-managed wrapper type.
newtype IconTheme = IconTheme (SP.ManagedPtr IconTheme)
    deriving (Eq)

instance SP.ManagedPtrNewtype IconTheme where
    toManagedPtr (IconTheme p) = p

foreign import ccall "gtk_icon_theme_get_type"
    c_gtk_icon_theme_get_type :: IO B.Types.GType

instance B.Types.TypedObject IconTheme where
    glibType = c_gtk_icon_theme_get_type

instance B.Types.GObject IconTheme

-- | Type class for types which can be safely cast to `IconTheme`, for instance with `toIconTheme`.
class (SP.GObject o, O.IsDescendantOf IconTheme o) => IsIconTheme o
instance (SP.GObject o, O.IsDescendantOf IconTheme o) => IsIconTheme o

instance O.HasParentTypes IconTheme
type instance O.ParentTypes IconTheme = '[GObject.Object.Object]

-- | Cast to `IconTheme`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toIconTheme :: (MIO.MonadIO m, IsIconTheme o) => o -> m IconTheme
toIconTheme = MIO.liftIO . B.ManagedPtr.unsafeCastTo IconTheme

-- | Convert 'IconTheme' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe IconTheme) where
    gvalueGType_ = c_gtk_icon_theme_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr IconTheme)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr IconTheme)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject IconTheme ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveIconThemeMethod (t :: Symbol) (o :: *) :: * where
    ResolveIconThemeMethod "addResourcePath" o = IconThemeAddResourcePathMethodInfo
    ResolveIconThemeMethod "appendSearchPath" o = IconThemeAppendSearchPathMethodInfo
    ResolveIconThemeMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveIconThemeMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveIconThemeMethod "chooseIcon" o = IconThemeChooseIconMethodInfo
    ResolveIconThemeMethod "chooseIconForScale" o = IconThemeChooseIconForScaleMethodInfo
    ResolveIconThemeMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveIconThemeMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveIconThemeMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveIconThemeMethod "hasIcon" o = IconThemeHasIconMethodInfo
    ResolveIconThemeMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveIconThemeMethod "listContexts" o = IconThemeListContextsMethodInfo
    ResolveIconThemeMethod "listIcons" o = IconThemeListIconsMethodInfo
    ResolveIconThemeMethod "loadIcon" o = IconThemeLoadIconMethodInfo
    ResolveIconThemeMethod "loadIconForScale" o = IconThemeLoadIconForScaleMethodInfo
    ResolveIconThemeMethod "loadSurface" o = IconThemeLoadSurfaceMethodInfo
    ResolveIconThemeMethod "lookupByGicon" o = IconThemeLookupByGiconMethodInfo
    ResolveIconThemeMethod "lookupByGiconForScale" o = IconThemeLookupByGiconForScaleMethodInfo
    ResolveIconThemeMethod "lookupIcon" o = IconThemeLookupIconMethodInfo
    ResolveIconThemeMethod "lookupIconForScale" o = IconThemeLookupIconForScaleMethodInfo
    ResolveIconThemeMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveIconThemeMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveIconThemeMethod "prependSearchPath" o = IconThemePrependSearchPathMethodInfo
    ResolveIconThemeMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveIconThemeMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveIconThemeMethod "rescanIfNeeded" o = IconThemeRescanIfNeededMethodInfo
    ResolveIconThemeMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveIconThemeMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveIconThemeMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveIconThemeMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveIconThemeMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveIconThemeMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveIconThemeMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveIconThemeMethod "getExampleIconName" o = IconThemeGetExampleIconNameMethodInfo
    ResolveIconThemeMethod "getIconSizes" o = IconThemeGetIconSizesMethodInfo
    ResolveIconThemeMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveIconThemeMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveIconThemeMethod "getSearchPath" o = IconThemeGetSearchPathMethodInfo
    ResolveIconThemeMethod "setCustomTheme" o = IconThemeSetCustomThemeMethodInfo
    ResolveIconThemeMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveIconThemeMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveIconThemeMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveIconThemeMethod "setScreen" o = IconThemeSetScreenMethodInfo
    ResolveIconThemeMethod "setSearchPath" o = IconThemeSetSearchPathMethodInfo
    ResolveIconThemeMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveIconThemeMethod t IconTheme, O.OverloadedMethod info IconTheme p) => OL.IsLabel t (IconTheme -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveIconThemeMethod t IconTheme, O.OverloadedMethod info IconTheme p, R.HasField t IconTheme p) => R.HasField t IconTheme p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveIconThemeMethod t IconTheme, O.OverloadedMethodInfo info IconTheme) => OL.IsLabel t (O.MethodProxy info IconTheme) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal IconTheme::changed
-- | Emitted when the current icon theme is switched or GTK+ detects
-- that a change has occurred in the contents of the current
-- icon theme.
type IconThemeChangedCallback =
    IO ()

type C_IconThemeChangedCallback =
    Ptr IconTheme ->                        -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_IconThemeChangedCallback`.
foreign import ccall "wrapper"
    mk_IconThemeChangedCallback :: C_IconThemeChangedCallback -> IO (FunPtr C_IconThemeChangedCallback)

wrap_IconThemeChangedCallback :: 
    GObject a => (a -> IconThemeChangedCallback) ->
    C_IconThemeChangedCallback
wrap_IconThemeChangedCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' iconTheme #changed callback
-- @
-- 
-- 
onIconThemeChanged :: (IsIconTheme a, MonadIO m) => a -> ((?self :: a) => IconThemeChangedCallback) -> m SignalHandlerId
onIconThemeChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_IconThemeChangedCallback wrapped
    wrapped'' <- mk_IconThemeChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [changed](#signal:changed) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' iconTheme #changed callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterIconThemeChanged :: (IsIconTheme a, MonadIO m) => a -> ((?self :: a) => IconThemeChangedCallback) -> m SignalHandlerId
afterIconThemeChanged obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_IconThemeChangedCallback wrapped
    wrapped'' <- mk_IconThemeChangedCallback wrapped'
    connectSignalFunPtr obj "changed" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data IconThemeChangedSignalInfo
instance SignalInfo IconThemeChangedSignalInfo where
    type HaskellCallbackType IconThemeChangedSignalInfo = IconThemeChangedCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_IconThemeChangedCallback cb
        cb'' <- mk_IconThemeChangedCallback cb'
        connectSignalFunPtr obj "changed" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme::changed"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#g:signal:changed"})

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList IconTheme
type instance O.AttributeList IconTheme = IconThemeAttributeList
type IconThemeAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList IconTheme = IconThemeSignalList
type IconThemeSignalList = ('[ '("changed", IconThemeChangedSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method IconTheme::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "IconTheme" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_theme_new" gtk_icon_theme_new :: 
    IO (Ptr IconTheme)

-- | Creates a new icon theme object. Icon theme objects are used
-- to lookup up an icon by name in a particular icon theme.
-- Usually, you’ll want to use 'GI.Gtk.Objects.IconTheme.iconThemeGetDefault'
-- or 'GI.Gtk.Objects.IconTheme.iconThemeGetForScreen' rather than creating
-- a new icon theme object for scratch.
-- 
-- /Since: 2.4/
iconThemeNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m IconTheme
    -- ^ __Returns:__ the newly created t'GI.Gtk.Objects.IconTheme.IconTheme' object.
iconThemeNew  = liftIO $ do
    result <- gtk_icon_theme_new
    checkUnexpectedReturnNULL "iconThemeNew" result
    result' <- (wrapObject IconTheme) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method IconTheme::add_resource_path
-- method type : OrdinaryMethod
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
--           { argCName = "path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a resource path" , sinceVersion = Nothing }
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

foreign import ccall "gtk_icon_theme_add_resource_path" gtk_icon_theme_add_resource_path :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    CString ->                              -- path : TBasicType TUTF8
    IO ()

-- | Adds a resource path that will be looked at when looking
-- for icons, similar to search paths.
-- 
-- This function should be used to make application-specific icons
-- available as part of the icon theme.
-- 
-- The resources are considered as part of the hicolor icon theme
-- and must be located in subdirectories that are defined in the
-- hicolor icon theme, such as @\@path\/16x16\/actions\/run.png@.
-- Icons that are directly placed in the resource path instead
-- of a subdirectory are also considered as ultimate fallback.
-- 
-- /Since: 3.14/
iconThemeAddResourcePath ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> T.Text
    -- ^ /@path@/: a resource path
    -> m ()
iconThemeAddResourcePath iconTheme path = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    path' <- textToCString path
    gtk_icon_theme_add_resource_path iconTheme' path'
    touchManagedPtr iconTheme
    freeMem path'
    return ()

#if defined(ENABLE_OVERLOADING)
data IconThemeAddResourcePathMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeAddResourcePathMethodInfo a signature where
    overloadedMethod = iconThemeAddResourcePath

instance O.OverloadedMethodInfo IconThemeAddResourcePathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeAddResourcePath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeAddResourcePath"
        })


#endif

-- method IconTheme::append_search_path
-- method type : OrdinaryMethod
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
--           { argCName = "path"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "directory name to append to the icon path"
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

foreign import ccall "gtk_icon_theme_append_search_path" gtk_icon_theme_append_search_path :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    CString ->                              -- path : TBasicType TFileName
    IO ()

-- | Appends a directory to the search path.
-- See 'GI.Gtk.Objects.IconTheme.iconThemeSetSearchPath'.
-- 
-- /Since: 2.4/
iconThemeAppendSearchPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> [Char]
    -- ^ /@path@/: directory name to append to the icon path
    -> m ()
iconThemeAppendSearchPath iconTheme path = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    path' <- stringToCString path
    gtk_icon_theme_append_search_path iconTheme' path'
    touchManagedPtr iconTheme
    freeMem path'
    return ()

#if defined(ENABLE_OVERLOADING)
data IconThemeAppendSearchPathMethodInfo
instance (signature ~ ([Char] -> m ()), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeAppendSearchPathMethodInfo a signature where
    overloadedMethod = iconThemeAppendSearchPath

instance O.OverloadedMethodInfo IconThemeAppendSearchPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeAppendSearchPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeAppendSearchPath"
        })


#endif

-- method IconTheme::choose_icon
-- method type : OrdinaryMethod
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
--           { argCName = "icon_names"
--           , argType = TCArray True (-1) (-1) (TBasicType TUTF8)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%NULL-terminated array of\n    icon names to lookup"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "desired icon size" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconLookupFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "flags modifying the behavior of the icon lookup"
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

foreign import ccall "gtk_icon_theme_choose_icon" gtk_icon_theme_choose_icon :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    Ptr CString ->                          -- icon_names : TCArray True (-1) (-1) (TBasicType TUTF8)
    Int32 ->                                -- size : TBasicType TInt
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "IconLookupFlags"})
    IO (Ptr Gtk.IconInfo.IconInfo)

-- | Looks up a named icon and returns a t'GI.Gtk.Objects.IconInfo.IconInfo' containing
-- information such as the filename of the icon. The icon
-- can then be rendered into a pixbuf using
-- 'GI.Gtk.Objects.IconInfo.iconInfoLoadIcon'. ('GI.Gtk.Objects.IconTheme.iconThemeLoadIcon'
-- combines these two steps if all you need is the pixbuf.)
-- 
-- If /@iconNames@/ contains more than one name, this function
-- tries them all in the given order before falling back to
-- inherited icon themes.
-- 
-- /Since: 2.12/
iconThemeChooseIcon ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> [T.Text]
    -- ^ /@iconNames@/: 'P.Nothing'-terminated array of
    --     icon names to lookup
    -> Int32
    -- ^ /@size@/: desired icon size
    -> [Gtk.Flags.IconLookupFlags]
    -- ^ /@flags@/: flags modifying the behavior of the icon lookup
    -> m (Maybe Gtk.IconInfo.IconInfo)
    -- ^ __Returns:__ a t'GI.Gtk.Objects.IconInfo.IconInfo' object
    -- containing information about the icon, or 'P.Nothing' if the icon wasn’t
    -- found.
iconThemeChooseIcon iconTheme iconNames size flags = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    iconNames' <- packZeroTerminatedUTF8CArray iconNames
    let flags' = gflagsToWord flags
    result <- gtk_icon_theme_choose_icon iconTheme' iconNames' size flags'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapObject Gtk.IconInfo.IconInfo) result'
        return result''
    touchManagedPtr iconTheme
    mapZeroTerminatedCArray freeMem iconNames'
    freeMem iconNames'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data IconThemeChooseIconMethodInfo
instance (signature ~ ([T.Text] -> Int32 -> [Gtk.Flags.IconLookupFlags] -> m (Maybe Gtk.IconInfo.IconInfo)), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeChooseIconMethodInfo a signature where
    overloadedMethod = iconThemeChooseIcon

instance O.OverloadedMethodInfo IconThemeChooseIconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeChooseIcon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeChooseIcon"
        })


#endif

-- method IconTheme::choose_icon_for_scale
-- method type : OrdinaryMethod
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
--           { argCName = "icon_names"
--           , argType = TCArray True (-1) (-1) (TBasicType TUTF8)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "%NULL-terminated\n    array of icon names to lookup"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "desired icon size" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "scale"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "desired scale" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconLookupFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "flags modifying the behavior of the icon lookup"
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

foreign import ccall "gtk_icon_theme_choose_icon_for_scale" gtk_icon_theme_choose_icon_for_scale :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    Ptr CString ->                          -- icon_names : TCArray True (-1) (-1) (TBasicType TUTF8)
    Int32 ->                                -- size : TBasicType TInt
    Int32 ->                                -- scale : TBasicType TInt
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "IconLookupFlags"})
    IO (Ptr Gtk.IconInfo.IconInfo)

-- | Looks up a named icon for a particular window scale and returns
-- a t'GI.Gtk.Objects.IconInfo.IconInfo' containing information such as the filename of the
-- icon. The icon can then be rendered into a pixbuf using
-- 'GI.Gtk.Objects.IconInfo.iconInfoLoadIcon'. ('GI.Gtk.Objects.IconTheme.iconThemeLoadIcon'
-- combines these two steps if all you need is the pixbuf.)
-- 
-- If /@iconNames@/ contains more than one name, this function
-- tries them all in the given order before falling back to
-- inherited icon themes.
-- 
-- /Since: 3.10/
iconThemeChooseIconForScale ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> [T.Text]
    -- ^ /@iconNames@/: 'P.Nothing'-terminated
    --     array of icon names to lookup
    -> Int32
    -- ^ /@size@/: desired icon size
    -> Int32
    -- ^ /@scale@/: desired scale
    -> [Gtk.Flags.IconLookupFlags]
    -- ^ /@flags@/: flags modifying the behavior of the icon lookup
    -> m (Maybe Gtk.IconInfo.IconInfo)
    -- ^ __Returns:__ a t'GI.Gtk.Objects.IconInfo.IconInfo' object
    --     containing information about the icon, or 'P.Nothing' if the
    --     icon wasn’t found.
iconThemeChooseIconForScale iconTheme iconNames size scale flags = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    iconNames' <- packZeroTerminatedUTF8CArray iconNames
    let flags' = gflagsToWord flags
    result <- gtk_icon_theme_choose_icon_for_scale iconTheme' iconNames' size scale flags'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapObject Gtk.IconInfo.IconInfo) result'
        return result''
    touchManagedPtr iconTheme
    mapZeroTerminatedCArray freeMem iconNames'
    freeMem iconNames'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data IconThemeChooseIconForScaleMethodInfo
instance (signature ~ ([T.Text] -> Int32 -> Int32 -> [Gtk.Flags.IconLookupFlags] -> m (Maybe Gtk.IconInfo.IconInfo)), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeChooseIconForScaleMethodInfo a signature where
    overloadedMethod = iconThemeChooseIconForScale

instance O.OverloadedMethodInfo IconThemeChooseIconForScaleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeChooseIconForScale",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeChooseIconForScale"
        })


#endif

-- method IconTheme::get_example_icon_name
-- method type : OrdinaryMethod
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
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_theme_get_example_icon_name" gtk_icon_theme_get_example_icon_name :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    IO CString

-- | Gets the name of an icon that is representative of the
-- current theme (for instance, to use when presenting
-- a list of themes to the user.)
-- 
-- /Since: 2.4/
iconThemeGetExampleIconName ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> m (Maybe T.Text)
    -- ^ __Returns:__ the name of an example icon or 'P.Nothing'.
    --     Free with 'GI.GLib.Functions.free'.
iconThemeGetExampleIconName iconTheme = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    result <- gtk_icon_theme_get_example_icon_name iconTheme'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- cstringToText result'
        freeMem result'
        return result''
    touchManagedPtr iconTheme
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data IconThemeGetExampleIconNameMethodInfo
instance (signature ~ (m (Maybe T.Text)), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeGetExampleIconNameMethodInfo a signature where
    overloadedMethod = iconThemeGetExampleIconName

instance O.OverloadedMethodInfo IconThemeGetExampleIconNameMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeGetExampleIconName",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeGetExampleIconName"
        })


#endif

-- method IconTheme::get_icon_sizes
-- method type : OrdinaryMethod
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
--           { argCName = "icon_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of an icon"
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
-- returnType: Just (TCArray True (-1) (-1) (TBasicType TInt))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_theme_get_icon_sizes" gtk_icon_theme_get_icon_sizes :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    CString ->                              -- icon_name : TBasicType TUTF8
    IO (Ptr Int32)

-- | Returns an array of integers describing the sizes at which
-- the icon is available without scaling. A size of -1 means
-- that the icon is available in a scalable format. The array
-- is zero-terminated.
-- 
-- /Since: 2.6/
iconThemeGetIconSizes ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> T.Text
    -- ^ /@iconName@/: the name of an icon
    -> m [Int32]
    -- ^ __Returns:__ An newly
    -- allocated array describing the sizes at which the icon is
    -- available. The array should be freed with 'GI.GLib.Functions.free' when it is no
    -- longer needed.
iconThemeGetIconSizes iconTheme iconName = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    iconName' <- textToCString iconName
    result <- gtk_icon_theme_get_icon_sizes iconTheme' iconName'
    checkUnexpectedReturnNULL "iconThemeGetIconSizes" result
    result' <- unpackZeroTerminatedStorableArray result
    freeMem result
    touchManagedPtr iconTheme
    freeMem iconName'
    return result'

#if defined(ENABLE_OVERLOADING)
data IconThemeGetIconSizesMethodInfo
instance (signature ~ (T.Text -> m [Int32]), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeGetIconSizesMethodInfo a signature where
    overloadedMethod = iconThemeGetIconSizes

instance O.OverloadedMethodInfo IconThemeGetIconSizesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeGetIconSizes",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeGetIconSizes"
        })


#endif

-- method IconTheme::get_search_path
-- method type : OrdinaryMethod
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
--           { argCName = "path"
--           , argType = TCArray False (-1) 2 (TBasicType TFileName)
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "\n    location to store a list of icon theme path directories or %NULL.\n    The stored value should be freed with g_strfreev()."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferEverything
--           }
--       , Arg
--           { argCName = "n_elements"
--           , argType = TBasicType TInt
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "location to store number of elements in @path, or %NULL"
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
--              { argCName = "n_elements"
--              , argType = TBasicType TInt
--              , direction = DirectionOut
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText =
--                        Just "location to store number of elements in @path, or %NULL"
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferEverything
--              }
--          ]
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_theme_get_search_path" gtk_icon_theme_get_search_path :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    Ptr (Ptr CString) ->                    -- path : TCArray False (-1) 2 (TBasicType TFileName)
    Ptr Int32 ->                            -- n_elements : TBasicType TInt
    IO ()

-- | Gets the current search path. See 'GI.Gtk.Objects.IconTheme.iconThemeSetSearchPath'.
-- 
-- /Since: 2.4/
iconThemeGetSearchPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> m ([[Char]])
iconThemeGetSearchPath iconTheme = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    path <- callocMem :: IO (Ptr (Ptr CString))
    nElements <- allocMem :: IO (Ptr Int32)
    gtk_icon_theme_get_search_path iconTheme' path nElements
    nElements' <- peek nElements
    path' <- peek path
    path'' <- (unpackFileNameArrayWithLength nElements') path'
    (mapCArrayWithLength nElements') freeMem path'
    freeMem path'
    touchManagedPtr iconTheme
    freeMem path
    freeMem nElements
    return path''

#if defined(ENABLE_OVERLOADING)
data IconThemeGetSearchPathMethodInfo
instance (signature ~ (m ([[Char]])), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeGetSearchPathMethodInfo a signature where
    overloadedMethod = iconThemeGetSearchPath

instance O.OverloadedMethodInfo IconThemeGetSearchPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeGetSearchPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeGetSearchPath"
        })


#endif

-- method IconTheme::has_icon
-- method type : OrdinaryMethod
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
--           { argCName = "icon_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of an icon"
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

foreign import ccall "gtk_icon_theme_has_icon" gtk_icon_theme_has_icon :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    CString ->                              -- icon_name : TBasicType TUTF8
    IO CInt

-- | Checks whether an icon theme includes an icon
-- for a particular name.
-- 
-- /Since: 2.4/
iconThemeHasIcon ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> T.Text
    -- ^ /@iconName@/: the name of an icon
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@iconTheme@/ includes an
    --  icon for /@iconName@/.
iconThemeHasIcon iconTheme iconName = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    iconName' <- textToCString iconName
    result <- gtk_icon_theme_has_icon iconTheme' iconName'
    let result' = (/= 0) result
    touchManagedPtr iconTheme
    freeMem iconName'
    return result'

#if defined(ENABLE_OVERLOADING)
data IconThemeHasIconMethodInfo
instance (signature ~ (T.Text -> m Bool), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeHasIconMethodInfo a signature where
    overloadedMethod = iconThemeHasIcon

instance O.OverloadedMethodInfo IconThemeHasIconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeHasIcon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeHasIcon"
        })


#endif

-- method IconTheme::list_contexts
-- method type : OrdinaryMethod
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
--       ]
-- Lengths: []
-- returnType: Just (TGList (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_theme_list_contexts" gtk_icon_theme_list_contexts :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    IO (Ptr (GList CString))

-- | Gets the list of contexts available within the current
-- hierarchy of icon themes.
-- See 'GI.Gtk.Objects.IconTheme.iconThemeListIcons' for details about contexts.
-- 
-- /Since: 2.12/
iconThemeListContexts ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> m [T.Text]
    -- ^ __Returns:__ a t'GI.GLib.Structs.List.List' list
    --     holding the names of all the contexts in the theme. You must first
    --     free each element in the list with 'GI.GLib.Functions.free', then free the list
    --     itself with @/g_list_free()/@.
iconThemeListContexts iconTheme = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    result <- gtk_icon_theme_list_contexts iconTheme'
    result' <- unpackGList result
    result'' <- mapM cstringToText result'
    mapGList freeMem result
    g_list_free result
    touchManagedPtr iconTheme
    return result''

#if defined(ENABLE_OVERLOADING)
data IconThemeListContextsMethodInfo
instance (signature ~ (m [T.Text]), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeListContextsMethodInfo a signature where
    overloadedMethod = iconThemeListContexts

instance O.OverloadedMethodInfo IconThemeListContextsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeListContexts",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeListContexts"
        })


#endif

-- method IconTheme::list_icons
-- method type : OrdinaryMethod
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
--           { argCName = "context"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a string identifying a particular type of\n          icon, or %NULL to list all icons."
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
-- returnType: Just (TGList (TBasicType TUTF8))
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_theme_list_icons" gtk_icon_theme_list_icons :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    CString ->                              -- context : TBasicType TUTF8
    IO (Ptr (GList CString))

-- | Lists the icons in the current icon theme. Only a subset
-- of the icons can be listed by providing a context string.
-- The set of values for the context string is system dependent,
-- but will typically include such values as “Applications” and
-- “MimeTypes”. Contexts are explained in the
-- <http://www.freedesktop.org/wiki/Specifications/icon-theme-spec Icon Theme Specification>.
-- The standard contexts are listed in the
-- <http://www.freedesktop.org/wiki/Specifications/icon-naming-spec Icon Naming Specification>.
-- Also see 'GI.Gtk.Objects.IconTheme.iconThemeListContexts'.
-- 
-- /Since: 2.4/
iconThemeListIcons ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> Maybe (T.Text)
    -- ^ /@context@/: a string identifying a particular type of
    --           icon, or 'P.Nothing' to list all icons.
    -> m [T.Text]
    -- ^ __Returns:__ a t'GI.GLib.Structs.List.List' list
    --     holding the names of all the icons in the theme. You must
    --     first free each element in the list with 'GI.GLib.Functions.free', then
    --     free the list itself with @/g_list_free()/@.
iconThemeListIcons iconTheme context = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    maybeContext <- case context of
        Nothing -> return nullPtr
        Just jContext -> do
            jContext' <- textToCString jContext
            return jContext'
    result <- gtk_icon_theme_list_icons iconTheme' maybeContext
    result' <- unpackGList result
    result'' <- mapM cstringToText result'
    mapGList freeMem result
    g_list_free result
    touchManagedPtr iconTheme
    freeMem maybeContext
    return result''

#if defined(ENABLE_OVERLOADING)
data IconThemeListIconsMethodInfo
instance (signature ~ (Maybe (T.Text) -> m [T.Text]), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeListIconsMethodInfo a signature where
    overloadedMethod = iconThemeListIcons

instance O.OverloadedMethodInfo IconThemeListIconsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeListIcons",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeListIcons"
        })


#endif

-- method IconTheme::load_icon
-- method type : OrdinaryMethod
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
--           { argCName = "icon_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the icon to lookup"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText =
--                     Just
--                       "the desired icon size. The resulting icon may not be\n    exactly this size; see gtk_icon_info_load_icon()."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconLookupFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "flags modifying the behavior of the icon lookup"
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

foreign import ccall "gtk_icon_theme_load_icon" gtk_icon_theme_load_icon :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    CString ->                              -- icon_name : TBasicType TUTF8
    Int32 ->                                -- size : TBasicType TInt
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "IconLookupFlags"})
    Ptr (Ptr GError) ->                     -- error
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

-- | Looks up an icon in an icon theme, scales it to the given size
-- and renders it into a pixbuf. This is a convenience function;
-- if more details about the icon are needed, use
-- 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon' followed by 'GI.Gtk.Objects.IconInfo.iconInfoLoadIcon'.
-- 
-- Note that you probably want to listen for icon theme changes and
-- update the icon. This is usually done by connecting to the
-- GtkWidget[styleSet](#g:signal:styleSet) signal. If for some reason you do not want to
-- update the icon when the icon theme changes, you should consider
-- using 'GI.GdkPixbuf.Objects.Pixbuf.pixbufCopy' to make a private copy of the pixbuf
-- returned by this function. Otherwise GTK+ may need to keep the old
-- icon theme loaded, which would be a waste of memory.
-- 
-- /Since: 2.4/
iconThemeLoadIcon ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> T.Text
    -- ^ /@iconName@/: the name of the icon to lookup
    -> Int32
    -- ^ /@size@/: the desired icon size. The resulting icon may not be
    --     exactly this size; see 'GI.Gtk.Objects.IconInfo.iconInfoLoadIcon'.
    -> [Gtk.Flags.IconLookupFlags]
    -- ^ /@flags@/: flags modifying the behavior of the icon lookup
    -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)
    -- ^ __Returns:__ the rendered icon; this may be
    --     a newly created icon or a new reference to an internal icon, so
    --     you must not modify the icon. Use 'GI.GObject.Objects.Object.objectUnref' to release
    --     your reference to the icon. 'P.Nothing' if the icon isn’t found. /(Can throw 'Data.GI.Base.GError.GError')/
iconThemeLoadIcon iconTheme iconName size flags = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    iconName' <- textToCString iconName
    let flags' = gflagsToWord flags
    onException (do
        result <- propagateGError $ gtk_icon_theme_load_icon iconTheme' iconName' size flags'
        maybeResult <- convertIfNonNull result $ \result' -> do
            result'' <- (wrapObject GdkPixbuf.Pixbuf.Pixbuf) result'
            return result''
        touchManagedPtr iconTheme
        freeMem iconName'
        return maybeResult
     ) (do
        freeMem iconName'
     )

#if defined(ENABLE_OVERLOADING)
data IconThemeLoadIconMethodInfo
instance (signature ~ (T.Text -> Int32 -> [Gtk.Flags.IconLookupFlags] -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeLoadIconMethodInfo a signature where
    overloadedMethod = iconThemeLoadIcon

instance O.OverloadedMethodInfo IconThemeLoadIconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeLoadIcon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeLoadIcon"
        })


#endif

-- method IconTheme::load_icon_for_scale
-- method type : OrdinaryMethod
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
--           { argCName = "icon_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the icon to lookup"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText =
--                     Just
--                       "the desired icon size. The resulting icon may not be\n    exactly this size; see gtk_icon_info_load_icon()."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "scale"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "desired scale" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconLookupFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "flags modifying the behavior of the icon lookup"
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

foreign import ccall "gtk_icon_theme_load_icon_for_scale" gtk_icon_theme_load_icon_for_scale :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    CString ->                              -- icon_name : TBasicType TUTF8
    Int32 ->                                -- size : TBasicType TInt
    Int32 ->                                -- scale : TBasicType TInt
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "IconLookupFlags"})
    Ptr (Ptr GError) ->                     -- error
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

-- | Looks up an icon in an icon theme for a particular window scale,
-- scales it to the given size and renders it into a pixbuf. This is a
-- convenience function; if more details about the icon are needed,
-- use 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon' followed by
-- 'GI.Gtk.Objects.IconInfo.iconInfoLoadIcon'.
-- 
-- Note that you probably want to listen for icon theme changes and
-- update the icon. This is usually done by connecting to the
-- GtkWidget[styleSet](#g:signal:styleSet) signal. If for some reason you do not want to
-- update the icon when the icon theme changes, you should consider
-- using 'GI.GdkPixbuf.Objects.Pixbuf.pixbufCopy' to make a private copy of the pixbuf
-- returned by this function. Otherwise GTK+ may need to keep the old
-- icon theme loaded, which would be a waste of memory.
-- 
-- /Since: 3.10/
iconThemeLoadIconForScale ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> T.Text
    -- ^ /@iconName@/: the name of the icon to lookup
    -> Int32
    -- ^ /@size@/: the desired icon size. The resulting icon may not be
    --     exactly this size; see 'GI.Gtk.Objects.IconInfo.iconInfoLoadIcon'.
    -> Int32
    -- ^ /@scale@/: desired scale
    -> [Gtk.Flags.IconLookupFlags]
    -- ^ /@flags@/: flags modifying the behavior of the icon lookup
    -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)
    -- ^ __Returns:__ the rendered icon; this may be
    --     a newly created icon or a new reference to an internal icon, so
    --     you must not modify the icon. Use 'GI.GObject.Objects.Object.objectUnref' to release
    --     your reference to the icon. 'P.Nothing' if the icon isn’t found. /(Can throw 'Data.GI.Base.GError.GError')/
iconThemeLoadIconForScale iconTheme iconName size scale flags = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    iconName' <- textToCString iconName
    let flags' = gflagsToWord flags
    onException (do
        result <- propagateGError $ gtk_icon_theme_load_icon_for_scale iconTheme' iconName' size scale flags'
        maybeResult <- convertIfNonNull result $ \result' -> do
            result'' <- (wrapObject GdkPixbuf.Pixbuf.Pixbuf) result'
            return result''
        touchManagedPtr iconTheme
        freeMem iconName'
        return maybeResult
     ) (do
        freeMem iconName'
     )

#if defined(ENABLE_OVERLOADING)
data IconThemeLoadIconForScaleMethodInfo
instance (signature ~ (T.Text -> Int32 -> Int32 -> [Gtk.Flags.IconLookupFlags] -> m (Maybe GdkPixbuf.Pixbuf.Pixbuf)), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeLoadIconForScaleMethodInfo a signature where
    overloadedMethod = iconThemeLoadIconForScale

instance O.OverloadedMethodInfo IconThemeLoadIconForScaleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeLoadIconForScale",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeLoadIconForScale"
        })


#endif

-- method IconTheme::load_surface
-- method type : OrdinaryMethod
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
--           { argCName = "icon_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the icon to lookup"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText =
--                     Just
--                       "the desired icon size. The resulting icon may not be\n    exactly this size; see gtk_icon_info_load_icon()."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "scale"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "desired scale" , sinceVersion = Nothing }
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
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconLookupFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "flags modifying the behavior of the icon lookup"
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

foreign import ccall "gtk_icon_theme_load_surface" gtk_icon_theme_load_surface :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    CString ->                              -- icon_name : TBasicType TUTF8
    Int32 ->                                -- size : TBasicType TInt
    Int32 ->                                -- scale : TBasicType TInt
    Ptr Gdk.Window.Window ->                -- for_window : TInterface (Name {namespace = "Gdk", name = "Window"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "IconLookupFlags"})
    Ptr (Ptr GError) ->                     -- error
    IO (Ptr Cairo.Surface.Surface)

-- | Looks up an icon in an icon theme for a particular window scale,
-- scales it to the given size and renders it into a cairo surface. This is a
-- convenience function; if more details about the icon are needed,
-- use 'GI.Gtk.Objects.IconTheme.iconThemeLookupIcon' followed by
-- 'GI.Gtk.Objects.IconInfo.iconInfoLoadSurface'.
-- 
-- Note that you probably want to listen for icon theme changes and
-- update the icon. This is usually done by connecting to the
-- GtkWidget[styleSet](#g:signal:styleSet) signal.
-- 
-- /Since: 3.10/
iconThemeLoadSurface ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a, Gdk.Window.IsWindow b) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> T.Text
    -- ^ /@iconName@/: the name of the icon to lookup
    -> Int32
    -- ^ /@size@/: the desired icon size. The resulting icon may not be
    --     exactly this size; see 'GI.Gtk.Objects.IconInfo.iconInfoLoadIcon'.
    -> Int32
    -- ^ /@scale@/: desired scale
    -> Maybe (b)
    -- ^ /@forWindow@/: t'GI.Gdk.Objects.Window.Window' to optimize drawing for, or 'P.Nothing'
    -> [Gtk.Flags.IconLookupFlags]
    -- ^ /@flags@/: flags modifying the behavior of the icon lookup
    -> m (Maybe Cairo.Surface.Surface)
    -- ^ __Returns:__ the rendered icon; this may be
    --     a newly created icon or a new reference to an internal icon, so
    --     you must not modify the icon. Use @/cairo_surface_destroy()/@ to
    --     release your reference to the icon. 'P.Nothing' if the icon isn’t
    --     found. /(Can throw 'Data.GI.Base.GError.GError')/
iconThemeLoadSurface iconTheme iconName size scale forWindow flags = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    iconName' <- textToCString iconName
    maybeForWindow <- case forWindow of
        Nothing -> return nullPtr
        Just jForWindow -> do
            jForWindow' <- unsafeManagedPtrCastPtr jForWindow
            return jForWindow'
    let flags' = gflagsToWord flags
    onException (do
        result <- propagateGError $ gtk_icon_theme_load_surface iconTheme' iconName' size scale maybeForWindow flags'
        maybeResult <- convertIfNonNull result $ \result' -> do
            result'' <- (wrapBoxed Cairo.Surface.Surface) result'
            return result''
        touchManagedPtr iconTheme
        whenJust forWindow touchManagedPtr
        freeMem iconName'
        return maybeResult
     ) (do
        freeMem iconName'
     )

#if defined(ENABLE_OVERLOADING)
data IconThemeLoadSurfaceMethodInfo
instance (signature ~ (T.Text -> Int32 -> Int32 -> Maybe (b) -> [Gtk.Flags.IconLookupFlags] -> m (Maybe Cairo.Surface.Surface)), MonadIO m, IsIconTheme a, Gdk.Window.IsWindow b) => O.OverloadedMethod IconThemeLoadSurfaceMethodInfo a signature where
    overloadedMethod = iconThemeLoadSurface

instance O.OverloadedMethodInfo IconThemeLoadSurfaceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeLoadSurface",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeLoadSurface"
        })


#endif

-- method IconTheme::lookup_by_gicon
-- method type : OrdinaryMethod
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
--           { argCName = "icon"
--           , argType = TInterface Name { namespace = "Gio" , name = "Icon" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GIcon to look up"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "desired icon size" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconLookupFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "flags modifying the behavior of the icon lookup"
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

foreign import ccall "gtk_icon_theme_lookup_by_gicon" gtk_icon_theme_lookup_by_gicon :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    Ptr Gio.Icon.Icon ->                    -- icon : TInterface (Name {namespace = "Gio", name = "Icon"})
    Int32 ->                                -- size : TBasicType TInt
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "IconLookupFlags"})
    IO (Ptr Gtk.IconInfo.IconInfo)

-- | Looks up an icon and returns a t'GI.Gtk.Objects.IconInfo.IconInfo' containing information
-- such as the filename of the icon. The icon can then be rendered
-- into a pixbuf using 'GI.Gtk.Objects.IconInfo.iconInfoLoadIcon'.
-- 
-- When rendering on displays with high pixel densities you should not
-- use a /@size@/ multiplied by the scaling factor returned by functions
-- like 'GI.Gdk.Objects.Window.windowGetScaleFactor'. Instead, you should use
-- 'GI.Gtk.Objects.IconTheme.iconThemeLookupByGiconForScale', as the assets loaded
-- for a given scaling factor may be different.
-- 
-- /Since: 2.14/
iconThemeLookupByGicon ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a, Gio.Icon.IsIcon b) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> b
    -- ^ /@icon@/: the t'GI.Gio.Interfaces.Icon.Icon' to look up
    -> Int32
    -- ^ /@size@/: desired icon size
    -> [Gtk.Flags.IconLookupFlags]
    -- ^ /@flags@/: flags modifying the behavior of the icon lookup
    -> m (Maybe Gtk.IconInfo.IconInfo)
    -- ^ __Returns:__ a t'GI.Gtk.Objects.IconInfo.IconInfo' containing
    --     information about the icon, or 'P.Nothing' if the icon wasn’t
    --     found. Unref with 'GI.GObject.Objects.Object.objectUnref'
iconThemeLookupByGicon iconTheme icon size flags = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    icon' <- unsafeManagedPtrCastPtr icon
    let flags' = gflagsToWord flags
    result <- gtk_icon_theme_lookup_by_gicon iconTheme' icon' size flags'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapObject Gtk.IconInfo.IconInfo) result'
        return result''
    touchManagedPtr iconTheme
    touchManagedPtr icon
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data IconThemeLookupByGiconMethodInfo
instance (signature ~ (b -> Int32 -> [Gtk.Flags.IconLookupFlags] -> m (Maybe Gtk.IconInfo.IconInfo)), MonadIO m, IsIconTheme a, Gio.Icon.IsIcon b) => O.OverloadedMethod IconThemeLookupByGiconMethodInfo a signature where
    overloadedMethod = iconThemeLookupByGicon

instance O.OverloadedMethodInfo IconThemeLookupByGiconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeLookupByGicon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeLookupByGicon"
        })


#endif

-- method IconTheme::lookup_by_gicon_for_scale
-- method type : OrdinaryMethod
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
--           { argCName = "icon"
--           , argType = TInterface Name { namespace = "Gio" , name = "Icon" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GIcon to look up"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "desired icon size" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "scale"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the desired scale" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconLookupFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "flags modifying the behavior of the icon lookup"
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

foreign import ccall "gtk_icon_theme_lookup_by_gicon_for_scale" gtk_icon_theme_lookup_by_gicon_for_scale :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    Ptr Gio.Icon.Icon ->                    -- icon : TInterface (Name {namespace = "Gio", name = "Icon"})
    Int32 ->                                -- size : TBasicType TInt
    Int32 ->                                -- scale : TBasicType TInt
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "IconLookupFlags"})
    IO (Ptr Gtk.IconInfo.IconInfo)

-- | Looks up an icon and returns a t'GI.Gtk.Objects.IconInfo.IconInfo' containing information
-- such as the filename of the icon. The icon can then be rendered into
-- a pixbuf using 'GI.Gtk.Objects.IconInfo.iconInfoLoadIcon'.
-- 
-- /Since: 3.10/
iconThemeLookupByGiconForScale ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a, Gio.Icon.IsIcon b) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> b
    -- ^ /@icon@/: the t'GI.Gio.Interfaces.Icon.Icon' to look up
    -> Int32
    -- ^ /@size@/: desired icon size
    -> Int32
    -- ^ /@scale@/: the desired scale
    -> [Gtk.Flags.IconLookupFlags]
    -- ^ /@flags@/: flags modifying the behavior of the icon lookup
    -> m (Maybe Gtk.IconInfo.IconInfo)
    -- ^ __Returns:__ a t'GI.Gtk.Objects.IconInfo.IconInfo' containing
    --     information about the icon, or 'P.Nothing' if the icon wasn’t
    --     found. Unref with 'GI.GObject.Objects.Object.objectUnref'
iconThemeLookupByGiconForScale iconTheme icon size scale flags = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    icon' <- unsafeManagedPtrCastPtr icon
    let flags' = gflagsToWord flags
    result <- gtk_icon_theme_lookup_by_gicon_for_scale iconTheme' icon' size scale flags'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapObject Gtk.IconInfo.IconInfo) result'
        return result''
    touchManagedPtr iconTheme
    touchManagedPtr icon
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data IconThemeLookupByGiconForScaleMethodInfo
instance (signature ~ (b -> Int32 -> Int32 -> [Gtk.Flags.IconLookupFlags] -> m (Maybe Gtk.IconInfo.IconInfo)), MonadIO m, IsIconTheme a, Gio.Icon.IsIcon b) => O.OverloadedMethod IconThemeLookupByGiconForScaleMethodInfo a signature where
    overloadedMethod = iconThemeLookupByGiconForScale

instance O.OverloadedMethodInfo IconThemeLookupByGiconForScaleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeLookupByGiconForScale",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeLookupByGiconForScale"
        })


#endif

-- method IconTheme::lookup_icon
-- method type : OrdinaryMethod
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
--           { argCName = "icon_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the icon to lookup"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "desired icon size" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconLookupFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "flags modifying the behavior of the icon lookup"
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

foreign import ccall "gtk_icon_theme_lookup_icon" gtk_icon_theme_lookup_icon :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    CString ->                              -- icon_name : TBasicType TUTF8
    Int32 ->                                -- size : TBasicType TInt
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "IconLookupFlags"})
    IO (Ptr Gtk.IconInfo.IconInfo)

-- | Looks up a named icon and returns a t'GI.Gtk.Objects.IconInfo.IconInfo' containing
-- information such as the filename of the icon. The icon
-- can then be rendered into a pixbuf using
-- 'GI.Gtk.Objects.IconInfo.iconInfoLoadIcon'. ('GI.Gtk.Objects.IconTheme.iconThemeLoadIcon'
-- combines these two steps if all you need is the pixbuf.)
-- 
-- When rendering on displays with high pixel densities you should not
-- use a /@size@/ multiplied by the scaling factor returned by functions
-- like 'GI.Gdk.Objects.Window.windowGetScaleFactor'. Instead, you should use
-- 'GI.Gtk.Objects.IconTheme.iconThemeLookupIconForScale', as the assets loaded
-- for a given scaling factor may be different.
-- 
-- /Since: 2.4/
iconThemeLookupIcon ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> T.Text
    -- ^ /@iconName@/: the name of the icon to lookup
    -> Int32
    -- ^ /@size@/: desired icon size
    -> [Gtk.Flags.IconLookupFlags]
    -- ^ /@flags@/: flags modifying the behavior of the icon lookup
    -> m (Maybe Gtk.IconInfo.IconInfo)
    -- ^ __Returns:__ a t'GI.Gtk.Objects.IconInfo.IconInfo' object
    --     containing information about the icon, or 'P.Nothing' if the
    --     icon wasn’t found.
iconThemeLookupIcon iconTheme iconName size flags = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    iconName' <- textToCString iconName
    let flags' = gflagsToWord flags
    result <- gtk_icon_theme_lookup_icon iconTheme' iconName' size flags'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapObject Gtk.IconInfo.IconInfo) result'
        return result''
    touchManagedPtr iconTheme
    freeMem iconName'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data IconThemeLookupIconMethodInfo
instance (signature ~ (T.Text -> Int32 -> [Gtk.Flags.IconLookupFlags] -> m (Maybe Gtk.IconInfo.IconInfo)), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeLookupIconMethodInfo a signature where
    overloadedMethod = iconThemeLookupIcon

instance O.OverloadedMethodInfo IconThemeLookupIconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeLookupIcon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeLookupIcon"
        })


#endif

-- method IconTheme::lookup_icon_for_scale
-- method type : OrdinaryMethod
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
--           { argCName = "icon_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the icon to lookup"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText = Just "desired icon size" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "scale"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the desired scale" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconLookupFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "flags modifying the behavior of the icon lookup"
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

foreign import ccall "gtk_icon_theme_lookup_icon_for_scale" gtk_icon_theme_lookup_icon_for_scale :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    CString ->                              -- icon_name : TBasicType TUTF8
    Int32 ->                                -- size : TBasicType TInt
    Int32 ->                                -- scale : TBasicType TInt
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "IconLookupFlags"})
    IO (Ptr Gtk.IconInfo.IconInfo)

-- | Looks up a named icon for a particular window scale and returns a
-- t'GI.Gtk.Objects.IconInfo.IconInfo' containing information such as the filename of the
-- icon. The icon can then be rendered into a pixbuf using
-- 'GI.Gtk.Objects.IconInfo.iconInfoLoadIcon'. ('GI.Gtk.Objects.IconTheme.iconThemeLoadIcon' combines
-- these two steps if all you need is the pixbuf.)
-- 
-- /Since: 3.10/
iconThemeLookupIconForScale ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> T.Text
    -- ^ /@iconName@/: the name of the icon to lookup
    -> Int32
    -- ^ /@size@/: desired icon size
    -> Int32
    -- ^ /@scale@/: the desired scale
    -> [Gtk.Flags.IconLookupFlags]
    -- ^ /@flags@/: flags modifying the behavior of the icon lookup
    -> m (Maybe Gtk.IconInfo.IconInfo)
    -- ^ __Returns:__ a t'GI.Gtk.Objects.IconInfo.IconInfo' object
    --     containing information about the icon, or 'P.Nothing' if the
    --     icon wasn’t found.
iconThemeLookupIconForScale iconTheme iconName size scale flags = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    iconName' <- textToCString iconName
    let flags' = gflagsToWord flags
    result <- gtk_icon_theme_lookup_icon_for_scale iconTheme' iconName' size scale flags'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapObject Gtk.IconInfo.IconInfo) result'
        return result''
    touchManagedPtr iconTheme
    freeMem iconName'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data IconThemeLookupIconForScaleMethodInfo
instance (signature ~ (T.Text -> Int32 -> Int32 -> [Gtk.Flags.IconLookupFlags] -> m (Maybe Gtk.IconInfo.IconInfo)), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeLookupIconForScaleMethodInfo a signature where
    overloadedMethod = iconThemeLookupIconForScale

instance O.OverloadedMethodInfo IconThemeLookupIconForScaleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeLookupIconForScale",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeLookupIconForScale"
        })


#endif

-- method IconTheme::prepend_search_path
-- method type : OrdinaryMethod
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
--           { argCName = "path"
--           , argType = TBasicType TFileName
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "directory name to prepend to the icon path"
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

foreign import ccall "gtk_icon_theme_prepend_search_path" gtk_icon_theme_prepend_search_path :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    CString ->                              -- path : TBasicType TFileName
    IO ()

-- | Prepends a directory to the search path.
-- See 'GI.Gtk.Objects.IconTheme.iconThemeSetSearchPath'.
-- 
-- /Since: 2.4/
iconThemePrependSearchPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> [Char]
    -- ^ /@path@/: directory name to prepend to the icon path
    -> m ()
iconThemePrependSearchPath iconTheme path = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    path' <- stringToCString path
    gtk_icon_theme_prepend_search_path iconTheme' path'
    touchManagedPtr iconTheme
    freeMem path'
    return ()

#if defined(ENABLE_OVERLOADING)
data IconThemePrependSearchPathMethodInfo
instance (signature ~ ([Char] -> m ()), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemePrependSearchPathMethodInfo a signature where
    overloadedMethod = iconThemePrependSearchPath

instance O.OverloadedMethodInfo IconThemePrependSearchPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemePrependSearchPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemePrependSearchPath"
        })


#endif

-- method IconTheme::rescan_if_needed
-- method type : OrdinaryMethod
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
--       ]
-- Lengths: []
-- returnType: Just (TBasicType TBoolean)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_theme_rescan_if_needed" gtk_icon_theme_rescan_if_needed :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    IO CInt

-- | Checks to see if the icon theme has changed; if it has, any
-- currently cached information is discarded and will be reloaded
-- next time /@iconTheme@/ is accessed.
-- 
-- /Since: 2.4/
iconThemeRescanIfNeeded ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if the icon theme has changed and needed
    --     to be reloaded.
iconThemeRescanIfNeeded iconTheme = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    result <- gtk_icon_theme_rescan_if_needed iconTheme'
    let result' = (/= 0) result
    touchManagedPtr iconTheme
    return result'

#if defined(ENABLE_OVERLOADING)
data IconThemeRescanIfNeededMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeRescanIfNeededMethodInfo a signature where
    overloadedMethod = iconThemeRescanIfNeeded

instance O.OverloadedMethodInfo IconThemeRescanIfNeededMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeRescanIfNeeded",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeRescanIfNeeded"
        })


#endif

-- method IconTheme::set_custom_theme
-- method type : OrdinaryMethod
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
--           { argCName = "theme_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "name of icon theme to use instead of\n  configured theme, or %NULL to unset a previously set custom theme"
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

foreign import ccall "gtk_icon_theme_set_custom_theme" gtk_icon_theme_set_custom_theme :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    CString ->                              -- theme_name : TBasicType TUTF8
    IO ()

-- | Sets the name of the icon theme that the t'GI.Gtk.Objects.IconTheme.IconTheme' object uses
-- overriding system configuration. This function cannot be called
-- on the icon theme objects returned from 'GI.Gtk.Objects.IconTheme.iconThemeGetDefault'
-- and 'GI.Gtk.Objects.IconTheme.iconThemeGetForScreen'.
-- 
-- /Since: 2.4/
iconThemeSetCustomTheme ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> Maybe (T.Text)
    -- ^ /@themeName@/: name of icon theme to use instead of
    --   configured theme, or 'P.Nothing' to unset a previously set custom theme
    -> m ()
iconThemeSetCustomTheme iconTheme themeName = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    maybeThemeName <- case themeName of
        Nothing -> return nullPtr
        Just jThemeName -> do
            jThemeName' <- textToCString jThemeName
            return jThemeName'
    gtk_icon_theme_set_custom_theme iconTheme' maybeThemeName
    touchManagedPtr iconTheme
    freeMem maybeThemeName
    return ()

#if defined(ENABLE_OVERLOADING)
data IconThemeSetCustomThemeMethodInfo
instance (signature ~ (Maybe (T.Text) -> m ()), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeSetCustomThemeMethodInfo a signature where
    overloadedMethod = iconThemeSetCustomTheme

instance O.OverloadedMethodInfo IconThemeSetCustomThemeMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeSetCustomTheme",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeSetCustomTheme"
        })


#endif

-- method IconTheme::set_screen
-- method type : OrdinaryMethod
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
--           { argCName = "screen"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Screen" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkScreen" , sinceVersion = Nothing }
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

foreign import ccall "gtk_icon_theme_set_screen" gtk_icon_theme_set_screen :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    Ptr Gdk.Screen.Screen ->                -- screen : TInterface (Name {namespace = "Gdk", name = "Screen"})
    IO ()

-- | Sets the screen for an icon theme; the screen is used
-- to track the user’s currently configured icon theme,
-- which might be different for different screens.
-- 
-- /Since: 2.4/
iconThemeSetScreen ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a, Gdk.Screen.IsScreen b) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> b
    -- ^ /@screen@/: a t'GI.Gdk.Objects.Screen.Screen'
    -> m ()
iconThemeSetScreen iconTheme screen = liftIO $ do
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    screen' <- unsafeManagedPtrCastPtr screen
    gtk_icon_theme_set_screen iconTheme' screen'
    touchManagedPtr iconTheme
    touchManagedPtr screen
    return ()

#if defined(ENABLE_OVERLOADING)
data IconThemeSetScreenMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsIconTheme a, Gdk.Screen.IsScreen b) => O.OverloadedMethod IconThemeSetScreenMethodInfo a signature where
    overloadedMethod = iconThemeSetScreen

instance O.OverloadedMethodInfo IconThemeSetScreenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeSetScreen",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeSetScreen"
        })


#endif

-- method IconTheme::set_search_path
-- method type : OrdinaryMethod
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
--           { argCName = "path"
--           , argType = TCArray False (-1) 2 (TBasicType TFileName)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "array of\n    directories that are searched for icon themes"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "n_elements"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "number of elements in @path."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: [ Arg
--              { argCName = "n_elements"
--              , argType = TBasicType TInt
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText = Just "number of elements in @path."
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_theme_set_search_path" gtk_icon_theme_set_search_path :: 
    Ptr IconTheme ->                        -- icon_theme : TInterface (Name {namespace = "Gtk", name = "IconTheme"})
    Ptr CString ->                          -- path : TCArray False (-1) 2 (TBasicType TFileName)
    Int32 ->                                -- n_elements : TBasicType TInt
    IO ()

-- | Sets the search path for the icon theme object. When looking
-- for an icon theme, GTK+ will search for a subdirectory of
-- one or more of the directories in /@path@/ with the same name
-- as the icon theme containing an index.theme file. (Themes from
-- multiple of the path elements are combined to allow themes to be
-- extended by adding icons in the user’s home directory.)
-- 
-- In addition if an icon found isn’t found either in the current
-- icon theme or the default icon theme, and an image file with
-- the right name is found directly in one of the elements of
-- /@path@/, then that image will be used for the icon name.
-- (This is legacy feature, and new icons should be put
-- into the fallback icon theme, which is called hicolor,
-- rather than directly on the icon path.)
-- 
-- /Since: 2.4/
iconThemeSetSearchPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconTheme a) =>
    a
    -- ^ /@iconTheme@/: a t'GI.Gtk.Objects.IconTheme.IconTheme'
    -> [[Char]]
    -- ^ /@path@/: array of
    --     directories that are searched for icon themes
    -> m ()
iconThemeSetSearchPath iconTheme path = liftIO $ do
    let nElements = fromIntegral $ P.length path
    iconTheme' <- unsafeManagedPtrCastPtr iconTheme
    path' <- packFileNameArray path
    gtk_icon_theme_set_search_path iconTheme' path' nElements
    touchManagedPtr iconTheme
    (mapCArrayWithLength nElements) freeMem path'
    freeMem path'
    return ()

#if defined(ENABLE_OVERLOADING)
data IconThemeSetSearchPathMethodInfo
instance (signature ~ ([[Char]] -> m ()), MonadIO m, IsIconTheme a) => O.OverloadedMethod IconThemeSetSearchPathMethodInfo a signature where
    overloadedMethod = iconThemeSetSearchPath

instance O.OverloadedMethodInfo IconThemeSetSearchPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconTheme.iconThemeSetSearchPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconTheme.html#v:iconThemeSetSearchPath"
        })


#endif

-- method IconTheme::add_builtin_icon
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "icon_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the icon to register"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText =
--                     Just
--                       "the size in pixels at which to register the icon (different\n    images can be registered for the same icon name at different sizes.)"
--                 , sinceVersion = Nothing
--                 }
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
--                 { rawDocText =
--                     Just "#GdkPixbuf that contains the image to use for @icon_name"
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

foreign import ccall "gtk_icon_theme_add_builtin_icon" gtk_icon_theme_add_builtin_icon :: 
    CString ->                              -- icon_name : TBasicType TUTF8
    Int32 ->                                -- size : TBasicType TInt
    Ptr GdkPixbuf.Pixbuf.Pixbuf ->          -- pixbuf : TInterface (Name {namespace = "GdkPixbuf", name = "Pixbuf"})
    IO ()

{-# DEPRECATED iconThemeAddBuiltinIcon ["(Since version 3.14)","Use 'GI.Gtk.Objects.IconTheme.iconThemeAddResourcePath'","    to add application-specific icons to the icon theme."] #-}
-- | Registers a built-in icon for icon theme lookups. The idea
-- of built-in icons is to allow an application or library
-- that uses themed icons to function requiring files to
-- be present in the file system. For instance, the default
-- images for all of GTK+’s stock icons are registered
-- as built-icons.
-- 
-- In general, if you use 'GI.Gtk.Objects.IconTheme.iconThemeAddBuiltinIcon'
-- you should also install the icon in the icon theme, so
-- that the icon is generally available.
-- 
-- This function will generally be used with pixbufs loaded
-- via 'GI.GdkPixbuf.Objects.Pixbuf.pixbufNewFromInline'.
-- 
-- /Since: 2.4/
iconThemeAddBuiltinIcon ::
    (B.CallStack.HasCallStack, MonadIO m, GdkPixbuf.Pixbuf.IsPixbuf a) =>
    T.Text
    -- ^ /@iconName@/: the name of the icon to register
    -> Int32
    -- ^ /@size@/: the size in pixels at which to register the icon (different
    --     images can be registered for the same icon name at different sizes.)
    -> a
    -- ^ /@pixbuf@/: t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf' that contains the image to use for /@iconName@/
    -> m ()
iconThemeAddBuiltinIcon iconName size pixbuf = liftIO $ do
    iconName' <- textToCString iconName
    pixbuf' <- unsafeManagedPtrCastPtr pixbuf
    gtk_icon_theme_add_builtin_icon iconName' size pixbuf'
    touchManagedPtr pixbuf
    freeMem iconName'
    return ()

#if defined(ENABLE_OVERLOADING)
#endif

-- method IconTheme::get_default
-- method type : MemberFunction
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "IconTheme" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_theme_get_default" gtk_icon_theme_get_default :: 
    IO (Ptr IconTheme)

-- | Gets the icon theme for the default screen. See
-- 'GI.Gtk.Objects.IconTheme.iconThemeGetForScreen'.
-- 
-- /Since: 2.4/
iconThemeGetDefault ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m IconTheme
    -- ^ __Returns:__ A unique t'GI.Gtk.Objects.IconTheme.IconTheme' associated with
    --     the default screen. This icon theme is associated with
    --     the screen and can be used as long as the screen
    --     is open. Do not ref or unref it.
iconThemeGetDefault  = liftIO $ do
    result <- gtk_icon_theme_get_default
    checkUnexpectedReturnNULL "iconThemeGetDefault" result
    result' <- (newObject IconTheme) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method IconTheme::get_for_screen
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "screen"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Screen" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkScreen" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "IconTheme" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_theme_get_for_screen" gtk_icon_theme_get_for_screen :: 
    Ptr Gdk.Screen.Screen ->                -- screen : TInterface (Name {namespace = "Gdk", name = "Screen"})
    IO (Ptr IconTheme)

-- | Gets the icon theme object associated with /@screen@/; if this
-- function has not previously been called for the given
-- screen, a new icon theme object will be created and
-- associated with the screen. Icon theme objects are
-- fairly expensive to create, so using this function
-- is usually a better choice than calling than 'GI.Gtk.Objects.IconTheme.iconThemeNew'
-- and setting the screen yourself; by using this function
-- a single icon theme object will be shared between users.
-- 
-- /Since: 2.4/
iconThemeGetForScreen ::
    (B.CallStack.HasCallStack, MonadIO m, Gdk.Screen.IsScreen a) =>
    a
    -- ^ /@screen@/: a t'GI.Gdk.Objects.Screen.Screen'
    -> m IconTheme
    -- ^ __Returns:__ A unique t'GI.Gtk.Objects.IconTheme.IconTheme' associated with
    --  the given screen. This icon theme is associated with
    --  the screen and can be used as long as the screen
    --  is open. Do not ref or unref it.
iconThemeGetForScreen screen = liftIO $ do
    screen' <- unsafeManagedPtrCastPtr screen
    result <- gtk_icon_theme_get_for_screen screen'
    checkUnexpectedReturnNULL "iconThemeGetForScreen" result
    result' <- (newObject IconTheme) result
    touchManagedPtr screen
    return result'

#if defined(ENABLE_OVERLOADING)
#endif


