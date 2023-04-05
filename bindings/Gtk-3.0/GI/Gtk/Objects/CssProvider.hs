{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- GtkCssProvider is an object implementing the t'GI.Gtk.Interfaces.StyleProvider.StyleProvider' interface.
-- It is able to parse [CSS-like][css-overview] input in order to style widgets.
-- 
-- An application can make GTK+ parse a specific CSS style sheet by calling
-- 'GI.Gtk.Objects.CssProvider.cssProviderLoadFromFile' or 'GI.Gtk.Objects.CssProvider.cssProviderLoadFromResource'
-- and adding the provider with 'GI.Gtk.Objects.StyleContext.styleContextAddProvider' or
-- 'GI.Gtk.Objects.StyleContext.styleContextAddProviderForScreen'.
-- 
-- In addition, certain files will be read when GTK+ is initialized. First, the
-- file @$XDG_CONFIG_HOME\/gtk-3.0\/gtk.css@ is loaded if it exists. Then, GTK+
-- loads the first existing file among
-- @XDG_DATA_HOME\/themes\/THEME\/gtk-VERSION\/gtk.css@,
-- @$HOME\/.themes\/THEME\/gtk-VERSION\/gtk.css@,
-- @$XDG_DATA_DIRS\/themes\/THEME\/gtk-VERSION\/gtk.css@ and
-- @DATADIR\/share\/themes\/THEME\/gtk-VERSION\/gtk.css@, where @THEME@ is the name of
-- the current theme (see the [Settings:gtkThemeName]("GI.Gtk.Objects.Settings#g:attr:gtkThemeName") setting), @DATADIR@
-- is the prefix configured when GTK+ was compiled (unless overridden by the
-- @GTK_DATA_PREFIX@ environment variable), and @VERSION@ is the GTK+ version number.
-- If no file is found for the current version, GTK+ tries older versions all the
-- way back to 3.0.
-- 
-- In the same way, GTK+ tries to load a gtk-keys.css file for the current
-- key theme, as defined by [Settings:gtkKeyThemeName]("GI.Gtk.Objects.Settings#g:attr:gtkKeyThemeName").

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.CssProvider
    ( 

-- * Exported types
    CssProvider(..)                         ,
    IsCssProvider                           ,
    toCssProvider                           ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [loadFromData]("GI.Gtk.Objects.CssProvider#g:method:loadFromData"), [loadFromFile]("GI.Gtk.Objects.CssProvider#g:method:loadFromFile"), [loadFromPath]("GI.Gtk.Objects.CssProvider#g:method:loadFromPath"), [loadFromResource]("GI.Gtk.Objects.CssProvider#g:method:loadFromResource"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [toString]("GI.Gtk.Objects.CssProvider#g:method:toString"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getIconFactory]("GI.Gtk.Interfaces.StyleProvider#g:method:getIconFactory"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getStyle]("GI.Gtk.Interfaces.StyleProvider#g:method:getStyle"), [getStyleProperty]("GI.Gtk.Interfaces.StyleProvider#g:method:getStyleProperty").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveCssProviderMethod                ,
#endif

-- ** getDefault #method:getDefault#

    cssProviderGetDefault                   ,


-- ** getNamed #method:getNamed#

    cssProviderGetNamed                     ,


-- ** loadFromData #method:loadFromData#

#if defined(ENABLE_OVERLOADING)
    CssProviderLoadFromDataMethodInfo       ,
#endif
    cssProviderLoadFromData                 ,


-- ** loadFromFile #method:loadFromFile#

#if defined(ENABLE_OVERLOADING)
    CssProviderLoadFromFileMethodInfo       ,
#endif
    cssProviderLoadFromFile                 ,


-- ** loadFromPath #method:loadFromPath#

#if defined(ENABLE_OVERLOADING)
    CssProviderLoadFromPathMethodInfo       ,
#endif
    cssProviderLoadFromPath                 ,


-- ** loadFromResource #method:loadFromResource#

#if defined(ENABLE_OVERLOADING)
    CssProviderLoadFromResourceMethodInfo   ,
#endif
    cssProviderLoadFromResource             ,


-- ** new #method:new#

    cssProviderNew                          ,


-- ** toString #method:toString#

#if defined(ENABLE_OVERLOADING)
    CssProviderToStringMethodInfo           ,
#endif
    cssProviderToString                     ,




 -- * Signals


-- ** parsingError #signal:parsingError#

    CssProviderParsingErrorCallback         ,
#if defined(ENABLE_OVERLOADING)
    CssProviderParsingErrorSignalInfo       ,
#endif
    afterCssProviderParsingError            ,
    onCssProviderParsingError               ,




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
import qualified GI.Gio.Interfaces.File as Gio.File
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.StyleProvider as Gtk.StyleProvider
import {-# SOURCE #-} qualified GI.Gtk.Structs.CssSection as Gtk.CssSection

-- | Memory-managed wrapper type.
newtype CssProvider = CssProvider (SP.ManagedPtr CssProvider)
    deriving (Eq)

instance SP.ManagedPtrNewtype CssProvider where
    toManagedPtr (CssProvider p) = p

foreign import ccall "gtk_css_provider_get_type"
    c_gtk_css_provider_get_type :: IO B.Types.GType

instance B.Types.TypedObject CssProvider where
    glibType = c_gtk_css_provider_get_type

instance B.Types.GObject CssProvider

-- | Type class for types which can be safely cast to `CssProvider`, for instance with `toCssProvider`.
class (SP.GObject o, O.IsDescendantOf CssProvider o) => IsCssProvider o
instance (SP.GObject o, O.IsDescendantOf CssProvider o) => IsCssProvider o

instance O.HasParentTypes CssProvider
type instance O.ParentTypes CssProvider = '[GObject.Object.Object, Gtk.StyleProvider.StyleProvider]

-- | Cast to `CssProvider`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toCssProvider :: (MIO.MonadIO m, IsCssProvider o) => o -> m CssProvider
toCssProvider = MIO.liftIO . B.ManagedPtr.unsafeCastTo CssProvider

-- | Convert 'CssProvider' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe CssProvider) where
    gvalueGType_ = c_gtk_css_provider_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr CssProvider)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr CssProvider)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject CssProvider ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveCssProviderMethod (t :: Symbol) (o :: *) :: * where
    ResolveCssProviderMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveCssProviderMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveCssProviderMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveCssProviderMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveCssProviderMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveCssProviderMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveCssProviderMethod "loadFromData" o = CssProviderLoadFromDataMethodInfo
    ResolveCssProviderMethod "loadFromFile" o = CssProviderLoadFromFileMethodInfo
    ResolveCssProviderMethod "loadFromPath" o = CssProviderLoadFromPathMethodInfo
    ResolveCssProviderMethod "loadFromResource" o = CssProviderLoadFromResourceMethodInfo
    ResolveCssProviderMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveCssProviderMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveCssProviderMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveCssProviderMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveCssProviderMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveCssProviderMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveCssProviderMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveCssProviderMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveCssProviderMethod "toString" o = CssProviderToStringMethodInfo
    ResolveCssProviderMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveCssProviderMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveCssProviderMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveCssProviderMethod "getIconFactory" o = Gtk.StyleProvider.StyleProviderGetIconFactoryMethodInfo
    ResolveCssProviderMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveCssProviderMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveCssProviderMethod "getStyle" o = Gtk.StyleProvider.StyleProviderGetStyleMethodInfo
    ResolveCssProviderMethod "getStyleProperty" o = Gtk.StyleProvider.StyleProviderGetStylePropertyMethodInfo
    ResolveCssProviderMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveCssProviderMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveCssProviderMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveCssProviderMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveCssProviderMethod t CssProvider, O.OverloadedMethod info CssProvider p) => OL.IsLabel t (CssProvider -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveCssProviderMethod t CssProvider, O.OverloadedMethod info CssProvider p, R.HasField t CssProvider p) => R.HasField t CssProvider p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveCssProviderMethod t CssProvider, O.OverloadedMethodInfo info CssProvider) => OL.IsLabel t (O.MethodProxy info CssProvider) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal CssProvider::parsing-error
-- | Signals that a parsing error occurred. the /@path@/, /@line@/ and /@position@/
-- describe the actual location of the error as accurately as possible.
-- 
-- Parsing errors are never fatal, so the parsing will resume after
-- the error. Errors may however cause parts of the given
-- data or even all of it to not be parsed at all. So it is a useful idea
-- to check that the parsing succeeds by connecting to this signal.
-- 
-- Note that this signal may be emitted at any time as the css provider
-- may opt to defer parsing parts or all of the input to a later time
-- than when a loading function was called.
type CssProviderParsingErrorCallback =
    Gtk.CssSection.CssSection
    -- ^ /@section@/: section the error happened in
    -> GError
    -- ^ /@error@/: The parsing error
    -> IO ()

type C_CssProviderParsingErrorCallback =
    Ptr CssProvider ->                      -- object
    Ptr Gtk.CssSection.CssSection ->
    Ptr GError ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_CssProviderParsingErrorCallback`.
foreign import ccall "wrapper"
    mk_CssProviderParsingErrorCallback :: C_CssProviderParsingErrorCallback -> IO (FunPtr C_CssProviderParsingErrorCallback)

wrap_CssProviderParsingErrorCallback :: 
    GObject a => (a -> CssProviderParsingErrorCallback) ->
    C_CssProviderParsingErrorCallback
wrap_CssProviderParsingErrorCallback gi'cb gi'selfPtr section error_ _ = do
    B.ManagedPtr.withTransient  section $ \section' -> do
        error_' <- (newBoxed GError) error_
        B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  section' error_'


-- | Connect a signal handler for the [parsingError](#signal:parsingError) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' cssProvider #parsingError callback
-- @
-- 
-- 
onCssProviderParsingError :: (IsCssProvider a, MonadIO m) => a -> ((?self :: a) => CssProviderParsingErrorCallback) -> m SignalHandlerId
onCssProviderParsingError obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CssProviderParsingErrorCallback wrapped
    wrapped'' <- mk_CssProviderParsingErrorCallback wrapped'
    connectSignalFunPtr obj "parsing-error" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [parsingError](#signal:parsingError) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' cssProvider #parsingError callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterCssProviderParsingError :: (IsCssProvider a, MonadIO m) => a -> ((?self :: a) => CssProviderParsingErrorCallback) -> m SignalHandlerId
afterCssProviderParsingError obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_CssProviderParsingErrorCallback wrapped
    wrapped'' <- mk_CssProviderParsingErrorCallback wrapped'
    connectSignalFunPtr obj "parsing-error" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data CssProviderParsingErrorSignalInfo
instance SignalInfo CssProviderParsingErrorSignalInfo where
    type HaskellCallbackType CssProviderParsingErrorSignalInfo = CssProviderParsingErrorCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_CssProviderParsingErrorCallback cb
        cb'' <- mk_CssProviderParsingErrorCallback cb'
        connectSignalFunPtr obj "parsing-error" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CssProvider::parsing-error"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CssProvider.html#g:signal:parsingError"})

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList CssProvider
type instance O.AttributeList CssProvider = CssProviderAttributeList
type CssProviderAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList CssProvider = CssProviderSignalList
type CssProviderSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo), '("parsingError", CssProviderParsingErrorSignalInfo)] :: [(Symbol, *)])

#endif

-- method CssProvider::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "CssProvider" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_css_provider_new" gtk_css_provider_new :: 
    IO (Ptr CssProvider)

-- | Returns a newly created t'GI.Gtk.Objects.CssProvider.CssProvider'.
cssProviderNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m CssProvider
    -- ^ __Returns:__ A new t'GI.Gtk.Objects.CssProvider.CssProvider'
cssProviderNew  = liftIO $ do
    result <- gtk_css_provider_new
    checkUnexpectedReturnNULL "cssProviderNew" result
    result' <- (wrapObject CssProvider) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method CssProvider::load_from_data
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "css_provider"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CssProvider" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCssProvider" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "data"
--           , argType = TCArray False (-1) 2 (TBasicType TUInt8)
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "CSS data loaded in memory"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "length"
--           , argType = TBasicType TInt64
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "the length of @data in bytes, or -1 for NUL terminated strings. If\n  @length is not -1, the code will assume it is not NUL terminated and will\n  potentially do a copy."
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
--              { argCName = "length"
--              , argType = TBasicType TInt64
--              , direction = DirectionIn
--              , mayBeNull = False
--              , argDoc =
--                  Documentation
--                    { rawDocText =
--                        Just
--                          "the length of @data in bytes, or -1 for NUL terminated strings. If\n  @length is not -1, the code will assume it is not NUL terminated and will\n  potentially do a copy."
--                    , sinceVersion = Nothing
--                    }
--              , argScope = ScopeTypeInvalid
--              , argClosure = -1
--              , argDestroy = -1
--              , argCallerAllocates = False
--              , transfer = TransferNothing
--              }
--          ]
-- returnType: Just (TBasicType TBoolean)
-- throws : True
-- Skip return : False

foreign import ccall "gtk_css_provider_load_from_data" gtk_css_provider_load_from_data :: 
    Ptr CssProvider ->                      -- css_provider : TInterface (Name {namespace = "Gtk", name = "CssProvider"})
    Ptr Word8 ->                            -- data : TCArray False (-1) 2 (TBasicType TUInt8)
    Int64 ->                                -- length : TBasicType TInt64
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Loads /@data@/ into /@cssProvider@/, and by doing so clears any previously loaded
-- information.
cssProviderLoadFromData ::
    (B.CallStack.HasCallStack, MonadIO m, IsCssProvider a) =>
    a
    -- ^ /@cssProvider@/: a t'GI.Gtk.Objects.CssProvider.CssProvider'
    -> ByteString
    -- ^ /@data@/: CSS data loaded in memory
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
cssProviderLoadFromData cssProvider data_ = liftIO $ do
    let length_ = fromIntegral $ B.length data_
    cssProvider' <- unsafeManagedPtrCastPtr cssProvider
    data_' <- packByteString data_
    onException (do
        _ <- propagateGError $ gtk_css_provider_load_from_data cssProvider' data_' length_
        touchManagedPtr cssProvider
        freeMem data_'
        return ()
     ) (do
        freeMem data_'
     )

#if defined(ENABLE_OVERLOADING)
data CssProviderLoadFromDataMethodInfo
instance (signature ~ (ByteString -> m ()), MonadIO m, IsCssProvider a) => O.OverloadedMethod CssProviderLoadFromDataMethodInfo a signature where
    overloadedMethod = cssProviderLoadFromData

instance O.OverloadedMethodInfo CssProviderLoadFromDataMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CssProvider.cssProviderLoadFromData",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CssProvider.html#v:cssProviderLoadFromData"
        })


#endif

-- method CssProvider::load_from_file
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "css_provider"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CssProvider" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCssProvider" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "file"
--           , argType = TInterface Name { namespace = "Gio" , name = "File" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "#GFile pointing to a file to load"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_css_provider_load_from_file" gtk_css_provider_load_from_file :: 
    Ptr CssProvider ->                      -- css_provider : TInterface (Name {namespace = "Gtk", name = "CssProvider"})
    Ptr Gio.File.File ->                    -- file : TInterface (Name {namespace = "Gio", name = "File"})
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Loads the data contained in /@file@/ into /@cssProvider@/, making it
-- clear any previously loaded information.
cssProviderLoadFromFile ::
    (B.CallStack.HasCallStack, MonadIO m, IsCssProvider a, Gio.File.IsFile b) =>
    a
    -- ^ /@cssProvider@/: a t'GI.Gtk.Objects.CssProvider.CssProvider'
    -> b
    -- ^ /@file@/: t'GI.Gio.Interfaces.File.File' pointing to a file to load
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
cssProviderLoadFromFile cssProvider file = liftIO $ do
    cssProvider' <- unsafeManagedPtrCastPtr cssProvider
    file' <- unsafeManagedPtrCastPtr file
    onException (do
        _ <- propagateGError $ gtk_css_provider_load_from_file cssProvider' file'
        touchManagedPtr cssProvider
        touchManagedPtr file
        return ()
     ) (do
        return ()
     )

#if defined(ENABLE_OVERLOADING)
data CssProviderLoadFromFileMethodInfo
instance (signature ~ (b -> m ()), MonadIO m, IsCssProvider a, Gio.File.IsFile b) => O.OverloadedMethod CssProviderLoadFromFileMethodInfo a signature where
    overloadedMethod = cssProviderLoadFromFile

instance O.OverloadedMethodInfo CssProviderLoadFromFileMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CssProvider.cssProviderLoadFromFile",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CssProvider.html#v:cssProviderLoadFromFile"
        })


#endif

-- method CssProvider::load_from_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "css_provider"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CssProvider" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCssProvider" , sinceVersion = Nothing }
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
--                 { rawDocText =
--                     Just
--                       "the path of a filename to load, in the GLib filename encoding"
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
-- throws : True
-- Skip return : False

foreign import ccall "gtk_css_provider_load_from_path" gtk_css_provider_load_from_path :: 
    Ptr CssProvider ->                      -- css_provider : TInterface (Name {namespace = "Gtk", name = "CssProvider"})
    CString ->                              -- path : TBasicType TUTF8
    Ptr (Ptr GError) ->                     -- error
    IO CInt

-- | Loads the data contained in /@path@/ into /@cssProvider@/, making it clear
-- any previously loaded information.
cssProviderLoadFromPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsCssProvider a) =>
    a
    -- ^ /@cssProvider@/: a t'GI.Gtk.Objects.CssProvider.CssProvider'
    -> T.Text
    -- ^ /@path@/: the path of a filename to load, in the GLib filename encoding
    -> m ()
    -- ^ /(Can throw 'Data.GI.Base.GError.GError')/
cssProviderLoadFromPath cssProvider path = liftIO $ do
    cssProvider' <- unsafeManagedPtrCastPtr cssProvider
    path' <- textToCString path
    onException (do
        _ <- propagateGError $ gtk_css_provider_load_from_path cssProvider' path'
        touchManagedPtr cssProvider
        freeMem path'
        return ()
     ) (do
        freeMem path'
     )

#if defined(ENABLE_OVERLOADING)
data CssProviderLoadFromPathMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsCssProvider a) => O.OverloadedMethod CssProviderLoadFromPathMethodInfo a signature where
    overloadedMethod = cssProviderLoadFromPath

instance O.OverloadedMethodInfo CssProviderLoadFromPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CssProvider.cssProviderLoadFromPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CssProvider.html#v:cssProviderLoadFromPath"
        })


#endif

-- method CssProvider::load_from_resource
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "css_provider"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CssProvider" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkCssProvider" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "resource_path"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GResource resource path"
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

foreign import ccall "gtk_css_provider_load_from_resource" gtk_css_provider_load_from_resource :: 
    Ptr CssProvider ->                      -- css_provider : TInterface (Name {namespace = "Gtk", name = "CssProvider"})
    CString ->                              -- resource_path : TBasicType TUTF8
    IO ()

-- | Loads the data contained in the resource at /@resourcePath@/ into
-- the t'GI.Gtk.Objects.CssProvider.CssProvider', clearing any previously loaded information.
-- 
-- To track errors while loading CSS, connect to the
-- [CssProvider::parsingError]("GI.Gtk.Objects.CssProvider#g:signal:parsingError") signal.
-- 
-- /Since: 3.16/
cssProviderLoadFromResource ::
    (B.CallStack.HasCallStack, MonadIO m, IsCssProvider a) =>
    a
    -- ^ /@cssProvider@/: a t'GI.Gtk.Objects.CssProvider.CssProvider'
    -> T.Text
    -- ^ /@resourcePath@/: a t'GI.Gio.Structs.Resource.Resource' resource path
    -> m ()
cssProviderLoadFromResource cssProvider resourcePath = liftIO $ do
    cssProvider' <- unsafeManagedPtrCastPtr cssProvider
    resourcePath' <- textToCString resourcePath
    gtk_css_provider_load_from_resource cssProvider' resourcePath'
    touchManagedPtr cssProvider
    freeMem resourcePath'
    return ()

#if defined(ENABLE_OVERLOADING)
data CssProviderLoadFromResourceMethodInfo
instance (signature ~ (T.Text -> m ()), MonadIO m, IsCssProvider a) => O.OverloadedMethod CssProviderLoadFromResourceMethodInfo a signature where
    overloadedMethod = cssProviderLoadFromResource

instance O.OverloadedMethodInfo CssProviderLoadFromResourceMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CssProvider.cssProviderLoadFromResource",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CssProvider.html#v:cssProviderLoadFromResource"
        })


#endif

-- method CssProvider::to_string
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "provider"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "CssProvider" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the provider to write to a string"
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
-- returnType: Just (TBasicType TUTF8)
-- throws : False
-- Skip return : False

foreign import ccall "gtk_css_provider_to_string" gtk_css_provider_to_string :: 
    Ptr CssProvider ->                      -- provider : TInterface (Name {namespace = "Gtk", name = "CssProvider"})
    IO CString

-- | Converts the /@provider@/ into a string representation in CSS
-- format.
-- 
-- Using 'GI.Gtk.Objects.CssProvider.cssProviderLoadFromData' with the return value
-- from this function on a new provider created with
-- 'GI.Gtk.Objects.CssProvider.cssProviderNew' will basically create a duplicate of
-- this /@provider@/.
-- 
-- /Since: 3.2/
cssProviderToString ::
    (B.CallStack.HasCallStack, MonadIO m, IsCssProvider a) =>
    a
    -- ^ /@provider@/: the provider to write to a string
    -> m T.Text
    -- ^ __Returns:__ a new string representing the /@provider@/.
cssProviderToString provider = liftIO $ do
    provider' <- unsafeManagedPtrCastPtr provider
    result <- gtk_css_provider_to_string provider'
    checkUnexpectedReturnNULL "cssProviderToString" result
    result' <- cstringToText result
    freeMem result
    touchManagedPtr provider
    return result'

#if defined(ENABLE_OVERLOADING)
data CssProviderToStringMethodInfo
instance (signature ~ (m T.Text), MonadIO m, IsCssProvider a) => O.OverloadedMethod CssProviderToStringMethodInfo a signature where
    overloadedMethod = cssProviderToString

instance O.OverloadedMethodInfo CssProviderToStringMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.CssProvider.cssProviderToString",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-CssProvider.html#v:cssProviderToString"
        })


#endif

-- method CssProvider::get_default
-- method type : MemberFunction
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "CssProvider" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_css_provider_get_default" gtk_css_provider_get_default :: 
    IO (Ptr CssProvider)

{-# DEPRECATED cssProviderGetDefault ["(Since version 3.24)","Use 'GI.Gtk.Objects.CssProvider.cssProviderNew' instead."] #-}
-- | Returns the provider containing the style settings used as a
-- fallback for all widgets.
cssProviderGetDefault ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m CssProvider
    -- ^ __Returns:__ The provider used for fallback styling.
    --          This memory is owned by GTK+, and you must not free it.
cssProviderGetDefault  = liftIO $ do
    result <- gtk_css_provider_get_default
    checkUnexpectedReturnNULL "cssProviderGetDefault" result
    result' <- (newObject CssProvider) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method CssProvider::get_named
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "A theme name" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "variant"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "variant to load, for example, \"dark\", or\n    %NULL for the default"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "CssProvider" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_css_provider_get_named" gtk_css_provider_get_named :: 
    CString ->                              -- name : TBasicType TUTF8
    CString ->                              -- variant : TBasicType TUTF8
    IO (Ptr CssProvider)

-- | Loads a theme from the usual theme paths
cssProviderGetNamed ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@name@/: A theme name
    -> Maybe (T.Text)
    -- ^ /@variant@/: variant to load, for example, \"dark\", or
    --     'P.Nothing' for the default
    -> m CssProvider
    -- ^ __Returns:__ a t'GI.Gtk.Objects.CssProvider.CssProvider' with the theme loaded.
    --     This memory is owned by GTK+, and you must not free it.
cssProviderGetNamed name variant = liftIO $ do
    name' <- textToCString name
    maybeVariant <- case variant of
        Nothing -> return nullPtr
        Just jVariant -> do
            jVariant' <- textToCString jVariant
            return jVariant'
    result <- gtk_css_provider_get_named name' maybeVariant
    checkUnexpectedReturnNULL "cssProviderGetNamed" result
    result' <- (newObject CssProvider) result
    freeMem name'
    freeMem maybeVariant
    return result'

#if defined(ENABLE_OVERLOADING)
#endif


