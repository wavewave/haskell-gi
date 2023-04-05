{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- An icon factory manages a collection of t'GI.Gtk.Structs.IconSet.IconSet'; a t'GI.Gtk.Structs.IconSet.IconSet' manages a
-- set of variants of a particular icon (i.e. a t'GI.Gtk.Structs.IconSet.IconSet' contains variants for
-- different sizes and widget states). Icons in an icon factory are named by a
-- stock ID, which is a simple string identifying the icon. Each t'GI.Gtk.Objects.Style.Style' has a
-- list of t'GI.Gtk.Objects.IconFactory.IconFactory' derived from the current theme; those icon factories
-- are consulted first when searching for an icon. If the theme doesn’t set a
-- particular icon, GTK+ looks for the icon in a list of default icon factories,
-- maintained by 'GI.Gtk.Objects.IconFactory.iconFactoryAddDefault' and
-- 'GI.Gtk.Objects.IconFactory.iconFactoryRemoveDefault'. Applications with icons should add a default
-- icon factory with their icons, which will allow themes to override the icons
-- for the application.
-- 
-- To display an icon, always use 'GI.Gtk.Objects.Style.styleLookupIconSet' on the widget that
-- will display the icon, or the convenience function
-- 'GI.Gtk.Objects.Widget.widgetRenderIcon'. These functions take the theme into account when
-- looking up the icon to use for a given stock ID.
-- 
-- # GtkIconFactory as GtkBuildable # {t'GI.Gtk.Objects.IconFactory.IconFactory'-BUILDER-UI}
-- 
-- GtkIconFactory supports a custom @\<sources>@ element, which can contain
-- multiple @\<source>@ elements. The following attributes are allowed:
-- 
-- * stock-id
-- 
-- 
--     The stock id of the source, a string. This attribute is
--     mandatory
-- 
-- * filename
-- 
-- 
--     The filename of the source, a string.  This attribute is
--     optional
-- 
-- * icon-name
-- 
-- 
--     The icon name for the source, a string.  This attribute is
--     optional.
-- 
-- * size
-- 
-- 
--     Size of the icon, a t'GI.Gtk.Enums.IconSize' enum value.  This attribute is
--     optional.
-- 
-- * direction
-- 
-- 
--     Direction of the source, a t'GI.Gtk.Enums.TextDirection' enum value.  This
--     attribute is optional.
-- 
-- * state
-- 
-- 
--     State of the source, a t'GI.Gtk.Enums.StateType' enum value.  This
--     attribute is optional.
-- 
-- 
-- ## A t'GI.Gtk.Objects.IconFactory.IconFactory' UI definition fragment. ##
-- 
-- >
-- ><object class="GtkIconFactory" id="iconfactory1">
-- >  <sources>
-- >    <source stock-id="apple-red" filename="apple-red.png"/>
-- >  </sources>
-- ></object>
-- ><object class="GtkWindow" id="window1">
-- >  <child>
-- >    <object class="GtkButton" id="apple_button">
-- >      <property name="label">apple-red</property>
-- >      <property name="use-stock">True</property>
-- >    </object>
-- >  </child>
-- ></object>
-- 

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.IconFactory
    ( 

-- * Exported types
    IconFactory(..)                         ,
    IsIconFactory                           ,
    toIconFactory                           ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [add]("GI.Gtk.Objects.IconFactory#g:method:add"), [addChild]("GI.Gtk.Interfaces.Buildable#g:method:addChild"), [addDefault]("GI.Gtk.Objects.IconFactory#g:method:addDefault"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [constructChild]("GI.Gtk.Interfaces.Buildable#g:method:constructChild"), [customFinished]("GI.Gtk.Interfaces.Buildable#g:method:customFinished"), [customTagEnd]("GI.Gtk.Interfaces.Buildable#g:method:customTagEnd"), [customTagStart]("GI.Gtk.Interfaces.Buildable#g:method:customTagStart"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [lookup]("GI.Gtk.Objects.IconFactory#g:method:lookup"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [parserFinished]("GI.Gtk.Interfaces.Buildable#g:method:parserFinished"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [removeDefault]("GI.Gtk.Objects.IconFactory#g:method:removeDefault"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getInternalChild]("GI.Gtk.Interfaces.Buildable#g:method:getInternalChild"), [getName]("GI.Gtk.Interfaces.Buildable#g:method:getName"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata").
-- 
-- ==== Setters
-- [setBuildableProperty]("GI.Gtk.Interfaces.Buildable#g:method:setBuildableProperty"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setName]("GI.Gtk.Interfaces.Buildable#g:method:setName"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveIconFactoryMethod                ,
#endif

-- ** add #method:add#

#if defined(ENABLE_OVERLOADING)
    IconFactoryAddMethodInfo                ,
#endif
    iconFactoryAdd                          ,


-- ** addDefault #method:addDefault#

#if defined(ENABLE_OVERLOADING)
    IconFactoryAddDefaultMethodInfo         ,
#endif
    iconFactoryAddDefault                   ,


-- ** lookup #method:lookup#

#if defined(ENABLE_OVERLOADING)
    IconFactoryLookupMethodInfo             ,
#endif
    iconFactoryLookup                       ,


-- ** lookupDefault #method:lookupDefault#

    iconFactoryLookupDefault                ,


-- ** new #method:new#

    iconFactoryNew                          ,


-- ** removeDefault #method:removeDefault#

#if defined(ENABLE_OVERLOADING)
    IconFactoryRemoveDefaultMethodInfo      ,
#endif
    iconFactoryRemoveDefault                ,




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
import {-# SOURCE #-} qualified GI.Gtk.Interfaces.Buildable as Gtk.Buildable
import {-# SOURCE #-} qualified GI.Gtk.Structs.IconSet as Gtk.IconSet

-- | Memory-managed wrapper type.
newtype IconFactory = IconFactory (SP.ManagedPtr IconFactory)
    deriving (Eq)

instance SP.ManagedPtrNewtype IconFactory where
    toManagedPtr (IconFactory p) = p

foreign import ccall "gtk_icon_factory_get_type"
    c_gtk_icon_factory_get_type :: IO B.Types.GType

instance B.Types.TypedObject IconFactory where
    glibType = c_gtk_icon_factory_get_type

instance B.Types.GObject IconFactory

-- | Type class for types which can be safely cast to `IconFactory`, for instance with `toIconFactory`.
class (SP.GObject o, O.IsDescendantOf IconFactory o) => IsIconFactory o
instance (SP.GObject o, O.IsDescendantOf IconFactory o) => IsIconFactory o

instance O.HasParentTypes IconFactory
type instance O.ParentTypes IconFactory = '[GObject.Object.Object, Gtk.Buildable.Buildable]

-- | Cast to `IconFactory`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toIconFactory :: (MIO.MonadIO m, IsIconFactory o) => o -> m IconFactory
toIconFactory = MIO.liftIO . B.ManagedPtr.unsafeCastTo IconFactory

-- | Convert 'IconFactory' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe IconFactory) where
    gvalueGType_ = c_gtk_icon_factory_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr IconFactory)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr IconFactory)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject IconFactory ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveIconFactoryMethod (t :: Symbol) (o :: *) :: * where
    ResolveIconFactoryMethod "add" o = IconFactoryAddMethodInfo
    ResolveIconFactoryMethod "addChild" o = Gtk.Buildable.BuildableAddChildMethodInfo
    ResolveIconFactoryMethod "addDefault" o = IconFactoryAddDefaultMethodInfo
    ResolveIconFactoryMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveIconFactoryMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveIconFactoryMethod "constructChild" o = Gtk.Buildable.BuildableConstructChildMethodInfo
    ResolveIconFactoryMethod "customFinished" o = Gtk.Buildable.BuildableCustomFinishedMethodInfo
    ResolveIconFactoryMethod "customTagEnd" o = Gtk.Buildable.BuildableCustomTagEndMethodInfo
    ResolveIconFactoryMethod "customTagStart" o = Gtk.Buildable.BuildableCustomTagStartMethodInfo
    ResolveIconFactoryMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveIconFactoryMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveIconFactoryMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveIconFactoryMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveIconFactoryMethod "lookup" o = IconFactoryLookupMethodInfo
    ResolveIconFactoryMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveIconFactoryMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveIconFactoryMethod "parserFinished" o = Gtk.Buildable.BuildableParserFinishedMethodInfo
    ResolveIconFactoryMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveIconFactoryMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveIconFactoryMethod "removeDefault" o = IconFactoryRemoveDefaultMethodInfo
    ResolveIconFactoryMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveIconFactoryMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveIconFactoryMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveIconFactoryMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveIconFactoryMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveIconFactoryMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveIconFactoryMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveIconFactoryMethod "getInternalChild" o = Gtk.Buildable.BuildableGetInternalChildMethodInfo
    ResolveIconFactoryMethod "getName" o = Gtk.Buildable.BuildableGetNameMethodInfo
    ResolveIconFactoryMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveIconFactoryMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveIconFactoryMethod "setBuildableProperty" o = Gtk.Buildable.BuildableSetBuildablePropertyMethodInfo
    ResolveIconFactoryMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveIconFactoryMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveIconFactoryMethod "setName" o = Gtk.Buildable.BuildableSetNameMethodInfo
    ResolveIconFactoryMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveIconFactoryMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveIconFactoryMethod t IconFactory, O.OverloadedMethod info IconFactory p) => OL.IsLabel t (IconFactory -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveIconFactoryMethod t IconFactory, O.OverloadedMethod info IconFactory p, R.HasField t IconFactory p) => R.HasField t IconFactory p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveIconFactoryMethod t IconFactory, O.OverloadedMethodInfo info IconFactory) => OL.IsLabel t (O.MethodProxy info IconFactory) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList IconFactory
type instance O.AttributeList IconFactory = IconFactoryAttributeList
type IconFactoryAttributeList = ('[ ] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList IconFactory = IconFactorySignalList
type IconFactorySignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method IconFactory::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "IconFactory" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_factory_new" gtk_icon_factory_new :: 
    IO (Ptr IconFactory)

{-# DEPRECATED iconFactoryNew ["(Since version 3.10)","Use t'GI.Gtk.Objects.IconTheme.IconTheme' instead."] #-}
-- | Creates a new t'GI.Gtk.Objects.IconFactory.IconFactory'. An icon factory manages a collection
-- of @/GtkIconSets/@; a t'GI.Gtk.Structs.IconSet.IconSet' manages a set of variants of a
-- particular icon (i.e. a t'GI.Gtk.Structs.IconSet.IconSet' contains variants for different
-- sizes and widget states). Icons in an icon factory are named by a
-- stock ID, which is a simple string identifying the icon. Each
-- t'GI.Gtk.Objects.Style.Style' has a list of @/GtkIconFactorys/@ derived from the current
-- theme; those icon factories are consulted first when searching for
-- an icon. If the theme doesn’t set a particular icon, GTK+ looks for
-- the icon in a list of default icon factories, maintained by
-- 'GI.Gtk.Objects.IconFactory.iconFactoryAddDefault' and
-- 'GI.Gtk.Objects.IconFactory.iconFactoryRemoveDefault'. Applications with icons should
-- add a default icon factory with their icons, which will allow
-- themes to override the icons for the application.
iconFactoryNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m IconFactory
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.IconFactory.IconFactory'
iconFactoryNew  = liftIO $ do
    result <- gtk_icon_factory_new
    checkUnexpectedReturnNULL "iconFactoryNew" result
    result' <- (wrapObject IconFactory) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method IconFactory::add
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "factory"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconFactory" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconFactory" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "stock_id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "icon name" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "icon_set"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconSet" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "icon set" , sinceVersion = Nothing }
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

foreign import ccall "gtk_icon_factory_add" gtk_icon_factory_add :: 
    Ptr IconFactory ->                      -- factory : TInterface (Name {namespace = "Gtk", name = "IconFactory"})
    CString ->                              -- stock_id : TBasicType TUTF8
    Ptr Gtk.IconSet.IconSet ->              -- icon_set : TInterface (Name {namespace = "Gtk", name = "IconSet"})
    IO ()

{-# DEPRECATED iconFactoryAdd ["(Since version 3.10)","Use t'GI.Gtk.Objects.IconTheme.IconTheme' instead."] #-}
-- | Adds the given /@iconSet@/ to the icon factory, under the name
-- /@stockId@/.  /@stockId@/ should be namespaced for your application,
-- e.g. “myapp-whatever-icon”.  Normally applications create a
-- t'GI.Gtk.Objects.IconFactory.IconFactory', then add it to the list of default factories with
-- 'GI.Gtk.Objects.IconFactory.iconFactoryAddDefault'. Then they pass the /@stockId@/ to
-- widgets such as t'GI.Gtk.Objects.Image.Image' to display the icon. Themes can provide
-- an icon with the same name (such as \"myapp-whatever-icon\") to
-- override your application’s default icons. If an icon already
-- existed in /@factory@/ for /@stockId@/, it is unreferenced and replaced
-- with the new /@iconSet@/.
iconFactoryAdd ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconFactory a) =>
    a
    -- ^ /@factory@/: a t'GI.Gtk.Objects.IconFactory.IconFactory'
    -> T.Text
    -- ^ /@stockId@/: icon name
    -> Gtk.IconSet.IconSet
    -- ^ /@iconSet@/: icon set
    -> m ()
iconFactoryAdd factory stockId iconSet = liftIO $ do
    factory' <- unsafeManagedPtrCastPtr factory
    stockId' <- textToCString stockId
    iconSet' <- unsafeManagedPtrGetPtr iconSet
    gtk_icon_factory_add factory' stockId' iconSet'
    touchManagedPtr factory
    touchManagedPtr iconSet
    freeMem stockId'
    return ()

#if defined(ENABLE_OVERLOADING)
data IconFactoryAddMethodInfo
instance (signature ~ (T.Text -> Gtk.IconSet.IconSet -> m ()), MonadIO m, IsIconFactory a) => O.OverloadedMethod IconFactoryAddMethodInfo a signature where
    overloadedMethod = iconFactoryAdd

instance O.OverloadedMethodInfo IconFactoryAddMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconFactory.iconFactoryAdd",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconFactory.html#v:iconFactoryAdd"
        })


#endif

-- method IconFactory::add_default
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "factory"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconFactory" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconFactory" , sinceVersion = Nothing }
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

foreign import ccall "gtk_icon_factory_add_default" gtk_icon_factory_add_default :: 
    Ptr IconFactory ->                      -- factory : TInterface (Name {namespace = "Gtk", name = "IconFactory"})
    IO ()

{-# DEPRECATED iconFactoryAddDefault ["(Since version 3.10)","Use t'GI.Gtk.Objects.IconTheme.IconTheme' instead."] #-}
-- | Adds an icon factory to the list of icon factories searched by
-- 'GI.Gtk.Objects.Style.styleLookupIconSet'. This means that, for example,
-- 'GI.Gtk.Objects.Image.imageNewFromStock' will be able to find icons in /@factory@/.
-- There will normally be an icon factory added for each library or
-- application that comes with icons. The default icon factories
-- can be overridden by themes.
iconFactoryAddDefault ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconFactory a) =>
    a
    -- ^ /@factory@/: a t'GI.Gtk.Objects.IconFactory.IconFactory'
    -> m ()
iconFactoryAddDefault factory = liftIO $ do
    factory' <- unsafeManagedPtrCastPtr factory
    gtk_icon_factory_add_default factory'
    touchManagedPtr factory
    return ()

#if defined(ENABLE_OVERLOADING)
data IconFactoryAddDefaultMethodInfo
instance (signature ~ (m ()), MonadIO m, IsIconFactory a) => O.OverloadedMethod IconFactoryAddDefaultMethodInfo a signature where
    overloadedMethod = iconFactoryAddDefault

instance O.OverloadedMethodInfo IconFactoryAddDefaultMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconFactory.iconFactoryAddDefault",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconFactory.html#v:iconFactoryAddDefault"
        })


#endif

-- method IconFactory::lookup
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "factory"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconFactory" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkIconFactory" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "stock_id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an icon name" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "IconSet" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_factory_lookup" gtk_icon_factory_lookup :: 
    Ptr IconFactory ->                      -- factory : TInterface (Name {namespace = "Gtk", name = "IconFactory"})
    CString ->                              -- stock_id : TBasicType TUTF8
    IO (Ptr Gtk.IconSet.IconSet)

{-# DEPRECATED iconFactoryLookup ["(Since version 3.10)","Use t'GI.Gtk.Objects.IconTheme.IconTheme' instead."] #-}
-- | Looks up /@stockId@/ in the icon factory, returning an icon set
-- if found, otherwise 'P.Nothing'. For display to the user, you should
-- use 'GI.Gtk.Objects.Style.styleLookupIconSet' on the t'GI.Gtk.Objects.Style.Style' for the
-- widget that will display the icon, instead of using this
-- function directly, so that themes are taken into account.
iconFactoryLookup ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconFactory a) =>
    a
    -- ^ /@factory@/: a t'GI.Gtk.Objects.IconFactory.IconFactory'
    -> T.Text
    -- ^ /@stockId@/: an icon name
    -> m Gtk.IconSet.IconSet
    -- ^ __Returns:__ icon set of /@stockId@/.
iconFactoryLookup factory stockId = liftIO $ do
    factory' <- unsafeManagedPtrCastPtr factory
    stockId' <- textToCString stockId
    result <- gtk_icon_factory_lookup factory' stockId'
    checkUnexpectedReturnNULL "iconFactoryLookup" result
    result' <- (newBoxed Gtk.IconSet.IconSet) result
    touchManagedPtr factory
    freeMem stockId'
    return result'

#if defined(ENABLE_OVERLOADING)
data IconFactoryLookupMethodInfo
instance (signature ~ (T.Text -> m Gtk.IconSet.IconSet), MonadIO m, IsIconFactory a) => O.OverloadedMethod IconFactoryLookupMethodInfo a signature where
    overloadedMethod = iconFactoryLookup

instance O.OverloadedMethodInfo IconFactoryLookupMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconFactory.iconFactoryLookup",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconFactory.html#v:iconFactoryLookup"
        })


#endif

-- method IconFactory::remove_default
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "factory"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconFactory" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just
--                       "a #GtkIconFactory previously added with gtk_icon_factory_add_default()"
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

foreign import ccall "gtk_icon_factory_remove_default" gtk_icon_factory_remove_default :: 
    Ptr IconFactory ->                      -- factory : TInterface (Name {namespace = "Gtk", name = "IconFactory"})
    IO ()

{-# DEPRECATED iconFactoryRemoveDefault ["(Since version 3.10)","Use t'GI.Gtk.Objects.IconTheme.IconTheme' instead."] #-}
-- | Removes an icon factory from the list of default icon
-- factories. Not normally used; you might use it for a library that
-- can be unloaded or shut down.
iconFactoryRemoveDefault ::
    (B.CallStack.HasCallStack, MonadIO m, IsIconFactory a) =>
    a
    -- ^ /@factory@/: a t'GI.Gtk.Objects.IconFactory.IconFactory' previously added with 'GI.Gtk.Objects.IconFactory.iconFactoryAddDefault'
    -> m ()
iconFactoryRemoveDefault factory = liftIO $ do
    factory' <- unsafeManagedPtrCastPtr factory
    gtk_icon_factory_remove_default factory'
    touchManagedPtr factory
    return ()

#if defined(ENABLE_OVERLOADING)
data IconFactoryRemoveDefaultMethodInfo
instance (signature ~ (m ()), MonadIO m, IsIconFactory a) => O.OverloadedMethod IconFactoryRemoveDefaultMethodInfo a signature where
    overloadedMethod = iconFactoryRemoveDefault

instance O.OverloadedMethodInfo IconFactoryRemoveDefaultMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.IconFactory.iconFactoryRemoveDefault",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-IconFactory.html#v:iconFactoryRemoveDefault"
        })


#endif

-- method IconFactory::lookup_default
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "stock_id"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "an icon name" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "IconSet" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_icon_factory_lookup_default" gtk_icon_factory_lookup_default :: 
    CString ->                              -- stock_id : TBasicType TUTF8
    IO (Ptr Gtk.IconSet.IconSet)

{-# DEPRECATED iconFactoryLookupDefault ["(Since version 3.10)","Use t'GI.Gtk.Objects.IconTheme.IconTheme' instead."] #-}
-- | Looks for an icon in the list of default icon factories.  For
-- display to the user, you should use 'GI.Gtk.Objects.Style.styleLookupIconSet' on
-- the t'GI.Gtk.Objects.Style.Style' for the widget that will display the icon, instead of
-- using this function directly, so that themes are taken into
-- account.
iconFactoryLookupDefault ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@stockId@/: an icon name
    -> m Gtk.IconSet.IconSet
    -- ^ __Returns:__ a t'GI.Gtk.Structs.IconSet.IconSet', or 'P.Nothing'
iconFactoryLookupDefault stockId = liftIO $ do
    stockId' <- textToCString stockId
    result <- gtk_icon_factory_lookup_default stockId'
    checkUnexpectedReturnNULL "iconFactoryLookupDefault" result
    result' <- (newBoxed Gtk.IconSet.IconSet) result
    freeMem stockId'
    return result'

#if defined(ENABLE_OVERLOADING)
#endif


