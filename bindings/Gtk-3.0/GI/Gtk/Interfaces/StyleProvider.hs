{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- GtkStyleProvider is an interface used to provide style information to a t'GI.Gtk.Objects.StyleContext.StyleContext'.
-- See 'GI.Gtk.Objects.StyleContext.styleContextAddProvider' and 'GI.Gtk.Objects.StyleContext.styleContextAddProviderForScreen'.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Interfaces.StyleProvider
    ( 

-- * Exported types
    StyleProvider(..)                       ,
    IsStyleProvider                         ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- /None/.
-- 
-- ==== Getters
-- [getIconFactory]("GI.Gtk.Interfaces.StyleProvider#g:method:getIconFactory"), [getStyle]("GI.Gtk.Interfaces.StyleProvider#g:method:getStyle"), [getStyleProperty]("GI.Gtk.Interfaces.StyleProvider#g:method:getStyleProperty").
-- 
-- ==== Setters
-- /None/.

#if defined(ENABLE_OVERLOADING)
    ResolveStyleProviderMethod              ,
#endif

-- ** getIconFactory #method:getIconFactory#

#if defined(ENABLE_OVERLOADING)
    StyleProviderGetIconFactoryMethodInfo   ,
#endif
    styleProviderGetIconFactory             ,


-- ** getStyle #method:getStyle#

#if defined(ENABLE_OVERLOADING)
    StyleProviderGetStyleMethodInfo         ,
#endif
    styleProviderGetStyle                   ,


-- ** getStyleProperty #method:getStyleProperty#

#if defined(ENABLE_OVERLOADING)
    StyleProviderGetStylePropertyMethodInfo ,
#endif
    styleProviderGetStyleProperty           ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.IconFactory as Gtk.IconFactory
import {-# SOURCE #-} qualified GI.Gtk.Objects.StyleProperties as Gtk.StyleProperties
import {-# SOURCE #-} qualified GI.Gtk.Structs.WidgetPath as Gtk.WidgetPath

-- interface StyleProvider 
-- | Memory-managed wrapper type.
newtype StyleProvider = StyleProvider (SP.ManagedPtr StyleProvider)
    deriving (Eq)

instance SP.ManagedPtrNewtype StyleProvider where
    toManagedPtr (StyleProvider p) = p

-- | Type class for types which implement `StyleProvider`.
class (ManagedPtrNewtype o, O.IsDescendantOf StyleProvider o) => IsStyleProvider o
instance (ManagedPtrNewtype o, O.IsDescendantOf StyleProvider o) => IsStyleProvider o
-- XXX Wrapping a foreign struct/union with no known destructor or size, leak?
instance BoxedPtr StyleProvider where
    boxedPtrCopy = return
    boxedPtrFree = \_x -> return ()


#if defined(ENABLE_OVERLOADING)
type family ResolveStyleProviderMethod (t :: Symbol) (o :: *) :: * where
    ResolveStyleProviderMethod "getIconFactory" o = StyleProviderGetIconFactoryMethodInfo
    ResolveStyleProviderMethod "getStyle" o = StyleProviderGetStyleMethodInfo
    ResolveStyleProviderMethod "getStyleProperty" o = StyleProviderGetStylePropertyMethodInfo
    ResolveStyleProviderMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveStyleProviderMethod t StyleProvider, O.OverloadedMethod info StyleProvider p) => OL.IsLabel t (StyleProvider -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveStyleProviderMethod t StyleProvider, O.OverloadedMethod info StyleProvider p, R.HasField t StyleProvider p) => R.HasField t StyleProvider p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveStyleProviderMethod t StyleProvider, O.OverloadedMethodInfo info StyleProvider) => OL.IsLabel t (O.MethodProxy info StyleProvider) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- method StyleProvider::get_icon_factory
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "provider"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleProvider" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleProvider"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "#GtkWidgetPath to query"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "IconFactory" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_provider_get_icon_factory" gtk_style_provider_get_icon_factory :: 
    Ptr StyleProvider ->                    -- provider : TInterface (Name {namespace = "Gtk", name = "StyleProvider"})
    Ptr Gtk.WidgetPath.WidgetPath ->        -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    IO (Ptr Gtk.IconFactory.IconFactory)

{-# DEPRECATED styleProviderGetIconFactory ["(Since version 3.8)","Will always return 'P.Nothing' for all GTK-provided style providers."] #-}
-- | Returns the t'GI.Gtk.Objects.IconFactory.IconFactory' defined to be in use for /@path@/, or 'P.Nothing' if none
-- is defined.
-- 
-- /Since: 3.0/
styleProviderGetIconFactory ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleProvider a) =>
    a
    -- ^ /@provider@/: a t'GI.Gtk.Interfaces.StyleProvider.StyleProvider'
    -> Gtk.WidgetPath.WidgetPath
    -- ^ /@path@/: t'GI.Gtk.Structs.WidgetPath.WidgetPath' to query
    -> m (Maybe Gtk.IconFactory.IconFactory)
    -- ^ __Returns:__ The icon factory to use for /@path@/, or 'P.Nothing'
styleProviderGetIconFactory provider path = liftIO $ do
    provider' <- unsafeManagedPtrCastPtr provider
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_style_provider_get_icon_factory provider' path'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gtk.IconFactory.IconFactory) result'
        return result''
    touchManagedPtr provider
    touchManagedPtr path
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data StyleProviderGetIconFactoryMethodInfo
instance (signature ~ (Gtk.WidgetPath.WidgetPath -> m (Maybe Gtk.IconFactory.IconFactory)), MonadIO m, IsStyleProvider a) => O.OverloadedMethod StyleProviderGetIconFactoryMethodInfo a signature where
    overloadedMethod = styleProviderGetIconFactory

instance O.OverloadedMethodInfo StyleProviderGetIconFactoryMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.StyleProvider.styleProviderGetIconFactory",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-StyleProvider.html#v:styleProviderGetIconFactory"
        })


#endif

-- method StyleProvider::get_style
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "provider"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleProvider" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleProvider"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "#GtkWidgetPath to query"
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
--               (TInterface Name { namespace = "Gtk" , name = "StyleProperties" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_provider_get_style" gtk_style_provider_get_style :: 
    Ptr StyleProvider ->                    -- provider : TInterface (Name {namespace = "Gtk", name = "StyleProvider"})
    Ptr Gtk.WidgetPath.WidgetPath ->        -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    IO (Ptr Gtk.StyleProperties.StyleProperties)

{-# DEPRECATED styleProviderGetStyle ["(Since version 3.8)","Will always return 'P.Nothing' for all GTK-provided style providers","    as the interface cannot correctly work the way CSS is specified."] #-}
-- | Returns the style settings affecting a widget defined by /@path@/, or 'P.Nothing' if
-- /@provider@/ doesn’t contemplate styling /@path@/.
-- 
-- /Since: 3.0/
styleProviderGetStyle ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleProvider a) =>
    a
    -- ^ /@provider@/: a t'GI.Gtk.Interfaces.StyleProvider.StyleProvider'
    -> Gtk.WidgetPath.WidgetPath
    -- ^ /@path@/: t'GI.Gtk.Structs.WidgetPath.WidgetPath' to query
    -> m (Maybe Gtk.StyleProperties.StyleProperties)
    -- ^ __Returns:__ a t'GI.Gtk.Objects.StyleProperties.StyleProperties' containing the
    -- style settings affecting /@path@/
styleProviderGetStyle provider path = liftIO $ do
    provider' <- unsafeManagedPtrCastPtr provider
    path' <- unsafeManagedPtrGetPtr path
    result <- gtk_style_provider_get_style provider' path'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (wrapObject Gtk.StyleProperties.StyleProperties) result'
        return result''
    touchManagedPtr provider
    touchManagedPtr path
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data StyleProviderGetStyleMethodInfo
instance (signature ~ (Gtk.WidgetPath.WidgetPath -> m (Maybe Gtk.StyleProperties.StyleProperties)), MonadIO m, IsStyleProvider a) => O.OverloadedMethod StyleProviderGetStyleMethodInfo a signature where
    overloadedMethod = styleProviderGetStyle

instance O.OverloadedMethodInfo StyleProviderGetStyleMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.StyleProvider.styleProviderGetStyle",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-StyleProvider.html#v:styleProviderGetStyle"
        })


#endif

-- method StyleProvider::get_style_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "provider"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StyleProvider" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyleProvider"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "path"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "WidgetPath" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "#GtkWidgetPath to query"
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
--               TInterface Name { namespace = "Gtk" , name = "StateFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "state to query the style property for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "pspec"
--           , argType = TParamSpec
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "The #GParamSpec to query"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "value"
--           , argType = TGValue
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for the property value"
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

foreign import ccall "gtk_style_provider_get_style_property" gtk_style_provider_get_style_property :: 
    Ptr StyleProvider ->                    -- provider : TInterface (Name {namespace = "Gtk", name = "StyleProvider"})
    Ptr Gtk.WidgetPath.WidgetPath ->        -- path : TInterface (Name {namespace = "Gtk", name = "WidgetPath"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    Ptr GParamSpec ->                       -- pspec : TParamSpec
    Ptr GValue ->                           -- value : TGValue
    IO CInt

-- | Looks up a widget style property as defined by /@provider@/ for
-- the widget represented by /@path@/.
-- 
-- /Since: 3.0/
styleProviderGetStyleProperty ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyleProvider a) =>
    a
    -- ^ /@provider@/: a t'GI.Gtk.Interfaces.StyleProvider.StyleProvider'
    -> Gtk.WidgetPath.WidgetPath
    -- ^ /@path@/: t'GI.Gtk.Structs.WidgetPath.WidgetPath' to query
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to query the style property for
    -> GParamSpec
    -- ^ /@pspec@/: The t'GI.GObject.Objects.ParamSpec.ParamSpec' to query
    -> m ((Bool, GValue))
    -- ^ __Returns:__ 'P.True' if the property was found and has a value, 'P.False' otherwise
styleProviderGetStyleProperty provider path state pspec = liftIO $ do
    provider' <- unsafeManagedPtrCastPtr provider
    path' <- unsafeManagedPtrGetPtr path
    let state' = gflagsToWord state
    pspec' <- unsafeManagedPtrGetPtr pspec
    value <- SP.callocBytes 24 :: IO (Ptr GValue)
    result <- gtk_style_provider_get_style_property provider' path' state' pspec' value
    let result' = (/= 0) result
    value' <- B.GValue.wrapGValuePtr value
    touchManagedPtr provider
    touchManagedPtr path
    touchManagedPtr pspec
    return (result', value')

#if defined(ENABLE_OVERLOADING)
data StyleProviderGetStylePropertyMethodInfo
instance (signature ~ (Gtk.WidgetPath.WidgetPath -> [Gtk.Flags.StateFlags] -> GParamSpec -> m ((Bool, GValue))), MonadIO m, IsStyleProvider a) => O.OverloadedMethod StyleProviderGetStylePropertyMethodInfo a signature where
    overloadedMethod = styleProviderGetStyleProperty

instance O.OverloadedMethodInfo StyleProviderGetStylePropertyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Interfaces.StyleProvider.styleProviderGetStyleProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Interfaces-StyleProvider.html#v:styleProviderGetStyleProperty"
        })


#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList StyleProvider = StyleProviderSignalList
type StyleProviderSignalList = ('[ ] :: [(Symbol, *)])

#endif


