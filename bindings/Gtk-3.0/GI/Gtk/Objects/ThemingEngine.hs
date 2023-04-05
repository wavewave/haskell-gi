{-# LANGUAGE TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.ThemingEngine.ThemingEngine' was the object used for rendering themed content
-- in GTK+ widgets. It used to allow overriding GTK+\'s default
-- implementation of rendering functions by allowing engines to be
-- loaded as modules.
-- 
-- t'GI.Gtk.Objects.ThemingEngine.ThemingEngine' has been deprecated in GTK+ 3.14 and will be
-- ignored for rendering. The advancements in CSS theming are good
-- enough to allow themers to achieve their goals without the need
-- to modify source code.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.ThemingEngine
    ( 

-- * Exported types
    ThemingEngine(..)                       ,
    IsThemingEngine                         ,
    toThemingEngine                         ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [hasClass]("GI.Gtk.Objects.ThemingEngine#g:method:hasClass"), [hasRegion]("GI.Gtk.Objects.ThemingEngine#g:method:hasRegion"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [lookupColor]("GI.Gtk.Objects.ThemingEngine#g:method:lookupColor"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stateIsRunning]("GI.Gtk.Objects.ThemingEngine#g:method:stateIsRunning"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getBackgroundColor]("GI.Gtk.Objects.ThemingEngine#g:method:getBackgroundColor"), [getBorder]("GI.Gtk.Objects.ThemingEngine#g:method:getBorder"), [getBorderColor]("GI.Gtk.Objects.ThemingEngine#g:method:getBorderColor"), [getColor]("GI.Gtk.Objects.ThemingEngine#g:method:getColor"), [getData]("GI.GObject.Objects.Object#g:method:getData"), [getDirection]("GI.Gtk.Objects.ThemingEngine#g:method:getDirection"), [getFont]("GI.Gtk.Objects.ThemingEngine#g:method:getFont"), [getJunctionSides]("GI.Gtk.Objects.ThemingEngine#g:method:getJunctionSides"), [getMargin]("GI.Gtk.Objects.ThemingEngine#g:method:getMargin"), [getPadding]("GI.Gtk.Objects.ThemingEngine#g:method:getPadding"), [getPath]("GI.Gtk.Objects.ThemingEngine#g:method:getPath"), [getProperty]("GI.Gtk.Objects.ThemingEngine#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getScreen]("GI.Gtk.Objects.ThemingEngine#g:method:getScreen"), [getState]("GI.Gtk.Objects.ThemingEngine#g:method:getState"), [getStyleProperty]("GI.Gtk.Objects.ThemingEngine#g:method:getStyleProperty").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveThemingEngineMethod              ,
#endif

-- ** getBackgroundColor #method:getBackgroundColor#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineGetBackgroundColorMethodInfo,
#endif
    themingEngineGetBackgroundColor         ,


-- ** getBorder #method:getBorder#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineGetBorderMethodInfo        ,
#endif
    themingEngineGetBorder                  ,


-- ** getBorderColor #method:getBorderColor#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineGetBorderColorMethodInfo   ,
#endif
    themingEngineGetBorderColor             ,


-- ** getColor #method:getColor#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineGetColorMethodInfo         ,
#endif
    themingEngineGetColor                   ,


-- ** getDirection #method:getDirection#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineGetDirectionMethodInfo     ,
#endif
    themingEngineGetDirection               ,


-- ** getFont #method:getFont#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineGetFontMethodInfo          ,
#endif
    themingEngineGetFont                    ,


-- ** getJunctionSides #method:getJunctionSides#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineGetJunctionSidesMethodInfo ,
#endif
    themingEngineGetJunctionSides           ,


-- ** getMargin #method:getMargin#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineGetMarginMethodInfo        ,
#endif
    themingEngineGetMargin                  ,


-- ** getPadding #method:getPadding#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineGetPaddingMethodInfo       ,
#endif
    themingEngineGetPadding                 ,


-- ** getPath #method:getPath#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineGetPathMethodInfo          ,
#endif
    themingEngineGetPath                    ,


-- ** getProperty #method:getProperty#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineGetPropertyMethodInfo      ,
#endif
    themingEngineGetProperty                ,


-- ** getScreen #method:getScreen#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineGetScreenMethodInfo        ,
#endif
    themingEngineGetScreen                  ,


-- ** getState #method:getState#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineGetStateMethodInfo         ,
#endif
    themingEngineGetState                   ,


-- ** getStyleProperty #method:getStyleProperty#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineGetStylePropertyMethodInfo ,
#endif
    themingEngineGetStyleProperty           ,


-- ** hasClass #method:hasClass#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineHasClassMethodInfo         ,
#endif
    themingEngineHasClass                   ,


-- ** hasRegion #method:hasRegion#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineHasRegionMethodInfo        ,
#endif
    themingEngineHasRegion                  ,


-- ** load #method:load#

    themingEngineLoad                       ,


-- ** lookupColor #method:lookupColor#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineLookupColorMethodInfo      ,
#endif
    themingEngineLookupColor                ,


-- ** stateIsRunning #method:stateIsRunning#

#if defined(ENABLE_OVERLOADING)
    ThemingEngineStateIsRunningMethodInfo   ,
#endif
    themingEngineStateIsRunning             ,




 -- * Properties


-- ** name #attr:name#
-- | The theming engine name, this name will be used when registering
-- custom properties, for a theming engine named \"Clearlooks\" registering
-- a \"glossy\" custom property, it could be referenced in the CSS file as
-- 
-- >
-- >-Clearlooks-glossy: true;
-- 
-- 
-- /Since: 3.0/

#if defined(ENABLE_OVERLOADING)
    ThemingEngineNamePropertyInfo           ,
#endif
    constructThemingEngineName              ,
    getThemingEngineName                    ,
#if defined(ENABLE_OVERLOADING)
    themingEngineName                       ,
#endif




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
import qualified GI.Gdk.Objects.Screen as Gdk.Screen
import qualified GI.Gdk.Structs.RGBA as Gdk.RGBA
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Structs.Border as Gtk.Border
import {-# SOURCE #-} qualified GI.Gtk.Structs.WidgetPath as Gtk.WidgetPath
import qualified GI.Pango.Structs.FontDescription as Pango.FontDescription

-- | Memory-managed wrapper type.
newtype ThemingEngine = ThemingEngine (SP.ManagedPtr ThemingEngine)
    deriving (Eq)

instance SP.ManagedPtrNewtype ThemingEngine where
    toManagedPtr (ThemingEngine p) = p

foreign import ccall "gtk_theming_engine_get_type"
    c_gtk_theming_engine_get_type :: IO B.Types.GType

instance B.Types.TypedObject ThemingEngine where
    glibType = c_gtk_theming_engine_get_type

instance B.Types.GObject ThemingEngine

-- | Type class for types which can be safely cast to `ThemingEngine`, for instance with `toThemingEngine`.
class (SP.GObject o, O.IsDescendantOf ThemingEngine o) => IsThemingEngine o
instance (SP.GObject o, O.IsDescendantOf ThemingEngine o) => IsThemingEngine o

instance O.HasParentTypes ThemingEngine
type instance O.ParentTypes ThemingEngine = '[GObject.Object.Object]

-- | Cast to `ThemingEngine`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toThemingEngine :: (MIO.MonadIO m, IsThemingEngine o) => o -> m ThemingEngine
toThemingEngine = MIO.liftIO . B.ManagedPtr.unsafeCastTo ThemingEngine

-- | Convert 'ThemingEngine' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe ThemingEngine) where
    gvalueGType_ = c_gtk_theming_engine_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr ThemingEngine)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr ThemingEngine)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject ThemingEngine ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveThemingEngineMethod (t :: Symbol) (o :: *) :: * where
    ResolveThemingEngineMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveThemingEngineMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveThemingEngineMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveThemingEngineMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveThemingEngineMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveThemingEngineMethod "hasClass" o = ThemingEngineHasClassMethodInfo
    ResolveThemingEngineMethod "hasRegion" o = ThemingEngineHasRegionMethodInfo
    ResolveThemingEngineMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveThemingEngineMethod "lookupColor" o = ThemingEngineLookupColorMethodInfo
    ResolveThemingEngineMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveThemingEngineMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveThemingEngineMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveThemingEngineMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveThemingEngineMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveThemingEngineMethod "stateIsRunning" o = ThemingEngineStateIsRunningMethodInfo
    ResolveThemingEngineMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveThemingEngineMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveThemingEngineMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveThemingEngineMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveThemingEngineMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveThemingEngineMethod "getBackgroundColor" o = ThemingEngineGetBackgroundColorMethodInfo
    ResolveThemingEngineMethod "getBorder" o = ThemingEngineGetBorderMethodInfo
    ResolveThemingEngineMethod "getBorderColor" o = ThemingEngineGetBorderColorMethodInfo
    ResolveThemingEngineMethod "getColor" o = ThemingEngineGetColorMethodInfo
    ResolveThemingEngineMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveThemingEngineMethod "getDirection" o = ThemingEngineGetDirectionMethodInfo
    ResolveThemingEngineMethod "getFont" o = ThemingEngineGetFontMethodInfo
    ResolveThemingEngineMethod "getJunctionSides" o = ThemingEngineGetJunctionSidesMethodInfo
    ResolveThemingEngineMethod "getMargin" o = ThemingEngineGetMarginMethodInfo
    ResolveThemingEngineMethod "getPadding" o = ThemingEngineGetPaddingMethodInfo
    ResolveThemingEngineMethod "getPath" o = ThemingEngineGetPathMethodInfo
    ResolveThemingEngineMethod "getProperty" o = ThemingEngineGetPropertyMethodInfo
    ResolveThemingEngineMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveThemingEngineMethod "getScreen" o = ThemingEngineGetScreenMethodInfo
    ResolveThemingEngineMethod "getState" o = ThemingEngineGetStateMethodInfo
    ResolveThemingEngineMethod "getStyleProperty" o = ThemingEngineGetStylePropertyMethodInfo
    ResolveThemingEngineMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveThemingEngineMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveThemingEngineMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveThemingEngineMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveThemingEngineMethod t ThemingEngine, O.OverloadedMethod info ThemingEngine p) => OL.IsLabel t (ThemingEngine -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveThemingEngineMethod t ThemingEngine, O.OverloadedMethod info ThemingEngine p, R.HasField t ThemingEngine p) => R.HasField t ThemingEngine p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveThemingEngineMethod t ThemingEngine, O.OverloadedMethodInfo info ThemingEngine) => OL.IsLabel t (O.MethodProxy info ThemingEngine) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- VVV Prop "name"
   -- Type: TBasicType TUTF8
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@name@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' themingEngine #name
-- @
getThemingEngineName :: (MonadIO m, IsThemingEngine o) => o -> m (Maybe T.Text)
getThemingEngineName obj = MIO.liftIO $ B.Properties.getObjectPropertyString obj "name"

-- | Construct a `GValueConstruct` with valid value for the “@name@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructThemingEngineName :: (IsThemingEngine o, MIO.MonadIO m) => T.Text -> m (GValueConstruct o)
constructThemingEngineName val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyString "name" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data ThemingEngineNamePropertyInfo
instance AttrInfo ThemingEngineNamePropertyInfo where
    type AttrAllowedOps ThemingEngineNamePropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint ThemingEngineNamePropertyInfo = IsThemingEngine
    type AttrSetTypeConstraint ThemingEngineNamePropertyInfo = (~) T.Text
    type AttrTransferTypeConstraint ThemingEngineNamePropertyInfo = (~) T.Text
    type AttrTransferType ThemingEngineNamePropertyInfo = T.Text
    type AttrGetType ThemingEngineNamePropertyInfo = (Maybe T.Text)
    type AttrLabel ThemingEngineNamePropertyInfo = "name"
    type AttrOrigin ThemingEngineNamePropertyInfo = ThemingEngine
    attrGet = getThemingEngineName
    attrSet = undefined
    attrTransfer _ v = do
        return v
    attrConstruct = constructThemingEngineName
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.name"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#g:attr:name"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList ThemingEngine
type instance O.AttributeList ThemingEngine = ThemingEngineAttributeList
type ThemingEngineAttributeList = ('[ '("name", ThemingEngineNamePropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
themingEngineName :: AttrLabelProxy "name"
themingEngineName = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList ThemingEngine = ThemingEngineSignalList
type ThemingEngineSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method ThemingEngine::get_background_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
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
--                 { rawDocText = Just "state to retrieve the color for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return value for the background color"
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_theming_engine_get_background_color" gtk_theming_engine_get_background_color :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    Ptr Gdk.RGBA.RGBA ->                    -- color : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    IO ()

{-# DEPRECATED themingEngineGetBackgroundColor ["(Since version 3.14)"] #-}
-- | Gets the background color for a given state.
-- 
-- /Since: 3.0/
themingEngineGetBackgroundColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to retrieve the color for
    -> m (Gdk.RGBA.RGBA)
themingEngineGetBackgroundColor engine state = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    let state' = gflagsToWord state
    color <- SP.callocBoxedBytes 32 :: IO (Ptr Gdk.RGBA.RGBA)
    gtk_theming_engine_get_background_color engine' state' color
    color' <- (wrapBoxed Gdk.RGBA.RGBA) color
    touchManagedPtr engine
    return color'

#if defined(ENABLE_OVERLOADING)
data ThemingEngineGetBackgroundColorMethodInfo
instance (signature ~ ([Gtk.Flags.StateFlags] -> m (Gdk.RGBA.RGBA)), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineGetBackgroundColorMethodInfo a signature where
    overloadedMethod = themingEngineGetBackgroundColor

instance O.OverloadedMethodInfo ThemingEngineGetBackgroundColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineGetBackgroundColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineGetBackgroundColor"
        })


#endif

-- method ThemingEngine::get_border
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
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
--                 { rawDocText = Just "state to retrieve the border for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "border"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Border" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return value for the border settings"
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_theming_engine_get_border" gtk_theming_engine_get_border :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    Ptr Gtk.Border.Border ->                -- border : TInterface (Name {namespace = "Gtk", name = "Border"})
    IO ()

{-# DEPRECATED themingEngineGetBorder ["(Since version 3.14)"] #-}
-- | Gets the border for a given state as a t'GI.Gtk.Structs.Border.Border'.
-- 
-- /Since: 3.0/
themingEngineGetBorder ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to retrieve the border for
    -> m (Gtk.Border.Border)
themingEngineGetBorder engine state = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    let state' = gflagsToWord state
    border <- SP.callocBoxedBytes 8 :: IO (Ptr Gtk.Border.Border)
    gtk_theming_engine_get_border engine' state' border
    border' <- (wrapBoxed Gtk.Border.Border) border
    touchManagedPtr engine
    return border'

#if defined(ENABLE_OVERLOADING)
data ThemingEngineGetBorderMethodInfo
instance (signature ~ ([Gtk.Flags.StateFlags] -> m (Gtk.Border.Border)), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineGetBorderMethodInfo a signature where
    overloadedMethod = themingEngineGetBorder

instance O.OverloadedMethodInfo ThemingEngineGetBorderMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineGetBorder",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineGetBorder"
        })


#endif

-- method ThemingEngine::get_border_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
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
--                 { rawDocText = Just "state to retrieve the color for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return value for the border color"
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_theming_engine_get_border_color" gtk_theming_engine_get_border_color :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    Ptr Gdk.RGBA.RGBA ->                    -- color : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    IO ()

{-# DEPRECATED themingEngineGetBorderColor ["(Since version 3.14)"] #-}
-- | Gets the border color for a given state.
-- 
-- /Since: 3.0/
themingEngineGetBorderColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to retrieve the color for
    -> m (Gdk.RGBA.RGBA)
themingEngineGetBorderColor engine state = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    let state' = gflagsToWord state
    color <- SP.callocBoxedBytes 32 :: IO (Ptr Gdk.RGBA.RGBA)
    gtk_theming_engine_get_border_color engine' state' color
    color' <- (wrapBoxed Gdk.RGBA.RGBA) color
    touchManagedPtr engine
    return color'

#if defined(ENABLE_OVERLOADING)
data ThemingEngineGetBorderColorMethodInfo
instance (signature ~ ([Gtk.Flags.StateFlags] -> m (Gdk.RGBA.RGBA)), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineGetBorderColorMethodInfo a signature where
    overloadedMethod = themingEngineGetBorderColor

instance O.OverloadedMethodInfo ThemingEngineGetBorderColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineGetBorderColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineGetBorderColor"
        })


#endif

-- method ThemingEngine::get_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
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
--                 { rawDocText = Just "state to retrieve the color for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return value for the foreground color"
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_theming_engine_get_color" gtk_theming_engine_get_color :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    Ptr Gdk.RGBA.RGBA ->                    -- color : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    IO ()

{-# DEPRECATED themingEngineGetColor ["(Since version 3.14)"] #-}
-- | Gets the foreground color for a given state.
-- 
-- /Since: 3.0/
themingEngineGetColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to retrieve the color for
    -> m (Gdk.RGBA.RGBA)
themingEngineGetColor engine state = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    let state' = gflagsToWord state
    color <- SP.callocBoxedBytes 32 :: IO (Ptr Gdk.RGBA.RGBA)
    gtk_theming_engine_get_color engine' state' color
    color' <- (wrapBoxed Gdk.RGBA.RGBA) color
    touchManagedPtr engine
    return color'

#if defined(ENABLE_OVERLOADING)
data ThemingEngineGetColorMethodInfo
instance (signature ~ ([Gtk.Flags.StateFlags] -> m (Gdk.RGBA.RGBA)), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineGetColorMethodInfo a signature where
    overloadedMethod = themingEngineGetColor

instance O.OverloadedMethodInfo ThemingEngineGetColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineGetColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineGetColor"
        })


#endif

-- method ThemingEngine::get_direction
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
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
--               (TInterface Name { namespace = "Gtk" , name = "TextDirection" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_theming_engine_get_direction" gtk_theming_engine_get_direction :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    IO CUInt

{-# DEPRECATED themingEngineGetDirection ["(Since version 3.8)","Use 'GI.Gtk.Objects.ThemingEngine.themingEngineGetState' and","  check for @/GTK_STATE_FLAG_DIR_LTR/@ and","  @/GTK_STATE_FLAG_DIR_RTL/@ instead."] #-}
-- | Returns the widget direction used for rendering.
-- 
-- /Since: 3.0/
themingEngineGetDirection ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> m Gtk.Enums.TextDirection
    -- ^ __Returns:__ the widget direction
themingEngineGetDirection engine = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    result <- gtk_theming_engine_get_direction engine'
    let result' = (toEnum . fromIntegral) result
    touchManagedPtr engine
    return result'

#if defined(ENABLE_OVERLOADING)
data ThemingEngineGetDirectionMethodInfo
instance (signature ~ (m Gtk.Enums.TextDirection), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineGetDirectionMethodInfo a signature where
    overloadedMethod = themingEngineGetDirection

instance O.OverloadedMethodInfo ThemingEngineGetDirectionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineGetDirection",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineGetDirection"
        })


#endif

-- method ThemingEngine::get_font
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
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
--                 { rawDocText = Just "state to retrieve the font for"
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
--               (TInterface
--                  Name { namespace = "Pango" , name = "FontDescription" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_theming_engine_get_font" gtk_theming_engine_get_font :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    IO (Ptr Pango.FontDescription.FontDescription)

{-# DEPRECATED themingEngineGetFont ["(Since version 3.8)","Use @/gtk_theming_engine_get()/@"] #-}
-- | Returns the font description for a given state.
-- 
-- /Since: 3.0/
themingEngineGetFont ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to retrieve the font for
    -> m Pango.FontDescription.FontDescription
    -- ^ __Returns:__ the t'GI.Pango.Structs.FontDescription.FontDescription' for the given
    --          state. This object is owned by GTK+ and should not be
    --          freed.
themingEngineGetFont engine state = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    let state' = gflagsToWord state
    result <- gtk_theming_engine_get_font engine' state'
    checkUnexpectedReturnNULL "themingEngineGetFont" result
    result' <- (newBoxed Pango.FontDescription.FontDescription) result
    touchManagedPtr engine
    return result'

#if defined(ENABLE_OVERLOADING)
data ThemingEngineGetFontMethodInfo
instance (signature ~ ([Gtk.Flags.StateFlags] -> m Pango.FontDescription.FontDescription), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineGetFontMethodInfo a signature where
    overloadedMethod = themingEngineGetFont

instance O.OverloadedMethodInfo ThemingEngineGetFontMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineGetFont",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineGetFont"
        })


#endif

-- method ThemingEngine::get_junction_sides
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
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
--               (TInterface Name { namespace = "Gtk" , name = "JunctionSides" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_theming_engine_get_junction_sides" gtk_theming_engine_get_junction_sides :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    IO CUInt

{-# DEPRECATED themingEngineGetJunctionSides ["(Since version 3.14)"] #-}
-- | Returns the widget direction used for rendering.
-- 
-- /Since: 3.0/
themingEngineGetJunctionSides ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> m [Gtk.Flags.JunctionSides]
    -- ^ __Returns:__ the widget direction
themingEngineGetJunctionSides engine = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    result <- gtk_theming_engine_get_junction_sides engine'
    let result' = wordToGFlags result
    touchManagedPtr engine
    return result'

#if defined(ENABLE_OVERLOADING)
data ThemingEngineGetJunctionSidesMethodInfo
instance (signature ~ (m [Gtk.Flags.JunctionSides]), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineGetJunctionSidesMethodInfo a signature where
    overloadedMethod = themingEngineGetJunctionSides

instance O.OverloadedMethodInfo ThemingEngineGetJunctionSidesMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineGetJunctionSides",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineGetJunctionSides"
        })


#endif

-- method ThemingEngine::get_margin
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
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
--                 { rawDocText = Just "state to retrieve the border for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "margin"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Border" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return value for the margin settings"
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_theming_engine_get_margin" gtk_theming_engine_get_margin :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    Ptr Gtk.Border.Border ->                -- margin : TInterface (Name {namespace = "Gtk", name = "Border"})
    IO ()

{-# DEPRECATED themingEngineGetMargin ["(Since version 3.14)"] #-}
-- | Gets the margin for a given state as a t'GI.Gtk.Structs.Border.Border'.
-- 
-- /Since: 3.0/
themingEngineGetMargin ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to retrieve the border for
    -> m (Gtk.Border.Border)
themingEngineGetMargin engine state = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    let state' = gflagsToWord state
    margin <- SP.callocBoxedBytes 8 :: IO (Ptr Gtk.Border.Border)
    gtk_theming_engine_get_margin engine' state' margin
    margin' <- (wrapBoxed Gtk.Border.Border) margin
    touchManagedPtr engine
    return margin'

#if defined(ENABLE_OVERLOADING)
data ThemingEngineGetMarginMethodInfo
instance (signature ~ ([Gtk.Flags.StateFlags] -> m (Gtk.Border.Border)), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineGetMarginMethodInfo a signature where
    overloadedMethod = themingEngineGetMargin

instance O.OverloadedMethodInfo ThemingEngineGetMarginMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineGetMargin",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineGetMargin"
        })


#endif

-- method ThemingEngine::get_padding
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
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
--                 { rawDocText = Just "state to retrieve the padding for"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "padding"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Border" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return value for the padding settings"
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_theming_engine_get_padding" gtk_theming_engine_get_padding :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    Ptr Gtk.Border.Border ->                -- padding : TInterface (Name {namespace = "Gtk", name = "Border"})
    IO ()

{-# DEPRECATED themingEngineGetPadding ["(Since version 3.14)"] #-}
-- | Gets the padding for a given state as a t'GI.Gtk.Structs.Border.Border'.
-- 
-- /Since: 3.0/
themingEngineGetPadding ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to retrieve the padding for
    -> m (Gtk.Border.Border)
themingEngineGetPadding engine state = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    let state' = gflagsToWord state
    padding <- SP.callocBoxedBytes 8 :: IO (Ptr Gtk.Border.Border)
    gtk_theming_engine_get_padding engine' state' padding
    padding' <- (wrapBoxed Gtk.Border.Border) padding
    touchManagedPtr engine
    return padding'

#if defined(ENABLE_OVERLOADING)
data ThemingEngineGetPaddingMethodInfo
instance (signature ~ ([Gtk.Flags.StateFlags] -> m (Gtk.Border.Border)), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineGetPaddingMethodInfo a signature where
    overloadedMethod = themingEngineGetPadding

instance O.OverloadedMethodInfo ThemingEngineGetPaddingMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineGetPadding",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineGetPadding"
        })


#endif

-- method ThemingEngine::get_path
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "WidgetPath" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_theming_engine_get_path" gtk_theming_engine_get_path :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    IO (Ptr Gtk.WidgetPath.WidgetPath)

{-# DEPRECATED themingEngineGetPath ["(Since version 3.14)"] #-}
-- | Returns the widget path used for style matching.
-- 
-- /Since: 3.0/
themingEngineGetPath ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> m Gtk.WidgetPath.WidgetPath
    -- ^ __Returns:__ A t'GI.Gtk.Structs.WidgetPath.WidgetPath'
themingEngineGetPath engine = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    result <- gtk_theming_engine_get_path engine'
    checkUnexpectedReturnNULL "themingEngineGetPath" result
    result' <- (newBoxed Gtk.WidgetPath.WidgetPath) result
    touchManagedPtr engine
    return result'

#if defined(ENABLE_OVERLOADING)
data ThemingEngineGetPathMethodInfo
instance (signature ~ (m Gtk.WidgetPath.WidgetPath), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineGetPathMethodInfo a signature where
    overloadedMethod = themingEngineGetPath

instance O.OverloadedMethodInfo ThemingEngineGetPathMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineGetPath",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineGetPath"
        })


#endif

-- method ThemingEngine::get_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "property"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the property name" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "state to retrieve the value for"
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
--                 { rawDocText =
--                     Just
--                       "return location for the property value,\n        you must free this memory using g_value_unset() once you are\n        done with it."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = True
--           , transfer = TransferEverything
--           }
--       ]
-- Lengths: []
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_theming_engine_get_property" gtk_theming_engine_get_property :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    CString ->                              -- property : TBasicType TUTF8
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateFlags"})
    Ptr GValue ->                           -- value : TGValue
    IO ()

{-# DEPRECATED themingEngineGetProperty ["(Since version 3.14)"] #-}
-- | Gets a property value as retrieved from the style settings that apply
-- to the currently rendered element.
-- 
-- /Since: 3.0/
themingEngineGetProperty ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> T.Text
    -- ^ /@property@/: the property name
    -> [Gtk.Flags.StateFlags]
    -- ^ /@state@/: state to retrieve the value for
    -> m (GValue)
themingEngineGetProperty engine property state = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    property' <- textToCString property
    let state' = gflagsToWord state
    value <- SP.callocBytes 24 :: IO (Ptr GValue)
    gtk_theming_engine_get_property engine' property' state' value
    value' <- B.GValue.wrapGValuePtr value
    touchManagedPtr engine
    freeMem property'
    return value'

#if defined(ENABLE_OVERLOADING)
data ThemingEngineGetPropertyMethodInfo
instance (signature ~ (T.Text -> [Gtk.Flags.StateFlags] -> m (GValue)), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineGetPropertyMethodInfo a signature where
    overloadedMethod = themingEngineGetProperty

instance O.OverloadedMethodInfo ThemingEngineGetPropertyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineGetProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineGetProperty"
        })


#endif

-- method ThemingEngine::get_screen
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
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
-- returnType: Just (TInterface Name { namespace = "Gdk" , name = "Screen" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_theming_engine_get_screen" gtk_theming_engine_get_screen :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    IO (Ptr Gdk.Screen.Screen)

{-# DEPRECATED themingEngineGetScreen ["(Since version 3.14)"] #-}
-- | Returns the t'GI.Gdk.Objects.Screen.Screen' to which /@engine@/ currently rendering to.
themingEngineGetScreen ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> m (Maybe Gdk.Screen.Screen)
    -- ^ __Returns:__ a t'GI.Gdk.Objects.Screen.Screen', or 'P.Nothing'.
themingEngineGetScreen engine = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    result <- gtk_theming_engine_get_screen engine'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject Gdk.Screen.Screen) result'
        return result''
    touchManagedPtr engine
    return maybeResult

#if defined(ENABLE_OVERLOADING)
data ThemingEngineGetScreenMethodInfo
instance (signature ~ (m (Maybe Gdk.Screen.Screen)), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineGetScreenMethodInfo a signature where
    overloadedMethod = themingEngineGetScreen

instance O.OverloadedMethodInfo ThemingEngineGetScreenMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineGetScreen",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineGetScreen"
        })


#endif

-- method ThemingEngine::get_state
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
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
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "StateFlags" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_theming_engine_get_state" gtk_theming_engine_get_state :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    IO CUInt

{-# DEPRECATED themingEngineGetState ["(Since version 3.14)"] #-}
-- | returns the state used when rendering.
-- 
-- /Since: 3.0/
themingEngineGetState ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> m [Gtk.Flags.StateFlags]
    -- ^ __Returns:__ the state flags
themingEngineGetState engine = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    result <- gtk_theming_engine_get_state engine'
    let result' = wordToGFlags result
    touchManagedPtr engine
    return result'

#if defined(ENABLE_OVERLOADING)
data ThemingEngineGetStateMethodInfo
instance (signature ~ (m [Gtk.Flags.StateFlags]), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineGetStateMethodInfo a signature where
    overloadedMethod = themingEngineGetState

instance O.OverloadedMethodInfo ThemingEngineGetStateMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineGetState",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineGetState"
        })


#endif

-- method ThemingEngine::get_style_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "property_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the name of the widget style property"
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
--                 { rawDocText =
--                     Just
--                       "Return location for the property value, free with\n        g_value_unset() after use."
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
-- returnType: Nothing
-- throws : False
-- Skip return : False

foreign import ccall "gtk_theming_engine_get_style_property" gtk_theming_engine_get_style_property :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    CString ->                              -- property_name : TBasicType TUTF8
    Ptr GValue ->                           -- value : TGValue
    IO ()

{-# DEPRECATED themingEngineGetStyleProperty ["(Since version 3.14)"] #-}
-- | Gets the value for a widget style property.
-- 
-- /Since: 3.0/
themingEngineGetStyleProperty ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> T.Text
    -- ^ /@propertyName@/: the name of the widget style property
    -> m (GValue)
themingEngineGetStyleProperty engine propertyName = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    propertyName' <- textToCString propertyName
    value <- SP.callocBytes 24 :: IO (Ptr GValue)
    gtk_theming_engine_get_style_property engine' propertyName' value
    value' <- B.GValue.wrapGValuePtr value
    touchManagedPtr engine
    freeMem propertyName'
    return value'

#if defined(ENABLE_OVERLOADING)
data ThemingEngineGetStylePropertyMethodInfo
instance (signature ~ (T.Text -> m (GValue)), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineGetStylePropertyMethodInfo a signature where
    overloadedMethod = themingEngineGetStyleProperty

instance O.OverloadedMethodInfo ThemingEngineGetStylePropertyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineGetStyleProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineGetStyleProperty"
        })


#endif

-- method ThemingEngine::has_class
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "style_class"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "class name to look up"
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

foreign import ccall "gtk_theming_engine_has_class" gtk_theming_engine_has_class :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    CString ->                              -- style_class : TBasicType TUTF8
    IO CInt

{-# DEPRECATED themingEngineHasClass ["(Since version 3.14)"] #-}
-- | Returns 'P.True' if the currently rendered contents have
-- defined the given class name.
-- 
-- /Since: 3.0/
themingEngineHasClass ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> T.Text
    -- ^ /@styleClass@/: class name to look up
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@engine@/ has /@className@/ defined
themingEngineHasClass engine styleClass = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    styleClass' <- textToCString styleClass
    result <- gtk_theming_engine_has_class engine' styleClass'
    let result' = (/= 0) result
    touchManagedPtr engine
    freeMem styleClass'
    return result'

#if defined(ENABLE_OVERLOADING)
data ThemingEngineHasClassMethodInfo
instance (signature ~ (T.Text -> m Bool), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineHasClassMethodInfo a signature where
    overloadedMethod = themingEngineHasClass

instance O.OverloadedMethodInfo ThemingEngineHasClassMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineHasClass",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineHasClass"
        })


#endif

-- method ThemingEngine::has_region
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "style_region"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a region name" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "RegionFlags" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for region flags"
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

foreign import ccall "gtk_theming_engine_has_region" gtk_theming_engine_has_region :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    CString ->                              -- style_region : TBasicType TUTF8
    Ptr CUInt ->                            -- flags : TInterface (Name {namespace = "Gtk", name = "RegionFlags"})
    IO CInt

{-# DEPRECATED themingEngineHasRegion ["(Since version 3.14)"] #-}
-- | Returns 'P.True' if the currently rendered contents have the
-- region defined. If /@flagsReturn@/ is not 'P.Nothing', it is set
-- to the flags affecting the region.
-- 
-- /Since: 3.0/
themingEngineHasRegion ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> T.Text
    -- ^ /@styleRegion@/: a region name
    -> m ((Bool, [Gtk.Flags.RegionFlags]))
    -- ^ __Returns:__ 'P.True' if region is defined
themingEngineHasRegion engine styleRegion = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    styleRegion' <- textToCString styleRegion
    flags <- allocMem :: IO (Ptr CUInt)
    result <- gtk_theming_engine_has_region engine' styleRegion' flags
    let result' = (/= 0) result
    flags' <- peek flags
    let flags'' = wordToGFlags flags'
    touchManagedPtr engine
    freeMem styleRegion'
    freeMem flags
    return (result', flags'')

#if defined(ENABLE_OVERLOADING)
data ThemingEngineHasRegionMethodInfo
instance (signature ~ (T.Text -> m ((Bool, [Gtk.Flags.RegionFlags]))), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineHasRegionMethodInfo a signature where
    overloadedMethod = themingEngineHasRegion

instance O.OverloadedMethodInfo ThemingEngineHasRegionMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineHasRegion",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineHasRegion"
        })


#endif

-- method ThemingEngine::lookup_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "color_name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "color name to lookup"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "color"
--           , argType = TInterface Name { namespace = "Gdk" , name = "RGBA" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Return location for the looked up color"
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

foreign import ccall "gtk_theming_engine_lookup_color" gtk_theming_engine_lookup_color :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    CString ->                              -- color_name : TBasicType TUTF8
    Ptr Gdk.RGBA.RGBA ->                    -- color : TInterface (Name {namespace = "Gdk", name = "RGBA"})
    IO CInt

{-# DEPRECATED themingEngineLookupColor ["(Since version 3.14)"] #-}
-- | Looks up and resolves a color name in the current style’s color map.
-- 
-- /Since: 3.0/
themingEngineLookupColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> T.Text
    -- ^ /@colorName@/: color name to lookup
    -> m ((Bool, Gdk.RGBA.RGBA))
    -- ^ __Returns:__ 'P.True' if /@colorName@/ was found and resolved, 'P.False' otherwise
themingEngineLookupColor engine colorName = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    colorName' <- textToCString colorName
    color <- SP.callocBoxedBytes 32 :: IO (Ptr Gdk.RGBA.RGBA)
    result <- gtk_theming_engine_lookup_color engine' colorName' color
    let result' = (/= 0) result
    color' <- (wrapBoxed Gdk.RGBA.RGBA) color
    touchManagedPtr engine
    freeMem colorName'
    return (result', color')

#if defined(ENABLE_OVERLOADING)
data ThemingEngineLookupColorMethodInfo
instance (signature ~ (T.Text -> m ((Bool, Gdk.RGBA.RGBA))), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineLookupColorMethodInfo a signature where
    overloadedMethod = themingEngineLookupColor

instance O.OverloadedMethodInfo ThemingEngineLookupColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineLookupColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineLookupColor"
        })


#endif

-- method ThemingEngine::state_is_running
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "engine"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "ThemingEngine" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkThemingEngine"
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
--                 { rawDocText = Just "a widget state" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "progress"
--           , argType = TBasicType TDouble
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "return location for the transition progress"
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

foreign import ccall "gtk_theming_engine_state_is_running" gtk_theming_engine_state_is_running :: 
    Ptr ThemingEngine ->                    -- engine : TInterface (Name {namespace = "Gtk", name = "ThemingEngine"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateType"})
    Ptr CDouble ->                          -- progress : TBasicType TDouble
    IO CInt

{-# DEPRECATED themingEngineStateIsRunning ["(Since version 3.6)","Always returns 'P.False'"] #-}
-- | Returns 'P.True' if there is a transition animation running for the
-- current region (see 'GI.Gtk.Objects.StyleContext.styleContextPushAnimatableRegion').
-- 
-- If /@progress@/ is not 'P.Nothing', the animation progress will be returned
-- there, 0.0 means the state is closest to being 'P.False', while 1.0 means
-- it’s closest to being 'P.True'. This means transition animations will
-- run from 0 to 1 when /@state@/ is being set to 'P.True' and from 1 to 0 when
-- it’s being set to 'P.False'.
-- 
-- /Since: 3.0/
themingEngineStateIsRunning ::
    (B.CallStack.HasCallStack, MonadIO m, IsThemingEngine a) =>
    a
    -- ^ /@engine@/: a t'GI.Gtk.Objects.ThemingEngine.ThemingEngine'
    -> Gtk.Enums.StateType
    -- ^ /@state@/: a widget state
    -> m ((Bool, Double))
    -- ^ __Returns:__ 'P.True' if there is a running transition animation for /@state@/.
themingEngineStateIsRunning engine state = liftIO $ do
    engine' <- unsafeManagedPtrCastPtr engine
    let state' = (fromIntegral . fromEnum) state
    progress <- allocMem :: IO (Ptr CDouble)
    result <- gtk_theming_engine_state_is_running engine' state' progress
    let result' = (/= 0) result
    progress' <- peek progress
    let progress'' = realToFrac progress'
    touchManagedPtr engine
    freeMem progress
    return (result', progress'')

#if defined(ENABLE_OVERLOADING)
data ThemingEngineStateIsRunningMethodInfo
instance (signature ~ (Gtk.Enums.StateType -> m ((Bool, Double))), MonadIO m, IsThemingEngine a) => O.OverloadedMethod ThemingEngineStateIsRunningMethodInfo a signature where
    overloadedMethod = themingEngineStateIsRunning

instance O.OverloadedMethodInfo ThemingEngineStateIsRunningMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.ThemingEngine.themingEngineStateIsRunning",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-ThemingEngine.html#v:themingEngineStateIsRunning"
        })


#endif

-- method ThemingEngine::load
-- method type : MemberFunction
-- Args: [ Arg
--           { argCName = "name"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "Theme engine name to load"
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
--               (TInterface Name { namespace = "Gtk" , name = "ThemingEngine" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_theming_engine_load" gtk_theming_engine_load :: 
    CString ->                              -- name : TBasicType TUTF8
    IO (Ptr ThemingEngine)

{-# DEPRECATED themingEngineLoad ["(Since version 3.14)"] #-}
-- | Loads and initializes a theming engine module from the
-- standard directories.
themingEngineLoad ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    T.Text
    -- ^ /@name@/: Theme engine name to load
    -> m (Maybe ThemingEngine)
    -- ^ __Returns:__ A theming engine, or 'P.Nothing' if
    -- the engine /@name@/ doesn’t exist.
themingEngineLoad name = liftIO $ do
    name' <- textToCString name
    result <- gtk_theming_engine_load name'
    maybeResult <- convertIfNonNull result $ \result' -> do
        result'' <- (newObject ThemingEngine) result'
        return result''
    freeMem name'
    return maybeResult

#if defined(ENABLE_OVERLOADING)
#endif


