{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- A t'GI.Gtk.Objects.Style.Style' object encapsulates the information that provides the look and
-- feel for a widget.
-- 
-- > In GTK+ 3.0, GtkStyle has been deprecated and replaced by
-- > t'GI.Gtk.Objects.StyleContext.StyleContext'.
-- 
-- Each t'GI.Gtk.Objects.Widget.Widget' has an associated t'GI.Gtk.Objects.Style.Style' object that is used when
-- rendering that widget. Also, a t'GI.Gtk.Objects.Style.Style' holds information for the five
-- possible widget states though not every widget supports all five
-- states; see t'GI.Gtk.Enums.StateType'.
-- 
-- Usually the t'GI.Gtk.Objects.Style.Style' for a widget is the same as the default style that
-- is set by GTK+ and modified the theme engine.
-- 
-- Usually applications should not need to use or modify the t'GI.Gtk.Objects.Style.Style' of
-- their widgets.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.Style
    ( 

-- * Exported types
    Style(..)                               ,
    IsStyle                                 ,
    toStyle                                 ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [applyDefaultBackground]("GI.Gtk.Objects.Style#g:method:applyDefaultBackground"), [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [copy]("GI.Gtk.Objects.Style#g:method:copy"), [detach]("GI.Gtk.Objects.Style#g:method:detach"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [hasContext]("GI.Gtk.Objects.Style#g:method:hasContext"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [lookupColor]("GI.Gtk.Objects.Style#g:method:lookupColor"), [lookupIconSet]("GI.Gtk.Objects.Style#g:method:lookupIconSet"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [renderIcon]("GI.Gtk.Objects.Style#g:method:renderIcon"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getStyleProperty]("GI.Gtk.Objects.Style#g:method:getStyleProperty").
-- 
-- ==== Setters
-- [setBackground]("GI.Gtk.Objects.Style#g:method:setBackground"), [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveStyleMethod                      ,
#endif

-- ** applyDefaultBackground #method:applyDefaultBackground#

#if defined(ENABLE_OVERLOADING)
    StyleApplyDefaultBackgroundMethodInfo   ,
#endif
    styleApplyDefaultBackground             ,


-- ** copy #method:copy#

#if defined(ENABLE_OVERLOADING)
    StyleCopyMethodInfo                     ,
#endif
    styleCopy                               ,


-- ** detach #method:detach#

#if defined(ENABLE_OVERLOADING)
    StyleDetachMethodInfo                   ,
#endif
    styleDetach                             ,


-- ** getStyleProperty #method:getStyleProperty#

#if defined(ENABLE_OVERLOADING)
    StyleGetStylePropertyMethodInfo         ,
#endif
    styleGetStyleProperty                   ,


-- ** hasContext #method:hasContext#

#if defined(ENABLE_OVERLOADING)
    StyleHasContextMethodInfo               ,
#endif
    styleHasContext                         ,


-- ** lookupColor #method:lookupColor#

#if defined(ENABLE_OVERLOADING)
    StyleLookupColorMethodInfo              ,
#endif
    styleLookupColor                        ,


-- ** lookupIconSet #method:lookupIconSet#

#if defined(ENABLE_OVERLOADING)
    StyleLookupIconSetMethodInfo            ,
#endif
    styleLookupIconSet                      ,


-- ** new #method:new#

    styleNew                                ,


-- ** renderIcon #method:renderIcon#

#if defined(ENABLE_OVERLOADING)
    StyleRenderIconMethodInfo               ,
#endif
    styleRenderIcon                         ,


-- ** setBackground #method:setBackground#

#if defined(ENABLE_OVERLOADING)
    StyleSetBackgroundMethodInfo            ,
#endif
    styleSetBackground                      ,




 -- * Properties


-- ** context #attr:context#
-- | /No description available in the introspection data./

#if defined(ENABLE_OVERLOADING)
    StyleContextPropertyInfo                ,
#endif
    constructStyleContext                   ,
    getStyleContext                         ,
#if defined(ENABLE_OVERLOADING)
    styleContext                            ,
#endif




 -- * Signals


-- ** realize #signal:realize#

    StyleRealizeCallback                    ,
#if defined(ENABLE_OVERLOADING)
    StyleRealizeSignalInfo                  ,
#endif
    afterStyleRealize                       ,
    onStyleRealize                          ,


-- ** unrealize #signal:unrealize#

    StyleUnrealizeCallback                  ,
#if defined(ENABLE_OVERLOADING)
    StyleUnrealizeSignalInfo                ,
#endif
    afterStyleUnrealize                     ,
    onStyleUnrealize                        ,




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

import qualified GI.Cairo.Structs.Context as Cairo.Context
import qualified GI.GObject.Objects.Object as GObject.Object
import qualified GI.Gdk.Objects.Window as Gdk.Window
import qualified GI.Gdk.Structs.Color as Gdk.Color
import qualified GI.GdkPixbuf.Objects.Pixbuf as GdkPixbuf.Pixbuf
import {-# SOURCE #-} qualified GI.Gtk.Enums as Gtk.Enums
import {-# SOURCE #-} qualified GI.Gtk.Objects.StyleContext as Gtk.StyleContext
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget
import {-# SOURCE #-} qualified GI.Gtk.Structs.IconSet as Gtk.IconSet
import {-# SOURCE #-} qualified GI.Gtk.Structs.IconSource as Gtk.IconSource

-- | Memory-managed wrapper type.
newtype Style = Style (SP.ManagedPtr Style)
    deriving (Eq)

instance SP.ManagedPtrNewtype Style where
    toManagedPtr (Style p) = p

foreign import ccall "gtk_style_get_type"
    c_gtk_style_get_type :: IO B.Types.GType

instance B.Types.TypedObject Style where
    glibType = c_gtk_style_get_type

instance B.Types.GObject Style

-- | Type class for types which can be safely cast to `Style`, for instance with `toStyle`.
class (SP.GObject o, O.IsDescendantOf Style o) => IsStyle o
instance (SP.GObject o, O.IsDescendantOf Style o) => IsStyle o

instance O.HasParentTypes Style
type instance O.ParentTypes Style = '[GObject.Object.Object]

-- | Cast to `Style`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toStyle :: (MIO.MonadIO m, IsStyle o) => o -> m Style
toStyle = MIO.liftIO . B.ManagedPtr.unsafeCastTo Style

-- | Convert 'Style' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe Style) where
    gvalueGType_ = c_gtk_style_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr Style)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr Style)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject Style ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveStyleMethod (t :: Symbol) (o :: *) :: * where
    ResolveStyleMethod "applyDefaultBackground" o = StyleApplyDefaultBackgroundMethodInfo
    ResolveStyleMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveStyleMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveStyleMethod "copy" o = StyleCopyMethodInfo
    ResolveStyleMethod "detach" o = StyleDetachMethodInfo
    ResolveStyleMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveStyleMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveStyleMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveStyleMethod "hasContext" o = StyleHasContextMethodInfo
    ResolveStyleMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveStyleMethod "lookupColor" o = StyleLookupColorMethodInfo
    ResolveStyleMethod "lookupIconSet" o = StyleLookupIconSetMethodInfo
    ResolveStyleMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveStyleMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveStyleMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveStyleMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveStyleMethod "renderIcon" o = StyleRenderIconMethodInfo
    ResolveStyleMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveStyleMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveStyleMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveStyleMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveStyleMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveStyleMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveStyleMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveStyleMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveStyleMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveStyleMethod "getStyleProperty" o = StyleGetStylePropertyMethodInfo
    ResolveStyleMethod "setBackground" o = StyleSetBackgroundMethodInfo
    ResolveStyleMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveStyleMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveStyleMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveStyleMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveStyleMethod t Style, O.OverloadedMethod info Style p) => OL.IsLabel t (Style -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveStyleMethod t Style, O.OverloadedMethod info Style p, R.HasField t Style p) => R.HasField t Style p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveStyleMethod t Style, O.OverloadedMethodInfo info Style) => OL.IsLabel t (O.MethodProxy info Style) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal Style::realize
-- | Emitted when the style has been initialized for a particular
-- visual. Connecting to this signal is probably seldom
-- useful since most of the time applications and widgets only
-- deal with styles that have been already realized.
-- 
-- /Since: 2.4/
type StyleRealizeCallback =
    IO ()

type C_StyleRealizeCallback =
    Ptr Style ->                            -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_StyleRealizeCallback`.
foreign import ccall "wrapper"
    mk_StyleRealizeCallback :: C_StyleRealizeCallback -> IO (FunPtr C_StyleRealizeCallback)

wrap_StyleRealizeCallback :: 
    GObject a => (a -> StyleRealizeCallback) ->
    C_StyleRealizeCallback
wrap_StyleRealizeCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [realize](#signal:realize) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' style #realize callback
-- @
-- 
-- 
onStyleRealize :: (IsStyle a, MonadIO m) => a -> ((?self :: a) => StyleRealizeCallback) -> m SignalHandlerId
onStyleRealize obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_StyleRealizeCallback wrapped
    wrapped'' <- mk_StyleRealizeCallback wrapped'
    connectSignalFunPtr obj "realize" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [realize](#signal:realize) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' style #realize callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterStyleRealize :: (IsStyle a, MonadIO m) => a -> ((?self :: a) => StyleRealizeCallback) -> m SignalHandlerId
afterStyleRealize obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_StyleRealizeCallback wrapped
    wrapped'' <- mk_StyleRealizeCallback wrapped'
    connectSignalFunPtr obj "realize" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data StyleRealizeSignalInfo
instance SignalInfo StyleRealizeSignalInfo where
    type HaskellCallbackType StyleRealizeSignalInfo = StyleRealizeCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_StyleRealizeCallback cb
        cb'' <- mk_StyleRealizeCallback cb'
        connectSignalFunPtr obj "realize" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Style::realize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Style.html#g:signal:realize"})

#endif

-- signal Style::unrealize
-- | Emitted when the aspects of the style specific to a particular visual
-- is being cleaned up. A connection to this signal can be useful
-- if a widget wants to cache objects as object data on t'GI.Gtk.Objects.Style.Style'.
-- This signal provides a convenient place to free such cached objects.
-- 
-- /Since: 2.4/
type StyleUnrealizeCallback =
    IO ()

type C_StyleUnrealizeCallback =
    Ptr Style ->                            -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_StyleUnrealizeCallback`.
foreign import ccall "wrapper"
    mk_StyleUnrealizeCallback :: C_StyleUnrealizeCallback -> IO (FunPtr C_StyleUnrealizeCallback)

wrap_StyleUnrealizeCallback :: 
    GObject a => (a -> StyleUnrealizeCallback) ->
    C_StyleUnrealizeCallback
wrap_StyleUnrealizeCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [unrealize](#signal:unrealize) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' style #unrealize callback
-- @
-- 
-- 
onStyleUnrealize :: (IsStyle a, MonadIO m) => a -> ((?self :: a) => StyleUnrealizeCallback) -> m SignalHandlerId
onStyleUnrealize obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_StyleUnrealizeCallback wrapped
    wrapped'' <- mk_StyleUnrealizeCallback wrapped'
    connectSignalFunPtr obj "unrealize" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [unrealize](#signal:unrealize) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' style #unrealize callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterStyleUnrealize :: (IsStyle a, MonadIO m) => a -> ((?self :: a) => StyleUnrealizeCallback) -> m SignalHandlerId
afterStyleUnrealize obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_StyleUnrealizeCallback wrapped
    wrapped'' <- mk_StyleUnrealizeCallback wrapped'
    connectSignalFunPtr obj "unrealize" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data StyleUnrealizeSignalInfo
instance SignalInfo StyleUnrealizeSignalInfo where
    type HaskellCallbackType StyleUnrealizeSignalInfo = StyleUnrealizeCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_StyleUnrealizeCallback cb
        cb'' <- mk_StyleUnrealizeCallback cb'
        connectSignalFunPtr obj "unrealize" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Style::unrealize"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Style.html#g:signal:unrealize"})

#endif

-- VVV Prop "context"
   -- Type: TInterface (Name {namespace = "Gtk", name = "StyleContext"})
   -- Flags: [PropertyReadable,PropertyWritable,PropertyConstructOnly]
   -- Nullable: (Nothing,Nothing)

-- | Get the value of the “@context@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' style #context
-- @
getStyleContext :: (MonadIO m, IsStyle o) => o -> m (Maybe Gtk.StyleContext.StyleContext)
getStyleContext obj = MIO.liftIO $ B.Properties.getObjectPropertyObject obj "context" Gtk.StyleContext.StyleContext

-- | Construct a `GValueConstruct` with valid value for the “@context@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructStyleContext :: (IsStyle o, MIO.MonadIO m, Gtk.StyleContext.IsStyleContext a) => a -> m (GValueConstruct o)
constructStyleContext val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyObject "context" (P.Just val)

#if defined(ENABLE_OVERLOADING)
data StyleContextPropertyInfo
instance AttrInfo StyleContextPropertyInfo where
    type AttrAllowedOps StyleContextPropertyInfo = '[ 'AttrConstruct, 'AttrGet, 'AttrClear]
    type AttrBaseTypeConstraint StyleContextPropertyInfo = IsStyle
    type AttrSetTypeConstraint StyleContextPropertyInfo = Gtk.StyleContext.IsStyleContext
    type AttrTransferTypeConstraint StyleContextPropertyInfo = Gtk.StyleContext.IsStyleContext
    type AttrTransferType StyleContextPropertyInfo = Gtk.StyleContext.StyleContext
    type AttrGetType StyleContextPropertyInfo = (Maybe Gtk.StyleContext.StyleContext)
    type AttrLabel StyleContextPropertyInfo = "context"
    type AttrOrigin StyleContextPropertyInfo = Style
    attrGet = getStyleContext
    attrSet = undefined
    attrTransfer _ v = do
        unsafeCastTo Gtk.StyleContext.StyleContext v
    attrConstruct = constructStyleContext
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Style.context"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Style.html#g:attr:context"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList Style
type instance O.AttributeList Style = StyleAttributeList
type StyleAttributeList = ('[ '("context", StyleContextPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
styleContext :: AttrLabelProxy "context"
styleContext = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList Style = StyleSignalList
type StyleSignalList = ('[ '("notify", GObject.Object.ObjectNotifySignalInfo), '("realize", StyleRealizeSignalInfo), '("unrealize", StyleUnrealizeSignalInfo)] :: [(Symbol, *)])

#endif

-- method Style::new
-- method type : Constructor
-- Args: []
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Style" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_new" gtk_style_new :: 
    IO (Ptr Style)

{-# DEPRECATED styleNew ["(Since version 3.0)","Use t'GI.Gtk.Objects.StyleContext.StyleContext'"] #-}
-- | Creates a new t'GI.Gtk.Objects.Style.Style'.
styleNew ::
    (B.CallStack.HasCallStack, MonadIO m) =>
    m Style
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.Style.Style'.
styleNew  = liftIO $ do
    result <- gtk_style_new
    checkUnexpectedReturnNULL "styleNew" result
    result' <- (wrapObject Style) result
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method Style::apply_default_background
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "cr"
--           , argType =
--               TInterface Name { namespace = "cairo" , name = "Context" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "x"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "y"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "width"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "height"
--           , argType = TBasicType TInt
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation { rawDocText = Nothing , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_apply_default_background" gtk_style_apply_default_background :: 
    Ptr Style ->                            -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Cairo.Context.Context ->            -- cr : TInterface (Name {namespace = "cairo", name = "Context"})
    Ptr Gdk.Window.Window ->                -- window : TInterface (Name {namespace = "Gdk", name = "Window"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    Int32 ->                                -- x : TBasicType TInt
    Int32 ->                                -- y : TBasicType TInt
    Int32 ->                                -- width : TBasicType TInt
    Int32 ->                                -- height : TBasicType TInt
    IO ()

{-# DEPRECATED styleApplyDefaultBackground ["(Since version 3.0)","Use t'GI.Gtk.Objects.StyleContext.StyleContext' instead"] #-}
-- | /No description available in the introspection data./
styleApplyDefaultBackground ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyle a, Gdk.Window.IsWindow b) =>
    a
    -> Cairo.Context.Context
    -> b
    -> Gtk.Enums.StateType
    -> Int32
    -> Int32
    -> Int32
    -> Int32
    -> m ()
styleApplyDefaultBackground style cr window stateType x y width height = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    cr' <- unsafeManagedPtrGetPtr cr
    window' <- unsafeManagedPtrCastPtr window
    let stateType' = (fromIntegral . fromEnum) stateType
    gtk_style_apply_default_background style' cr' window' stateType' x y width height
    touchManagedPtr style
    touchManagedPtr cr
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleApplyDefaultBackgroundMethodInfo
instance (signature ~ (Cairo.Context.Context -> b -> Gtk.Enums.StateType -> Int32 -> Int32 -> Int32 -> Int32 -> m ()), MonadIO m, IsStyle a, Gdk.Window.IsWindow b) => O.OverloadedMethod StyleApplyDefaultBackgroundMethodInfo a signature where
    overloadedMethod = styleApplyDefaultBackground

instance O.OverloadedMethodInfo StyleApplyDefaultBackgroundMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Style.styleApplyDefaultBackground",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Style.html#v:styleApplyDefaultBackground"
        })


#endif

-- method Style::copy
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       ]
-- Lengths: []
-- returnType: Just (TInterface Name { namespace = "Gtk" , name = "Style" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_style_copy" gtk_style_copy :: 
    Ptr Style ->                            -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    IO (Ptr Style)

{-# DEPRECATED styleCopy ["(Since version 3.0)","Use t'GI.Gtk.Objects.StyleContext.StyleContext' instead"] #-}
-- | Creates a copy of the passed in t'GI.Gtk.Objects.Style.Style' object.
styleCopy ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyle a) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> m Style
    -- ^ __Returns:__ a copy of /@style@/
styleCopy style = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    result <- gtk_style_copy style'
    checkUnexpectedReturnNULL "styleCopy" result
    result' <- (wrapObject Style) result
    touchManagedPtr style
    return result'

#if defined(ENABLE_OVERLOADING)
data StyleCopyMethodInfo
instance (signature ~ (m Style), MonadIO m, IsStyle a) => O.OverloadedMethod StyleCopyMethodInfo a signature where
    overloadedMethod = styleCopy

instance O.OverloadedMethodInfo StyleCopyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Style.styleCopy",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Style.html#v:styleCopy"
        })


#endif

-- method Style::detach
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_detach" gtk_style_detach :: 
    Ptr Style ->                            -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    IO ()

{-# DEPRECATED styleDetach ["(Since version 3.0)","Use t'GI.Gtk.Objects.StyleContext.StyleContext' instead"] #-}
-- | Detaches a style from a window. If the style is not attached
-- to any windows anymore, it is unrealized. See @/gtk_style_attach()/@.
styleDetach ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyle a) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> m ()
styleDetach style = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    gtk_style_detach style'
    touchManagedPtr style
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleDetachMethodInfo
instance (signature ~ (m ()), MonadIO m, IsStyle a) => O.OverloadedMethod StyleDetachMethodInfo a signature where
    overloadedMethod = styleDetach

instance O.OverloadedMethodInfo StyleDetachMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Style.styleDetach",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Style.html#v:styleDetach"
        })


#endif

-- method Style::get_style_property
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "widget_type"
--           , argType = TBasicType TGType
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GType of a descendant of #GtkWidget"
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
--                 { rawDocText = Just "the name of the style property to get"
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
--                       "a #GValue where the value of the property being\n    queried will be stored"
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

foreign import ccall "gtk_style_get_style_property" gtk_style_get_style_property :: 
    Ptr Style ->                            -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    CGType ->                               -- widget_type : TBasicType TGType
    CString ->                              -- property_name : TBasicType TUTF8
    Ptr GValue ->                           -- value : TGValue
    IO ()

-- | Queries the value of a style property corresponding to a
-- widget class is in the given style.
-- 
-- /Since: 2.16/
styleGetStyleProperty ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyle a) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> GType
    -- ^ /@widgetType@/: the t'GType' of a descendant of t'GI.Gtk.Objects.Widget.Widget'
    -> T.Text
    -- ^ /@propertyName@/: the name of the style property to get
    -> m (GValue)
styleGetStyleProperty style widgetType propertyName = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    let widgetType' = gtypeToCGType widgetType
    propertyName' <- textToCString propertyName
    value <- SP.callocBytes 24 :: IO (Ptr GValue)
    gtk_style_get_style_property style' widgetType' propertyName' value
    value' <- B.GValue.wrapGValuePtr value
    touchManagedPtr style
    freeMem propertyName'
    return value'

#if defined(ENABLE_OVERLOADING)
data StyleGetStylePropertyMethodInfo
instance (signature ~ (GType -> T.Text -> m (GValue)), MonadIO m, IsStyle a) => O.OverloadedMethod StyleGetStylePropertyMethodInfo a signature where
    overloadedMethod = styleGetStyleProperty

instance O.OverloadedMethodInfo StyleGetStylePropertyMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Style.styleGetStyleProperty",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Style.html#v:styleGetStyleProperty"
        })


#endif

-- method Style::has_context
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_has_context" gtk_style_has_context :: 
    Ptr Style ->                            -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    IO CInt

-- | Returns whether /@style@/ has an associated t'GI.Gtk.Objects.StyleContext.StyleContext'.
-- 
-- /Since: 3.0/
styleHasContext ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyle a) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> m Bool
    -- ^ __Returns:__ 'P.True' if /@style@/ has a t'GI.Gtk.Objects.StyleContext.StyleContext'
styleHasContext style = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    result <- gtk_style_has_context style'
    let result' = (/= 0) result
    touchManagedPtr style
    return result'

#if defined(ENABLE_OVERLOADING)
data StyleHasContextMethodInfo
instance (signature ~ (m Bool), MonadIO m, IsStyle a) => O.OverloadedMethod StyleHasContextMethodInfo a signature where
    overloadedMethod = styleHasContext

instance O.OverloadedMethodInfo StyleHasContextMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Style.styleHasContext",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Style.html#v:styleHasContext"
        })


#endif

-- method Style::lookup_color
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "the name of the logical color to look up"
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
--           , argType = TInterface Name { namespace = "Gdk" , name = "Color" }
--           , direction = DirectionOut
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the #GdkColor to fill in"
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

foreign import ccall "gtk_style_lookup_color" gtk_style_lookup_color :: 
    Ptr Style ->                            -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    CString ->                              -- color_name : TBasicType TUTF8
    Ptr Gdk.Color.Color ->                  -- color : TInterface (Name {namespace = "Gdk", name = "Color"})
    IO CInt

{-# DEPRECATED styleLookupColor ["(Since version 3.0)","Use 'GI.Gtk.Objects.StyleContext.styleContextLookupColor' instead"] #-}
-- | Looks up /@colorName@/ in the style’s logical color mappings,
-- filling in /@color@/ and returning 'P.True' if found, otherwise
-- returning 'P.False'. Do not cache the found mapping, because
-- it depends on the t'GI.Gtk.Objects.Style.Style' and might change when a theme
-- switch occurs.
-- 
-- /Since: 2.10/
styleLookupColor ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyle a) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> T.Text
    -- ^ /@colorName@/: the name of the logical color to look up
    -> m ((Bool, Gdk.Color.Color))
    -- ^ __Returns:__ 'P.True' if the mapping was found.
styleLookupColor style colorName = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    colorName' <- textToCString colorName
    color <- SP.callocBoxedBytes 12 :: IO (Ptr Gdk.Color.Color)
    result <- gtk_style_lookup_color style' colorName' color
    let result' = (/= 0) result
    color' <- (wrapBoxed Gdk.Color.Color) color
    touchManagedPtr style
    freeMem colorName'
    return (result', color')

#if defined(ENABLE_OVERLOADING)
data StyleLookupColorMethodInfo
instance (signature ~ (T.Text -> m ((Bool, Gdk.Color.Color))), MonadIO m, IsStyle a) => O.OverloadedMethod StyleLookupColorMethodInfo a signature where
    overloadedMethod = styleLookupColor

instance O.OverloadedMethodInfo StyleLookupColorMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Style.styleLookupColor",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Style.html#v:styleLookupColor"
        })


#endif

-- method Style::lookup_icon_set
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_lookup_icon_set" gtk_style_lookup_icon_set :: 
    Ptr Style ->                            -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    CString ->                              -- stock_id : TBasicType TUTF8
    IO (Ptr Gtk.IconSet.IconSet)

{-# DEPRECATED styleLookupIconSet ["(Since version 3.0)","Use 'GI.Gtk.Objects.StyleContext.styleContextLookupIconSet' instead"] #-}
-- | Looks up /@stockId@/ in the icon factories associated with /@style@/
-- and the default icon factory, returning an icon set if found,
-- otherwise 'P.Nothing'.
styleLookupIconSet ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyle a) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> T.Text
    -- ^ /@stockId@/: an icon name
    -> m Gtk.IconSet.IconSet
    -- ^ __Returns:__ icon set of /@stockId@/
styleLookupIconSet style stockId = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    stockId' <- textToCString stockId
    result <- gtk_style_lookup_icon_set style' stockId'
    checkUnexpectedReturnNULL "styleLookupIconSet" result
    result' <- (newBoxed Gtk.IconSet.IconSet) result
    touchManagedPtr style
    freeMem stockId'
    return result'

#if defined(ENABLE_OVERLOADING)
data StyleLookupIconSetMethodInfo
instance (signature ~ (T.Text -> m Gtk.IconSet.IconSet), MonadIO m, IsStyle a) => O.OverloadedMethod StyleLookupIconSetMethodInfo a signature where
    overloadedMethod = styleLookupIconSet

instance O.OverloadedMethodInfo StyleLookupIconSetMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Style.styleLookupIconSet",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Style.html#v:styleLookupIconSet"
        })


#endif

-- method Style::render_icon
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "source"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "IconSource" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText =
--                     Just "the #GtkIconSource specifying the icon to render"
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "direction"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "TextDirection" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a text direction" , sinceVersion = Nothing }
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
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
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
--                       "the size to render the icon at (#GtkIconSize). A size of\n    `(GtkIconSize)-1` means render at the size of the source and\n    don\8217t scale."
--                 , sinceVersion = Nothing
--                 }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "the widget" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "detail"
--           , argType = TBasicType TUTF8
--           , direction = DirectionIn
--           , mayBeNull = True
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a style detail" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_render_icon" gtk_style_render_icon :: 
    Ptr Style ->                            -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Gtk.IconSource.IconSource ->        -- source : TInterface (Name {namespace = "Gtk", name = "IconSource"})
    CUInt ->                                -- direction : TInterface (Name {namespace = "Gtk", name = "TextDirection"})
    CUInt ->                                -- state : TInterface (Name {namespace = "Gtk", name = "StateType"})
    Int32 ->                                -- size : TBasicType TInt
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CString ->                              -- detail : TBasicType TUTF8
    IO (Ptr GdkPixbuf.Pixbuf.Pixbuf)

{-# DEPRECATED styleRenderIcon ["(Since version 3.0)","Use 'GI.Gtk.Functions.renderIconPixbuf' instead"] #-}
-- | Renders the icon specified by /@source@/ at the given /@size@/
-- according to the given parameters and returns the result in a
-- pixbuf.
styleRenderIcon ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyle a, Gtk.Widget.IsWidget b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> Gtk.IconSource.IconSource
    -- ^ /@source@/: the t'GI.Gtk.Structs.IconSource.IconSource' specifying the icon to render
    -> Gtk.Enums.TextDirection
    -- ^ /@direction@/: a text direction
    -> Gtk.Enums.StateType
    -- ^ /@state@/: a state
    -> Int32
    -- ^ /@size@/: the size to render the icon at (t'GI.Gtk.Enums.IconSize'). A size of
    --     @(GtkIconSize)-1@ means render at the size of the source and
    --     don’t scale.
    -> Maybe (b)
    -- ^ /@widget@/: the widget
    -> Maybe (T.Text)
    -- ^ /@detail@/: a style detail
    -> m GdkPixbuf.Pixbuf.Pixbuf
    -- ^ __Returns:__ a newly-created t'GI.GdkPixbuf.Objects.Pixbuf.Pixbuf'
    --     containing the rendered icon
styleRenderIcon style source direction state size widget detail = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    source' <- unsafeManagedPtrGetPtr source
    let direction' = (fromIntegral . fromEnum) direction
    let state' = (fromIntegral . fromEnum) state
    maybeWidget <- case widget of
        Nothing -> return nullPtr
        Just jWidget -> do
            jWidget' <- unsafeManagedPtrCastPtr jWidget
            return jWidget'
    maybeDetail <- case detail of
        Nothing -> return nullPtr
        Just jDetail -> do
            jDetail' <- textToCString jDetail
            return jDetail'
    result <- gtk_style_render_icon style' source' direction' state' size maybeWidget maybeDetail
    checkUnexpectedReturnNULL "styleRenderIcon" result
    result' <- (wrapObject GdkPixbuf.Pixbuf.Pixbuf) result
    touchManagedPtr style
    touchManagedPtr source
    whenJust widget touchManagedPtr
    freeMem maybeDetail
    return result'

#if defined(ENABLE_OVERLOADING)
data StyleRenderIconMethodInfo
instance (signature ~ (Gtk.IconSource.IconSource -> Gtk.Enums.TextDirection -> Gtk.Enums.StateType -> Int32 -> Maybe (b) -> Maybe (T.Text) -> m GdkPixbuf.Pixbuf.Pixbuf), MonadIO m, IsStyle a, Gtk.Widget.IsWidget b) => O.OverloadedMethod StyleRenderIconMethodInfo a signature where
    overloadedMethod = styleRenderIcon

instance O.OverloadedMethodInfo StyleRenderIconMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Style.styleRenderIcon",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Style.html#v:styleRenderIcon"
        })


#endif

-- method Style::set_background
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "style"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Style" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkStyle" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "window"
--           , argType = TInterface Name { namespace = "Gdk" , name = "Window" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GdkWindow" , sinceVersion = Nothing }
--           , argScope = ScopeTypeInvalid
--           , argClosure = -1
--           , argDestroy = -1
--           , argCallerAllocates = False
--           , transfer = TransferNothing
--           }
--       , Arg
--           { argCName = "state_type"
--           , argType =
--               TInterface Name { namespace = "Gtk" , name = "StateType" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a state" , sinceVersion = Nothing }
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

foreign import ccall "gtk_style_set_background" gtk_style_set_background :: 
    Ptr Style ->                            -- style : TInterface (Name {namespace = "Gtk", name = "Style"})
    Ptr Gdk.Window.Window ->                -- window : TInterface (Name {namespace = "Gdk", name = "Window"})
    CUInt ->                                -- state_type : TInterface (Name {namespace = "Gtk", name = "StateType"})
    IO ()

{-# DEPRECATED styleSetBackground ["(Since version 3.0)","Use 'GI.Gtk.Objects.StyleContext.styleContextSetBackground' instead"] #-}
-- | Sets the background of /@window@/ to the background color or pixmap
-- specified by /@style@/ for the given state.
styleSetBackground ::
    (B.CallStack.HasCallStack, MonadIO m, IsStyle a, Gdk.Window.IsWindow b) =>
    a
    -- ^ /@style@/: a t'GI.Gtk.Objects.Style.Style'
    -> b
    -- ^ /@window@/: a t'GI.Gdk.Objects.Window.Window'
    -> Gtk.Enums.StateType
    -- ^ /@stateType@/: a state
    -> m ()
styleSetBackground style window stateType = liftIO $ do
    style' <- unsafeManagedPtrCastPtr style
    window' <- unsafeManagedPtrCastPtr window
    let stateType' = (fromIntegral . fromEnum) stateType
    gtk_style_set_background style' window' stateType'
    touchManagedPtr style
    touchManagedPtr window
    return ()

#if defined(ENABLE_OVERLOADING)
data StyleSetBackgroundMethodInfo
instance (signature ~ (b -> Gtk.Enums.StateType -> m ()), MonadIO m, IsStyle a, Gdk.Window.IsWindow b) => O.OverloadedMethod StyleSetBackgroundMethodInfo a signature where
    overloadedMethod = styleSetBackground

instance O.OverloadedMethodInfo StyleSetBackgroundMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.Style.styleSetBackground",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-Style.html#v:styleSetBackground"
        })


#endif


