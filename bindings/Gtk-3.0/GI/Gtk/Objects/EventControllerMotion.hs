{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.EventControllerMotion.EventControllerMotion' is an event controller meant for situations
-- where you need to track the position of the pointer.
-- 
-- This object was added in 3.24.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.EventControllerMotion
    ( 

-- * Exported types
    EventControllerMotion(..)               ,
    IsEventControllerMotion                 ,
    toEventControllerMotion                 ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [handleEvent]("GI.Gtk.Objects.EventController#g:method:handleEvent"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reset]("GI.Gtk.Objects.EventController#g:method:reset"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getPropagationPhase]("GI.Gtk.Objects.EventController#g:method:getPropagationPhase"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getWidget]("GI.Gtk.Objects.EventController#g:method:getWidget").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setPropagationPhase]("GI.Gtk.Objects.EventController#g:method:setPropagationPhase"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveEventControllerMotionMethod      ,
#endif

-- ** new #method:new#

    eventControllerMotionNew                ,




 -- * Signals


-- ** enter #signal:enter#

    EventControllerMotionEnterCallback      ,
#if defined(ENABLE_OVERLOADING)
    EventControllerMotionEnterSignalInfo    ,
#endif
    afterEventControllerMotionEnter         ,
    onEventControllerMotionEnter            ,


-- ** leave #signal:leave#

    EventControllerMotionLeaveCallback      ,
#if defined(ENABLE_OVERLOADING)
    EventControllerMotionLeaveSignalInfo    ,
#endif
    afterEventControllerMotionLeave         ,
    onEventControllerMotionLeave            ,


-- ** motion #signal:motion#

    EventControllerMotionMotionCallback     ,
#if defined(ENABLE_OVERLOADING)
    EventControllerMotionMotionSignalInfo   ,
#endif
    afterEventControllerMotionMotion        ,
    onEventControllerMotionMotion           ,




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
import {-# SOURCE #-} qualified GI.Gtk.Objects.EventController as Gtk.EventController
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype EventControllerMotion = EventControllerMotion (SP.ManagedPtr EventControllerMotion)
    deriving (Eq)

instance SP.ManagedPtrNewtype EventControllerMotion where
    toManagedPtr (EventControllerMotion p) = p

foreign import ccall "gtk_event_controller_motion_get_type"
    c_gtk_event_controller_motion_get_type :: IO B.Types.GType

instance B.Types.TypedObject EventControllerMotion where
    glibType = c_gtk_event_controller_motion_get_type

instance B.Types.GObject EventControllerMotion

-- | Type class for types which can be safely cast to `EventControllerMotion`, for instance with `toEventControllerMotion`.
class (SP.GObject o, O.IsDescendantOf EventControllerMotion o) => IsEventControllerMotion o
instance (SP.GObject o, O.IsDescendantOf EventControllerMotion o) => IsEventControllerMotion o

instance O.HasParentTypes EventControllerMotion
type instance O.ParentTypes EventControllerMotion = '[Gtk.EventController.EventController, GObject.Object.Object]

-- | Cast to `EventControllerMotion`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toEventControllerMotion :: (MIO.MonadIO m, IsEventControllerMotion o) => o -> m EventControllerMotion
toEventControllerMotion = MIO.liftIO . B.ManagedPtr.unsafeCastTo EventControllerMotion

-- | Convert 'EventControllerMotion' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe EventControllerMotion) where
    gvalueGType_ = c_gtk_event_controller_motion_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr EventControllerMotion)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr EventControllerMotion)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject EventControllerMotion ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveEventControllerMotionMethod (t :: Symbol) (o :: *) :: * where
    ResolveEventControllerMotionMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveEventControllerMotionMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveEventControllerMotionMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveEventControllerMotionMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveEventControllerMotionMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveEventControllerMotionMethod "handleEvent" o = Gtk.EventController.EventControllerHandleEventMethodInfo
    ResolveEventControllerMotionMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveEventControllerMotionMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveEventControllerMotionMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveEventControllerMotionMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveEventControllerMotionMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveEventControllerMotionMethod "reset" o = Gtk.EventController.EventControllerResetMethodInfo
    ResolveEventControllerMotionMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveEventControllerMotionMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveEventControllerMotionMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveEventControllerMotionMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveEventControllerMotionMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveEventControllerMotionMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveEventControllerMotionMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveEventControllerMotionMethod "getPropagationPhase" o = Gtk.EventController.EventControllerGetPropagationPhaseMethodInfo
    ResolveEventControllerMotionMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveEventControllerMotionMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveEventControllerMotionMethod "getWidget" o = Gtk.EventController.EventControllerGetWidgetMethodInfo
    ResolveEventControllerMotionMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveEventControllerMotionMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveEventControllerMotionMethod "setPropagationPhase" o = Gtk.EventController.EventControllerSetPropagationPhaseMethodInfo
    ResolveEventControllerMotionMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveEventControllerMotionMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveEventControllerMotionMethod t EventControllerMotion, O.OverloadedMethod info EventControllerMotion p) => OL.IsLabel t (EventControllerMotion -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveEventControllerMotionMethod t EventControllerMotion, O.OverloadedMethod info EventControllerMotion p, R.HasField t EventControllerMotion p) => R.HasField t EventControllerMotion p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveEventControllerMotionMethod t EventControllerMotion, O.OverloadedMethodInfo info EventControllerMotion) => OL.IsLabel t (O.MethodProxy info EventControllerMotion) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal EventControllerMotion::enter
-- | Signals that the pointer has entered the widget.
type EventControllerMotionEnterCallback =
    Double
    -- ^ /@x@/: the x coordinate
    -> Double
    -- ^ /@y@/: the y coordinate
    -> IO ()

type C_EventControllerMotionEnterCallback =
    Ptr EventControllerMotion ->            -- object
    CDouble ->
    CDouble ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EventControllerMotionEnterCallback`.
foreign import ccall "wrapper"
    mk_EventControllerMotionEnterCallback :: C_EventControllerMotionEnterCallback -> IO (FunPtr C_EventControllerMotionEnterCallback)

wrap_EventControllerMotionEnterCallback :: 
    GObject a => (a -> EventControllerMotionEnterCallback) ->
    C_EventControllerMotionEnterCallback
wrap_EventControllerMotionEnterCallback gi'cb gi'selfPtr x y _ = do
    let x' = realToFrac x
    let y' = realToFrac y
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  x' y'


-- | Connect a signal handler for the [enter](#signal:enter) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' eventControllerMotion #enter callback
-- @
-- 
-- 
onEventControllerMotionEnter :: (IsEventControllerMotion a, MonadIO m) => a -> ((?self :: a) => EventControllerMotionEnterCallback) -> m SignalHandlerId
onEventControllerMotionEnter obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerMotionEnterCallback wrapped
    wrapped'' <- mk_EventControllerMotionEnterCallback wrapped'
    connectSignalFunPtr obj "enter" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [enter](#signal:enter) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' eventControllerMotion #enter callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEventControllerMotionEnter :: (IsEventControllerMotion a, MonadIO m) => a -> ((?self :: a) => EventControllerMotionEnterCallback) -> m SignalHandlerId
afterEventControllerMotionEnter obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerMotionEnterCallback wrapped
    wrapped'' <- mk_EventControllerMotionEnterCallback wrapped'
    connectSignalFunPtr obj "enter" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EventControllerMotionEnterSignalInfo
instance SignalInfo EventControllerMotionEnterSignalInfo where
    type HaskellCallbackType EventControllerMotionEnterSignalInfo = EventControllerMotionEnterCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EventControllerMotionEnterCallback cb
        cb'' <- mk_EventControllerMotionEnterCallback cb'
        connectSignalFunPtr obj "enter" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerMotion::enter"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerMotion.html#g:signal:enter"})

#endif

-- signal EventControllerMotion::leave
-- | Signals that pointer has left the widget.
type EventControllerMotionLeaveCallback =
    IO ()

type C_EventControllerMotionLeaveCallback =
    Ptr EventControllerMotion ->            -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EventControllerMotionLeaveCallback`.
foreign import ccall "wrapper"
    mk_EventControllerMotionLeaveCallback :: C_EventControllerMotionLeaveCallback -> IO (FunPtr C_EventControllerMotionLeaveCallback)

wrap_EventControllerMotionLeaveCallback :: 
    GObject a => (a -> EventControllerMotionLeaveCallback) ->
    C_EventControllerMotionLeaveCallback
wrap_EventControllerMotionLeaveCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [leave](#signal:leave) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' eventControllerMotion #leave callback
-- @
-- 
-- 
onEventControllerMotionLeave :: (IsEventControllerMotion a, MonadIO m) => a -> ((?self :: a) => EventControllerMotionLeaveCallback) -> m SignalHandlerId
onEventControllerMotionLeave obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerMotionLeaveCallback wrapped
    wrapped'' <- mk_EventControllerMotionLeaveCallback wrapped'
    connectSignalFunPtr obj "leave" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [leave](#signal:leave) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' eventControllerMotion #leave callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEventControllerMotionLeave :: (IsEventControllerMotion a, MonadIO m) => a -> ((?self :: a) => EventControllerMotionLeaveCallback) -> m SignalHandlerId
afterEventControllerMotionLeave obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerMotionLeaveCallback wrapped
    wrapped'' <- mk_EventControllerMotionLeaveCallback wrapped'
    connectSignalFunPtr obj "leave" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EventControllerMotionLeaveSignalInfo
instance SignalInfo EventControllerMotionLeaveSignalInfo where
    type HaskellCallbackType EventControllerMotionLeaveSignalInfo = EventControllerMotionLeaveCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EventControllerMotionLeaveCallback cb
        cb'' <- mk_EventControllerMotionLeaveCallback cb'
        connectSignalFunPtr obj "leave" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerMotion::leave"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerMotion.html#g:signal:leave"})

#endif

-- signal EventControllerMotion::motion
-- | Emitted when the pointer moves inside the widget.
type EventControllerMotionMotionCallback =
    Double
    -- ^ /@x@/: the x coordinate
    -> Double
    -- ^ /@y@/: the y coordinate
    -> IO ()

type C_EventControllerMotionMotionCallback =
    Ptr EventControllerMotion ->            -- object
    CDouble ->
    CDouble ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EventControllerMotionMotionCallback`.
foreign import ccall "wrapper"
    mk_EventControllerMotionMotionCallback :: C_EventControllerMotionMotionCallback -> IO (FunPtr C_EventControllerMotionMotionCallback)

wrap_EventControllerMotionMotionCallback :: 
    GObject a => (a -> EventControllerMotionMotionCallback) ->
    C_EventControllerMotionMotionCallback
wrap_EventControllerMotionMotionCallback gi'cb gi'selfPtr x y _ = do
    let x' = realToFrac x
    let y' = realToFrac y
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  x' y'


-- | Connect a signal handler for the [motion](#signal:motion) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' eventControllerMotion #motion callback
-- @
-- 
-- 
onEventControllerMotionMotion :: (IsEventControllerMotion a, MonadIO m) => a -> ((?self :: a) => EventControllerMotionMotionCallback) -> m SignalHandlerId
onEventControllerMotionMotion obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerMotionMotionCallback wrapped
    wrapped'' <- mk_EventControllerMotionMotionCallback wrapped'
    connectSignalFunPtr obj "motion" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [motion](#signal:motion) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' eventControllerMotion #motion callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEventControllerMotionMotion :: (IsEventControllerMotion a, MonadIO m) => a -> ((?self :: a) => EventControllerMotionMotionCallback) -> m SignalHandlerId
afterEventControllerMotionMotion obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerMotionMotionCallback wrapped
    wrapped'' <- mk_EventControllerMotionMotionCallback wrapped'
    connectSignalFunPtr obj "motion" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EventControllerMotionMotionSignalInfo
instance SignalInfo EventControllerMotionMotionSignalInfo where
    type HaskellCallbackType EventControllerMotionMotionSignalInfo = EventControllerMotionMotionCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EventControllerMotionMotionCallback cb
        cb'' <- mk_EventControllerMotionMotionCallback cb'
        connectSignalFunPtr obj "motion" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerMotion::motion"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerMotion.html#g:signal:motion"})

#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList EventControllerMotion
type instance O.AttributeList EventControllerMotion = EventControllerMotionAttributeList
type EventControllerMotionAttributeList = ('[ '("propagationPhase", Gtk.EventController.EventControllerPropagationPhasePropertyInfo), '("widget", Gtk.EventController.EventControllerWidgetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList EventControllerMotion = EventControllerMotionSignalList
type EventControllerMotionSignalList = ('[ '("enter", EventControllerMotionEnterSignalInfo), '("leave", EventControllerMotionLeaveSignalInfo), '("motion", EventControllerMotionMotionSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo)] :: [(Symbol, *)])

#endif

-- method EventControllerMotion::new
-- method type : Constructor
-- Args: [ Arg
--           { argCName = "widget"
--           , argType = TInterface Name { namespace = "Gtk" , name = "Widget" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "a #GtkWidget" , sinceVersion = Nothing }
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
--                  Name { namespace = "Gtk" , name = "EventControllerMotion" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_event_controller_motion_new" gtk_event_controller_motion_new :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    IO (Ptr EventControllerMotion)

-- | Creates a new event controller that will handle motion events
-- for the given /@widget@/.
-- 
-- /Since: 3.24/
eventControllerMotionNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> m EventControllerMotion
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.EventControllerMotion.EventControllerMotion'
eventControllerMotionNew widget = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    result <- gtk_event_controller_motion_new widget'
    checkUnexpectedReturnNULL "eventControllerMotionNew" result
    result' <- (wrapObject EventControllerMotion) result
    touchManagedPtr widget
    return result'

#if defined(ENABLE_OVERLOADING)
#endif


