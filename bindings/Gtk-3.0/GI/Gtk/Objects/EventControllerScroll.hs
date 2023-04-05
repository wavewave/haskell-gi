{-# LANGUAGE ImplicitParams, RankNTypes, TypeApplications #-}


-- | Copyright  : Will Thompson and Iñaki García Etxebarria
-- License    : LGPL-2.1
-- Maintainer : Iñaki García Etxebarria
-- 
-- t'GI.Gtk.Objects.EventControllerScroll.EventControllerScroll' is an event controller meant to handle
-- scroll events from mice and touchpads. It is capable of handling
-- both discrete and continuous scroll events, abstracting them both
-- on the [EventControllerScroll::scroll]("GI.Gtk.Objects.EventControllerScroll#g:signal:scroll") signal (deltas in the
-- discrete case are multiples of 1).
-- 
-- In the case of continuous scroll events, t'GI.Gtk.Objects.EventControllerScroll.EventControllerScroll'
-- encloses all [EventControllerScroll::scroll]("GI.Gtk.Objects.EventControllerScroll#g:signal:scroll") events between two
-- [EventControllerScroll::scrollBegin]("GI.Gtk.Objects.EventControllerScroll#g:signal:scrollBegin") and [EventControllerScroll::scrollEnd]("GI.Gtk.Objects.EventControllerScroll#g:signal:scrollEnd")
-- signals.
-- 
-- The behavior of the event controller can be modified by the
-- flags given at creation time, or modified at a later point through
-- 'GI.Gtk.Objects.EventControllerScroll.eventControllerScrollSetFlags' (e.g. because the scrolling
-- conditions of the widget changed).
-- 
-- The controller can be set up to emit motion for either\/both vertical
-- and horizontal scroll events through @/GTK_EVENT_CONTROLLER_SCROLL_VERTICAL/@,
-- @/GTK_EVENT_CONTROLLER_SCROLL_HORIZONTAL/@ and @/GTK_EVENT_CONTROLLER_SCROLL_BOTH/@.
-- If any axis is disabled, the respective [EventControllerScroll::scroll]("GI.Gtk.Objects.EventControllerScroll#g:signal:scroll")
-- delta will be 0. Vertical scroll events will be translated to horizontal
-- motion for the devices incapable of horizontal scrolling.
-- 
-- The event controller can also be forced to emit discrete events on all devices
-- through @/GTK_EVENT_CONTROLLER_SCROLL_DISCRETE/@. This can be used to implement
-- discrete actions triggered through scroll events (e.g. switching across
-- combobox options).
-- 
-- The @/GTK_EVENT_CONTROLLER_SCROLL_KINETIC/@ flag toggles the emission of the
-- [EventControllerScroll::decelerate]("GI.Gtk.Objects.EventControllerScroll#g:signal:decelerate") signal, emitted at the end of scrolling
-- with two X\/Y velocity arguments that are consistent with the motion that
-- was received.
-- 
-- This object was added in 3.24.

#if (MIN_VERSION_haskell_gi_overloading(1,0,0) && !defined(__HADDOCK_VERSION__))
#define ENABLE_OVERLOADING
#endif

module GI.Gtk.Objects.EventControllerScroll
    ( 

-- * Exported types
    EventControllerScroll(..)               ,
    IsEventControllerScroll                 ,
    toEventControllerScroll                 ,


 -- * Methods
-- | 
-- 
--  === __Click to display all available methods, including inherited ones__
-- ==== Methods
-- [bindProperty]("GI.GObject.Objects.Object#g:method:bindProperty"), [bindPropertyFull]("GI.GObject.Objects.Object#g:method:bindPropertyFull"), [forceFloating]("GI.GObject.Objects.Object#g:method:forceFloating"), [freezeNotify]("GI.GObject.Objects.Object#g:method:freezeNotify"), [getv]("GI.GObject.Objects.Object#g:method:getv"), [handleEvent]("GI.Gtk.Objects.EventController#g:method:handleEvent"), [isFloating]("GI.GObject.Objects.Object#g:method:isFloating"), [notify]("GI.GObject.Objects.Object#g:method:notify"), [notifyByPspec]("GI.GObject.Objects.Object#g:method:notifyByPspec"), [ref]("GI.GObject.Objects.Object#g:method:ref"), [refSink]("GI.GObject.Objects.Object#g:method:refSink"), [reset]("GI.Gtk.Objects.EventController#g:method:reset"), [runDispose]("GI.GObject.Objects.Object#g:method:runDispose"), [stealData]("GI.GObject.Objects.Object#g:method:stealData"), [stealQdata]("GI.GObject.Objects.Object#g:method:stealQdata"), [thawNotify]("GI.GObject.Objects.Object#g:method:thawNotify"), [unref]("GI.GObject.Objects.Object#g:method:unref"), [watchClosure]("GI.GObject.Objects.Object#g:method:watchClosure").
-- 
-- ==== Getters
-- [getData]("GI.GObject.Objects.Object#g:method:getData"), [getFlags]("GI.Gtk.Objects.EventControllerScroll#g:method:getFlags"), [getPropagationPhase]("GI.Gtk.Objects.EventController#g:method:getPropagationPhase"), [getProperty]("GI.GObject.Objects.Object#g:method:getProperty"), [getQdata]("GI.GObject.Objects.Object#g:method:getQdata"), [getWidget]("GI.Gtk.Objects.EventController#g:method:getWidget").
-- 
-- ==== Setters
-- [setData]("GI.GObject.Objects.Object#g:method:setData"), [setDataFull]("GI.GObject.Objects.Object#g:method:setDataFull"), [setFlags]("GI.Gtk.Objects.EventControllerScroll#g:method:setFlags"), [setPropagationPhase]("GI.Gtk.Objects.EventController#g:method:setPropagationPhase"), [setProperty]("GI.GObject.Objects.Object#g:method:setProperty").

#if defined(ENABLE_OVERLOADING)
    ResolveEventControllerScrollMethod      ,
#endif

-- ** getFlags #method:getFlags#

#if defined(ENABLE_OVERLOADING)
    EventControllerScrollGetFlagsMethodInfo ,
#endif
    eventControllerScrollGetFlags           ,


-- ** new #method:new#

    eventControllerScrollNew                ,


-- ** setFlags #method:setFlags#

#if defined(ENABLE_OVERLOADING)
    EventControllerScrollSetFlagsMethodInfo ,
#endif
    eventControllerScrollSetFlags           ,




 -- * Properties


-- ** flags #attr:flags#
-- | The flags affecting event controller behavior
-- 
-- /Since: 3.24/

#if defined(ENABLE_OVERLOADING)
    EventControllerScrollFlagsPropertyInfo  ,
#endif
    constructEventControllerScrollFlags     ,
#if defined(ENABLE_OVERLOADING)
    eventControllerScrollFlags              ,
#endif
    getEventControllerScrollFlags           ,
    setEventControllerScrollFlags           ,




 -- * Signals


-- ** decelerate #signal:decelerate#

    EventControllerScrollDecelerateCallback ,
#if defined(ENABLE_OVERLOADING)
    EventControllerScrollDecelerateSignalInfo,
#endif
    afterEventControllerScrollDecelerate    ,
    onEventControllerScrollDecelerate       ,


-- ** scroll #signal:scroll#

    EventControllerScrollScrollCallback     ,
#if defined(ENABLE_OVERLOADING)
    EventControllerScrollScrollSignalInfo   ,
#endif
    afterEventControllerScrollScroll        ,
    onEventControllerScrollScroll           ,


-- ** scrollBegin #signal:scrollBegin#

    EventControllerScrollScrollBeginCallback,
#if defined(ENABLE_OVERLOADING)
    EventControllerScrollScrollBeginSignalInfo,
#endif
    afterEventControllerScrollScrollBegin   ,
    onEventControllerScrollScrollBegin      ,


-- ** scrollEnd #signal:scrollEnd#

    EventControllerScrollScrollEndCallback  ,
#if defined(ENABLE_OVERLOADING)
    EventControllerScrollScrollEndSignalInfo,
#endif
    afterEventControllerScrollScrollEnd     ,
    onEventControllerScrollScrollEnd        ,




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
import {-# SOURCE #-} qualified GI.Gtk.Flags as Gtk.Flags
import {-# SOURCE #-} qualified GI.Gtk.Objects.EventController as Gtk.EventController
import {-# SOURCE #-} qualified GI.Gtk.Objects.Widget as Gtk.Widget

-- | Memory-managed wrapper type.
newtype EventControllerScroll = EventControllerScroll (SP.ManagedPtr EventControllerScroll)
    deriving (Eq)

instance SP.ManagedPtrNewtype EventControllerScroll where
    toManagedPtr (EventControllerScroll p) = p

foreign import ccall "gtk_event_controller_scroll_get_type"
    c_gtk_event_controller_scroll_get_type :: IO B.Types.GType

instance B.Types.TypedObject EventControllerScroll where
    glibType = c_gtk_event_controller_scroll_get_type

instance B.Types.GObject EventControllerScroll

-- | Type class for types which can be safely cast to `EventControllerScroll`, for instance with `toEventControllerScroll`.
class (SP.GObject o, O.IsDescendantOf EventControllerScroll o) => IsEventControllerScroll o
instance (SP.GObject o, O.IsDescendantOf EventControllerScroll o) => IsEventControllerScroll o

instance O.HasParentTypes EventControllerScroll
type instance O.ParentTypes EventControllerScroll = '[Gtk.EventController.EventController, GObject.Object.Object]

-- | Cast to `EventControllerScroll`, for types for which this is known to be safe. For general casts, use `Data.GI.Base.ManagedPtr.castTo`.
toEventControllerScroll :: (MIO.MonadIO m, IsEventControllerScroll o) => o -> m EventControllerScroll
toEventControllerScroll = MIO.liftIO . B.ManagedPtr.unsafeCastTo EventControllerScroll

-- | Convert 'EventControllerScroll' to and from 'Data.GI.Base.GValue.GValue'. See 'Data.GI.Base.GValue.toGValue' and 'Data.GI.Base.GValue.fromGValue'.
instance B.GValue.IsGValue (Maybe EventControllerScroll) where
    gvalueGType_ = c_gtk_event_controller_scroll_get_type
    gvalueSet_ gv P.Nothing = B.GValue.set_object gv (FP.nullPtr :: FP.Ptr EventControllerScroll)
    gvalueSet_ gv (P.Just obj) = B.ManagedPtr.withManagedPtr obj (B.GValue.set_object gv)
    gvalueGet_ gv = do
        ptr <- B.GValue.get_object gv :: IO (FP.Ptr EventControllerScroll)
        if ptr /= FP.nullPtr
        then P.Just <$> B.ManagedPtr.newObject EventControllerScroll ptr
        else return P.Nothing
        
    

#if defined(ENABLE_OVERLOADING)
type family ResolveEventControllerScrollMethod (t :: Symbol) (o :: *) :: * where
    ResolveEventControllerScrollMethod "bindProperty" o = GObject.Object.ObjectBindPropertyMethodInfo
    ResolveEventControllerScrollMethod "bindPropertyFull" o = GObject.Object.ObjectBindPropertyFullMethodInfo
    ResolveEventControllerScrollMethod "forceFloating" o = GObject.Object.ObjectForceFloatingMethodInfo
    ResolveEventControllerScrollMethod "freezeNotify" o = GObject.Object.ObjectFreezeNotifyMethodInfo
    ResolveEventControllerScrollMethod "getv" o = GObject.Object.ObjectGetvMethodInfo
    ResolveEventControllerScrollMethod "handleEvent" o = Gtk.EventController.EventControllerHandleEventMethodInfo
    ResolveEventControllerScrollMethod "isFloating" o = GObject.Object.ObjectIsFloatingMethodInfo
    ResolveEventControllerScrollMethod "notify" o = GObject.Object.ObjectNotifyMethodInfo
    ResolveEventControllerScrollMethod "notifyByPspec" o = GObject.Object.ObjectNotifyByPspecMethodInfo
    ResolveEventControllerScrollMethod "ref" o = GObject.Object.ObjectRefMethodInfo
    ResolveEventControllerScrollMethod "refSink" o = GObject.Object.ObjectRefSinkMethodInfo
    ResolveEventControllerScrollMethod "reset" o = Gtk.EventController.EventControllerResetMethodInfo
    ResolveEventControllerScrollMethod "runDispose" o = GObject.Object.ObjectRunDisposeMethodInfo
    ResolveEventControllerScrollMethod "stealData" o = GObject.Object.ObjectStealDataMethodInfo
    ResolveEventControllerScrollMethod "stealQdata" o = GObject.Object.ObjectStealQdataMethodInfo
    ResolveEventControllerScrollMethod "thawNotify" o = GObject.Object.ObjectThawNotifyMethodInfo
    ResolveEventControllerScrollMethod "unref" o = GObject.Object.ObjectUnrefMethodInfo
    ResolveEventControllerScrollMethod "watchClosure" o = GObject.Object.ObjectWatchClosureMethodInfo
    ResolveEventControllerScrollMethod "getData" o = GObject.Object.ObjectGetDataMethodInfo
    ResolveEventControllerScrollMethod "getFlags" o = EventControllerScrollGetFlagsMethodInfo
    ResolveEventControllerScrollMethod "getPropagationPhase" o = Gtk.EventController.EventControllerGetPropagationPhaseMethodInfo
    ResolveEventControllerScrollMethod "getProperty" o = GObject.Object.ObjectGetPropertyMethodInfo
    ResolveEventControllerScrollMethod "getQdata" o = GObject.Object.ObjectGetQdataMethodInfo
    ResolveEventControllerScrollMethod "getWidget" o = Gtk.EventController.EventControllerGetWidgetMethodInfo
    ResolveEventControllerScrollMethod "setData" o = GObject.Object.ObjectSetDataMethodInfo
    ResolveEventControllerScrollMethod "setDataFull" o = GObject.Object.ObjectSetDataFullMethodInfo
    ResolveEventControllerScrollMethod "setFlags" o = EventControllerScrollSetFlagsMethodInfo
    ResolveEventControllerScrollMethod "setPropagationPhase" o = Gtk.EventController.EventControllerSetPropagationPhaseMethodInfo
    ResolveEventControllerScrollMethod "setProperty" o = GObject.Object.ObjectSetPropertyMethodInfo
    ResolveEventControllerScrollMethod l o = O.MethodResolutionFailed l o

instance (info ~ ResolveEventControllerScrollMethod t EventControllerScroll, O.OverloadedMethod info EventControllerScroll p) => OL.IsLabel t (EventControllerScroll -> p) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.overloadedMethod @info
#else
    fromLabel _ = O.overloadedMethod @info
#endif

#if MIN_VERSION_base(4,13,0)
instance (info ~ ResolveEventControllerScrollMethod t EventControllerScroll, O.OverloadedMethod info EventControllerScroll p, R.HasField t EventControllerScroll p) => R.HasField t EventControllerScroll p where
    getField = O.overloadedMethod @info

#endif

instance (info ~ ResolveEventControllerScrollMethod t EventControllerScroll, O.OverloadedMethodInfo info EventControllerScroll) => OL.IsLabel t (O.MethodProxy info EventControllerScroll) where
#if MIN_VERSION_base(4,10,0)
    fromLabel = O.MethodProxy
#else
    fromLabel _ = O.MethodProxy
#endif

#endif

-- signal EventControllerScroll::decelerate
-- | Emitted after scroll is finished if the @/GTK_EVENT_CONTROLLER_SCROLL_KINETIC/@
-- flag is set. /@velX@/ and /@velY@/ express the initial velocity that was
-- imprinted by the scroll events. /@velX@/ and /@velY@/ are expressed in
-- pixels\/ms.
type EventControllerScrollDecelerateCallback =
    Double
    -- ^ /@velX@/: X velocity
    -> Double
    -- ^ /@velY@/: Y velocity
    -> IO ()

type C_EventControllerScrollDecelerateCallback =
    Ptr EventControllerScroll ->            -- object
    CDouble ->
    CDouble ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EventControllerScrollDecelerateCallback`.
foreign import ccall "wrapper"
    mk_EventControllerScrollDecelerateCallback :: C_EventControllerScrollDecelerateCallback -> IO (FunPtr C_EventControllerScrollDecelerateCallback)

wrap_EventControllerScrollDecelerateCallback :: 
    GObject a => (a -> EventControllerScrollDecelerateCallback) ->
    C_EventControllerScrollDecelerateCallback
wrap_EventControllerScrollDecelerateCallback gi'cb gi'selfPtr velX velY _ = do
    let velX' = realToFrac velX
    let velY' = realToFrac velY
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  velX' velY'


-- | Connect a signal handler for the [decelerate](#signal:decelerate) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' eventControllerScroll #decelerate callback
-- @
-- 
-- 
onEventControllerScrollDecelerate :: (IsEventControllerScroll a, MonadIO m) => a -> ((?self :: a) => EventControllerScrollDecelerateCallback) -> m SignalHandlerId
onEventControllerScrollDecelerate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerScrollDecelerateCallback wrapped
    wrapped'' <- mk_EventControllerScrollDecelerateCallback wrapped'
    connectSignalFunPtr obj "decelerate" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [decelerate](#signal:decelerate) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' eventControllerScroll #decelerate callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEventControllerScrollDecelerate :: (IsEventControllerScroll a, MonadIO m) => a -> ((?self :: a) => EventControllerScrollDecelerateCallback) -> m SignalHandlerId
afterEventControllerScrollDecelerate obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerScrollDecelerateCallback wrapped
    wrapped'' <- mk_EventControllerScrollDecelerateCallback wrapped'
    connectSignalFunPtr obj "decelerate" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EventControllerScrollDecelerateSignalInfo
instance SignalInfo EventControllerScrollDecelerateSignalInfo where
    type HaskellCallbackType EventControllerScrollDecelerateSignalInfo = EventControllerScrollDecelerateCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EventControllerScrollDecelerateCallback cb
        cb'' <- mk_EventControllerScrollDecelerateCallback cb'
        connectSignalFunPtr obj "decelerate" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerScroll::decelerate"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerScroll.html#g:signal:decelerate"})

#endif

-- signal EventControllerScroll::scroll
-- | Signals that the widget should scroll by the
-- amount specified by /@dx@/ and /@dy@/.
type EventControllerScrollScrollCallback =
    Double
    -- ^ /@dx@/: X delta
    -> Double
    -- ^ /@dy@/: Y delta
    -> IO ()

type C_EventControllerScrollScrollCallback =
    Ptr EventControllerScroll ->            -- object
    CDouble ->
    CDouble ->
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EventControllerScrollScrollCallback`.
foreign import ccall "wrapper"
    mk_EventControllerScrollScrollCallback :: C_EventControllerScrollScrollCallback -> IO (FunPtr C_EventControllerScrollScrollCallback)

wrap_EventControllerScrollScrollCallback :: 
    GObject a => (a -> EventControllerScrollScrollCallback) ->
    C_EventControllerScrollScrollCallback
wrap_EventControllerScrollScrollCallback gi'cb gi'selfPtr dx dy _ = do
    let dx' = realToFrac dx
    let dy' = realToFrac dy
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self)  dx' dy'


-- | Connect a signal handler for the [scroll](#signal:scroll) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' eventControllerScroll #scroll callback
-- @
-- 
-- 
onEventControllerScrollScroll :: (IsEventControllerScroll a, MonadIO m) => a -> ((?self :: a) => EventControllerScrollScrollCallback) -> m SignalHandlerId
onEventControllerScrollScroll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerScrollScrollCallback wrapped
    wrapped'' <- mk_EventControllerScrollScrollCallback wrapped'
    connectSignalFunPtr obj "scroll" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [scroll](#signal:scroll) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' eventControllerScroll #scroll callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEventControllerScrollScroll :: (IsEventControllerScroll a, MonadIO m) => a -> ((?self :: a) => EventControllerScrollScrollCallback) -> m SignalHandlerId
afterEventControllerScrollScroll obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerScrollScrollCallback wrapped
    wrapped'' <- mk_EventControllerScrollScrollCallback wrapped'
    connectSignalFunPtr obj "scroll" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EventControllerScrollScrollSignalInfo
instance SignalInfo EventControllerScrollScrollSignalInfo where
    type HaskellCallbackType EventControllerScrollScrollSignalInfo = EventControllerScrollScrollCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EventControllerScrollScrollCallback cb
        cb'' <- mk_EventControllerScrollScrollCallback cb'
        connectSignalFunPtr obj "scroll" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerScroll::scroll"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerScroll.html#g:signal:scroll"})

#endif

-- signal EventControllerScroll::scroll-begin
-- | Signals that a new scrolling operation has begun. It will
-- only be emitted on devices capable of it.
type EventControllerScrollScrollBeginCallback =
    IO ()

type C_EventControllerScrollScrollBeginCallback =
    Ptr EventControllerScroll ->            -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EventControllerScrollScrollBeginCallback`.
foreign import ccall "wrapper"
    mk_EventControllerScrollScrollBeginCallback :: C_EventControllerScrollScrollBeginCallback -> IO (FunPtr C_EventControllerScrollScrollBeginCallback)

wrap_EventControllerScrollScrollBeginCallback :: 
    GObject a => (a -> EventControllerScrollScrollBeginCallback) ->
    C_EventControllerScrollScrollBeginCallback
wrap_EventControllerScrollScrollBeginCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [scrollBegin](#signal:scrollBegin) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' eventControllerScroll #scrollBegin callback
-- @
-- 
-- 
onEventControllerScrollScrollBegin :: (IsEventControllerScroll a, MonadIO m) => a -> ((?self :: a) => EventControllerScrollScrollBeginCallback) -> m SignalHandlerId
onEventControllerScrollScrollBegin obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerScrollScrollBeginCallback wrapped
    wrapped'' <- mk_EventControllerScrollScrollBeginCallback wrapped'
    connectSignalFunPtr obj "scroll-begin" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [scrollBegin](#signal:scrollBegin) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' eventControllerScroll #scrollBegin callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEventControllerScrollScrollBegin :: (IsEventControllerScroll a, MonadIO m) => a -> ((?self :: a) => EventControllerScrollScrollBeginCallback) -> m SignalHandlerId
afterEventControllerScrollScrollBegin obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerScrollScrollBeginCallback wrapped
    wrapped'' <- mk_EventControllerScrollScrollBeginCallback wrapped'
    connectSignalFunPtr obj "scroll-begin" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EventControllerScrollScrollBeginSignalInfo
instance SignalInfo EventControllerScrollScrollBeginSignalInfo where
    type HaskellCallbackType EventControllerScrollScrollBeginSignalInfo = EventControllerScrollScrollBeginCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EventControllerScrollScrollBeginCallback cb
        cb'' <- mk_EventControllerScrollScrollBeginCallback cb'
        connectSignalFunPtr obj "scroll-begin" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerScroll::scroll-begin"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerScroll.html#g:signal:scrollBegin"})

#endif

-- signal EventControllerScroll::scroll-end
-- | Signals that a new scrolling operation has finished. It will
-- only be emitted on devices capable of it.
type EventControllerScrollScrollEndCallback =
    IO ()

type C_EventControllerScrollScrollEndCallback =
    Ptr EventControllerScroll ->            -- object
    Ptr () ->                               -- user_data
    IO ()

-- | Generate a function pointer callable from C code, from a `C_EventControllerScrollScrollEndCallback`.
foreign import ccall "wrapper"
    mk_EventControllerScrollScrollEndCallback :: C_EventControllerScrollScrollEndCallback -> IO (FunPtr C_EventControllerScrollScrollEndCallback)

wrap_EventControllerScrollScrollEndCallback :: 
    GObject a => (a -> EventControllerScrollScrollEndCallback) ->
    C_EventControllerScrollScrollEndCallback
wrap_EventControllerScrollScrollEndCallback gi'cb gi'selfPtr _ = do
    B.ManagedPtr.withTransient gi'selfPtr $ \gi'self -> gi'cb (Coerce.coerce gi'self) 


-- | Connect a signal handler for the [scrollEnd](#signal:scrollEnd) signal, to be run before the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.on' eventControllerScroll #scrollEnd callback
-- @
-- 
-- 
onEventControllerScrollScrollEnd :: (IsEventControllerScroll a, MonadIO m) => a -> ((?self :: a) => EventControllerScrollScrollEndCallback) -> m SignalHandlerId
onEventControllerScrollScrollEnd obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerScrollScrollEndCallback wrapped
    wrapped'' <- mk_EventControllerScrollScrollEndCallback wrapped'
    connectSignalFunPtr obj "scroll-end" wrapped'' SignalConnectBefore Nothing

-- | Connect a signal handler for the [scrollEnd](#signal:scrollEnd) signal, to be run after the default handler.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Signals.after' eventControllerScroll #scrollEnd callback
-- @
-- 
-- 
-- 
-- By default the object invoking the signal is not passed to the callback.
-- If you need to access it, you can use the implit @?self@ parameter.
-- Note that this requires activating the @ImplicitParams@ GHC extension.
-- 
afterEventControllerScrollScrollEnd :: (IsEventControllerScroll a, MonadIO m) => a -> ((?self :: a) => EventControllerScrollScrollEndCallback) -> m SignalHandlerId
afterEventControllerScrollScrollEnd obj cb = liftIO $ do
    let wrapped self = let ?self = self in cb
    let wrapped' = wrap_EventControllerScrollScrollEndCallback wrapped
    wrapped'' <- mk_EventControllerScrollScrollEndCallback wrapped'
    connectSignalFunPtr obj "scroll-end" wrapped'' SignalConnectAfter Nothing


#if defined(ENABLE_OVERLOADING)
data EventControllerScrollScrollEndSignalInfo
instance SignalInfo EventControllerScrollScrollEndSignalInfo where
    type HaskellCallbackType EventControllerScrollScrollEndSignalInfo = EventControllerScrollScrollEndCallback
    connectSignal obj cb connectMode detail = do
        let cb' = wrap_EventControllerScrollScrollEndCallback cb
        cb'' <- mk_EventControllerScrollScrollEndCallback cb'
        connectSignalFunPtr obj "scroll-end" cb'' connectMode detail
    dbgSignalInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerScroll::scroll-end"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerScroll.html#g:signal:scrollEnd"})

#endif

-- VVV Prop "flags"
   -- Type: TInterface (Name {namespace = "Gtk", name = "EventControllerScrollFlags"})
   -- Flags: [PropertyReadable,PropertyWritable]
   -- Nullable: (Just False,Just False)

-- | Get the value of the “@flags@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.get' eventControllerScroll #flags
-- @
getEventControllerScrollFlags :: (MonadIO m, IsEventControllerScroll o) => o -> m [Gtk.Flags.EventControllerScrollFlags]
getEventControllerScrollFlags obj = MIO.liftIO $ B.Properties.getObjectPropertyFlags obj "flags"

-- | Set the value of the “@flags@” property.
-- When <https://github.com/haskell-gi/haskell-gi/wiki/Overloading overloading> is enabled, this is equivalent to
-- 
-- @
-- 'Data.GI.Base.Attributes.set' eventControllerScroll [ #flags 'Data.GI.Base.Attributes.:=' value ]
-- @
setEventControllerScrollFlags :: (MonadIO m, IsEventControllerScroll o) => o -> [Gtk.Flags.EventControllerScrollFlags] -> m ()
setEventControllerScrollFlags obj val = MIO.liftIO $ do
    B.Properties.setObjectPropertyFlags obj "flags" val

-- | Construct a `GValueConstruct` with valid value for the “@flags@” property. This is rarely needed directly, but it is used by `Data.GI.Base.Constructible.new`.
constructEventControllerScrollFlags :: (IsEventControllerScroll o, MIO.MonadIO m) => [Gtk.Flags.EventControllerScrollFlags] -> m (GValueConstruct o)
constructEventControllerScrollFlags val = MIO.liftIO $ do
    MIO.liftIO $ B.Properties.constructObjectPropertyFlags "flags" val

#if defined(ENABLE_OVERLOADING)
data EventControllerScrollFlagsPropertyInfo
instance AttrInfo EventControllerScrollFlagsPropertyInfo where
    type AttrAllowedOps EventControllerScrollFlagsPropertyInfo = '[ 'AttrSet, 'AttrConstruct, 'AttrGet]
    type AttrBaseTypeConstraint EventControllerScrollFlagsPropertyInfo = IsEventControllerScroll
    type AttrSetTypeConstraint EventControllerScrollFlagsPropertyInfo = (~) [Gtk.Flags.EventControllerScrollFlags]
    type AttrTransferTypeConstraint EventControllerScrollFlagsPropertyInfo = (~) [Gtk.Flags.EventControllerScrollFlags]
    type AttrTransferType EventControllerScrollFlagsPropertyInfo = [Gtk.Flags.EventControllerScrollFlags]
    type AttrGetType EventControllerScrollFlagsPropertyInfo = [Gtk.Flags.EventControllerScrollFlags]
    type AttrLabel EventControllerScrollFlagsPropertyInfo = "flags"
    type AttrOrigin EventControllerScrollFlagsPropertyInfo = EventControllerScroll
    attrGet = getEventControllerScrollFlags
    attrSet = setEventControllerScrollFlags
    attrTransfer _ v = do
        return v
    attrConstruct = constructEventControllerScrollFlags
    attrClear = undefined
    dbgAttrInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerScroll.flags"
        , O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerScroll.html#g:attr:flags"
        })
#endif

#if defined(ENABLE_OVERLOADING)
instance O.HasAttributeList EventControllerScroll
type instance O.AttributeList EventControllerScroll = EventControllerScrollAttributeList
type EventControllerScrollAttributeList = ('[ '("flags", EventControllerScrollFlagsPropertyInfo), '("propagationPhase", Gtk.EventController.EventControllerPropagationPhasePropertyInfo), '("widget", Gtk.EventController.EventControllerWidgetPropertyInfo)] :: [(Symbol, *)])
#endif

#if defined(ENABLE_OVERLOADING)
eventControllerScrollFlags :: AttrLabelProxy "flags"
eventControllerScrollFlags = AttrLabelProxy

#endif

#if defined(ENABLE_OVERLOADING)
type instance O.SignalList EventControllerScroll = EventControllerScrollSignalList
type EventControllerScrollSignalList = ('[ '("decelerate", EventControllerScrollDecelerateSignalInfo), '("notify", GObject.Object.ObjectNotifySignalInfo), '("scroll", EventControllerScrollScrollSignalInfo), '("scrollBegin", EventControllerScrollScrollBeginSignalInfo), '("scrollEnd", EventControllerScrollScrollEndSignalInfo)] :: [(Symbol, *)])

#endif

-- method EventControllerScroll::new
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
--       , Arg
--           { argCName = "flags"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "EventControllerScrollFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "behavior flags" , sinceVersion = Nothing }
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
--                  Name { namespace = "Gtk" , name = "EventControllerScroll" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_event_controller_scroll_new" gtk_event_controller_scroll_new :: 
    Ptr Gtk.Widget.Widget ->                -- widget : TInterface (Name {namespace = "Gtk", name = "Widget"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "EventControllerScrollFlags"})
    IO (Ptr EventControllerScroll)

-- | Creates a new event controller that will handle scroll events
-- for the given /@widget@/.
-- 
-- /Since: 3.24/
eventControllerScrollNew ::
    (B.CallStack.HasCallStack, MonadIO m, Gtk.Widget.IsWidget a) =>
    a
    -- ^ /@widget@/: a t'GI.Gtk.Objects.Widget.Widget'
    -> [Gtk.Flags.EventControllerScrollFlags]
    -- ^ /@flags@/: behavior flags
    -> m EventControllerScroll
    -- ^ __Returns:__ a new t'GI.Gtk.Objects.EventControllerScroll.EventControllerScroll'
eventControllerScrollNew widget flags = liftIO $ do
    widget' <- unsafeManagedPtrCastPtr widget
    let flags' = gflagsToWord flags
    result <- gtk_event_controller_scroll_new widget' flags'
    checkUnexpectedReturnNULL "eventControllerScrollNew" result
    result' <- (wrapObject EventControllerScroll) result
    touchManagedPtr widget
    return result'

#if defined(ENABLE_OVERLOADING)
#endif

-- method EventControllerScroll::get_flags
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "controller"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "EventControllerScroll" }
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
-- returnType: Just
--               (TInterface
--                  Name { namespace = "Gtk" , name = "EventControllerScrollFlags" })
-- throws : False
-- Skip return : False

foreign import ccall "gtk_event_controller_scroll_get_flags" gtk_event_controller_scroll_get_flags :: 
    Ptr EventControllerScroll ->            -- controller : TInterface (Name {namespace = "Gtk", name = "EventControllerScroll"})
    IO CUInt

-- | Gets the flags conditioning the scroll controller behavior.
-- 
-- /Since: 3.24/
eventControllerScrollGetFlags ::
    (B.CallStack.HasCallStack, MonadIO m, IsEventControllerScroll a) =>
    a
    -> m [Gtk.Flags.EventControllerScrollFlags]
    -- ^ __Returns:__ the controller flags.
eventControllerScrollGetFlags controller = liftIO $ do
    controller' <- unsafeManagedPtrCastPtr controller
    result <- gtk_event_controller_scroll_get_flags controller'
    let result' = wordToGFlags result
    touchManagedPtr controller
    return result'

#if defined(ENABLE_OVERLOADING)
data EventControllerScrollGetFlagsMethodInfo
instance (signature ~ (m [Gtk.Flags.EventControllerScrollFlags]), MonadIO m, IsEventControllerScroll a) => O.OverloadedMethod EventControllerScrollGetFlagsMethodInfo a signature where
    overloadedMethod = eventControllerScrollGetFlags

instance O.OverloadedMethodInfo EventControllerScrollGetFlagsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerScroll.eventControllerScrollGetFlags",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerScroll.html#v:eventControllerScrollGetFlags"
        })


#endif

-- method EventControllerScroll::set_flags
-- method type : OrdinaryMethod
-- Args: [ Arg
--           { argCName = "controller"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "EventControllerScroll" }
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
--           { argCName = "flags"
--           , argType =
--               TInterface
--                 Name { namespace = "Gtk" , name = "EventControllerScrollFlags" }
--           , direction = DirectionIn
--           , mayBeNull = False
--           , argDoc =
--               Documentation
--                 { rawDocText = Just "behavior flags" , sinceVersion = Nothing }
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

foreign import ccall "gtk_event_controller_scroll_set_flags" gtk_event_controller_scroll_set_flags :: 
    Ptr EventControllerScroll ->            -- controller : TInterface (Name {namespace = "Gtk", name = "EventControllerScroll"})
    CUInt ->                                -- flags : TInterface (Name {namespace = "Gtk", name = "EventControllerScrollFlags"})
    IO ()

-- | Sets the flags conditioning scroll controller behavior.
-- 
-- /Since: 3.24/
eventControllerScrollSetFlags ::
    (B.CallStack.HasCallStack, MonadIO m, IsEventControllerScroll a) =>
    a
    -> [Gtk.Flags.EventControllerScrollFlags]
    -- ^ /@flags@/: behavior flags
    -> m ()
eventControllerScrollSetFlags controller flags = liftIO $ do
    controller' <- unsafeManagedPtrCastPtr controller
    let flags' = gflagsToWord flags
    gtk_event_controller_scroll_set_flags controller' flags'
    touchManagedPtr controller
    return ()

#if defined(ENABLE_OVERLOADING)
data EventControllerScrollSetFlagsMethodInfo
instance (signature ~ ([Gtk.Flags.EventControllerScrollFlags] -> m ()), MonadIO m, IsEventControllerScroll a) => O.OverloadedMethod EventControllerScrollSetFlagsMethodInfo a signature where
    overloadedMethod = eventControllerScrollSetFlags

instance O.OverloadedMethodInfo EventControllerScrollSetFlagsMethodInfo a where
    overloadedMethodInfo = P.Just (O.ResolvedSymbolInfo {
        O.resolvedSymbolName = "GI.Gtk.Objects.EventControllerScroll.eventControllerScrollSetFlags",
        O.resolvedSymbolURL = "https://hackage.haskell.org/package/gi-gtk-3.0.41/docs/GI-Gtk-Objects-EventControllerScroll.html#v:eventControllerScrollSetFlags"
        })


#endif


